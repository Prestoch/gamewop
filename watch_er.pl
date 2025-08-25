#!/usr/bin/env perl
use strict;
use warnings;
use JSON::PP qw(decode_json encode_json);
use HTTP::Tiny;
use Time::HiRes qw(sleep);
use POSIX qw(strftime);
use Cwd qw(abs_path getcwd);
use File::Basename qw(dirname);
use File::Spec;

# ----- Paths and helpers -----
my $BASE_DIR = dirname(abs_path($0));
sub local_file { my ($f) = @_; return File::Spec->catfile($BASE_DIR, $f); }

# ----- Config -----
my $POLL_SECS = defined $ENV{WATCH_POLL_SECS} ? 0.0 + $ENV{WATCH_POLL_SECS} : 30;
my $SCRAPEDO_API_KEY = $ENV{SCRAPEDO_API_KEY} // '';
my $SCRAPEDO_ENDPOINT = $ENV{SCRAPEDO_ENDPOINT} // 'https://api.scrape.do';
my $SCRAPEDO_RENDER = $ENV{SCRAPEDO_RENDER} // '';

my $http = HTTP::Tiny->new(
  agent => 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0 Safari/537.36 Perl-HTTP::Tiny',
  timeout => 60,
);

sub url_encode { my ($s)=@_; $s//= ''; $s =~ s/([^A-Za-z0-9\-_.~])/sprintf("%%%02X", ord($1))/eg; return $s; }

sub http_get_text {
  my ($url) = @_;
  my $res = $http->get($url);
  return $res->{success} ? ($res->{content} || '') : '';
}

sub fetch_with_scrapedo {
  my ($url) = @_;
  return '' unless $SCRAPEDO_API_KEY;
  my $qs = 'token=' . url_encode($SCRAPEDO_API_KEY) . '&url=' . url_encode($url);
  $qs .= '&render=true' if $SCRAPEDO_RENDER && $SCRAPEDO_RENDER =~ /^(1|true|yes)$/i;
  my $api_url = $SCRAPEDO_ENDPOINT . '?' . $qs;
  return http_get_text($api_url);
}

sub fetch_url {
  my ($url) = @_;
  my $html = '';
  $html = fetch_with_scrapedo($url) if $SCRAPEDO_API_KEY;
  $html = http_get_text($url) unless $html;
  return $html // '';
}

# ----- Load settings -----
sub load_settings {
  my $path = local_file('settings.json');
  if (open my $fh, '<', $path) { local $/; my $txt = <$fh>; close $fh; my $s = eval{ decode_json($txt||'{}') }; return $@?{}:($s||{}); }
  return {};
}

# ----- Load cs.json (JS) -----
my (@HEROES, @HEROES_WR, @WIN_RATES);
sub load_cs {
  my $path = local_file('cs.json');
  open my $fh, '<', $path or die "Missing cs.json - generate it first";
  local $/; my $txt = <$fh>; close $fh;
  my ($h)  = $txt =~ /\bvar\s+heroes\s*=\s*(\[[\s\S]*?\])/s;
  my ($wr) = $txt =~ /\bheroes_wr\s*=\s*(\[[\s\S]*?\])/s;
  my ($mat)= $txt =~ /\bwin_rates\s*=\s*(\[[\s\S]*?\])/s;
  die "cs.json parse error: heroes" unless $h;
  die "cs.json parse error: heroes_wr" unless $wr;
  die "cs.json parse error: win_rates" unless $mat;
  my $j = JSON::PP->new;
  @HEROES     = @{ $j->decode($h) };
  @HEROES_WR  = @{ $j->decode($wr) };
  @WIN_RATES  = @{ $j->decode($mat) };
}

sub normalize_name {
  my ($name) = @_;
  $name //= '';
  $name =~ s/\s+/ /g; $name =~ s/^\s+|\s+$//g;
  $name =~ s/'//g; $name =~ s/\./ /g; $name =~ s/-/ /g; $name =~ tr/A-Z/a-z/;
  return $name;
}

my %ALIASES = (
  'wisp' => 'io',
  'furion' => "natures prophet",
  'outworld destroyer' => 'outworld devourer',
  'windrunner' => 'windranger',
  'doombringer' => 'doom',
  'nevermore' => 'shadow fiend',
  'skeleton king' => 'wraith king',
  'zuus' => 'zeus',
);

sub hero_index_by_name {
  my ($raw) = @_;
  my $key = normalize_name($raw);
  $key = $ALIASES{$key} if exists $ALIASES{$key};
  # heroes list already normalized without apostrophes in generation
  for (my $i=0; $i<@HEROES; $i++) {
    my $hn = normalize_name($HEROES[$i] // '');
    return $i if $hn eq $key;
  }
  # fallback: substring exact token match
  for (my $i=0; $i<@HEROES; $i++) {
    my $hn = normalize_name($HEROES[$i] // '');
    return $i if $hn eq $key || index($hn, $key) >= 0 || index($key, $hn) >= 0;
  }
  return -1;
}

# ----- Advantage helpers (mirror dotabuffcp.js) -----
my $ADV_C = 4000.0;
my $ADV_LAMBDA = 1.0;

sub adv_shrink { my ($adv, $n) = @_; my $nn = $n||0; return $adv * ($nn / ($nn + $ADV_C)); }
sub adv_clip   { my ($adv) = @_; return 10.0 * tanh($adv / 10.0); }
sub adv_weight { my ($adv, $n) = @_; return adv_clip(adv_shrink($adv, $n)); }
sub sigmoid    { my ($x) = @_; return 1.0/(1.0+exp(-$x)); }
sub logit      { my ($p) = @_; my $e=1e-6; $p = ($p<$e)?$e:(($p>1-$e)?(1-$e):$p); return log($p/(1-$p)); }

sub edge_adv_for {
  my ($a_idx, $b_idx) = @_;
  return 0 unless defined $WIN_RATES[$b_idx] && defined $WIN_RATES[$b_idx][$a_idx];
  my $edge = $WIN_RATES[$b_idx][$a_idx];
  my $raw = -1.0 * ($edge->[0] // 0); # flip sign
  my $n = 0 + ($edge->[2] // 0);
  return adv_weight($raw, $n);
}

sub team_score {
  my ($teamA, $teamB) = @_;
  # base log-odds from individual hero win rates
  my $score = 0.0;
  for my $id (@$teamA) {
    next unless defined $id && $id >= 0 && defined $HEROES_WR[$id];
    my $wr = 0.0 + $HEROES_WR[$id];
    $score += (logit($wr/100.0) - logit(0.5));
  }
  # matchup edges vs the other team
  for my $idA (@$teamA) {
    next unless defined $idA && $idA >= 0;
    my $sum = 0.0;
    for my $idB (@$teamB) { next unless defined $idB && $idB >= 0; $sum += edge_adv_for($idA, $idB); }
    $score += $ADV_LAMBDA * (-$sum);
  }
  return $score;
}

sub any_hero_adv_threshold {
  my ($enemy, $cond, $thr) = @_;
  return 0 unless defined $thr && ($cond||'') =~ /^(gt|lt)$/;
  my $best = ($cond eq 'gt') ? -1e9 : 1e9;
  my $hit = 0;
  for (my $i=0; $i<@HEROES; $i++) {
    my $sum = 0.0;
    for my $idB (@$enemy) { next unless defined $idB && $idB >= 0; $sum += edge_adv_for($i, $idB); }
    my $val = -$sum; # display orientation
    if ($cond eq 'gt') { $best = $val if $val > $best; $hit ||= ($val >= $thr); }
    else { $best = $val if $val < $best; $hit ||= ($val <= $thr); }
    last if $hit;
  }
  return $hit ? 1 : 0;
}

sub parse_match_urls_from_home {
  my ($html) = @_;
  my %seen; my @urls;
  while ($html =~ m{href\s*=\s*"(\/[^"\s]*match[^"\s]*)"}ig) {
    my $p = $1; next if $seen{$p}++;
    my $abs = ($p =~ /^https?:/) ? $p : ('https://cyberscore.live' . $p);
    push @urls, $abs;
  }
  return \@urls;
}

sub parse_picks_from_match_html {
  my ($html) = @_;
  # Heuristic: collect hero names from img alt/title
  my @names;
  while ($html =~ m{<(?:img|span|div)[^>]+(?:alt|title)\s*=\s*"([^"]+?)"[^>]*>}ig) {
    push @names, $1;
  }
  # Map to hero ids
  my @ids;
  for my $n (@names) {
    my $idx = hero_index_by_name($n);
    push @ids, $idx if $idx >= 0;
  }
  # Try to split into two teams of 5 by first 10 unique ids order
  my @uniq; my %seen;
  for my $id (@ids) { next if $seen{$id}++; push @uniq, $id; last if @uniq >= 10; }
  return ([],[]) unless @uniq >= 2;
  my @a = @uniq[0 .. (($#uniq >= 4)?4:$#uniq)];
  my @b = @uniq[(($#uniq >= 5)?5:scalar(@uniq)) .. (($#uniq >= 9)?9:$#uniq)];
  return (\@a, \@b);
}

sub send_email_via_sendmail {
  my (%args) = @_;
  my $to = $args{to}; my $from = $args{from}||''; my $subject = $args{subject}||'Watcher alert'; my $body = $args{body}||'';
  my $sendmail = '/usr/sbin/sendmail';
  return 0 unless $to && -x $sendmail;
  my $pid = open(my $mail, '|-', "$sendmail -t");
  return 0 unless $pid;
  print $mail "From: $from\n" if $from;
  print $mail "To: $to\n";
  print $mail "Subject: $subject\n\n$body\n";
  close $mail;
  return 1;
}

sub load_status { my $p = local_file('.watch_status.json'); if (open my $fh,'<',$p){ local $/; my $t=<$fh>; close $fh; my $j=eval{decode_json($t||'{}')}; return $@?{}:($j||{});} return {}; }
sub save_status { my ($st)=@_; my $p=local_file('.watch_status.json'); if (open my $fh,'>',$p){ print $fh encode_json($st||{}); close $fh; return 1 } return 0; }

sub main_loop {
  chdir $BASE_DIR;
  load_cs();
  my $st = load_status();
  $st->{state} = 'watching'; $st->{started_at} ||= time; $st->{last_poll} ||= 0; $st->{alerts} ||= 0; save_status($st);
  my $settings = load_settings();

  my $cond_logic = ($settings->{condition_logic}||'all') eq 'any' ? 'any' : 'all';
  my $ta = $settings->{total_adv} || {};
  my $ta_enabled = !defined $ta->{enabled} ? 1 : ($ta->{enabled} ? 1 : 0);
  my $ta_min = (defined $ta->{min} && $ta->{min} ne '') ? 0.0 + $ta->{min} : undef;
  my $ta_max = (defined $ta->{max} && $ta->{max} ne '') ? 0.0 + $ta->{max} : undef;
  # legacy migration (cond/threshold)
  if (!defined $ta_min && ($ta->{cond}||'') eq 'gt' && defined $ta->{threshold}) { $ta_min = 0.0 + $ta->{threshold}; }
  if (!defined $ta_max && ($ta->{cond}||'') eq 'lt' && defined $ta->{threshold}) { $ta_max = 0.0 + $ta->{threshold}; }

  my $ha = $settings->{hero_adv} || {};
  my $ha_enabled = !defined $ha->{enabled} ? 1 : ($ha->{enabled} ? 1 : 0);
  my $ha_cond = ($ha->{cond}||'gt') =~ /^(gt|lt)$/ ? $ha->{cond} : 'gt';
  my $ha_thr = defined $ha->{threshold} ? 0.0 + $ha->{threshold} : undef;

  my $watch_heroes_enabled = $settings->{watch_heroes_enabled} ? 1 : 0;
  my %watch_set = map { normalize_name($_)=>1 } @{ $settings->{watch_heroes} || [] };

  my %notified;

  while (1) {
    my $home = fetch_url('https://cyberscore.live/en/');
    if (!$home) { warn "Failed to fetch cyberscore home\n"; sleep($POLL_SECS); next; }
    my $urls = parse_match_urls_from_home($home);
    my $checked = 0; my $found = 0;
    URL: for my $u (@$urls) {
      next unless $u && $u =~ /match/i;
      $checked++;
      my $html = fetch_url($u);
      next unless $html && $html =~ /(?i)pick|draft|ban|hero/;
      my ($a,$b) = parse_picks_from_match_html($html);
      next unless $a && $b;
      next unless scalar(@$a) >= 2 && scalar(@$b) >= 2; # at least some picks
      my $key = join(',',@$a) . '|' . join(',',@$b);
      next if $notified{$key}++;
      $found++;

      # compute scores when we have full 5v5 else compute partial anyway
      my $scoreA = team_score($a,$b);
      my $scoreB = team_score($b,$a);
      my $diff = $scoreA - $scoreB;

      # conditions
      my @conds;
      if ($ta_enabled && (defined $ta_min || defined $ta_max)) {
        my $ok = 0;
        $ok ||= (defined $ta_min && $diff >= $ta_min);
        $ok ||= (defined $ta_max && $diff <= $ta_max);
        push @conds, $ok ? 1 : 0;
      }
      if ($ha_enabled && defined $ha_thr) {
        my $ok = any_hero_adv_threshold($b, $ha_cond, $ha_thr);
        push @conds, $ok ? 1 : 0;
      }
      if ($watch_heroes_enabled && scalar keys %watch_set) {
        my $ok = 0; for my $hid (@$a, @$b) { next unless $hid>=0; my $nm = normalize_name($HEROES[$hid]||''); if ($watch_set{$nm}) { $ok=1; last; } }
        push @conds, $ok ? 1 : 0;
      }

      my $should_alert = 0;
      if (!@conds) { $should_alert = 0; }
      elsif ($cond_logic eq 'all') { $should_alert = (grep { !$_ } @conds) ? 0 : 1; }
      else { $should_alert = (grep { $_ } @conds) ? 1 : 0; }

      if ($should_alert) {
        my $to = $settings->{email_to}||''; my $from = $settings->{email_from}||'';
        my $subject = sprintf('[Dota Watcher] Picks alert (diff=%.2f)', $diff);
        my $namesA = join(', ', map { $HEROES[$_] } @$a);
        my $namesB = join(', ', map { $HEROES[$_] } @$b);
        my $body = '';
        $body .= "Match: $u\n";
        $body .= sprintf("Diff (A-B): %.4f\n\n", $diff);
        $body .= "Team A: $namesA\n";
        $body .= "Team B: $namesB\n";
        my $ok = send_email_via_sendmail(to=>$to, from=>$from, subject=>$subject, body=>$body);
        if ($ok) { $st->{alerts} = ($st->{alerts}||0) + 1; $st->{last_alert_at} = time; save_status($st); print STDOUT "ALERT sent for $u (diff=$diff)\n"; }
        else { print STDOUT "ALERT FAILED for $u (diff=$diff)\n"; }
      } else {
        print STDOUT sprintf("No alert: diff=%.2f url=%s\n", $diff, $u);
      }
    }
    $st->{last_poll} = time; $st->{last_checked} = $checked; $st->{last_found} = $found; save_status($st);
    sleep($POLL_SECS);
  }
}

main_loop();

