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
my $CS_API_URL = $ENV{CYBERSCORE_API_URL} // 'https://api.cyberscore.live/api/v1/matches/?limit=20&liveOrUpcoming=1';

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

sub http_get_json {
  my ($url) = @_;
  my $res = $http->get($url, { headers => { 'Accept' => 'application/json' } });
  return undef unless $res->{success} && $res->{content};
  my $data = eval { decode_json($res->{content}) };
  return $@ ? undef : $data;
}

sub scrapedo_get_json {
  my ($url) = @_;
  return undef unless $SCRAPEDO_API_KEY;
  my $raw = fetch_with_scrapedo($url);
  return undef unless defined $raw && length $raw;
  my $data = eval { decode_json($raw) };
  return $@ ? undef : $data;
}

# ----- Load settings -----
sub load_settings {
  my $path = local_file('settings.json');
  if (open my $fh, '<', $path) { local $/; my $txt = <$fh>; close $fh; my $s = eval{ decode_json($txt||'{}') }; return $@?{}:($s||{}); }
  return {};
}

# ----- Load cs.json (JS) -----
my (@HEROES, @HEROES_WR, @WIN_RATES, @HEROES_BG);
sub extract_json_array {
  my ($src, $needle) = @_;
  my $start = index($src, $needle);
  return '' if $start < 0;
  my $i = index($src, '[', $start);
  return '' if $i < 0;
  my ($depth,$in_str,$esc,$end,$len)=(0,0,0,-1,length($src));
  for (my $p=$i; $p<$len; $p++) {
    my $ch = substr($src,$p,1);
    if ($in_str) {
      if ($esc) { $esc=0; next; }
      if ($ch eq '\\') { $esc=1; next; }
      if ($ch eq '"') { $in_str=0; next; }
      next;
    } else {
      if ($ch eq '"') { $in_str=1; next; }
      if ($ch eq '[') { $depth++; next; }
      if ($ch eq ']') { $depth--; if ($depth==0){ $end=$p; last; } next; }
    }
  }
  return '' if $end < 0;
  return substr($src, $i, $end - $i + 1);
}

sub load_cs {
  my $path = local_file('cs.json');
  open my $fh, '<', $path or die "Missing cs.json - generate it first";
  local $/; my $txt = <$fh>; close $fh;
  my $h   = extract_json_array($txt, 'var heroes');
  my $bg  = extract_json_array($txt, 'heroes_bg');
  my $wr  = extract_json_array($txt, 'heroes_wr');
  my $mat = extract_json_array($txt, 'win_rates');
  die "cs.json parse error: heroes" unless $h;
  die "cs.json parse error: heroes_wr" unless $wr;
  die "cs.json parse error: win_rates" unless $mat;
  my $j = JSON::PP->new;
  @HEROES     = @{ $j->decode($h) };
  if ($bg) { @HEROES_BG = @{ $j->decode($bg) }; }
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
sub adv_clip   { my ($adv) = @_; my $x=$adv/10.0; my $e=exp(2*$x); my $t=($e-1)/($e+1); return 10.0 * $t; }
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

sub extract_picks_from_api_match {
  my ($m) = @_;
  my (@a,@b);
  if (ref $m eq 'HASH') {
    for my $key (qw/draft picks events/) {
      next unless ref $m->{$key} eq 'ARRAY';
      for my $e (@{ $m->{$key} }) {
        next unless ref $e eq 'HASH';
        my $side = lc(($e->{side}//$e->{team}//$e->{faction}//'')."");
        $side = 'radiant' if $side =~ /^(radiant|r|0)$/;
        $side = 'dire' if $side =~ /^(dire|d|1)$/;
        my $hname = $e->{hero}//$e->{hero_name}//$e->{heroName}//$e->{name}//'';
        $hname = $hname->{name} if ref $hname eq 'HASH' && $hname->{name};
        next unless $hname && !ref $hname;
        my $idx = hero_index_by_name($hname);
        next unless $idx >= 0;
        if ($side eq 'radiant') { push @a, $idx; } elsif ($side eq 'dire') { push @b, $idx; }
      }
    }
  }
  @a = @a[0..4] if @a > 5; @b = @b[0..4] if @b > 5;
  return (\@a, \@b);
}

sub send_email_via_sendmail {
  my (%args) = @_;
  my $to = $args{to}; my $from = $args{from}||''; my $subject = $args{subject}||'Watcher alert'; my $body = $args{body}||''; my $is_html=$args{html}?1:0;
  my $sendmail = '/usr/sbin/sendmail';
  return 0 unless $to && -x $sendmail;
  my $cmd = $from ? "$sendmail -t -f $from" : "$sendmail -t";
  my $pid = open(my $mail, '|-', $cmd);
  return 0 unless $pid;
  print $mail "From: $from\n" if $from;
  print $mail "To: $to\n";
  print $mail "Subject: $subject\n";
  print $mail "MIME-Version: 1.0\n";
  if ($is_html) {
    print $mail "Content-Type: text/html; charset=UTF-8\n";
    print $mail "Content-Transfer-Encoding: 8bit\n\n";
    print $mail $body; print $mail "\n";
  } else {
    print $mail "\n$body\n";
  }
  close $mail;
  return 1;
}

sub hero_icon_url { my($id)=@_; return '' unless defined $id && $id>=0; return $HEROES_BG[$id]||''; }
sub per_hero_advantages { my($team,$enemy)=@_; my @v; for my $hid (@$team){ my $s=0.0; if(defined $hid && $hid>=0){ for my $eid (@$enemy){ next unless defined $eid && $eid>=0; $s += -edge_adv_for($hid,$eid) } } push @v,$s } return \@v }
sub per_hero_totals { my($team,$enemy)=@_; my @v; for my $hid (@$team){ my $base=0.0; if(defined $hid && $hid>=0 && defined $HEROES_WR[$hid]){ my $wr=0.0+$HEROES_WR[$hid]; $base = (logit($wr/100.0)-logit(0.5)); } my $sum=0.0; if(defined $hid && $hid>=0){ for my $eid (@$enemy){ next unless defined $eid && $eid>=0; $sum += -edge_adv_for($hid,$eid) } } push @v, ($base+$sum) } return \@v }
sub fmt_adv { my($v)=@_; return sprintf($v>=0?'+%.2f':'%.2f',$v); }
sub extract_team_names_from_match { my($m)=@_; my($A,$B)=('',''); if(ref $m eq 'HASH'){ for my $pair (['radiantTeamName','direTeamName'],['radiant_name','dire_name'],['radiantTeam','direTeam'],['teamA','teamB'],['team1','team2']){ my($ka,$kb)=@$pair; my $va=$m->{$ka}; my $vb=$m->{$kb}; if(!$A && defined $va){ $A = ref $va eq 'HASH' ? ($va->{name}||$va->{shortName}||$va->{displayName}||'') : $va; } if(!$B && defined $vb){ $B = ref $vb eq 'HASH' ? ($vb->{name}||$vb->{shortName}||$vb->{displayName}||'') : $vb; } last if $A && $B; } if((!$A || !$B) && ref $m->{teams} eq 'ARRAY' && @{$m->{teams}}>=2){ my $t1=$m->{teams}[0]; my $t2=$m->{teams}[1]; $A ||= (ref $t1 eq 'HASH') ? ($t1->{name}||$t1->{shortName}||'') : ''; $B ||= (ref $t2 eq 'HASH') ? ($t2->{name}||$t2->{shortName}||'') : ''; } } $A||='Radiant'; $B||='Dire'; return ($A,$B); }
sub build_email_html {
  my ($A,$B,$teamAName,$teamBName,$series_name)=@_;
  my $totA=per_hero_totals($A,$B); my $totB=per_hero_totals($B,$A);
  my $advA=per_hero_advantages($A,$B); my $advB=per_hero_advantages($B,$A);
  my $mk=sub{ my($ids,$valsTotals,$valsAdv)=@_; my($r1,$r2)=('',''); for(my $i=0;$i<5;$i++){ my $id=$ids->[$i]//-1; my $src=hero_icon_url($id); my $nm=$HEROES[$id]//''; my $img=$src? sprintf('<img src="%s" alt="%s" style="width:96px;height:auto;display:block;margin:0 auto;border-radius:6px;">',$src,$nm):'<div style="width:96px;height:54px;background:#eee;display:block;margin:0 auto;border-radius:6px;"></div>'; my $v=$valsAdv->[$i]//0; my $wr=(defined $HEROES_WR[$id])?sprintf('%.2f',$HEROES_WR[$id]):'--'; my $sign=$v>=0?'+':'-'; my $abs=sprintf('%.2f',abs($v)); my $col=$v>=0?'#0a0':'#c00'; $r1.='<td style="text-align:center;padding:8px 6px;">'.$img.'</td>'; $r2.='<td style="text-align:center;padding:0 6px 10px 6px;color:'.$col.';font:14px/16px Arial,Helvetica,sans-serif;">'.$wr.' '.$sign.' '.$abs.'</td>'; } return ($r1,$r2); };
  my ($r1a,$r2a)=$mk->($A,$totA,$advA); my ($r1b,$r2b)=$mk->($B,$totB,$advB);
  my $sumA=0; $sumA+=$_ for @$totA; my $sumB=0; $sumB+=$_ for @$totB; my $diff=$sumA-$sumB; my $diff_col=$diff>=0?'#0a0':'#c00';
  my $html=''; $html.='<html><body style="margin:0;padding:12px 12px 16px 12px;background:#fff;">';
  if ($series_name) { $html.=sprintf('<div style="font:700 18px/22px Arial,Helvetica,sans-serif;margin:0 0 6px 0;">%s</div>', $series_name); }
  $html.=sprintf('<div style="font:700 15px/18px Arial,Helvetica,sans-serif;margin:0 0 8px 0;">%s vs %s</div>',$teamAName,$teamBName);
  $html.='<div style="width:100%;max-width:560px;">';
  $html.='<div style="font:700 13px Arial,Helvetica,sans-serif;margin:8px 0 4px 0;">Team A</div>';
  $html.='<table role="presentation" cellpadding="0" cellspacing="0" style="width:100%;"><tr>'.$r1a.'</tr><tr>'.$r2a.'</tr></table>';
  $html.='<div style="font:700 13px Arial,Helvetica,sans-serif;margin:16px 0 4px 0;">Team B</div>';
  $html.='<table role="presentation" cellpadding="0" cellspacing="0" style="width:100%;"><tr>'.$r1b.'</tr><tr>'.$r2b.'</tr></table>';
  $html.=sprintf('<div style="text-align:center;margin:12px 0 0 0;font:700 26px/28px Arial,Helvetica,sans-serif;color:%s;">%s</div>', $diff_col, fmt_adv($diff));
  $html.='</div>';
  $html.='</body></html>';
  return $html;
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
    my $list = http_get_json($CS_API_URL);
    $list ||= scrapedo_get_json($CS_API_URL);
    my $checked = 0; my $found = 0;
    if (ref $list eq 'HASH' && ref $list->{results} eq 'ARRAY') { $list = $list->{results}; }
    $list = [] unless ref $list eq 'ARRAY';
    for my $m (@$list) {
      $checked++;
      my ($a,$b) = extract_picks_from_api_match($m);
      next unless $a && $b && (scalar(@$a)+scalar(@$b)) >= 2;
      my $url = $m->{url} || $m->{webUrl} || '';
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
        my ($teamAName,$teamBName) = extract_team_names_from_match($m);
        my $subject = sprintf('%s vs %s', $teamAName, $teamBName);
        my $html = build_email_html($a,$b,$teamAName,$teamBName,$diff,$url);
        my $ok = send_email_via_sendmail(to=>$to, from=>$from, subject=>$subject, body=>$html, html=>1);
        if ($ok) { $st->{alerts} = ($st->{alerts}||0) + 1; $st->{last_alert_at} = time; save_status($st); print STDOUT "ALERT sent (API) diff=$diff\n"; }
        else { print STDOUT "ALERT FAILED (API) diff=$diff\n"; }
      } else {
        print STDOUT sprintf("No alert (API): diff=%.2f\n", $diff);
      }
    }
    $st->{last_poll} = time; $st->{last_checked} = $checked; $st->{last_found} = $found; save_status($st);
    sleep($POLL_SECS);
  }
}

main_loop();

