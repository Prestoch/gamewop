#!/usr/bin/env perl
use strict;
use warnings;
use JSON::PP qw(decode_json encode_json);
use HTTP::Tiny;
use Time::HiRes qw(sleep);
use POSIX qw(strftime);
use Cwd qw(abs_path);
use File::Basename qw(dirname);
use File::Spec;

# ----- Paths and helpers -----
my $BASE_DIR = dirname(abs_path($0));
sub local_file { my ($f) = @_; return File::Spec->catfile($BASE_DIR, $f); }

# ----- Config -----
my $POLL_SECS    = defined $ENV{WATCH_POLL_SECS} ? 0.0 + $ENV{WATCH_POLL_SECS} : 30;
my $STRATZ_TOKEN = $ENV{STRATZ_TOKEN} // '';
my $STRATZ_URL   = $ENV{STRATZ_URL} // 'https://api.stratz.com/graphql';
my $DEBUG        = $ENV{WATCH_DEBUG} ? 1 : 0;

my $http = HTTP::Tiny->new(
  agent  => 'Mozilla/5.0 (X11; Linux x86_64) Safari/537.36 Perl-HTTP::Tiny',
  timeout => 60,
);

sub http_post_json {
  my ($url, $obj_ref, $headers) = @_;
  my %hdr = (
    'Content-Type' => 'application/json',
    'Accept'       => 'application/json',
  );
  if ($headers) { @hdr{ keys %$headers } = values %$headers; }
  my $res = $http->post($url, {
    headers => \%hdr,
    content => encode_json($obj_ref || {}),
  });
  return undef unless $res->{success} && defined $res->{content} && length $res->{content};
  my $data = eval { decode_json($res->{content}) };
  if ($@ && $DEBUG) { print STDOUT "DEBUG decode error: $@\n"; }
  return $data;
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
  @HEROES    = @{ $j->decode($h) };
  if ($bg) { @HEROES_BG = @{ $j->decode($bg) }; }
  @HEROES_WR = @{ $j->decode($wr) };
  @WIN_RATES = @{ $j->decode($mat) };
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
  'furion' => 'natures prophet',
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
  for (my $i=0; $i<@HEROES; $i++) {
    my $hn = normalize_name($HEROES[$i] // '');
    return $i if $hn eq $key;
  }
  for (my $i=0; $i<@HEROES; $i++) {
    my $hn = normalize_name($HEROES[$i] // '');
    return $i if $hn eq $key || index($hn, $key) >= 0 || index($key, $hn) >= 0;
  }
  return -1;
}

# ----- Advantage helpers (same as UI logic) -----
my $ADV_C      = 4000.0;
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
  my $raw = -1.0 * ($edge->[0] // 0);
  my $n = 0 + ($edge->[2] // 0);
  return adv_weight($raw, $n);
}
sub team_score {
  my ($teamA, $teamB) = @_;
  my $score = 0.0;
  for my $id (@$teamA) {
    next unless defined $id && $id >= 0 && defined $HEROES_WR[$id];
    my $wr = 0.0 + $HEROES_WR[$id];
    $score += (logit($wr/100.0) - logit(0.5));
  }
  for my $idA (@$teamA) {
    next unless defined $idA && $idA >= 0;
    my $sum = 0.0; for my $idB (@$teamB) { next unless defined $idB && $idB >= 0; $sum += edge_adv_for($idA,$idB); }
    $score += $ADV_LAMBDA * (-$sum);
  }
  return $score;
}

sub any_hero_adv_threshold {
  my ($enemy, $cond, $thr) = @_;
  return 0 unless defined $thr && ($cond||'') =~ /^(gt|lt)$/;
  for (my $i=0; $i<@HEROES; $i++) {
    my $sum = 0.0; for my $idB (@$enemy) { next unless defined $idB && $idB >= 0; $sum += edge_adv_for($i,$idB); }
    my $val = -$sum;
    return 1 if ($cond eq 'gt' && $val >= $thr) || ($cond eq 'lt' && $val <= $thr);
  }
  return 0;
}

# ----- STRATZ helpers -----
sub build_live_query {
  return {
    query => q(
      query Live {
        live {
          matches {
            id
            radiantTeamName
            direTeamName
            url
            picks { isRadiant hero { id displayName name shortName } }
            picksBans { isPick isRadiant hero { id displayName name shortName } }
            players { isRadiant heroId hero { id displayName name shortName } }
          }
        }
      }
    ),
    variables => {},
    operationName => 'Live',
  };
}

sub extract_picks_from_match {
  my ($m) = @_;
  my (@a,@b);
  my $pushn = sub {
    my ($isRad, $name) = @_;
    return unless defined $name && length $name;
    my $idx = hero_index_by_name($name);
    return unless $idx >= 0;
    if ($isRad) { push @a, $idx; } else { push @b, $idx; }
  };
  if (ref $m->{picks} eq 'ARRAY') {
    for my $p (@{ $m->{picks} }) {
      next unless ref $p eq 'HASH';
      my $name = '';
      if (ref $p->{hero} eq 'HASH') { $name = $p->{hero}{displayName} // $p->{hero}{name} // $p->{hero}{shortName} // ''; }
      $pushn->($p->{isRadiant}?1:0, $name);
    }
  }
  if (!@a && !@b && ref $m->{picksBans} eq 'ARRAY') {
    for my $pb (@{ $m->{picksBans} }) {
      next unless ref $pb eq 'HASH';
      next unless $pb->{isPick};
      my $name = '';
      if (ref $pb->{hero} eq 'HASH') { $name = $pb->{hero}{displayName} // $pb->{hero}{name} // $pb->{hero}{shortName} // ''; }
      $pushn->($pb->{isRadiant}?1:0, $name);
    }
  }
  if (!@a && !@b && ref $m->{players} eq 'ARRAY') {
    # Fallback: map by players' heroes (first 5 radiant/dire)
    for my $pl (@{ $m->{players} }) {
      next unless ref $pl eq 'HASH';
      my $name = '';
      if (ref $pl->{hero} eq 'HASH') { $name = $pl->{hero}{displayName} // $pl->{hero}{name} // $pl->{hero}{shortName} // ''; }
      my $isRad = $pl->{isRadiant} ? 1 : 0;
      $pushn->($isRad, $name) if $name;
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
  my $pid = open(my $mail, '|-', $cmd) or return 0;
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

sub per_hero_advantages {
  my ($team, $enemy) = @_;
  my @vals; for my $hid (@$team) { my $s=0.0; if (defined $hid && $hid>=0) { for my $eid (@$enemy){ next unless defined $eid && $eid>=0; $s += -edge_adv_for($hid,$eid) } } push @vals,$s }
  return \@vals;
}
sub per_hero_totals {
  my ($team,$enemy)=@_;
  my @vals;
  for my $hid (@$team) {
    my $wr = (defined $hid && $hid>=0 && defined $HEROES_WR[$hid]) ? (0.0+$HEROES_WR[$hid]) : 0.0;
    my $sum=0.0; if (defined $hid && $hid>=0) { for my $eid (@$enemy){ next unless defined $eid && $eid>=0; $sum += -edge_adv_for($hid,$eid) } }
    push @vals, ($wr+$sum);
  }
  return \@vals;
}
sub fmt_adv { my($v)=@_; return sprintf($v>=0?'+%.2f':'%.2f',$v); }
sub build_email_html {
  my ($A,$B,$teamAName,$teamBName,$series_name,$url) = @_;
  my $totA = per_hero_totals($A,$B); my $totB = per_hero_totals($B,$A);
  my $advA = per_hero_advantages($A,$B); my $advB = per_hero_advantages($B,$A);
  my $mk=sub{ my($ids,$valsTotals,$valsAdv)=@_; my($r1,$r2)=('',''); for(my $i=0;$i<5;$i++){ my $id=$ids->[$i]//-1; my $src=hero_icon_url($id); my $nm=$HEROES[$id]//''; my $img=$src? sprintf('<img src="%s" alt="%s" style="width:96px;height:auto;display:block;margin:0 auto;border-radius:6px;">',$src,$nm):'<div style="width:96px;height:54px;background:#eee;display:block;margin:0 auto;border-radius:6px;"></div>'; my $v=$valsAdv->[$i]//0; my $wr=(defined $HEROES_WR[$id])?sprintf('%.2f',$HEROES_WR[$id]):'--'; my $sign=$v>=0?'+':'-'; my $abs=sprintf('%.2f',abs($v)); my $col=$v>=0?'#0a0':'#c00'; $r1.='<td style="text-align:center;padding:8px 6px;">'.$img.'</td>'; $r2.='<td style="text-align:center;padding:0 6px 10px 6px;color:'.$col.';font:14px/16px Arial,Helvetica,sans-serif;">'.$wr.' '.$sign.' '.$abs.'</td>'; } return ($r1,$r2); };
  my ($r1a,$r2a)=$mk->($A,$totA,$advA); my ($r1b,$r2b)=$mk->($B,$totB,$advB);
  my $sumA=0; $sumA+=$_ for @$totA; my $sumB=0; $sumB+=$_ for @$totB; my $diff=$sumA-$sumB; my $diff_col=($diff>=0)?'#0a0':'#c00';
  my $html='';
  $html.='<html><body style="margin:0;padding:12px 12px 16px 12px;background:#fff;">';
  if ($series_name) { $html.=sprintf('<div style="font:700 18px/22px Arial,Helvetica,sans-serif;margin:0 0 6px 0;">%s</div>',$series_name); }
  $html.=sprintf('<div style="font:700 15px/18px Arial,Helvetica,sans-serif;margin:0 0 8px 0;">%s vs %s</div>',$teamAName,$teamBName);
  $html.='<div style="width:100%;max-width:560px;">';
  $html.=sprintf('<div style="font:700 13px Arial,Helvetica,sans-serif;margin:8px 0 4px 0;">%s</div>',$teamAName);
  $html.='<table role="presentation" cellpadding="0" cellspacing="0" style="width:100%;"><tr>'.$r1a.'</tr><tr>'.$r2a.'</tr></table>';
  $html.=sprintf('<div style="font:700 13px Arial,Helvetica,sans-serif;margin:16px 0 4px 0;">%s</div>',$teamBName);
  $html.='<table role="presentation" cellpadding="0" cellspacing="0" style="width:100%;"><tr>'.$r1b.'</tr><tr>'.$r2b.'</tr></table>';
  $html.=sprintf('<div style="text-align:center;margin:12px 0 0 0;font:700 26px/28px Arial,Helvetica,sans-serif;color:%s;">%s</div>', $diff_col, fmt_adv($diff));
  $html.='</div>';
  $html.='</body></html>';
  return $html;
}

sub load_status { my $p = local_file('.watch_status.json'); if (open my $fh,'<',$p){ local $/; my $t=<$fh>; close $fh; my $j=eval{decode_json($t||'{}')}; return $@?{}:($j||{});} return {}; }
sub save_status { my ($st)=@_; my $p=local_file('.watch_status.json'); if (open my $fh,'>',$p){ print $fh encode_json($st||{}); close $fh; return 1 } return 0; }

sub main_loop {
  die "Missing STRATZ_TOKEN env" unless $STRATZ_TOKEN;
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
  if (!defined $ta_min && ($ta->{cond}||'') eq 'gt' && defined $ta->{threshold}) { $ta_min = 0.0 + $ta->{threshold}; }
  if (!defined $ta_max && ($ta->{cond}||'') eq 'lt' && defined $ta->{threshold}) { $ta_max = 0.0 + $ta->{threshold}; }

  my $ha = $settings->{hero_adv} || {};
  my $ha_enabled = !defined $ha->{enabled} ? 1 : ($ha->{enabled} ? 1 : 0);
  my $ha_cond = ($ha->{cond}||'gt') =~ /^(gt|lt)$/ ? $ha->{cond} : 'gt';
  my $ha_thr = defined $ha->{threshold} ? 0.0 + $ha->{threshold} : undef;

  my $watch_heroes_enabled = $settings->{watch_heroes_enabled} ? 1 : 0;
  my %watch_set = map { normalize_name($_)=>1 } @{ $settings->{watch_heroes} || [] };

  my %seen_alert;

  while (1) {
    my $payload = build_live_query();
    my $data = http_post_json($STRATZ_URL, $payload, { 'Authorization' => 'Bearer '.$STRATZ_TOKEN });
    my $matches = [];
    if ($data && ref $data eq 'HASH') {
      if (ref $data->{data} eq 'HASH' && ref $data->{data}{live} eq 'HASH' && ref $data->{data}{live}{matches} eq 'ARRAY') {
        $matches = $data->{data}{live}{matches};
      } elsif (ref $data->{matches} eq 'ARRAY') {
        $matches = $data->{matches};
      }
    }

    my ($checked,$found) = (0,0);
    for my $m (@$matches) {
      $checked++;
      my ($a,$b) = extract_picks_from_match($m);
      next unless $a && $b && (scalar(@$a)+scalar(@$b)) >= 2;
      my $url = $m->{url} || '';
      my $key = join(',',@$a).'|'.join(',',@$b);
      next if $seen_alert{$key}++;
      $found++;

      my $scoreA = team_score($a,$b);
      my $scoreB = team_score($b,$a);
      my $diff = $scoreA - $scoreB;

      my @conds;
      if ($ta_enabled && (defined $ta_min || defined $ta_max)) {
        my $ok=0; $ok ||= (defined $ta_min && $diff >= $ta_min); $ok ||= (defined $ta_max && $diff <= $ta_max); push @conds, $ok?1:0;
      }
      if ($ha_enabled && defined $ha_thr) {
        my $ok = any_hero_adv_threshold($b, $ha_cond, $ha_thr); push @conds, $ok?1:0;
      }
      if ($watch_heroes_enabled && scalar keys %watch_set) {
        my $ok=0; for my $hid (@$a, @$b) { next unless $hid>=0; my $nm = normalize_name($HEROES[$hid]||''); if ($watch_set{$nm}) { $ok=1; last; } } push @conds, $ok?1:0;
      }

      my $should_alert = (!@conds) ? 0 : (($cond_logic eq 'all') ? ((grep { !$_ } @conds)?0:1) : ((grep { $_ } @conds)?1:0));
      if ($should_alert) {
        my $to = $settings->{email_to}||''; my $from = $settings->{email_from}||'';
        my $teamAName = $m->{radiantTeamName} || 'Radiant';
        my $teamBName = $m->{direTeamName}    || 'Dire';
        my $subject = sprintf('%s vs %s', $teamAName, $teamBName);
        my $series = $m->{seriesName} || $m->{tournamentName} || $m->{leagueName} || '';
        if (ref $series eq 'HASH') { for my $kk (qw/name shortName displayName title/) { $series = $series->{$kk} if $series->{$kk}; } }
        my $html = build_email_html($a,$b,$teamAName,$teamBName,$series,$url);
        my $ok = send_email_via_sendmail(to=>$to, from=>$from, subject=>$subject, body=>$html, html=>1);
        if ($ok) { $st->{alerts} = ($st->{alerts}||0)+1; $st->{last_alert_at}=time; print STDOUT "ALERT sent (API) diff=$diff\n"; }
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

