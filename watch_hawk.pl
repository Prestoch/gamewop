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
my $POLL_SECS        = defined $ENV{WATCH_POLL_SECS} ? 0.0 + $ENV{WATCH_POLL_SECS} : 30;
my $FLARESOLVERR_URL = $ENV{FLARESOLVERR_URL} // '';
my $SCRAPEDO_API_KEY = $ENV{SCRAPEDO_API_KEY} // '';
my $SCRAPEDO_ENDPOINT = $ENV{SCRAPEDO_ENDPOINT} // 'https://api.scrape.do';
my $DEBUG            = $ENV{WATCH_DEBUG} ? 1 : 0;

my $HAWK_BASE = $ENV{HAWK_BASE} // 'https://hawk.live';
my @HAWK_LIVE_PATHS = split /\s*,\s*/, ($ENV{HAWK_PATHS} // '/, /dota2, /dota-2, /en, /en/dota2');

my $http = HTTP::Tiny->new(
  agent  => 'Mozilla/5.0 (X11; Linux x86_64) Safari/537.36 Perl-HTTP::Tiny',
  timeout => 60,
);

sub url_cat { my ($base,$p) = @_; $base =~ s{/+$}{}; $p ||= '/'; return $p =~ m{^https?://} ? $p : ($base . ($p =~ m{^/} ? $p : ("/".$p))); }
sub url_encode { my ($s)=@_; $s//= ''; $s =~ s/([^A-Za-z0-9\-_.~])/sprintf("%%%02X", ord($1))/eg; return $s; }

# ----- Fetch helpers -----
sub flare_get_html {
  my ($url) = @_;
  return '' unless $FLARESOLVERR_URL;
  my $endpoint = $FLARESOLVERR_URL;
  $endpoint .= '/v1' unless $endpoint =~ m{/v1/?$};
  my $payload = { cmd => 'request.get', url => $url, maxTimeout => 60000, headers => { 'User-Agent' => ($http->{agent}||'curl') } };
  my $res = $http->post($endpoint, { headers => { 'Content-Type' => 'application/json' }, content => encode_json($payload) });
  return '' unless $res->{success} && $res->{content};
  my $j = eval { decode_json($res->{content}) };
  return '' if $@ || !$j || ($j->{status}||'') ne 'ok';
  return $j->{solution}{response} || '';
}

sub scrapedo_get_html {
  my ($url) = @_;
  return '' unless $SCRAPEDO_API_KEY;
  my $api_url = $SCRAPEDO_ENDPOINT . '?token=' . url_encode($SCRAPEDO_API_KEY) . '&url=' . url_encode($url);
  my $res = $http->get($api_url);
  return $res->{success} ? ($res->{content}||'') : '';
}

sub fetch_html {
  my ($url) = @_;
  my $html = '';
  $html = flare_get_html($url) if $FLARESOLVERR_URL;
  $html = scrapedo_get_html($url) unless $html;
  $html = ($http->get($url))->{content} unless $html;
  $html //= '';
  $html =~ s/\r//g; return $html;
}

# ----- Load settings / cs.json -----
sub load_settings {
  my $p = local_file('settings.json');
  if (open my $fh,'<',$p){ local $/; my $t=<$fh>; close $fh; my $s=eval{decode_json($t||'{}')}; return $@?{}:($s||{}) }
  {}
}

my (@HEROES, @HEROES_WR, @WIN_RATES);
sub extract_json_array {
  my ($src, $needle) = @_;
  my $start = index($src, $needle);
  return '' if $start < 0;
  my $i = index($src, '[', $start);
  return '' if $i < 0;
  my ($depth,$in_str,$esc,$end,$len)=(0,0,0,-1,length($src));
  for (my $p=$i; $p<$len; $p++) {
    my $ch = substr($src,$p,1);
    if ($in_str) { if ($esc) { $esc=0; next; } if ($ch eq '\\') { $esc=1; next; } if ($ch eq '"') { $in_str=0; next; } next; }
    else { if ($ch eq '"') { $in_str=1; next; } if ($ch eq '[') { $depth++; next; } if ($ch eq ']') { $depth--; if ($depth==0){ $end=$p; last; } next; } }
  }
  return '' if $end < 0;
  return substr($src, $i, $end - $i + 1);
}
sub load_cs {
  my $p = local_file('cs.json');
  open my $fh,'<',$p or die "Missing cs.json\n";
  local $/; my $t=<$fh>; close $fh;
  my $h   = extract_json_array($t,'var heroes');
  my $wr  = extract_json_array($t,'heroes_wr');
  my $mat = extract_json_array($t,'win_rates');
  die "cs.json parse error\n" unless $h && $wr && $mat;
  my $j=JSON::PP->new;
  @HEROES    = @{ $j->decode($h) };
  @HEROES_WR = @{ $j->decode($wr) };
  @WIN_RATES = @{ $j->decode($mat) };
}

sub normalize_name { my ($n)=@_; $n//= ''; $n =~ s/\s+/ /g; $n =~ s/^\s+|\s+$//g; $n =~ s/'//g; $n =~ s/\./ /g; $n =~ s/-/ /g; $n =~ tr/A-Z/a-z/; return $n; }
my %ALIASES = (
  'wisp'=>'io','furion'=>'natures prophet','outworld destroyer'=>'outworld devourer',
  'windrunner'=>'windranger','doombringer'=>'doom','nevermore'=>'shadow fiend',
  'skeleton king'=>'wraith king','zuus'=>'zeus',
);
sub hero_index_by_name {
  my ($raw)=@_; my $k=normalize_name($raw); $k=$ALIASES{$k} if exists $ALIASES{$k};
  for (my $i=0;$i<@HEROES;$i++){ my $hn=normalize_name($HEROES[$i]//''); return $i if $hn eq $k }
  for (my $i=0;$i<@HEROES;$i++){ my $hn=normalize_name($HEROES[$i]//''); return $i if index($hn,$k)>=0 || index($k,$hn)>=0 }
  return -1;
}

# Advantage helpers
my $ADV_C=4000.0; my $ADV_LAMBDA=1.0;
sub adv_shrink{ my($a,$n)=@_; my $nn=$n||0; return $a*($nn/($nn+$ADV_C)) }
sub adv_clip  { my($a)=@_; 10.0 * tanh($a/10.0) }
sub adv_weight{ my($a,$n)=@_; adv_clip(adv_shrink($a,$n)) }
sub logit     { my($p)=@_; my $e=1e-6; $p= $p<$e?$e:($p>1-$e?1-$e:$p); log($p/(1-$p)) }
sub edge_adv_for{ my($a,$b)=@_; return 0 unless $WIN_RATES[$b] && $WIN_RATES[$b][$a]; my $e=$WIN_RATES[$b][$a]; my $raw= -1.0*($e->[0]//0); my $n=0+($e->[2]//0); adv_weight($raw,$n) }
sub team_score{ my($A,$B)=@_; my $s=0.0; for my $id (@$A){ next unless defined $id && $id>=0 && defined $HEROES_WR[$id]; my $wr=0.0+$HEROES_WR[$id]; $s += (logit($wr/100.0)-logit(0.5)); } for my $a (@$A){ next unless defined $a && $a>=0; my $sum=0; for my $b (@$B){ next unless defined $b && $b>=0; $sum += edge_adv_for($a,$b) } $s += $ADV_LAMBDA * (-$sum) } $s }
sub any_hero_adv_threshold{ my($E,$cond,$thr)=@_; return 0 unless defined $thr && ($cond||'')=~/^(gt|lt)$/; for(my $i=0;$i<@HEROES;$i++){ my $sum=0; for my $b (@$E){ next unless defined $b && $b>=0; $sum += edge_adv_for($i,$b) } my $v=-$sum; return 1 if ($cond eq 'gt' && $v >= $thr) || ($cond eq 'lt' && $v <= $thr) } 0 }

# Hawk parsing
sub parse_live_links {
  my ($html) = @_;
  my %seen; my @urls;
  while ($html =~ m{href\s*=\s*"(\/[^"\s#?]*match[^"\s]*)"}ig) {
    my $p=$1; next if $seen{$p}++;
    push @urls, url_cat($HAWK_BASE,$p);
  }
  return \@urls;
}

sub extract_picks_from_html {
  my ($html) = @_;
  my @names;
  while ($html =~ m{<(?:img|span|div)[^>]+(?:alt|title)\s*=\s*"([^"]+?)"[^>]*>}ig) { push @names, $1; }
  my @ids;
  for my $n (@names){ my $idx=hero_index_by_name($n); push @ids,$idx if $idx>=0; }
  my @uniq; my %seen;
  for my $id (@ids){ next if $seen{$id}++; push @uniq,$id; last if @uniq>=10; }
  return ([],[]) unless @uniq>=2;
  my @a=@uniq[0..(($#uniq>=4)?4:$#uniq)];
  my @b=@uniq[(($#uniq>=5)?5:scalar(@uniq))..(($#uniq>=9)?9:$#uniq)];
  return (\@a,\@b);
}

sub send_email {
  my (%a)=@_;
  my $to=$a{to}||''; return 0 unless $to; my $from=$a{from}||'';
  my $sm='/usr/sbin/sendmail'; return 0 unless -x $sm;
  my $cmd = $from ? "$sm -t -f $from" : "$sm -t";
  my $pid = open(my $m,'|-',$cmd) or return 0;
  print $m "From: $from\n" if $from; print $m "To: $to\n"; print $m "Subject: $a{subject}\n\n$a{body}\n"; close $m; 1
}

sub load_status { my $p=local_file('.watch_status.json'); if(open my $fh,'<',$p){ local $/; my $t=<$fh>; close $fh; my $j=eval{decode_json($t||'{}')}; return $@?{}:($j||{}) } {} }
sub save_status { my($st)=@_; my $p=local_file('.watch_status.json'); if(open my $fh,'>',$p){ print $fh encode_json($st||{}); close $fh; 1 } 0 }

sub main_loop {
  chdir $BASE_DIR;
  load_cs();
  my $st=load_status(); $st->{state}='watching'; $st->{started_at} ||= time; $st->{last_poll} ||= 0; $st->{alerts} ||= 0; save_status($st);
  my $s = load_settings();
  my $logic = ($s->{condition_logic}||'all') eq 'any' ? 'any' : 'all';
  my $ta = $s->{total_adv}||{}; my $ta_en = !defined $ta->{enabled} ? 1 : ($ta->{enabled}?1:0);
  my $ta_min = (defined $ta->{min} && $ta->{min} ne '') ? 0.0+$ta->{min} : undef;
  my $ta_max = (defined $ta::{max} && $ta->{max} ne '') ? 0.0+$ta->{max} : undef;
  if(!defined $ta_min && ($ta->{cond}||'') eq 'gt' && defined $ta->{threshold}){ $ta_min=0.0+$ta->{threshold} }
  if(!defined $ta_max && ($ta->{cond}||'') eq 'lt' && defined $ta->{threshold}){ $ta_max=0.0+$ta->{threshold} }
  my $ha=$s->{hero_adv}||{}; my $ha_en=!defined $ha->{enabled}?1:($ha->{enabled}?1:0);
  my $ha_cond= ($ha->{cond}||'gt') =~ /^(gt|lt)$/ ? $ha->{cond} : 'gt';
  my $ha_thr = defined $ha->{threshold} ? 0.0+$ha->{threshold} : undef;
  my $wh_en  = $s->{watch_heroes_enabled}?1:0; my %wh = map { normalize_name($_)=>1 } @{ $s->{watch_heroes}||[] };

  my %seen;
  while (1) {
    my $live_html='';
    for my $p (@HAWK_LIVE_PATHS){ my $u=url_cat($HAWK_BASE,$p); $live_html = fetch_html($u); last if $live_html && $live_html =~ /match/i; }
    my $checked=0; my $found=0;
    if ($live_html) {
      my $urls = parse_live_links($live_html);
      for my $u (@$urls){
        $checked++;
        my $html = fetch_html($u); next unless $html && $html =~ /hero|pick|draft/i;
        my ($a,$b) = extract_picks_from_html($html); next unless $a && $b && (@$a+@$b)>=2;
        my $key = join(',',@$a).'|'.join(',',@$b); next if $seen{$key}++;
        $found++;
        my $scoreA = team_score($a,$b); my $scoreB = team_score($b,$a); my $diff = $scoreA-$scoreB;
        my @conds; if($ta_en && (defined $ta_min || defined $ta_max)){ my $ok=0; $ok||=(defined $ta_min && $diff>=$ta_min); $ok||=(defined $ta_max && $diff<=$ta_max); push @conds,$ok?1:0 }
        if($ha_en && defined $ha_thr){ my $ok = any_hero_adv_threshold($b,$ha_cond,$ha_thr); push @conds,$ok?1:0 }
        if($wh_en && %wh){ my $ok=0; for my $hid (@$a,@$b){ next unless $hid>=0; my $nm=normalize_name($HEROES[$hid]||''); if($wh{$nm}){ $ok=1; last } } push @conds,$ok?1:0 }
        my $alert = (!@conds) ? 0 : ($logic eq 'all' ? ((grep{!$_}@conds)?0:1) : ((grep{$_}@conds)?1:0));
        if($alert){
          my $to=$s->{email_to}||''; my $from=$s->{email_from}||''; my $sub=sprintf('[Dota Watcher] Hawk.live (diff=%.2f)',$diff);
          my $namesA=join(', ', map{$HEROES[$_]}@$a); my $namesB=join(', ', map{$HEROES[$_]}@$b);
          my $body="Match: $u\n" . sprintf("Diff (A-B): %.4f\n\n",$diff);
          $body .= "Team A: $namesA\nTeam B: $namesB\n\n";
          $body .= "Team A base (log-odds):\n";
          for my $idA (@$a){ next unless defined $idA && $idA>=0 && defined $HEROES_WR[$idA]; my $wrA=0.0+$HEROES_WR[$idA]; my $baseA=logit($wrA/100.0)-logit(0.5); $body .= sprintf("  - %s: %.4f (WR %.2f%%)\n",$HEROES[$idA],$baseA,$wrA) }
          $body .= sprintf("Team A matchups (weighted, lambda=%.2f):\n", $ADV_LAMBDA);
          for my $idA (@$a){ next unless defined $idA && $idA>=0; my @t; my $sA=0; for my $idB (@$b){ next unless defined $idB && $idB>=0; my $t=-edge_adv_for($idA,$idB); push @t, sprintf("%.2f",$t); $sA += $t } $body .= sprintf("  - %s: [%s] sum=%.2f\n", $HEROES[$idA], join(', ',@t), $sA) }
          $body .= "\nTeam B base (log-odds):\n";
          for my $idB (@$b){ next unless defined $idB && $idB>=0 && defined $HEROES_WR[$idB]; my $wrB=0.0+$HEROES_WR[$idB]; my $baseB=logit($wrB/100.0)-logit(0.5); $body .= sprintf("  - %s: %.4f (WR %.2f%%)\n",$HEROES[$idB],$baseB,$wrB) }
          $body .= sprintf("Team B matchups (weighted, lambda=%.2f):\n", $ADV_LAMBDA);
          for my $idB (@$b){ next unless defined $idB && $idB>=0; my @tB; my $sB=0; for my $idA (@$a){ next unless defined $idA && $idA>=0; my $tB=-edge_adv_for($idB,$idA); push @tB, sprintf("%.2f",$tB); $sB += $tB } $body .= sprintf("  - %s: [%s] sum=%.2f\n", $HEROES[$idB], join(', ',@tB), $sB) }
          $body .= sprintf("\nScore A: %.2f\nScore B: %.2f\nDiff: %.2f\n", team_score($a,$b), team_score($b,$a), $diff);
          if(send_email(to=>$to,from=>$from,subject=>$sub,body=>$body)){ $st->{alerts}=($st->{alerts}||0)+1; $st->{last_alert_at}=time; print STDOUT "ALERT sent (API) diff=$diff\n" } else { print STDOUT "ALERT FAILED (API) diff=$diff\n" }
        } else {
          print STDOUT sprintf("No alert (API): diff=%.2f\n", $diff);
        }
      }
    }
    $st->{last_poll}=time; $st->{last_checked}=$checked; $st->{last_found}=$found; save_status($st);
    sleep($POLL_SECS);
  }
}

main_loop();

