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

# Flush STDOUT/STDERR promptly so short foreground runs show logs
select(STDOUT); $|=1; select(STDERR); $|=1;

# ----- Config -----
my $POLL_SECS        = defined $ENV{WATCH_POLL_SECS} ? 0.0 + $ENV{WATCH_POLL_SECS} : 30;
my $FLARESOLVERR_URL = $ENV{FLARESOLVERR_URL} // '';
my $SCRAPEDO_API_KEY = $ENV{SCRAPEDO_API_KEY} // '';
my $SCRAPEDO_ENDPOINT = $ENV{SCRAPEDO_ENDPOINT} // 'https://api.scrape.do';
my $SCRAPEDO_RENDER  = $ENV{SCRAPEDO_RENDER} ? 1 : 0;
my $DEBUG            = $ENV{WATCH_DEBUG} ? 1 : 0;

my $HAWK_BASE = $ENV{HAWK_BASE} // 'https://hawk.live';
my @HAWK_LIVE_PATHS = split /\s*,\s*/, (
  $ENV{HAWK_PATHS} // '/, /dota2, /dota-2, /en, /en/dota2, /en/dota-2, /en/dota-2/matches, /dota-2/matches, /dota2/matches, /en/dota2/matches, /dota-2/live, /en/dota-2/live, /dota2/live, /en/dota2/live'
);
my @HAWK_MATCH_URLS = $ENV{HAWK_MATCH_URLS} ? split(/\s*,\s*/, $ENV{HAWK_MATCH_URLS}) : ();
my $HAWK_API_CACHE = local_file('.hawk_endpoint');

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
  my $res = $http->post($endpoint, { headers => { 'Content-Type' => 'application/json', 'Accept-Encoding' => 'identity' }, content => encode_json($payload) });
  return '' unless $res->{success} && $res->{content};
  my $j = eval { decode_json($res->{content}) };
  return '' if $@ || !$j || ($j->{status}||'') ne 'ok';
  return $j->{solution}{response} || '';
}

sub scrapedo_get_html {
  my ($url) = @_;
  return '' unless $SCRAPEDO_API_KEY;
  my $api_url = $SCRAPEDO_ENDPOINT . '?token=' . url_encode($SCRAPEDO_API_KEY) . '&url=' . url_encode($url) . ($SCRAPEDO_RENDER ? '&render=true' : '');
  my $res = $http->get($api_url, { headers => { 'Accept-Encoding' => 'identity' } });
  return $res->{success} ? ($res->{content}||'') : '';
}

sub scrapedo_get_json {
  my ($url) = @_;
  return undef unless $SCRAPEDO_API_KEY;
  my $api_url = $SCRAPEDO_ENDPOINT . '?token=' . url_encode($SCRAPEDO_API_KEY) . '&url=' . url_encode($url) . ($SCRAPEDO_RENDER ? '&render=true' : '');
  my $res = $http->get($api_url, { headers => { 'Accept' => 'application/json', 'Accept-Encoding' => 'identity' } });
  return undef unless $res->{success} && $res->{content};
  my $d = eval { decode_json($res->{content}) };
  return $@ ? undef : $d;
}

sub fetch_html {
  my ($url) = @_;
  my $html = '';
  $html = flare_get_html($url) if $FLARESOLVERR_URL;
  $html = scrapedo_get_html($url) unless $html;
  $html = ($http->get($url, { headers => { 'Accept-Encoding' => 'identity' } }))->{content} unless $html;
  $html //= '';
  $html =~ s/\r//g; return $html;
}

# ---- Optional JSON discovery ----
sub discover_app_bundles {
  print STDOUT "DEBUG: discovering Hawk endpoints from root $HAWK_BASE/\n" if $DEBUG;
  my $root = fetch_html(url_cat($HAWK_BASE,'/'));
  return [] unless $root;
  my %seen; my @bundles;
  while ($root =~ m{<script[^>]+src=["']/build/assets/([^"']+?\.js)["']}ig) { my $b=$1; next if $seen{$b}++; push @bundles,$b; }
  while ($root =~ m{<link[^>]+rel=["']modulepreload["'][^>]+href=["']/build/assets/([^"']+?\.js)["']}ig) { my $b=$1; next if $seen{$b}++; push @bundles,$b; }
  for my $pat (qw/app main index vendor vendors chunk runtime/) {
    my ($b) = $root =~ m{build/assets/([A-Za-z0-9_.-]*$pat[-_][A-Za-z0-9_.-]*\.js)}i; if ($b && !$seen{$b}++){ push @bundles,$b }
  }
  print STDOUT sprintf("DEBUG: found %d bundles\n", scalar(@bundles)) if $DEBUG;
  return \@bundles;
}

sub fetch_asset {
  my ($path) = @_;
  my $url = url_cat($HAWK_BASE, "/build/assets/$path");
  if ($SCRAPEDO_API_KEY) {
    # Always fetch raw asset without render=true to avoid HTML wrappers
    my $api_url = $SCRAPEDO_ENDPOINT . '?token=' . url_encode($SCRAPEDO_API_KEY) . '&url=' . url_encode($url);
    my $r1 = $http->get($api_url, { headers => { 'Accept-Encoding' => 'identity' } });
    if ($r1->{success} && defined $r1->{content}) { return $r1->{content}; }
  }
  my $res = $http->get($url, { headers => { 'Accept-Encoding' => 'identity' } });
  return ($res->{success} ? ($res->{content}||'') : '');
}

sub discover_hawk_endpoints {
  my %seen; my @candidates;
  if (open my $rf,'<',$HAWK_API_CACHE){ my $u=<$rf>; close $rf; $u =~ s/\s+//g; if ($u){ $seen{$u}=1; push @candidates,$u; print STDOUT "DEBUG: using cached endpoint $u\n" if $DEBUG; } }
  my $bundles = discover_app_bundles();
  if ($bundles && @$bundles){
    BUNDLE:
    for my $bundle (@$bundles){
      my $js = fetch_asset($bundle); next unless $js;
      # Extract absolute URLs limited to hawk.live domain
      while ($js =~ m{https?://[^"'\)\s]+}g) {
        my $u = $&; next unless $u =~ m{^https?://([A-Za-z0-9.-]*\.)?hawk\.live(?::\d+)?/}i; next unless $u =~ /(api|graphql|match|live)/i; next if $seen{$u}++; push @candidates, $u;
      }
      # Extract relative API-like paths
      while ($js =~ m{['"](/[^'"\)\s]+)['"]}g) {
        my $p=$1; next unless $p =~ m{^/(api|graphql|en/.*api|ru/.*api|.*match|.*live)}i; my $u=url_cat($HAWK_BASE,$p); next if $seen{$u}++; push @candidates,$u;
      }
      last BUNDLE if @candidates;
    }
  } else { print STDOUT "DEBUG: no app bundles found; discovery limited to cache\n" if $DEBUG; }
  # Filter out obvious non-API and malformed entries
  @candidates = grep { $_ !~ m{</a>} && $_ !~ m{^https?://[^/]+/$} && $_ !~ m{primebrand\.site}i } @candidates;
  print STDOUT sprintf("DEBUG: candidate endpoints: %s\n", join(', ', @candidates)) if $DEBUG && @candidates;
  if (@candidates){
    my $primary = $candidates[0];
    if ($primary =~ /(api|live|match)/i && $primary !~ m{^https?://[^/]+/$}){
      if (open my $wf,'>',$HAWK_API_CACHE){ print $wf $primary; close $wf; }
    }
  }
  return \@candidates;
}

sub fetch_json {
  my ($url)=@_;
  if ($SCRAPEDO_API_KEY) {
    my $dj = scrapedo_get_json($url);
    return $dj if $dj;
  }
  my $res=$http->get($url, { headers => { 'Accept'=>'application/json', 'Accept-Encoding'=>'identity' } });
  if (!$res->{success}){ print STDOUT sprintf("DEBUG: GET %s failed status=%s reason=%s\n", $url, ($res->{status}||'?'), ($res->{reason}||'?')) if $DEBUG; return undef }
  my $content = $res->{content} // '';
  if ($DEBUG){ my $len = length($content||''); print STDOUT sprintf("DEBUG: GET %s ok, bytes=%d\n", $url, $len); }
  return undef unless $content;
  my $d=eval{ decode_json($content) }; if ($@){ print STDOUT "DEBUG: JSON decode error\n" if $DEBUG; return undef } return $d;
}

sub find_match_array_in_hash {
  my ($h) = @_; return [] unless ref $h eq 'HASH';
  for my $k (qw/matches live liveMatches events games data list edges items nodes/) {
    if (ref $h->{$k} eq 'ARRAY' && @{$h->{$k}}) { return $h->{$k}; }
    if (ref $h->{$k} eq 'HASH') {
      my $inner = find_match_array_in_hash($h->{$k});
      return $inner if ref $inner eq 'ARRAY' && @$inner;
    }
  }
  return [];
}

# ----- Load settings / cs.json -----
sub load_settings {
  my $p = local_file('settings.json');
  if (open my $fh,'<',$p){ local $/; my $t=<$fh>; close $fh; my $s=eval{decode_json($t||'{}')}; return $@?{}:($s||{}) }
  {}
}

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
  my $bg  = extract_json_array($t,'heroes_bg');
  my $wr  = extract_json_array($t,'heroes_wr');
  my $mat = extract_json_array($t,'win_rates');
  die "cs.json parse error\n" unless $h && $wr && $mat;
  my $j=JSON::PP->new;
  @HEROES    = @{ $j->decode($h) };
  if ($bg) { @HEROES_BG = @{ $j->decode($bg) }; }
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
sub adv_clip  { my($a)=@_; my $x=$a/10.0; my $e=exp(2*$x); my $t=($e-1)/($e+1); 10.0 * $t }
sub adv_weight{ my($a,$n)=@_; adv_clip(adv_shrink($a,$n)) }
sub logit     { my($p)=@_; my $e=1e-6; $p= $p<$e?$e:($p>1-$e?1-$e:$p); log($p/(1-$p)) }
sub edge_adv_for{ my($a,$b)=@_; return 0 unless $WIN_RATES[$b] && $WIN_RATES[$b][$a]; my $e=$WIN_RATES[$b][$a]; my $raw= -1.0*($e->[0]//0); my $n=0+($e->[2]//0); adv_weight($raw,$n) }
sub team_score{ my($A,$B)=@_; my $s=0.0; for my $id (@$A){ next unless defined $id && $id>=0 && defined $HEROES_WR[$id]; my $wr=0.0+$HEROES_WR[$id]; $s += (logit($wr/100.0)-logit(0.5)); } for my $a (@$A){ next unless defined $a && $a>=0; my $sum=0; for my $b (@$B){ next unless defined $b && $b>=0; $sum += edge_adv_for($a,$b) } $s += $ADV_LAMBDA * (-$sum) } $s }
sub any_hero_adv_threshold{ my($E,$cond,$thr)=@_; return 0 unless defined $thr && ($cond||'')=~/^(gt|lt)$/; for(my $i=0;$i<@HEROES;$i++){ my $sum=0; for my $b (@$E){ next unless defined $b && $b>=0; $sum += edge_adv_for($i,$b) } my $v=-$sum; return 1 if ($cond eq 'gt' && $v >= $thr) || ($cond eq 'lt' && $v <= $thr) } 0 }

# Hawk parsing
sub parse_live_links {
  my ($html) = @_;
  my %seen; my @urls;
  while ($html =~ m{<a[^>]+href\s*=\s*["']([^"']+)["'][^>]*>}ig) {
    my $p=$1; next if $seen{$p}++;
    next unless $p =~ m{^/};
    next if $p =~ m{\.(?:css|js|png|jpe?g|svg|webp|ico)(?:\?|$)}i;
    my $is_matchish = ($p =~ m{/(?:match|matches|game|live)/}i) || ($p =~ m{/dota(?:-?2)?}i);
    my $has_id     = ($p =~ m{/(\d{4,}|\d+)(?:/|$)});
    next unless $is_matchish || $has_id;
    push @urls, url_cat($HAWK_BASE,$p);
  }
  return \@urls;
}

sub extract_picks_from_html {
  my ($html) = @_;
  my @names;
  # 1) alt/title
  while ($html =~ m{<(?:img|span|div)[^>]+(?:alt|title)\s*=\s*"([^"]+?)"[^>]*>}ig) { push @names, $1; }
  # 2) data attributes
  while ($html =~ m{\bdata-(?:hero|hero-name|heroname)\s*=\s*"([^"]+?)"}ig) { push @names, $1; }
  # 3) image src with hero slug
  while ($html =~ m{<img[^>]+src\s*=\s*"([^"]+)"[^>]*>}ig) {
    my $u=$1; next unless $u =~ m{/(hero|heroes|portraits|icons|images)/}i;
    my ($seg) = $u =~ m{/([A-Za-z0-9_\-]+)\.(?:png|jpg|jpeg|webp|svg)(?:\?|$)}i;
    if ($seg){ my $s=$seg; $s =~ tr/_\-/  /; push @names, $s; }
  }
  # 4) inline style background-image
  while ($html =~ m{style\s*=\s*"[^"]*url\(([^\)]+)\)[^"]*"}ig){
    my $u=$1; $u =~ s/^['"]|['"]$//g; next unless $u =~ m{/(hero|heroes|portraits|icons|images)/}i;
    my ($seg) = $u =~ m{/([A-Za-z0-9_\-]+)\.(?:png|jpg|jpeg|webp|svg)(?:\?|$)}i;
    if ($seg){ my $s=$seg; $s =~ tr/_\-/  /; push @names, $s; }
  }
  # Map to hero ids
  my @ids;
  for my $n (@names){ my $idx=hero_index_by_name($n); push @ids,$idx if $idx>=0; }
  my @uniq; my %seen;
  for my $id (@ids){ next if $seen{$id}++; push @uniq,$id; last if @uniq>=10; }
  if ($DEBUG) {
    print STDOUT sprintf("DEBUG: pick-parse names=%d ids=%d uniq=%d\n", scalar(@names), scalar(@ids), scalar(@uniq));
  }
  return ([],[]) unless @uniq>=2;
  my @a=@uniq[0..(($#uniq>=4)?4:$#uniq)];
  my @b=@uniq[(($#uniq>=5)?5:scalar(@uniq))..(($#uniq>=9)?9:$#uniq)];
  return (\@a,\@b);
}

sub extract_team_names_from_html {
  my ($html) = @_;
  my ($A,$B)=('','');
  if ($html =~ m{<meta[^>]+property\s*=\s*"og:title"[^>]+content\s*=\s*"([^"]+)"}i) {
    my $t=$1; $t =~ s/\s+/ /g; $t =~ s/^\s+|\s+$//g; if ($t =~ /(.*?)\s+vs\s+(.*)/i) { ($A,$B)=($1,$2); }
  }
  if ((!$A || !$B) && $html =~ m{<title[^>]*>([^<]+)</title>}i) {
    my $t=$1; $t =~ s/\s+/ /g; $t =~ s/^\s+|\s+$//g; if ($t =~ /(.*?)\s+vs\s+(.*)/i) { ($A,$B)=($1,$2); }
  }
  $A ||= 'Radiant'; $B ||= 'Dire';
  $A = sanitize_team_label($A); $B = sanitize_team_label($B);
  return ($A,$B);
}

sub sanitize_team_label {
  my ($s)=@_; $s//= '';
  $s =~ s/\s*(?:—|\|).*\z//;     # cut trailing after em-dash or pipe
  $s =~ s/\s*map\s*\d+.*\z//i;  # drop trailing map info
  $s =~ s/\s{2,}/ /g; $s =~ s/^\s+|\s+$//g; return $s;
}

sub sanitize_series_name {
  my ($s)=@_; $s//= '';
  $s =~ s/^\s+|\s+$//g;
  return '' if $s =~ /^hawk\s*\.?\s*live$/i;
  return '' if $s =~ /^live$/i;
  return '' if $s =~ /^match(?:es)?$/i;
  return $s;
}

sub extract_series_from_match {
  my ($m)=@_; return '' unless ref $m eq 'HASH';
  for my $k (qw/seriesName series tournamentName tournament leagueName league eventName event title name/) {
    my $v = $m->{$k};
    if (defined $v) {
      if (ref $v eq 'HASH') { for my $kk (qw/name shortName displayName title/) { return $v->{$kk} if $v->{$kk}; } }
      else { return $v; }
    }
  }
  return '';
}

sub extract_series_from_html {
  my ($html)=@_;
  # Prefer explicit series/tournament block from header side
  if ($html =~ m{class=["'][^"']*v-block__header-side[^"']*["'][^>]*>([\s\S]*?)</[^>]+>}i) {
    my $inner = $1; $inner =~ s/<[^>]+>/ /g; $inner =~ s/\s+/ /g; $inner =~ s/^\s+|\s+$//g; my $s = sanitize_series_name($inner); return $s if $s;
  }
  # Fallbacks (og:title or title)
  my $t='';
  if ($html =~ m{<meta[^>]+property\s*=\s*"og:title"[^>]+content\s*=\s*"([^"]+)"}i) { $t=$1; }
  elsif ($html =~ m{<title[^>]*>([^<]+)</title>}i) { $t=$1; }
  $t//= '';
  $t =~ s/\s+/ /g; $t =~ s/^\s+|\s+$//g;
  if ($t =~ /\s(?:—|\|)\s*(.+?)\s*(?:\||$)/) { return sanitize_series_name($1); }
  return '';
}

sub send_email {
  my (%a)=@_;
  my $to=$a{to}||''; return 0 unless $to; my $from=$a{from}||'';
  my $is_html = $a{html} ? 1 : 0;
  my $sm='/usr/sbin/sendmail'; return 0 unless -x $sm;
  my $env_from = $ENV{WATCH_FROM} // '';
  my $from_addr = $from || $env_from;
  my $debug_mail = $ENV{WATCH_MAIL_DEBUG} ? 1 : 0;
  my $cmd;
  if ($from_addr) { $cmd = $debug_mail ? "$sm -t -v -f $from_addr" : "$sm -t -f $from_addr"; }
  else { $cmd = $debug_mail ? "$sm -t -v" : "$sm -t"; }
  print STDOUT sprintf("MAIL enqueue: to=%s from=%s debug=%d\n", $to, ($from_addr||'(none)'), $debug_mail);
  my $pid = open(my $m,'|-',$cmd) or return 0;
  print $m "From: $from_addr\n" if $from_addr;
  print $m "To: $to\n";
  print $m "Subject: $a{subject}\n";
  print $m "MIME-Version: 1.0\n";
  if ($is_html) {
    print $m "Content-Type: text/html; charset=UTF-8\n";
    print $m "Content-Transfer-Encoding: 8bit\n\n";
    print $m ($a{body}//'');
    print $m "\n";
  } else {
    print $m "\n".($a{body}//'')."\n";
  }
  close $m; 1
}

sub hero_icon_url { my($id)=@_; return '' unless defined $id && $id>=0; return $HEROES_BG[$id]||''; }

sub per_hero_advantages {
  my ($team, $enemy) = @_;
  my @vals;
  for my $hid (@$team) {
    my $s = 0.0;
    if (defined $hid && $hid>=0) {
      for my $eid (@$enemy) { next unless defined $eid && $eid>=0; $s += -edge_adv_for($hid,$eid); }
    }
    push @vals, $s;
  }
  return \@vals;
}

sub per_hero_totals {
  my ($team, $enemy) = @_;
  my @vals;
  for my $hid (@$team) {
    my $wr = (defined $hid && $hid>=0 && defined $HEROES_WR[$hid]) ? (0.0 + $HEROES_WR[$hid]) : 0.0;
    my $sum  = 0.0;
    if (defined $hid && $hid>=0) { for my $eid (@$enemy) { next unless defined $eid && $eid>=0; $sum += -edge_adv_for($hid,$eid); } }
    push @vals, ($wr + $sum);
  }
  return \@vals;
}

sub fmt_adv { my($v)=@_; return sprintf($v>=0?'+%.2f':'%.2f', $v); }

sub extract_team_names_from_match {
  my ($m) = @_;
  my ($A,$B) = ('','');
  if (ref $m eq 'HASH') {
    for my $pair (
      ['radiantTeamName','direTeamName'],
      ['radiant_name','dire_name'],
      ['radiantTeam','direTeam'],
      ['teamA','teamB'],
      ['team1','team2'],
    ) {
      my ($ka,$kb)=@$pair;
      my $va = $m->{$ka}; my $vb = $m->{$kb};
      if (!$A && defined $va) { $A = ref $va eq 'HASH' ? ($va->{name}||$va->{shortName}||$va->{displayName}||'') : $va; }
      if (!$B && defined $vb) { $B = ref $vb eq 'HASH' ? ($vb->{name}||$vb->{shortName}||$vb->{displayName}||'') : $vb; }
      last if $A && $B;
    }
    if ((!$A || !$B) && ref $m->{teams} eq 'ARRAY' && @{$m->{teams}}>=2) {
      my $t1 = $m->{teams}[0]; my $t2 = $m->{teams}[1];
      $A ||= (ref $t1 eq 'HASH') ? ($t1->{name}||$t1->{shortName}||'') : '';
      $B ||= (ref $t2 eq 'HASH') ? ($t2->{name}||$t2->{shortName}||'') : '';
    }
  }
  $A ||= 'Radiant'; $B ||= 'Dire';
  return ($A,$B);
}

sub build_email_html {
  my ($A,$B,$teamAName,$teamBName,$series_name) = @_;
  my $totA = per_hero_totals($A,$B);
  my $totB = per_hero_totals($B,$A);
  my $advA = per_hero_advantages($A,$B);
  my $advB = per_hero_advantages($B,$A);
  my $mk_cells = sub {
    my ($ids,$valsTotals,$valsAdv) = @_;
    my $row1 = '';
    my $row2 = '';
    for (my $i=0; $i<5; $i++) {
      my $id = $ids->[$i] // -1;
      my $src = hero_icon_url($id);
      my $nm  = $HEROES[$id] // '';
      my $img = $src ? sprintf('<img src="%s" alt="%s" style="width:96px;height:auto;display:block;margin:0 auto;border-radius:6px;">',$src,$nm)
                     : '<div style="width:96px;height:54px;background:#eee;display:block;margin:0 auto;border-radius:6px;"></div>';
      my $v  = $valsAdv->[$i] // 0;
      my $wr = (defined $HEROES_WR[$id]) ? sprintf('%.2f',$HEROES_WR[$id]) : '--';
      my $sign = $v>=0 ? '+' : '-'; my $abs = sprintf('%.2f', abs($v));
      my $col = $v>=0 ? '#0a0' : '#c00';
      $row1 .= '<td style="text-align:center;padding:8px 6px;">'.$img.'</td>';
      $row2 .= '<td style="text-align:center;padding:0 6px 10px 6px;color:'.$col.';font:14px/16px Arial,Helvetica,sans-serif;">'.$wr.' '.$sign.' '.$abs.'</td>';
    }
    return ($row1,$row2);
  };
  my ($r1a,$r2a) = $mk_cells->($A,$totA,$advA);
  my ($r1b,$r2b) = $mk_cells->($B,$totB,$advB);
  my $sumA = 0; $sumA += $_ for @$totA;
  my $sumB = 0; $sumB += $_ for @$totB;
  my $diff = $sumA - $sumB;
  my $diff_col = $diff>=0 ? '#0a0' : '#c00';
  my $html = '';
  $html .= '<html><body style="margin:0;padding:12px 12px 16px 12px;background:#fff;">';
  $series_name = sanitize_series_name($series_name);
  if ($series_name) { $html .= sprintf('<div style="font:700 12px/16px Arial,Helvetica,sans-serif;margin:0 0 8px 0; text-align:left;">%s</div>', sanitize_series_name($series_name)); }
  $html .= '<div style="width:100%;max-width:560px;">';
  $html .= sprintf('<div style="font:700 14px Arial,Helvetica,sans-serif;margin:8px 0 4px 0;">%s</div>', $teamAName);
  $html .= '<table role="presentation" cellpadding="0" cellspacing="0" style="width:100%;"><tr>'.$r1a.'</tr><tr>'.$r2a.'</tr></table>';
  $html .= sprintf('<div style="font:700 14px Arial,Helvetica,sans-serif;margin:16px 0 4px 0;">%s</div>', $teamBName);
  $html .= '<table role="presentation" cellpadding="0" cellspacing="0" style="width:100%;"><tr>'.$r1b.'</tr><tr>'.$r2b.'</tr></table>';
  $html .= sprintf('<div style="text-align:center;margin:12px 0 0 0;font:700 26px/28px Arial,Helvetica,sans-serif;color:%s;">%s</div>', $diff_col, fmt_adv($diff));
  $html .= '</div>';
  $html .= '</body></html>';
  return $html;
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
    my $checked=0; my $found=0;
    print STDOUT "DEBUG: poll start\n" if $DEBUG;

    # Try JSON endpoints discovered from app bundle
    my $apis = discover_hawk_endpoints();
    my $handled = 0;
    if ($apis && ref $apis eq 'ARRAY' && @$apis) {
      print STDOUT sprintf("DEBUG: trying %d JSON endpoints\n", scalar(@$apis)) if $DEBUG;
      for my $api (@$apis) {
        print STDOUT "DEBUG: fetch JSON -> $api\n" if $DEBUG;
        my $data = fetch_json($api);
        print STDOUT (defined $data ? "DEBUG: got JSON\n" : "DEBUG: no data\n") if $DEBUG;
        next unless $data;
        my $list = [];
        if (ref $data eq 'ARRAY') { $list = $data; }
        elsif (ref $data eq 'HASH') {
          $list = find_match_array_in_hash($data);
        }
        if (ref $list eq 'ARRAY' && @$list) {
          $handled = 1;
          for my $m (@$list) {
            $checked++;
            # Heuristic: extract hero names from fields
            my (@a,@b);
            if (ref $m eq 'HASH') {
              for my $k (qw/picks picksBans teamPicks team_picks radiantPicks direPicks/) {
                if (ref $m->{$k} eq 'ARRAY') {
                  for my $e (@{ $m->{$k} }) {
                    next unless ref $e eq 'HASH';
                    my $isPick = defined $e->{isPick} ? ($e->{isPick}?1:0) : 1;
                    next unless $isPick;
                    my $isRad = ($e->{isRadiant}||$e->{radiant}||$e->{team}&&lc($e->{team})eq'radiant') ? 1 : 0;
                    my $name = '';
                    if (ref $e->{hero} eq 'HASH') { $name = $e->{hero}{displayName} // $e->{hero}{name} // $e->{hero}{shortName} // ''; }
                    $name ||= $e->{heroName} // $e->{name} // '';
                    next unless $name;
                    my $idx = hero_index_by_name($name); next unless $idx>=0;
                    if ($isRad) { push @a,$idx; } else { push @b,$idx; }
                  }
                }
              }
            }
            @a=@a[0..4] if @a>5; @b=@b[0..4] if @b>5;
            next unless @a+@b >= 2;
            my $mid = '';
            if (ref $m eq 'HASH') { $mid = $m->{id} // $m->{matchId} // $m->{gameId} // $m->{url} // $m->{webUrl} // ''; }
            my $key = ($mid ? ($mid.'#') : '') . join(',',@a).'|'.join(',',@b);
            next if $seen{$key}++;
            $found++;
            my $scoreA=team_score(\@a,\@b); my $scoreB=team_score(\@b,\@a); my $diff=$scoreA-$scoreB;
            my $tA = per_hero_totals(\@a,\@b); my $tB = per_hero_totals(\@b,\@a);
            my ($sumA2,$sumB2)=(0,0); $sumA2+=$_ for @$tA; $sumB2+=$_ for @$tB;
            my $diff_alert = $sumA2 - $sumB2;
            print STDOUT sprintf("DEBUG: sums(A=%.2f,B=%.2f) diff_alert=%.2f ta_min=%s ta_max=%s\n", $sumA2,$sumB2,$diff_alert, (defined $ta_min?$ta_min:'-'), (defined $ta_max?$ta_max:'-')) if $DEBUG;
            my @conds; if($ta_en && (defined $ta_min || defined $ta_max)){ my $ok=0; $ok||=(defined $ta_min && $diff_alert>=$ta_min); $ok||=(defined $ta_max && $diff_alert<=$ta_max); push @conds,$ok?1:0 }
            if($ha_en && defined $ha_thr){ my $ok = any_hero_adv_threshold(\@b,$ha_cond,$ha_thr); push @conds,$ok?1:0 }
            if($wh_en && %wh){ my $ok=0; for my $hid (@a,@b){ next unless $hid>=0; my $nm=normalize_name($HEROES[$hid]||''); if($wh{$nm}){ $ok=1; last } } push @conds,$ok?1:0 }
            my $alert = (!@conds)?0:($logic eq 'all' ? ((grep{!$_}@conds)?0:1) : ((grep{$_}@conds)?1:0));
            if($alert){
              my $to=$s->{email_to}||''; my $from=$s->{email_from}||'';
              my ($teamAName,$teamBName) = extract_team_names_from_match($m);
              $teamAName = sanitize_team_label($teamAName);
              $teamBName = sanitize_team_label($teamBName);
              my $series = extract_series_from_match($m);
              my $sub = $series ? sprintf('%s vs %s - %s', $teamAName, $teamBName, $series)
                                : sprintf('%s vs %s', $teamAName, $teamBName);
              my $html = build_email_html(\@a,\@b,$teamAName,$teamBName,$series);
              if(send_email(to=>$to,from=>$from,subject=>$sub,body=>$html,html=>1)){ $st->{alerts}=($st->{alerts}||0)+1; $st->{last_alert_at}=time; print STDOUT sprintf("ALERT sent (API) diff_alert=%.2f (old_diff=%.2f)\n", $diff_alert, $diff) } else { print STDOUT sprintf("ALERT FAILED (API) diff_alert=%.2f (old_diff=%.2f)\n", $diff_alert, $diff) }
            } else { print STDOUT sprintf("No alert (API): diff_alert=%.2f\n", $diff_alert); }
          }
        }
      }
    }

    # Fallback to HTML if no JSON candidates matched
    if (!$handled) {
      print STDOUT "DEBUG: no JSON handled; falling back to HTML\n" if $DEBUG;
      my %seen_url; my @all_urls;
      # Add direct match URLs from env if provided
      for my $u (@HAWK_MATCH_URLS){ next unless defined $u && length $u; $u =~ s/^\s+|\s+$//g; next if $seen_url{$u}++; push @all_urls, $u; }
      for my $p (@HAWK_LIVE_PATHS){
        my @variants; push @variants, url_cat($HAWK_BASE,$p);
        for my $lang (qw/en ru/){ push @variants, url_cat($HAWK_BASE, $p =~ m{^/} ? "/$lang".$p : "/$lang/$p"); }
        for my $root (@variants){
          print STDOUT "DEBUG: fetch root -> $root\n" if $DEBUG;
          my $html = fetch_html($root);
          next unless $html && $html =~ /(match|live|dota)/i;
          my $urls = parse_live_links($html);
          print STDOUT sprintf("DEBUG: root %s links=%d\n", $root, scalar(@$urls)) if $DEBUG;
          for my $u (@$urls){ next if $seen_url{$u}++; push @all_urls, $u; }
        }
      }
      if (@all_urls) {
        print STDOUT sprintf("DEBUG: found %d total live match links\n", scalar(@all_urls)) if $DEBUG;
        for my $u (@all_urls){
          $checked++;
          print STDOUT "DEBUG: fetch match -> $u\n" if $DEBUG;
          my $html = fetch_html($u); next unless $html && $html =~ /hero|pick|draft|Radiant|Dire|background-image|data-hero/i;
          my ($a,$b) = extract_picks_from_html($html); next unless $a && $b && (@$a+@$b)>=2;
          my ($mid) = $u =~ m{/(\d+)(?:/|$)}; $mid ||= '';
          my $key = ($mid ? ($mid.'#') : '') . join(',',@$a).'|'.join(',',@$b); next if $seen{$key}++;
          $found++;
          my $scoreA = team_score($a,$b); my $scoreB = team_score($b,$a); my $diff = $scoreA-$scoreB;
          my $tA = per_hero_totals($a,$b); my $tB = per_hero_totals($b,$a);
          my ($sumA2,$sumB2)=(0,0); $sumA2+=$_ for @$tA; $sumB2+=$_ for @$tB;
          my $diff_alert = $sumA2 - $sumB2;
          print STDOUT sprintf("DEBUG: sums(A=%.2f,B=%.2f) diff_alert=%.2f ta_min=%s ta_max=%s\n", $sumA2,$sumB2,$diff_alert, (defined $ta_min?$ta_min:'-'), (defined $ta_max?$ta_max:'-')) if $DEBUG;
          my @conds; if($ta_en && (defined $ta_min || defined $ta_max)){ my $ok=0; $ok||=(defined $ta_min && $diff_alert>=$ta_min); $ok||=(defined $ta_max && $diff_alert<=$ta_max); push @conds,$ok?1:0 }
          if($ha_en && defined $ha_thr){ my $ok = any_hero_adv_threshold($b,$ha_cond,$ha_thr); push @conds,$ok?1:0 }
          if($wh_en && %wh){ my $ok=0; for my $hid (@$a,@$b){ next unless $hid>=0; my $nm=normalize_name($HEROES[$hid]||''); if($wh{$nm}){ $ok=1; last } } push @conds,$ok?1:0 }
          my $alert = (!@conds) ? 0 : ($logic eq 'all' ? ((grep{!$_}@conds)?0:1) : ((grep{$_}@conds)?1:0));
          if($alert){
            my $to=$s->{email_to}||''; my $from=$s->{email_from}||'';
            my ($teamAName,$teamBName) = extract_team_names_from_html($html);
            $teamAName = sanitize_team_label($teamAName);
            $teamBName = sanitize_team_label($teamBName);
            my $series = extract_series_from_html($html);
            next if $teamAName eq 'Radiant' && $teamBName eq 'Dire';
            my $sub = $series ? sprintf('%s vs %s - %s', $teamAName, $teamBName, $series)
                              : sprintf('%s vs %s', $teamAName, $teamBName);
            my $html_body = build_email_html($a,$b,$teamAName,$teamBName,$series);
            if(send_email(to=>$to,from=>$from,subject=>$sub,body=>$html_body,html=>1)){ $st->{alerts}=($st->{alerts}||0)+1; $st->{last_alert_at}=time; print STDOUT sprintf("ALERT sent (API) diff_alert=%.2f (old_diff=%.2f)\n", $diff_alert, $diff) } else { print STDOUT sprintf("ALERT FAILED (API) diff_alert=%.2f (old_diff=%.2f)\n", $diff_alert, $diff) }
          } else { print STDOUT sprintf("No alert (API): diff_alert=%.2f\n", $diff_alert); }
        }
      }
    }
    $st->{last_poll}=time; $st->{last_checked}=$checked; $st->{last_found}=$found; save_status($st);
    sleep($POLL_SECS);
  }
}

main_loop();

