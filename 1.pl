#!/usr/bin/env perl
##########################################################################
# generate-data_js.pl - Generate data.js for dotabuffcp                  #
# Copyright (C) <2014>  Onur Aslan  <onuraslan@gmail.com>                #
#                                                                        #
# See COPYING for distribution information.                              #
##########################################################################


use strict;
use warnings;
use JSON::PP qw(decode_json encode_json);
use POSIX qw/strftime/;
use HTTP::Tiny;



my $DEBUG = 0;
BEGIN { select(STDERR); $|=1; select(STDOUT); $|=1; }
my @heroes;
my @heroes_bg;
my @heroes_wr;
my @win_rates;
my %slug_to_index;

my $http = HTTP::Tiny->new (
  agent => 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0 Safari/537.36 Perl-HTTP::Tiny',
  timeout => 60,
);

my $FLARESOLVERR_URL = $ENV{FLARESOLVERR_URL} // 'http://89.111.153.115:8191/v1';
my $FLARE_SESSION_ID;
my $FLARE_HEALTHY;
my $SCRAPEDO_API_KEY = $ENV{SCRAPEDO_API_KEY} // '';
my $SCRAPEDO_ENDPOINT = $ENV{SCRAPEDO_ENDPOINT} // 'https://api.scrape.do';

sub flare_healthy {
  return $FLARE_HEALTHY if defined $FLARE_HEALTHY;
  return $FLARE_HEALTHY = 0 unless $FLARESOLVERR_URL;
  my $hurl = $FLARESOLVERR_URL; $hurl =~ s{/v1$}{/health};
  my $res = $http->get($hurl);
  if ($res->{success} && ($res->{content}||'') =~ /ok/i) { $FLARE_HEALTHY = 1; }
  else { $FLARE_HEALTHY = 0; }
  $DEBUG and warn "FlareSolverr health: " . ($FLARE_HEALTHY?'ok':'unavailable') . "\n";
  return $FLARE_HEALTHY;
}

sub flare_session_create {
  return if !$FLARESOLVERR_URL || $FLARE_SESSION_ID;
  my $res = $http->post($FLARESOLVERR_URL, {
    headers => { 'Content-Type' => 'application/json' },
    content => '{"cmd":"sessions.create"}',
  });
  if ($res->{success}) {
    my $j; eval { $j = decode_json($res->{content}); };
    $FLARE_SESSION_ID = $j->{session} if !$@ && $j && $j->{session};
    warn "Created FlareSolverr session: $FLARE_SESSION_ID\n" if $DEBUG && $FLARE_SESSION_ID;
  }
}

sub flare_session_destroy {
  return unless $FLARE_SESSION_ID;
  my $payload = '{"cmd":"sessions.destroy","session":"' . $FLARE_SESSION_ID . '"}';
  $http->post($FLARESOLVERR_URL, { headers => { 'Content-Type' => 'application/json' }, content => $payload });
  $FLARE_SESSION_ID = undef;
}

sub read_data {
  my $content = '';
  $content .= $_ while (<DATA>);
  return $content;
}


sub hero_link {
  my $hero = $_[0];
  $hero =~ s/'//g;
  $hero =~ s/ /-/g;
  $hero =~ tr/[A-Z]/[a-z]/;
  return $hero;
}


sub normalize_hero_name {
  my ($name) = @_;
  $name =~ s/'//g;
  return $name;
}


sub hero_id {
  my $hero = $_[0];

  my $c = 0;
  for (@heroes) {
    return $c if ($_ eq $hero);
    ++$c;
  }
  return -1;
}


sub http_get_json {
  my ($url) = @_;
  my $attempt = 0;
  my $max_attempts = 3;
  while (1) {
    my $res = $http->get ($url);
    if ($res->{success}) {
      my $data;
      my $content = $res->{content};
      eval {
        $data = decode_json ($content);
      };
      if ($@) {
        warn "JSON decode error for $url: $@\n";
        $data = undef;
      }
      return $data if defined $data;
    } else {
      warn "HTTP error $res->{status} for $url";
    }
    $attempt++;
    last if $attempt >= $max_attempts;
    my $sleep = $attempt < 5 ? 5 : 15;
    warn "Retrying in $sleep seconds...\n";
    sleep ($sleep);
  }
  return undef;
}

sub fetch_with_cf {
  my ($url) = @_;

  if ($FLARESOLVERR_URL && flare_healthy()) {
    # Always use a session for Dotabuff
    flare_session_create();
    my $payload_obj = {
      cmd => 'request.get',
      url => $url,
      maxTimeout => 90000,
      headers => {
        'User-Agent' => 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0 Safari/537.36',
        'Accept' => 'text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8',
        'Accept-Language' => 'en-US,en;q=0.9',
        'Referer' => 'https://www.dotabuff.com/',
      },
    };
    $payload_obj->{session} = $FLARE_SESSION_ID if $FLARE_SESSION_ID;
    my $res = $http->post ($FLARESOLVERR_URL, {
      headers => { 'Content-Type' => 'application/json' },
      content => encode_json($payload_obj),
    });
    if ($res->{success}) {
      my $data;
      eval { $data = decode_json ($res->{content}); };
      if (!$@ && ref $data eq 'HASH' && ($data->{status} // '') eq 'ok') {
        if (defined $data->{solution} && defined $data->{solution}->{response}) {
          return $data->{solution}->{response};
        }
      }
    }
  }

  # Try Scrape.do as a fallback if configured
  if ($SCRAPEDO_API_KEY) {
    my $content = fetch_with_scrapedo($url);
    return $content if defined $content;
  }

  my $res2 = $http->get ($url);
  return $res2->{success} ? $res2->{content} : undef;
}

sub url_encode {
  my ($s) = @_;
  $s //= '';
  $s =~ s/([^A-Za-z0-9\-_.~])/sprintf("%%%02X", ord($1))/eg;
  return $s;
}

sub fetch_with_scrapedo {
  my ($url) = @_;
  return undef unless $SCRAPEDO_API_KEY;
  my $ep = $SCRAPEDO_ENDPOINT;
  my $render = defined $ENV{SCRAPEDO_RENDER} ? $ENV{SCRAPEDO_RENDER} : '';
  my $qs = 'token=' . url_encode($SCRAPEDO_API_KEY) . '&url=' . url_encode($url);
  $qs .= '&render=true' if $render && $render =~ /^(1|true|yes)$/i;
  my $api_url = $ep . '?' . $qs;
  my $res = $http->get($api_url);
  return $res->{success} ? $res->{content} : undef;
}

sub normalize_html_text {
  my ($t) = @_;
  $t //= '';
  $t =~ s/&[^;]+;//g;   # strip basic HTML entities
  $t =~ s/'//g;         # align with local hero names (e.g., Nature's -> Natures)
  $t =~ s/^\s+|\s+$//g;
  return $t;
}

# Extract overall Win Rate (top-of-page) from a Dotabuff hero HTML
sub extract_win_rate_from_html {
  my ($html) = @_;
  return undef unless defined $html && length $html;

  my $scope = $html;
  # Limit to content before the first table to avoid row percentages
  if ($scope =~ m{^(.*?<table)}is) { $scope = $1; }

  # Pattern 1: dt then dd
  if ($scope =~ m{<dt>\s*Win Rate\s*</dt>\s*<dd[^>]*>\s*(?:<span[^>]*class="(?:won|lost)"[^>]*>)?\s*([0-9]+(?:\.[0-9]+)?)%}is) {
    return $1;
  }
  # Pattern 2: dd then dt
  if ($scope =~ m{<dd[^>]*>\s*(?:<span[^>]*class="(?:won|lost)"[^>]*>)?\s*([0-9]+(?:\.[0-9]+)?)%\s*</dd>\s*<dt>\s*Win Rate\s*</dt>}is) {
    return $1;
  }
  # Proximity search around label
  if ($scope =~ m{Win\s*Rate(.{0,400})<span[^>]*class="(?:won|lost)"[^>]*>\s*([0-9]+(?:\.[0-9]+)?)%}is) {
    return $2;
  }
  # Fallback: first won/lost span before tables
  if ($scope =~ m{<span[^>]*class="(?:won|lost)"[^>]*>\s*([0-9]+(?:\.[0-9]+)?)%}is) {
    return $1;
  }
  return undef;
}

sub get_dotabuff_counters_for_hero {
  my ($idx) = @_;
  my $slug = hero_link ($heroes[$idx]);

  # Skip separate main page fetch for speed; try to extract WR from counters page when possible
  # my $url_main = 'https://www.dotabuff.com/heroes/' . $slug;
  # my $html_main = fetch_with_cf ($url_main);
  # if (defined $html_main) {
  #   my $wr = extract_win_rate_from_html ($html_main);
  #   if (defined $wr) { $heroes_wr[$idx] = sprintf('%.2f', $wr); }
  # }

  # 2) Fetch counters page for matchup rows and (as fallback) Win Rate
  my $url = 'https://www.dotabuff.com/heroes/' . $slug . '/counters?date=year';
  $DEBUG and warn "Getting Dotabuff counters for $heroes[$idx] at $url\n";

  my $html = fetch_with_cf ($url);
  return unless defined $html;

  # Fallback Win Rate extraction if header not set
  if (!defined $heroes_wr[$idx] || $heroes_wr[$idx] eq '50.00') {
    my $wr2 = extract_win_rate_from_html ($html);
    if (defined $wr2) {
      $heroes_wr[$idx] = sprintf('%.2f', $wr2);
    }
  }

  while ($html =~ m{<tr[^>]*>(.*?)</tr>}sig) {
    my $row = $1;

    my ($slug2) = $row =~ m{<a[^>]+href="/heroes/([a-z0-9-]+)["#]}i;
    next unless $slug2;
    my $opp_idx = $slug_to_index{$slug2};
    next if !defined $opp_idx;

    my @vals = ($row =~ m{<td[^>]*data-value="([\-0-9.,]+)"[^>]*>}gi);
    my ($adv,$wr_vs) = (undef, undef);
    if (@vals >= 2) { ($adv,$wr_vs) = ($vals[0], $vals[1]); }
    else {
      my @tds = ($row =~ m{<td[^>]*>(.*?)</td>}sig);
      for my $td (@tds) {
        my ($num) = $td =~ m{([\-+]?\d+(?:\.\d+)?)%}i;
        next unless defined $num;
        if (!defined $adv) { $adv = $num; next; }
        if (!defined $wr_vs) { $wr_vs = $num; last; }
      }
    }
    for ($adv, $wr_vs) { $_ = defined $_ ? $_ : 0; s/,//g; }

    my ($matches) = $row =~ m{data-value="([0-9,]+)"[^>]*>\s*[0-9,]+\s*<}i;
    $matches //= 0; $matches =~ s/,//g;

    $win_rates[$idx][$opp_idx] = [ sprintf('%.4f', $adv+0), sprintf('%.4f', $wr_vs+0), 0 + $matches ];
  }
}

sub get_winrates_dotabuff {
  my $total = scalar @heroes;
  warn "Fetching matchups for $total heroes\n";
  my $CONCURRENCY = $ENV{GEN_CONCURRENCY} ? 0 + $ENV{GEN_CONCURRENCY} : 4;
  $CONCURRENCY = 1 if $CONCURRENCY < 1;
  if ($CONCURRENCY == 1) {
    for (my $i = 0; $i < $total; $i++) {
      warn sprintf("[matchups %3d/%3d] %-20s\n", $i+1, $total, $heroes[$i]);
      get_dotabuff_counters_for_hero ($i);
    }
    return;
  }

  my $tmpdir = ".dotabuff_tmp_$$";
  mkdir $tmpdir;

  my $chunk = int(($total + $CONCURRENCY - 1) / $CONCURRENCY);
  my @pids;
  for (my $w = 0; $w < $CONCURRENCY; $w++) {
    my $start = $w * $chunk;
    my $end   = $start + $chunk - 1;
    $end = $total - 1 if $end >= $total;
    next if $start > $end;

    my $pid = fork();
    if (!defined $pid) { warn "fork() failed"; next; }
    if ($pid == 0) {
      # child
      for (my $i = $start; $i <= $end; $i++) {
        warn sprintf("[matchups %3d/%3d] %-20s (worker %d/%d)\n", $i+1, $total, $heroes[$i], $w+1, $CONCURRENCY);
        get_dotabuff_counters_for_hero($i);
        # write per-hero row
        my $out = {
          idx => $i,
          wr  => $heroes_wr[$i],
          row => $win_rates[$i] || [],
        };
        if (open my $fh, '>', "$tmpdir/h_$i.json") {
          print $fh encode_json($out);
          close $fh;
        }
      }
      exit 0;
    } else {
      push @pids, $pid;
    }
  }

  # wait for children
  for my $pid (@pids) { waitpid($pid, 0); }

  # merge results
  opendir(my $dh, $tmpdir);
  while (my $f = readdir($dh)) {
    next unless $f =~ /^h_(\d+)\.json$/;
    my $i = 0 + $1;
    my $path = "$tmpdir/$f";
    if (open my $fh, '<', $path) {
      local $/; my $txt = <$fh>; close $fh;
      my $j; eval { $j = decode_json($txt) };
      if (!$@ && $j && defined $j->{idx}) {
        my $idx = 0 + $j->{idx};
        $heroes_wr[$idx] = $j->{wr} if defined $j->{wr};
        $win_rates[$idx] = $j->{row} if defined $j->{row};
      }
    }
    unlink $path;
  }
  closedir($dh);
  rmdir $tmpdir;
}


sub get_heroes {
  warn "Fetching hero list from Dotabuff via FlareSolverr\n";
  my $list_url_primary = 'https://www.dotabuff.com/heroes?date=1y';
  # Fallback page removed to avoid facet-based duplicates

  my @pairs;
  my %seen;
  my %slug_to_img;

  my $collect_from_html = sub {
    my ($html) = @_;
    return 0 unless defined $html && length $html;

    # 1) Prefer anchors explicitly tagged as hero links (by class or data attribute)
    while ($html =~ m{<a[^>]*(?:class="[^"]*(?:link-type-hero|hero-link)[^"]*"|data-link-type="hero")[^>]*href="(?:https?://www\.dotabuff\.com)?/heroes/([a-z0-9-]+)(?:["/#?])}ig) {
      my $slug = $1;
      next if $seen{$slug}++;
      my $name = $slug; $name =~ s/-/ /g; $name =~ s/\b(\w)/\U$1/g; $name = normalize_hero_name ($name);
      push @pairs, [ $name, $slug ];
    }

    # Gather hero image references present in the page (maps slug->image path)
    while ($html =~ m{(/assets/heroes/([a-z0-9-]+)[^"]*?\.(?:jpg|jpeg|png|webp))}ig) {
      my ($path, $s) = ($1, $2);
      $slug_to_img{$s} //= 'https://www.dotabuff.com' . $path;
    }

    # 2) Fallback selector on the SAME PAGE: any anchor to /heroes/<slug> excluding known non-hero slugs
    if (@pairs < 100) {
      while ($html =~ m{href="(?:https?://www\.dotabuff\.com)?/heroes/([a-z0-9-]+)(?:["/#?])}ig) {
        my $slug = $1;
        next if $seen{$slug}++;
        next if $slug =~ /^(?:meta|played|winning|damage|economy|lanes|statistics|compare|guides|matchups|positions|talents|trends|most-|least-)$/i;
        my $name = $slug; $name =~ s/-/ /g; $name =~ s/\b(\w)/\U$1/g; $name = normalize_hero_name ($name);
        push @pairs, [ $name, $slug ];
      }
    }

    return scalar @pairs;
  };

  warn "GET " . $list_url_primary . "\n";
  my $html1 = fetch_with_cf ($list_url_primary);
  if (defined $html1 && $html1 =~ /Just a moment|cf-browser-verification|Cloudflare|Attention Required/i) {
    warn "Cloudflare interstitial detected; retrying with session...\n";
    $ENV{GEN_USE_SESSION} = 1;
    flare_session_create();
    $html1 = fetch_with_cf ($list_url_primary);
  }
  my $count1 = $collect_from_html->($html1);
  warn "Found $count1 hero candidates\n";

  # Fallback URLs if primary yields too few (site variations)
  if ($count1 < 50) {
    my $alt1 = 'https://www.dotabuff.com/heroes?date=1y&show=heroes';
    warn "GET " . $alt1 . "\n";
    my $h2 = fetch_with_cf($alt1);
    my $c2 = $collect_from_html->($h2);
    warn "Fallback alt1 found $c2 heroes\n";
    if ($c2 < 50) {
      my $alt2 = 'https://www.dotabuff.com/heroes?view=meta&mode=all-pick&date=1y';
      warn "GET " . $alt2 . "\n";
      my $h3 = fetch_with_cf($alt2);
      my $c3 = $collect_from_html->($h3);
      warn "Fallback alt2 found $c3 heroes\n";
      if ($c3 < 50) {
        my $alt3 = 'https://www.dotabuff.com/heroes';
        warn "GET " . $alt3 . "\n";
        my $h4 = fetch_with_cf($alt3);
        my $c4 = $collect_from_html->($h4);
        warn "Fallback alt3 found $c4 heroes\n";
      }
    }
  }

  # Final fallback: build from image slugs if anchors failed but images exist
  if (@pairs < 50 && (scalar keys %slug_to_img) >= 50) {
    for my $slug (keys %slug_to_img) {
      next if $seen{$slug}++;
      my $name = $slug; $name =~ s/-/ /g; $name =~ s/\b(\w)/\U$1/g; $name = normalize_hero_name ($name);
      push @pairs, [ $name, $slug ];
    }
    warn sprintf("Image-based fallback produced %d heroes\n", scalar @pairs);
  }

  # Sort alphabetically and finalize
  @pairs = grep { defined $_ && defined $_->[0] && defined $_->[1] } @pairs;
  # Drop known non-hero slugs just in case
  @pairs = grep { $_->[1] !~ /^(?:meta|played|winning|damage|economy|lanes|statistics|compare|guides|matchups|positions|talents|trends)$/i } @pairs;
  # If we collected many images, prefer slugs that have an image; otherwise keep all
  if ((scalar keys %slug_to_img) > (@pairs/2)) {
    @pairs = grep { exists $slug_to_img{ $_->[1] } } @pairs;
  }
  @pairs = sort { lc($a->[0]) cmp lc($b->[0]) } @pairs;

  # If still clearly wrong, fail fast only when nothing parsed
  if (@pairs < 50) {
    if (@pairs == 0) {
      die "No heroes parsed from Dotabuff; check FlareSolverr and markup";
    } else {
      warn sprintf("Few heroes parsed (%d); continuing anyway\n", scalar @pairs);
    }
  }

  # Populate heroes, images, and seed win rates (to be refined from counters pages)
  for my $p (@pairs) {
    my ($name, $slug) = @$p;
    push @heroes, $name;
    $slug_to_index{$slug} = scalar(@heroes) - 1;
    my $img_url = $slug_to_img{$slug};
    if (!defined $img_url) {
      my $react_slug = $slug; $react_slug =~ s/[^a-z0-9]+/_/g; $react_slug =~ s/__+/_/g;
      $img_url = 'https://cdn.cloudflare.steamstatic.com/apps/dota2/images/dota_react/heroes/' . $react_slug . '.png';
    }
    push @heroes_bg, $img_url;
    push @heroes_wr, sprintf('%.2f', 50.0);
  }
  warn sprintf("Parsed %d heroes\n", scalar @heroes);
  # heroes_wr will be updated per hero from counters pages
}


sub get_winrates_of_hero_by_index { return; }


sub get_winrates {
  get_winrates_dotabuff ();
}


sub print_winrates {
  my $json = JSON::PP->new;

  warn "Writing win rates to cs.json\n";

  my $content = '';
  $content .= 'var heroes = ' . $json->encode ([ @heroes ]);
  $content .= ', heroes_bg = ' . $json->encode ([ @heroes_bg ]);
  $content .= ', heroes_wr = ' . $json->encode ([ @heroes_wr ]);
  $content .= ', win_rates = ' . $json->encode ([ @win_rates ]);
  $content .= ', update_time = "' . strftime("%Y-%m-%d", localtime (time ())) . '";\n';

  open my $fh, '>cs.json';
  print $fh $content; close $fh;
  if (open my $fh2, '>cs.js') { print $fh2 $content; close $fh2; }
  warn "Wrote cs.json and cs.js\n";
}


$_ eq '--debug' and $DEBUG++ for @ARGV;

get_heroes ();
get_winrates ();
print_winrates ();

__DATA__