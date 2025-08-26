#!/usr/bin/env perl
use strict;
use warnings;
use JSON::PP qw(decode_json);
use POSIX qw/strftime/;
use HTTP::Tiny;
use Time::HiRes qw(sleep);

# Autoflush so progress appears immediately
BEGIN { select(STDERR); $|=1; select(STDOUT); $|=1; }

my $DEBUG = 0;
$_ eq '--debug' and $DEBUG++ for @ARGV;

my @heroes;
my @heroes_bg;
my @heroes_wr;
my @win_rates;
my %od_id_to_index;

my $http = HTTP::Tiny->new(
  agent  => 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0 Safari/537.36 Perl-HTTP::Tiny',
  timeout => 60,
  verify_SSL => 0,
);

my $OD_API_KEY       = $ENV{OPEN_DOTA_API_KEY} // $ENV{OD_API_KEY} // '';
my $OD_DELAY_SECS    = defined $ENV{OPEN_DOTA_DELAY_SECS} ? 0.0 + $ENV{OPEN_DOTA_DELAY_SECS} : (defined $ENV{OD_DELAY_SECS} ? 0.0 + $ENV{OD_DELAY_SECS} : 3.5);
my $OD_MAX_RETRIES   = defined $ENV{OPEN_DOTA_MAX_RETRIES} ? int($ENV{OPEN_DOTA_MAX_RETRIES}) : (defined $ENV{OD_MAX_RETRIES} ? int($ENV{OD_MAX_RETRIES}) : 12);
my $OD_DELAY_JITTER  = defined $ENV{OPEN_DOTA_DELAY_JITTER} ? 0.0 + $ENV{OPEN_DOTA_DELAY_JITTER} : 0.7;

sub append_api_key {
  my ($url) = @_;
  return $url unless $OD_API_KEY;
  return $url . (($url =~ /\?/ ? '&' : '?') . 'api_key=' . $OD_API_KEY);
}

sub http_get_json {
  my ($orig_url) = @_;
  my $url = append_api_key($orig_url);
  my $attempt = 0;
  while ($attempt < $OD_MAX_RETRIES) {
    $attempt++;
    my $res = $http->get($url);
    if ($res->{success}) {
      my $data;
      eval { $data = decode_json($res->{content}); };
      if (!$@ && defined $data) {
        my $wait = $OD_DELAY_SECS + ($OD_DELAY_JITTER > 0 ? rand($OD_DELAY_JITTER) : 0);
        sleep($wait) if $wait > 0;
        return $data;
      }
      warn "HTTP::Tiny JSON decode error for $url: $@\n" if $DEBUG;
    } else {
      my $status = $res->{status} || 0;
      warn sprintf("HTTP::Tiny fetch failed %d for %s (attempt %d/%d)\n", $status, $url, $attempt, $OD_MAX_RETRIES) if $DEBUG;
      if ($status == 429 || $status >= 500) {
        my $ra = 0;
        if ($res->{headers} && (my $h = $res->{headers}{'retry-after'})) {
          $ra = $h =~ /^(\d+(?:\.\d+)?)$/ ? 0.0 + $1 : 0;
        }
        my $backoff = $ra > 0 ? $ra : (2 ** ($attempt - 1));
        $backoff = 30 if $backoff > 30;
        sleep($backoff);
        next;
      }
    }
    # Fallback to curl with retries
    my $ua = $http->{agent} || 'curl';
    my $cmd = "curl -sS --fail --compressed --retry 5 --retry-all-errors --retry-delay 2 -H 'User-Agent: $ua' '" . $url . "'";
    my $out = `$cmd`;
    if ($? == 0 && $out) {
      my $data2;
      eval { $data2 = decode_json($out); };
      if (!$@ && defined $data2) {
        my $wait = $OD_DELAY_SECS + ($OD_DELAY_JITTER > 0 ? rand($OD_DELAY_JITTER) : 0);
        sleep($wait) if $wait > 0;
        return $data2;
      }
      warn "curl JSON decode error for $url: $@\n" if $DEBUG;
    } else {
      warn sprintf("curl failed for %s (exit %d)\n", $url, $? ) if $DEBUG;
    }
    # backoff before next loop
    sleep(2 ** ($attempt - 1));
  }
  return undef;
}

sub normalize_name {
  my ($name) = @_;
  $name //= '';
  $name =~ s/'//g;
  return $name;
}

sub fetch_heroes {
  warn "Fetching heroStats from OpenDota\n";
  my $stats = http_get_json('https://api.opendota.com/api/heroStats');
  die "Failed to fetch heroStats" unless defined $stats && ref $stats eq 'ARRAY';

  my $total = scalar(@$stats);
  my $idx = 0;
  for my $h (@$stats) {
    my $api_id = $h->{id};
    my $name = normalize_name($h->{localized_name});
    my $img  = $h->{img} // '';

    my $total_picks = 0;
    my $total_wins  = 0;
    for my $br (1..8) {
      my $p = $h->{"${br}_pick"} // 0;
      my $w = $h->{"${br}_win"}  // 0;
      $total_picks += $p;
      $total_wins  += $w;
    }
    my $wr = 0.0;
    $wr = 100.0 * $total_wins / $total_picks if $total_picks > 0;

    $od_id_to_index{$api_id} = $idx;
    $heroes[$idx]    = $name;
    my $file = '';
    if ($img =~ m{/heroes/([^/?]+)}) { $file = $1; $file =~ s/\?.*$//; }
    $heroes_bg[$idx] = $file ? ('https://cdn.cloudflare.steamstatic.com/apps/dota2/images/dota_react/heroes/' . $file) : '';
    $heroes_wr[$idx] = sprintf('%.2f', $wr);
    warn sprintf("[heroes %3d/%3d] %-20s WR=%s\n", $idx+1, $total, $name, $heroes_wr[$idx]);
    $idx++;
  }

  warn sprintf("Loaded %d heroes\n", scalar @heroes);
}

sub fetch_matchups_for_hero {
  my ($idx) = @_;
  my $api_id;
  for my $k (keys %od_id_to_index) { if ($od_id_to_index{$k} == $idx) { $api_id = $k; last; } }
  return unless defined $api_id;

  my $url = 'https://api.opendota.com/api/heroes/' . $api_id . '/matchups';
  my $rows = http_get_json($url);
  return unless defined $rows && ref $rows eq 'ARRAY';

  for my $m (@$rows) {
    my $opp_od  = $m->{hero_id};
    my $games   = $m->{games} // $m->{games_played} // 0;
    my $wins    = $m->{wins} // 0;
    next unless $opp_od && $games > 0;
    my $opp_idx = $od_id_to_index{$opp_od};
    next unless defined $opp_idx;

    my $wr_vs = 100.0 * $wins / $games;
    my $adv   = $wr_vs - 50.0;

    $win_rates[$opp_idx][$idx] = [
      sprintf('%.4f', -$adv),
      sprintf('%.4f', $wr_vs),
      0 + $games,
    ];
  }
}

sub fetch_matchups {
  my $total = scalar @heroes;
  warn "Fetching matchups for " . $total . " heroes\n";
  for (my $i = 0; $i < @heroes; $i++) {
    warn sprintf("[matchups %3d/%3d] %-20s\n", $i+1, $total, $heroes[$i]);
    fetch_matchups_for_hero($i);
  }
}

sub write_cs_json {
  my $j = JSON::PP->new;
  my $content = '';
  $content .= 'var heroes = '    . $j->encode([ @heroes ]);
  $content .= ', heroes_bg = '   . $j->encode([ @heroes_bg ]);
  $content .= ', heroes_wr = '   . $j->encode([ @heroes_wr ]);
  $content .= ', win_rates = '   . $j->encode([ @win_rates ]);
  $content .= ', update_time = "' . strftime("%Y-%m-%d", localtime) . '";\n';

  open my $fh, '>cs.json' or die $!;
  print $fh $content; close $fh;
  # Also write a JS extension variant for hosts that block .json
  if (open my $fh2, '>cs.js') { print $fh2 $content; close $fh2; }
  warn "Wrote cs.json and cs.js\n";
}

fetch_heroes();
fetch_matchups();
write_cs_json();