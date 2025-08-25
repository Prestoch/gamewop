#!/usr/bin/env perl
use strict;
use warnings;
use JSON::PP qw(decode_json encode_json);
use CGI qw/ -utf8 /;
use CGI::Carp qw(fatalsToBrowser);
use File::Spec;
use File::Basename qw(dirname);
use Cwd qw(abs_path);
use Time::HiRes qw(usleep);

my $BASE_DIR = dirname(abs_path($0));
sub local_file { my ($f) = @_; return File::Spec->catfile($BASE_DIR, $f); }

sub respond {
  my ($obj) = @_;
  print "Content-Type: application/json\r\n\r\n";
  print encode_json($obj);
  exit 0;
}

sub read_body {
  my $len = $ENV{CONTENT_LENGTH} || 0;
  my $data = '';
  if ($len > 0) { read STDIN, $data, $len; }
  return $data;
}

sub load_settings {
  my $path = local_file('settings.json');
  if (open my $fh, '<', $path) {
    local $/; my $txt = <$fh>; close $fh;
    my $s; eval { $s = decode_json($txt || '{}'); };
    return $@ ? {} : ($s || {});
  }
  return {};
}

sub save_settings {
  my ($settings) = @_;
  my $path = local_file('settings.json');
  open my $fh, '>', $path or return 0;
  print $fh encode_json($settings);
  close $fh;
  return 1;
}

sub run_generate {
  my ($source) = @_;
  $source ||= 'opendota';
  my $script = $source eq 'dotabuff' ? '1.pl' : 'opendota.pl';
  my $full = local_file($script);
  unless (-x $full || -f $full) { return (0, "Script not found in web root: $script"); }
  chdir($BASE_DIR);
  my $cmd = "/usr/bin/env perl $full";
  my $out = `$cmd 2>&1`;
  my $ok = ($? == 0) ? 1 : 0;
  return ($ok, $out || ($ok ? 'Generation done' : ''));
}

# ----- Async generation helpers -----
sub gen_pid_file    { return local_file('.gen_pid'); }
sub gen_status_file { return local_file('.gen_status.json'); }
sub gen_log_file    { return local_file('.gen_output.log'); }
sub watch_pid_file  { return local_file('.watch_pid'); }
sub watch_status_file { return local_file('.watch_status.json'); }
sub watch_log_file  { return local_file('.watch_output.log'); }

sub read_json_file {
  my ($path, $fallback) = @_;
  if (open my $fh, '<', $path) {
    local $/; my $txt = <$fh>; close $fh;
    my $data = eval { decode_json($txt || '{}') };
    return $@ ? ($fallback // {}) : ($data // ($fallback // {}));
  }
  return $fallback // {};
}

sub write_json_file {
  my ($path, $obj_ref) = @_;
  if (open my $fh, '>', $path) {
    print $fh encode_json($obj_ref || {});
    close $fh;
    return 1;
  }
  return 0;
}

sub is_pid_alive {
  my ($pid) = @_;
  return 0 unless $pid && $pid =~ /^\d+$/;
  return kill 0, $pid ? 1 : 0;
}

sub get_generation_status {
  my $status = read_json_file(gen_status_file(), { state => 'idle', progress => 0 });
  my $pid = $status->{pid} || 0;
  # Fallback: read PID from file if missing
  if (!$pid) {
    if (open my $pfh, '<', gen_pid_file()) {
      my $p = <$pfh>; close $pfh;
      $p =~ s/\D+//g;
      if ($p) { $pid = 0 + $p; $status->{pid} = $pid; }
    }
  }

  # Read tail of log for progress parsing
  my $tail = '';
  my $logp = gen_log_file();
  if (open my $lfh, '<', $logp) {
    my $size = -s $lfh;
    my $max  = 40000; # ~40KB tail
    if ($size && $size > $max) { seek $lfh, $size - $max, 0; }
    local $/; $tail = <$lfh>; close $lfh;
  }

  # Sanitize and clamp tail for JSON safety
  my $tail_safe = $tail;
  if (defined $tail_safe) {
    $tail_safe =~ s/[^\x09\x0A\x0D\x20-\x7E]/?/g;
    $tail_safe = substr($tail_safe, -4000) if length($tail_safe) > 4000;
  } else {
    $tail_safe = '';
  }

  # Parse progress (matchups stage primary, heroes total if present)
  if ($tail) {
    my ($h_total,$m_done,$m_total,$cur_hero) = (0,0,0,'');
    if ($tail =~ /Loaded\s+(\d+)\s+heroes/i) { $h_total = 0 + $1; }
    if ($tail =~ /\[matchups\s+(\d+)\/(\d+)\]\s*(.+?)\s*$/m) {
      ($m_done,$m_total,$cur_hero) = ($1,$2,$3);
    }

    my $pct = $status->{progress} || 0;
    if ($m_total && $m_total > 0) {
      my $p2 = int(($m_done * 100) / $m_total);
      $pct = $p2 if $p2 > $pct;
      $status->{total}   = $m_total;
      $status->{current} = $cur_hero if $cur_hero;
      $status->{state}   = 'running';
    } elsif ($h_total && $h_total > 0) {
      $status->{total} = $h_total;
      $status->{state} = 'running' unless ($status->{state} && $status->{state} ne 'running');
    }
    $pct = 100 if $pct > 100;
    $status->{progress} = $pct;

    if ($tail =~ /Wrote\s+cs\.json/i) {
      $status->{progress} = 100;
      $status->{ok} = JSON::PP::true;
      $status->{state} = 'done';
      $status->{finished_at} = time;
    }

    # Detect obvious failure patterns
    my $err = '';
    if ($tail =~ /Failed to fetch heroStats/i) { $err = 'Failed to fetch heroStats'; }
    elsif ($tail =~ /curl:\s*\(\d+\).*\b429\b/i) { $err = 'Rate limited (429)'; }
    elsif ($tail =~ /HTTP::Tiny fetch failed\s+429/i) { $err = 'Rate limited (429)'; }
    if ($err && (!$status->{ok} || ($status->{state}||'') ne 'done')) {
      $status->{state} = 'failed';
      $status->{error} = $err;
    }
  }

  # Persist refreshed status
  write_json_file(gen_status_file(), $status);
  return ($status, $tail_safe);
}

sub start_generation_async {
  my ($source) = @_;
  $source ||= 'opendota';

  my ($status) = get_generation_status();
  if (($status->{state}||'') eq 'running') {
    return (0, 'Generation already running');
  }

  # Reset status and log
  my $sfile = gen_status_file();
  my $lfile = gen_log_file();
  unlink $lfile; # fresh log
  my $st = {
    state => 'running',
    source => $source,
    progress => 0,
    current => '',
    total => 0,
    pid => 0,
    ok => JSON::PP::false,
    started_at => time,
  };
  write_json_file($sfile, $st);

  my $script = $source eq 'dotabuff' ? '1.pl' : 'opendota.pl';
  my $cmd_script = local_file($script);

  my $pid = fork();
  if (!defined $pid) {
    $st->{state} = 'failed';
    $st->{error} = 'Fork failed';
    write_json_file($sfile, $st);
    return (0, 'Fork failed');
  }
  if ($pid) {
    # Parent: record PID and return immediately
    $st->{pid} = $pid;
    write_json_file($sfile, $st);
    if (open my $pfh, '>', gen_pid_file()) { print $pfh $pid; close $pfh; }
    return (1, 'started');
  }

  # Child: detach and exec
  eval {
    require POSIX;
    POSIX::setsid();
  };
  $SIG{HUP} = 'IGNORE';

  chdir($BASE_DIR);
  open STDIN,  '<', '/dev/null';
  open STDOUT, '>>', $lfile;
  select STDOUT; $| = 1;
  open STDERR, '>>', $lfile;
  $| = 1;

  exec('/usr/bin/env', 'perl', $cmd_script);
  print STDERR "exec failed: $!\n";
  exit 1;
}

sub diagnostics_info {
  my %info;
  $info{base_dir} = $BASE_DIR;
  $info{cwd} = Cwd::getcwd();
  $info{perl_path} = $^X;
  $info{perl_version} = sprintf('v%vd', $^V);
  $info{cgi} = {
    server => $ENV{SERVER_SOFTWARE} || '',
    gateway => $ENV{GATEWAY_INTERFACE} || '',
  };
  my @files = (
    { name => '1.pl' },
    { name => 'opendota.pl' },
    { name => 'admin_api.pl' },
    { name => 'admin.html' },
    { name => 'settings.json' },
    { name => 'cs.json' },
  );
  for my $f (@files) {
    my $p = local_file($f->{name});
    $f->{path} = $p;
    $f->{exists} = -e $p ? JSON::PP::true : JSON::PP::false;
    $f->{executable} = -x $p ? JSON::PP::true : JSON::PP::false;
    $f->{writable} = -w $p ? JSON::PP::true : JSON::PP::false;
  }
  $info{files} = \@files;
  # directory writability test
  my $tmp = local_file('.write_test.tmp');
  my $can_write_dir = 0;
  eval {
    if (open my $fh, '>', $tmp) { print $fh "ok"; close $fh; unlink $tmp; $can_write_dir = 1; }
  };
  $info{can_write_directory} = $can_write_dir ? JSON::PP::true : JSON::PP::false;
  # env hints (do not expose secrets)
  $info{env} = {
    FLARESOLVERR_URL => $ENV{FLARESOLVERR_URL} ? JSON::PP::true : JSON::PP::false,
    SCRAPEDO_API_KEY => $ENV{SCRAPEDO_API_KEY} ? JSON::PP::true : JSON::PP::false,
  };
  return \%info;
}

sub send_test_email {
  my ($settings) = @_;
  my $to   = $settings->{email_to}   || '';
  my $from = $settings->{email_from} || '';
  return (0, 'Missing recipient email') unless $to;
  my $subject = 'DotaBuffCP test email';
  my $body = "This is a test email from DotaBuffCP admin panel.";
  my $sendmail = '/usr/sbin/sendmail';
  if (-x $sendmail) {
    my $pid = open(my $mail, '|-', "$sendmail -t");
    if ($pid) {
      print $mail "From: $from\n" if $from;
      print $mail "To: $to\n";
      print $mail "Subject: $subject\n\n$body\n";
      close $mail;
      return (1, 'sent');
    }
  }
  return (0, 'sendmail not available');
}

sub kill_pid {
  my ($pid) = @_;
  return 0 unless $pid && $pid =~ /^\d+$/;
  my $alive = kill 0, $pid;
  return 1 unless $alive;
  kill 'TERM', $pid;
  usleep(300_000);
  $alive = kill 0, $pid;
  if ($alive) { kill 'KILL', $pid; usleep(200_000); }
  return 1;
}

sub reset_generation {
  my $status = read_json_file(gen_status_file(), { state => 'idle', progress => 0 });
  my $pid = $status->{pid} || 0;
  if (!$pid) {
    if (open my $pfh, '<', gen_pid_file()) {
      my $p = <$pfh>; close $pfh;
      $p =~ s/\D+//g;
      $pid = 0 + $p if $p;
    }
  }
  kill_pid($pid) if $pid;
  unlink gen_pid_file();
  unlink gen_log_file();
  write_json_file(gen_status_file(), { state => 'idle', progress => 0, pid => 0, ok => JSON::PP::false });
  return 1;
}

sub main {
  my $q = CGI->new;
  my $raw = read_body();
  my $req = eval { decode_json($raw || '{}') };

  my $method = '';
  my $payload = {};
  if (!$@ && ref $req eq 'HASH') {
    $method  = $req->{method}  || '';
    $payload = $req->{payload} || {};
  }

  if (!$method) {
    my $m_param = $q->param('method') || $q->param('m') || $q->param('action') || '';
    my $p_param = $q->param('payload');
    my %pl_hash;
    if (defined $p_param && length $p_param) {
      my $pj = eval { decode_json($p_param) };
      if (!$@ && ref $pj eq 'HASH') { %pl_hash = %{$pj}; }
    } else {
      for my $name ($q->param) {
        next if $name eq 'method' || $name eq 'm' || $name eq 'action' || $name eq 'payload';
        my $val = scalar $q->param($name);
        $pl_hash{$name} = $val;
      }
    }
    $method  = $m_param if $m_param;
    $payload = \%pl_hash if scalar keys %pl_hash;
  }

  if (!$method && defined $ENV{PATH_INFO} && $ENV{PATH_INFO} =~ m{/([A-Za-z_][A-Za-z0-9_]*)$}) {
    $method = $1;
  }

  if ($method eq 'get_settings') {
    my $s = load_settings();
    respond({ ok => JSON::PP::true, settings => $s });

  } elsif ($method eq 'save_settings') {
    my $s = $payload->{settings} || {};
    my $ok = save_settings($s);
    respond({ ok => $ok ? JSON::PP::true : JSON::PP::false });

  } elsif ($method eq 'generate') {
    if ($payload->{async}) {
      my ($ok, $msg) = start_generation_async($payload->{source});
      respond({ ok => $ok ? JSON::PP::true : JSON::PP::false, message => $msg });
    } else {
      my ($ok, $out) = run_generate($payload->{source});
      respond({ ok => $ok ? JSON::PP::true : JSON::PP::false, output => $out });
    }

  } elsif ($method eq 'send_test_email') {
    my $s = $payload->{settings} || load_settings();
    my ($ok, $msg) = send_test_email($s);
    respond({ ok => $ok ? JSON::PP::true : JSON::PP::false, message => $msg });

  } elsif ($method eq 'diagnostics') {
    my $d = diagnostics_info();
    respond({ ok => JSON::PP::true, diagnostics => $d });

  } elsif ($method eq 'gen_status') {
    my ($st, $tail) = get_generation_status();
    respond({ ok => JSON::PP::true, status => $st, tail => $tail });
  } elsif ($method eq 'gen_reset') {
    my $ok = reset_generation();
    respond({ ok => $ok ? JSON::PP::true : JSON::PP::false });
  } elsif ($method eq 'watch_start') {
    my $pid = fork();
    if (!defined $pid) { respond({ ok => JSON::PP::false, error => 'Fork failed' }); }
    if ($pid) {
      if (open my $pfh, '>', watch_pid_file()) { print $pfh $pid; close $pfh; }
      respond({ ok => JSON::PP::true, pid => $pid });
    }
    # child
    eval { require POSIX; POSIX::setsid(); };
    $SIG{HUP} = 'IGNORE';
    chdir($BASE_DIR);
    open STDIN,  '<', '/dev/null';
    open STDOUT, '>>', watch_log_file(); select STDOUT; $|=1;
    open STDERR, '>>', watch_log_file(); $|=1;
    exec('/usr/bin/env', 'perl', local_file('watch_er.pl'));
    print STDERR "exec failed: $!\n"; exit 1;
  } elsif ($method eq 'watch_status') {
    my $st = read_json_file(watch_status_file(), { state => 'idle' });
    my $tail = '';
    if (open my $lfh, '<', watch_log_file()) { my $size = -s $lfh; my $max=20000; if ($size && $size > $max) { seek $lfh, $size - $max, 0; } local $/; $tail = <$lfh>; close $lfh; }
    respond({ ok => JSON::PP::true, status => $st, tail => $tail });
  } elsif ($method eq 'watch_reset') {
    my $pid = 0; if (open my $pfh, '<', watch_pid_file()) { my $p = <$pfh>; close $pfh; $p =~ s/\D+//g; $pid = 0 + ($p||0); }
    kill_pid($pid) if $pid;
    unlink watch_pid_file(); unlink watch_log_file();
    write_json_file(watch_status_file(), { state => 'idle' });
    respond({ ok => JSON::PP::true });
  } else {
    respond({ ok => JSON::PP::false, error => 'Unknown method' });
  }
}

main();