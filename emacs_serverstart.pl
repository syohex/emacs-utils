#!/usr/bin/env perl
use strict;
use warnings;

use Fcntl qw(:flock);
use File::Which qw(which);

unless (which("wmctrl")) {
    exit 1;
}

my ($emacs_pid, $has_window) = @ARGV[0,1];
unless ($emacs_pid && $has_window) {
    die "Usage $0 emacs_pid [x or nox]\n"
}

my $server_wid_file = $ENV{HOME} . '/.emacs.d/server_wid';
if ($has_window eq 'x') {
    server_is_x($emacs_pid);
} else {
    server_is_nox();
}

sub server_is_x {
    my $pid = shift;
    my $emacs_window_id;

    open my $fh, "-|", qw/wmctrl -p -l/ or die "Can't open wmctrl process\n";
    while (my $line = <$fh>) {
        chomp $line;
        my ($wid, $pid) = (split ' ', $line)[0,2];

        if ($emacs_pid eq $pid) {
            $emacs_window_id = $wid;
            last;
        }
    }
    close $fh;

    open my $wfh, ">", $server_wid_file or die "Can't open write file\n";
    flock $wfh, LOCK_EX;
    print $wfh $emacs_window_id;
    close $wfh;
}

sub server_is_nox {
    my $server_window_name = 'emacsserver_run';

    unlink $server_wid_file if -e $server_wid_file;

    open my $fh, "-|", qw/wmctrl -l/ or die "Can't open wmctrl process\n";
    while (my $line = <$fh>) {
        chomp $line;
        my $window_title = (split ' ', $line)[3];

        if ($window_title eq $server_window_name) {
            # rename old server run terminal
            my @cmd = (qw/wmctrl -F -r/, $server_window_name, qw/-T Terminal/);
            system();
            last;
        }
    }
    close $fh;

    # change current terminal window
    my @cmd = (qw/wmctrl -r :ACTIVE: -T/, $server_window_name);
    system(@cmd);
}
