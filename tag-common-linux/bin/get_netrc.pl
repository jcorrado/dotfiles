#!/usr/bin/perl

# Parse .netrc and return password for first mataching machine and
# login.

use warnings;
use strict;

use Getopt::Std;

our ($opt_m, $opt_l);
getopts('m:l:');

my $netrc = "$ENV{HOME}/.netrc";

open(my $fh, '<', "$netrc")
    || die "could not open $netrc for read: $!\n";

while (<$fh>) {
    m{ ^machine  \s+ $opt_m   \s+
        login    \s+ $opt_l   \s+
        password \s+ ([^\s]+) \s*$ }x;
    if (defined $1) {
	print $1;
	exit 0;
    }
}

close $fh;

exit 0;
