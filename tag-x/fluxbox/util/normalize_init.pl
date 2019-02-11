#!/usr/bin/perl

# My Fluxbox 1.3.5 doesn't have a stable `init' file structure, so
# trackig in version control is challenging.  This little utility
# rewrites the init file with a fixed output format.

use warnings;
use strict;

open my $fh, "< init"
    or die "couldn't open init for read: $!\n";
my $init;
{
    local $/;
    undef $/;
    $init = <$fh>;
}
close $fh;

undef $fh;
open $fh, "> init"
    or die "couldn't open init for write: $!\n";

foreach my $line (sort split /\n/, $init) {
    my ($k, $v) = $line =~ /^([.\w]+:)\s*(.*)$/;
    print $fh "$k $v\n";
}
close $fh;
