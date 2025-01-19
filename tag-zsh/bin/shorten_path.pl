#!/usr/bin/perl

# Compress path, I use this in bash shell prompts, xterm titles,
# screen, and now tmux+zsh.

use warnings;
use strict;

my $max_len = $ARGV[0] || 20;
my $line = <STDIN>;
$line =~ s!$ENV{HOME}!~!;

while (length $line > $max_len) {
    if ($line =~ m!/[^/]{2,}/!) {
        $line =~ s!/([^/])[^/]+!/$1!;
    } else {
        my ($dir) = $line =~ m!/([^/]+)$!;
        my $dir_len = length $dir;
        my $len = $dir_len > $max_len ? $dir_len : $max_len;
        $line =~ s/^(.).+(.{$len,$len})$/$1..$2/;
        last;
    }
}

print "$line\n";
