#! /usr/bin/env perl

use 5.10.0;
use strict;
use warnings;
use List::MoreUtils 'all';

for my $t (1 .. <>) {
  my ($N, $K) = split /\s+/, <>;

  my @s; $#s = $N-1;
  while ($K--) {
    my $p = 1;
    for (@s) {
      $p = $_;
      $_ = !$_;
      last unless $p;
    }
  }
  say "Case #$t: ", (all {$_} @s) ? 'ON' : 'OFF';
}
