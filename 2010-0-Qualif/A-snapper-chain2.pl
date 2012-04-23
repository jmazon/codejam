#! /usr/bin/env perl

use 5.10.0;
use strict;
use warnings;

for my $t (1 .. <>) {
  my ($N, $K) = split /\s+/, <>;
  $N = (1<<$N) - 1;
  say "Case #$t: ", ($N == ($N & $K)) ? 'ON' : 'OFF';
}
