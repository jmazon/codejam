#! /usr/bin/env perl

use 5.024;
use warnings;

my $t = 100; say $t;

while ($t--) {
  my $m = 100; say $m;
  my (@ri1,@ri2,@m);
  for (1 .. $m) {
    my ($a,$b) = (1 + int(($m - 1) * rand()));
    do { $b = 2 + int(($m-1) * rand()) } until $b != $a;
    ($a,$b) = ($b,$a) if $b < $a;
    say "$a $b";
    push @m, int(1_000_000_001 * rand());
  }
  say "@m";
}
