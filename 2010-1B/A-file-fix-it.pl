#! /usr/bin/env perl

use 5.010;
use strict;
use warnings;

use File::Spec;

sub line { my $_ = <>; chomp; @_ = File::Spec->splitdir($_); shift; @_ }
sub mkdirs {
  my $fs = shift;
  my $first = shift // return 0;
  if (exists $fs->{$first}) { mkdirs($fs->{$first}, @_) }
  else { $fs->{$first} = {}; return 1 + mkdirs($fs->{$first}, @_) }
}

for my $t (1..<>) {
  my ($n,$m) = split /\s+/, <>;
  my %fs;
  mkdirs \%fs, line for 1..$n;
  my $c = 0;
  $c += mkdirs \%fs, line for 1..$m;
  say "Case #$t: $c";
}
