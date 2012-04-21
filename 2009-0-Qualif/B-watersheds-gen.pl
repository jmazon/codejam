#! /usr/bin/env perl

use 5.010;
use strict;
use warnings;

say 100;
for (1..100) {
  say "100 100";
  for (1..100) {
    say join ' ', map int rand 10000, 1..100;
  }
}
