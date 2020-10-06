#! /usr/bin/env perl
use 5.024;
use warnings;

say 4096;
for my $t (0..4095) {
  say '3 4';
  say map $t & $_ ? 'W' : 'B', 1,2,4,8;
  say map $t & $_ ? 'W' : 'B', 16,32,64,128;
  say map $t & $_ ? 'W' : 'B', 256,512,1024,2048;
}
