use 5.010;
use strict;
use warnings;

my $t = <>;
for my $i (1..$t) {
    <>;
    @_ = split /\s+/, <>;
    my $c = 0;
    $_[$_] != $_+1 and $c++ for 0..$#_;
    say "Case #$i: $c.000000";
}
