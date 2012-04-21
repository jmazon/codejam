use 5.010;
use List::Util qw(reduce);

my $t = 100;
say $t;
while ($t--) {
    my $n = 2 + int rand 999;
    say $n;
    @_ = map 1 + int rand 999999, 1..$n;
    $_ = reduce { $a ^ $b } @_;
    if (rand 3 > 1) { $_[0] ^= $_ }
    say "@_";
}
