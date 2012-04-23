#! perl -pal

next if $. == 1;
$_ .= '0' x 40 for @F[1..$#F];
$_ = join ' ', @F;
