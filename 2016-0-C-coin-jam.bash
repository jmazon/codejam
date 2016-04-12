read
echo 'Case #1:'
read N J

bc <<< "obase = 2; for (n = 2^($N-1)+1; n < 2^$N; n += 2) n " |
sed 's/\(.*\)/ibase=2;\1;ibase=11;\1;ibase=11;\1;ibase=11;\1;ibase=11;\1;ibase=11;\1;ibase=11;\1;ibase=11;\1;ibase=11;\1;/' |
bc |
xargs -L9 factor |
perl -nE 'BEGIN { $r = qr/(\d+): (\d+) .*\n/a; $R = $r x 9 } $p .= $_; if ($. % 9 == 0) { $p =~ /$R/o and say "$17 $2 $4 $6 $8 $10 $12 $14 $16 $18"; undef $p}' |
head -n $J

# small input: 1.154s
# large input: 1m25.836s
