#! /bin/sed -f

/[^ES]/ d

# This handles I/O
x; s/^$/:000/; s/$/%/
:incr;   s/0%/1/; s/1%/2/; s/2%/3/; s/3%/4/
s/4%/5/; s/5%/6/; s/6%/7/; s/7%/8/; s/8%/9/
t carry; :carry; s/9%/%0/; t incr; x
G; s/\(.*\)\n:0*\(.*\)/Case #\2: \1/

# This actually solves the problem
y/ES/SE/
