#! /usr/bin/sed -nf

1 { s/.*/1/ ; x ; d }

N ; N ; N ; N

/[XT]\{4\}/                       b X
/[XT]....[XT]....[XT]....[XT]/    b X
/[XT].....[XT].....[XT].....[XT]/ b X
/...[XT]...[XT]...[XT]...[XT].../ b X

/[OT]\{4\}/                       b O
/[OT]....[OT]....[OT]....[OT]/    b O
/[OT].....[OT].....[OT].....[OT]/ b O
/...[OT]...[OT]...[OT]...[OT].../ b O

/\./ b I

s/.*/Draw/

:R
G
s/\(.*\)\n\(.*\)/Case #\2: \1/
p

# gotta love doing that with sed :-/
x
s/$/-/
:P
s/9-/-0/
s/8-/9/
s/7-/8/
s/6-/7/
s/5-/6/
s/4-/5/
s/3-/4/
s/2-/3/
s/1-/2/
s/0-/1/
s/^-/1/
/-/ b P
x
d

:X
s/.*/X won/
b R

:O
s/.*/O won/
b R

:I
s/.*/Game has not completed/
b R
