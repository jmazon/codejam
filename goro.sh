#! /bin/bash

read t
for ((i=1;i<=t;i++))
do
  read n
  read ns
  c=0
  for ((ei=1;ei<=n;ei++))
  do
    e=${ns%% *}
    ns=${ns#* }
    ((e==ei)) || ((c++))
  done
  echo "Case #$i: $c.000000"
done
