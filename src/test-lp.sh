#! /bin/sh

for i in test/*graph; do
  printf "%20s " ${i#test/}
  ./scs-lp -s -e < $i
done
