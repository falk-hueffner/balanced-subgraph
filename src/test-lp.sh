#! /bin/sh

for i in test/*graph; do
  printf "%20s " ${i#test/}
  ./bsg-lp -s -e < $i
done
