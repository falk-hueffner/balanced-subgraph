#! /bin/sh

for i in test/*graph; do
  printf "%20s " ${i#test/}
  ./ulp -s < $i
done
