#! /bin/sh

ulimit -t 10

for i in test/*graph; do
  printf "%20s " ${i#test/}
  ./ulp -s < $i || echo
done
