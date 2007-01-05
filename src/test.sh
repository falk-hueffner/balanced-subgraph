#! /bin/sh

ulimit -t 60

for i in test/*graph; do
  printf "%20s " ${i#test/}
  ./ulp -s < $i || echo
done
