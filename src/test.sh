#! /usr/bin/env bash

ulimit -t 60

error=0

cat test/tests | \
while read g k; do
  printf "%s\t" $g
  kk=$(./scs -s < test/$g.graph | tee /dev/stderr | awk '{print $3}')
  if [ $k -ne $kk ]; then
    printf "FAILED %s: correct %d result %d\n" $g $k $kk
    error=1
  fi
done

exit $error
