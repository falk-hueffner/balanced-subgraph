#! /bin/zsh

for n in {6..100}; do
  for m in {$((n/2))..$((n * (n-1) / 2))}; do
    for k in {0..$m}; do
      printf "n = %d m = %d k = %d\n" n m k
      for i in {1..10}; do
        ./gen-bsg $n $m $k > test.graph
	k1=$(./bsg -s < test.graph | awk '{print $3}')
	k2=$(./bsg-lp -e -s < test.graph | awk '{print $3}')
	[ x"$k1" != x"$k2" ] && exit
      done
    done
 done
done
