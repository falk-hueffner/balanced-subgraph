#! /bin/zsh

for n in {2..100}; do
  for m in {1..$((n * (n-1) / 2))}; do
    for k in {0..$m}; do
      printf "n = %d m = %d k = %d\n" n m k
      for i in {1..100}; do
        ./gen-ulp $n $m $k > test.graph
	k1=$(./ulp -s < test.graph | awk '{print $3}')
	k2=$(./ulp-lp -e -s < test.graph | awk '{print $3}')
	[ x"$k1" != x"$k2" ] && exit
      done
    done
 done
done
