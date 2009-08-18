#! /bin/sh

if ~/ulp/src/bsg -s -d < $1
then false
else true
fi


#r1=$(~/ulp/src/bsg.ok -s < $1 | awk '{print $3}')
#r2=$(~/ulp/src/bsg    -s < $1 | awk '{print $3}')
#if [ "x$r1" = "x$r2" ]
#then false
#else true
#fi

#(~/ulp/src/bsg -s -d 2>&1 | grep -q Not_sign_consistent < $1) && exit 1
#exit 0
