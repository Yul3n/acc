<$PLAN9/src/mkhdr

CC=cc
CFLAGS=-c
AR=ar
LD=cc

TARG=acc
OFILES=acc.$O

<$PLAN9/src/mkone

test: acc
	./o.acc test1.ac | jq -M
