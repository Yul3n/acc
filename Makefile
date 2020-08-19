acc: acc.c
	cc acc.c -o acc
test: acc
	./acc test1.ac
	cc out.s
	./a.out || echo "\$\?"
