acc: acc.c
	cc acc.c -o acc
test: acc test1.ac
	./acc test1.ac
	cc out.s
	./a.out 
