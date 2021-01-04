acc: acc.c test.ac
	cc acc.c -o acc
	./acc
test: acc test.ac
	./acc
