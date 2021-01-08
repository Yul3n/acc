acc: acc.c test.ac
	cc acc.c -o acc
	./acc | jq -M
test: acc test.ac
	./acc
