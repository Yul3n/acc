acc: acc.c test.ac
	cc -std=c99 acc.c -o acc
	./acc | jq -M
test: acc test.ac
	./acc
