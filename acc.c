#include <stdlib.h>
#include <stdio.h>

typedef struct {
	enum {
		T_ADD = 1, T_SUB, T_MUL, T_DIV, T_INT
	} type;
	int int_val;
} Token;

typedef struct expr {
	enum {
		OP_ADD, OP_SUB, OP_MUL, OP_DIV, INT_LIT
	} op;
	struct expr *left;
	struct expr *right;
	int int_val;
} Expr;

static const int punct[128] = {
	['*'] = T_MUL,
	['+'] = T_ADD,
	['-'] = T_SUB,
	['/'] = T_DIV,
};

FILE *in;
int linum;
int unused_char;

void
putback_char(int c)
{
	unused_char = c;
}

int
is_digit(int c)
{
	return c >= 48 && c <= 57;
}

int
next_char()
{
	if (unused_char) {
		char c = unused_char;
		unused_char = 0;
		return c;
	}
	return fgetc(in);
}

Token
mktoken(int type, int int_val)
{
	Token t;

	t = (Token) {
		.int_val = int_val,
		.type    = type
	};
	return t;
}

Token
mktint(int int_val)
{
	return mktoken(T_INT, int_val);
}

Token
lex()
{
	char c;

	c = next_char();
	if (is_digit(c)) {
		int n;
		n = c - '0';
		while (is_digit(c = next_char())) {
			n *= 10;
			n += c - '0';
		}
		putback_char(c);
		return mktint(n);
	} else if(punct[c]) return mktoken(punct[c], 0);
	fprintf(stderr, "Unexpected character: '%c'\n", (char)c);
	exit(1);
}

Expr *
mkexpr(unsigned int op, Expr *left, Expr *right, int int_val)
{
	Expr *e;

	e = (Expr *)malloc(sizeof(Expr));
	if (!e) {
		fprintf(stderr, "Unable to alloc memory in mkexpr()");
		exit(1);
	}
	e->op      = op;
	e->left    = left;
	e->right   = right;
	e->int_val = int_val;
	return e;
}

Expr *
mkeint(int int_val)
{
	return mkexpr(INT_LIT, NULL, NULL, int_val);
}

int
main()
{
}
