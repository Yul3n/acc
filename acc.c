#include <stdlib.h>
#include <stdio.h>

typedef struct {
	enum {
		T_ADD = 1, T_SUB, T_MUL, T_DIV, T_INT, T_EOF
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
	['+'] = T_ADD,
	['-'] = T_SUB,
	['*'] = T_MUL,
	['/'] = T_DIV,
};

static const int token_op[T_INT] = {
	[T_ADD] = OP_ADD,
	[T_SUB] = OP_SUB,
	[T_MUL] = OP_MUL,
	[T_DIV] = OP_DIV
};

FILE *in;
int linum = 1;
int unused_char;

static void
putback_char(int c)
{
	unused_char = c;
}

static int
is_digit(int c)
{
	return c >= 48 && c <= 57;
}

static int
next_char(void)
{
	if (unused_char) {
		char c = unused_char;
		unused_char = 0;
		return c;
	}
	return fgetc(in);
}

static void
skip_space(void)
{
	int c;

	while ((c = next_char()) == ' ');
	putback_char(c);
}

static Token
mktoken(int type, int int_val)
{
	Token t;

	t = (Token) {
		.int_val = int_val,
		.type    = type
	};
	return t;
}

static Token
mktint(int int_val)
{
	return mktoken(T_INT, int_val);
}

static Token
lex(void)
{
	char c;

	skip_space();
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

Token unused_token;

static Token
next_token(void)
{
	return lex();
}

static Expr *
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

static Expr *
mkeint(int int_val)
{
	return mkexpr(INT_LIT, NULL, NULL, int_val);
}

static Expr *
mkebin(int op, Expr *left, Expr *right)
{
	return mkexpr(op, right, left, 0);
}

static Expr *
prim_expr(void)
{
	Token t;

	t = next_token();
	switch (t.type) {
	case T_INT: return mkeint(t.int_val);
	default:
		fprintf(stderr, "Unexpected token on line %d.\n", linum);
		exit(1);
	}
}

static Expr *
binexpr(void)
{
	Expr *e, *left, *right;
	Token t;

	left = prim_expr();
	t = next_token();
	if (t.type == T_EOF)
		return left;
	right = binexpr();
	return mkebin(t.type, left, right);
}

int
main()
{
	unused_token = mktoken(T_EOF, 0);
}
