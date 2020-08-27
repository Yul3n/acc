#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>

#define SYMSIZE 1024

typedef struct {
	enum {
		T_ADD = 1, T_SUB, T_MUL, T_DIV, T_INT_LIT, T_SEMI, T_PRINT,
		T_EQU, T_INT, T_IDE, T_EOF
	} type;
	int int_val;
	char *ide;
} Token;

typedef struct expr {
	enum {
		OP_ADD, OP_SUB, OP_MUL, OP_DIV, INT_LIT, VAR
	} op;
	union {
		struct {
			struct expr *left;
			struct expr *right;
		};
		int int_val;
		char *var;
	};
} Expr;

typedef struct {
	char *name;
} Symbol;

static Symbol symbol_table[SYMSIZE];
static int sym_num = 0;

static void cprintint(int);
static int compile_expr(Expr *);

static const int punct[128] = {
	['+'] = T_ADD,
	['-'] = T_SUB,
	['*'] = T_MUL,
	['/'] = T_DIV,
	[';'] = T_SEMI,
	['='] = T_EQU
};

static const int token_op[T_INT_LIT] = {
	[T_ADD] = OP_ADD,
	[T_SUB] = OP_SUB,
	[T_MUL] = OP_MUL,
	[T_DIV] = OP_DIV
};

static const int prec_op[T_EOF + 1] = {
	[T_ADD] = 10,
	[T_SUB] = 10,
	[T_MUL] = 20,
	[T_DIV] = 20,
	[T_INT_LIT] = 0,
	[T_EOF] = 0
};

FILE *in;
int linum = 1;
int unused_char = 0;

static void
putback_char(int c)
{
	unused_char = c;
}

static int
next_char(void)
{
	if (unused_char) {
		int c = unused_char;
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
mktoken(int type, int int_val, char *ide)
{
	Token t;

	t = (Token) {
		.int_val = int_val,
		.type    = type,
		.ide     = ide
	};
	return t;
}

static Token
mktint(int int_val)
{
	return mktoken(T_INT_LIT, int_val, NULL);
}

static int
isident(int c)
{
	return isdigit(c) || isalpha(c) || c == '_';
}

static char *
lex_ident(void)
{
	int size, i, c;
	char *s, *p;

	i    = 0;
	size = 256;
	p    = (s = malloc(size)) - 1;
	if (!p) {
		fprintf(stderr, "Unable to allocate memory for the lexer.\n");
		exit(1);
	}
	while (isident(c = next_char())) {
		/* Allocate more memory for the string if we've reachec its limit. */
		if (++i >= size) {
			size += 256;
			s     = realloc(s, size);
			p     = s + i - 2;
		}
		*(++p) = c;
	}
	/* NUL-terminate the string. */
	s[i] = 0;
	putback_char(c);
	return s;

}

static int
keyword(char *s)
{
	/* Switch on the first later to avoid wasting a lot of time on unuseful
	 * comparaisons. */
	switch (*s) {
	case 'p':
		if (!strcmp(s, "print")) return T_PRINT;
		break;
	case 'i':
		if (!strcmp(s, "int")) return T_INT;
	}
	return 0;
}

static Token
lex(void)
{
	int c;

	skip_space();
	c = next_char();
	if (c == EOF) return mktoken(T_EOF, 0, NULL);
	if (isalpha(c) || c == '_') {
		putback_char(c);
		char *s = lex_ident();
		int ttype = keyword(s);
		if (ttype) return mktoken(ttype, 0, NULL);
		return mktoken(T_IDE, 0, s);
	}
	if (isdigit(c)) {
		int n;
		n = c - '0';
		while (isdigit(c = next_char())) {
			n *= 10;
			n += c - '0';
		}
		putback_char(c);
		return mktint(n);
	} if(punct[c]) return mktoken(punct[c], 0, NULL);
	if (c == '\n') {
		++linum;
		return lex();
	}
	fprintf(stderr, "Unexpected character: '%c'\n", (char)c);
	exit(1);
}

Token unused_token;
Token current_token;

static void
putback_token(Token t)
{
	unused_token = t;
}

static Token
next_token(void)
{
	Token t;

	if (unused_token.type == T_EOF)
		t = lex();
	else {
		t = unused_token;
		unused_token = mktoken(T_EOF, 0, NULL);
	}
	return current_token = t;
}

/****************************************************************************/
/*                               SYMBOL TABLE                               */
/****************************************************************************/

/****************************************************************************/
/*                                  PARSER                                  */
/****************************************************************************/

static void
assert(int ttype, char *name)
{
	if (current_token.type != ttype) {
		fprintf(stderr, "Expected: %s.\n", name);
		exit(1);
	}
	next_token();
}

static Expr *
mkexpr(unsigned int op, Expr *left, Expr *right, int int_val)
{
	Expr *e;

	e = (Expr *)malloc(sizeof(Expr));
	if (!e) {
		fprintf(stderr, "Unable to alloc memory in mkexpr().\n");
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

	switch (current_token.type) {
	case T_INT_LIT: {
		int n = current_token.int_val;
		next_token();
		return mkeint(n);
	}
	default:
		fprintf(stderr, "Unexpected token on line %d.\n", linum);
		exit(1);
	}
}

int
op_precedence(int op)
{
	int op_p;

	op_p = prec_op[op];
	if (op_p != 0) return op_p;
	fprintf(stderr, "Syntax error on line %d, unxepected token.\n", linum);
	exit(1);
}

static Expr *
binexpr(int prec)
{
	Expr *e, *left, *right;
	int ttype;

	left = prim_expr();
	ttype = current_token.type;
	if (current_token.type == T_SEMI)
		return left;

	while (op_precedence(ttype) > prec) {
		next_token();
		right = binexpr(prec_op[ttype]);
		left = mkebin(token_op[ttype], left, right);
		if ((ttype = current_token.type) == T_SEMI)
			return left;
	}
	return left;
}

void
print_stmt(void)
{
	Expr *e;

	assert(T_PRINT, "print");
	e = binexpr(0);
	assert(T_SEMI, ";");
	cprintint(compile_expr(e));
}

void
var_decl_stmt(void)
{

}

void
statement(void)
{

	switch (current_token.type) {
	case T_PRINT:
		print_stmt();
		break;
	default:
		fprintf(stderr, "Unexpected token.");
		exit(1);
	}
}

/****************************************************************************/
/*                              CODE GENERATION                             */
/****************************************************************************/

static char *reglist[4] = { "%r8", "%r9", "%r10", "%r11" };
static int freeregs[4]  = { 0, 0, 0, 0 };
FILE *out;

static int
alloc_reg()
{
	for (int i = 0; i < 4; ++i)
		if (!freeregs[i]) {
			freeregs[i] = 1;
			return i;
		}
	fprintf(stderr, "Unable to allocate a register");
	exit(1);
}

static void
free_reg(int reg)
{
	freeregs[reg] = 0;
}

static int
cadd(int l, int r)
{
	fprintf(out, "addq %s, %s\n", reglist[r], reglist[l]);
	free_reg(r);
	return l;
}
static int
csub(int l, int r)
{
	fprintf(out, "subq %s, %s\n", reglist[r], reglist[l]);
	free_reg(r);
	return l;
}

static int
cmul(int l, int r)
{
	fprintf(out, "imulq %s, %s\n", reglist[r], reglist[l]);
	free_reg(r);
	return l;
}

static int
cdiv(int l, int r)
{
	fprintf(out,
	        "movq %s, %%rax\n"
	        "cqo\n"
	        "idivq %s\n"
	        "movq %%rax, %s\n", reglist[l], reglist[r], reglist[l]);
	return l;
}

static int
cload(int n)
{
	int reg;

	reg = alloc_reg();
	fprintf(out, "movq $%d, %s\n", n, reglist[reg]);
	return reg;
}

static void
cprintint(int reg)
{
	fprintf(out,
	        "movq %s, %%rdi\n"
	        "call printint\n", reglist[reg]);
}

static int
compile_expr(Expr *e)
{
	int lreg, rreg;

	if (e->right) rreg = compile_expr(e->right);
	if (e->left) lreg = compile_expr(e->left);

	switch (e->op) {
	case OP_ADD: return cadd(lreg, rreg);
	case OP_SUB: return csub(lreg, rreg);
	case OP_MUL: return cmul(lreg, rreg);
	case OP_DIV: return cdiv(lreg, rreg);
	case INT_LIT: return cload(e->int_val);
	}
}

static void
cprolog()
{
	fputs(".text\n"
	      ".LC0:\n"
	      ".string\t\"%d\\n\"\n"
	      "printint:\n"
	      "pushq %rbp\n"
	      "movq %rsp, %rbp\n"
	      "subq $16, %rsp\n"
	      "movl %edi, -4(%rbp)\n"
	      "movl -4(%rbp), %eax\n"
	      "movl %eax, %esi\n"
	      "leaq .LC0(%rip), %rdi\n"
	      "movl $0, %eax\n"
	      "call printf@PLT\n"
	      "nop\n"
	      "leave\n"
	      "ret\n"
	      ".globl main\n"
	      ".type main, @function\n"
	      "main:\n"
	      "pushq %rbp\n"
	      "movq %rsp, %rbp\n",
	      out);
}

static void
cepilog(void)
{
	fprintf(out,
	        "movq $0, %%rdi\n"
	        "movq $60, %%rax\n"
	        "syscall\n");
}

int
main(int argc, char *argv[])
{
	Expr *e;

	unused_token = mktoken(T_EOF, 0, NULL);
	in  = fopen(argv[1], "r");
	out = fopen("out.s", "w");
	/* Get the first token */
	next_token();
	cprolog();
	while (current_token.type != T_EOF) {
		statement();
	}
	cepilog();
	fclose(in);
	fclose(out);
}
