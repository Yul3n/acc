#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>

#define SYMSIZE 1024

typedef struct {
	enum {
		T_ADD = 1, T_SUB, T_MUL, T_DIV, T_NEQU, T_GT, T_GE, T_LT, T_LE,
		T_NOT, T_DEQU, T_INT_LIT, T_EQU, T_SEMI, T_PRINT, T_INT, T_IF,
		T_ELSE, T_IDE, T_LBRACK, T_RBRACK, T_LPAR, T_RPAR, T_EOF
	} type;
	int int_val;
	char *ide;
} Token;

typedef struct expr {
	enum {
		OP_ADD = 1, OP_SUB, OP_MUL, OP_DIV, OP_NEQU, OP_GT, OP_GE, OP_LT,
		OP_LE, OP_NOT, OP_EQU, ASSIGN, PRINT, INT_LIT, VAR, IF, DEXPR
	} op;
	struct expr *left;
	struct expr *condition;
	struct expr *right;
	int int_val;
	char *var;
} Expr;

typedef struct {
	char *name;
} Symbol;

static Symbol symbol_table[SYMSIZE];
static int sym_num = 0;

static void cprintint(int);
static void cglobsym(char *);
static int  compile_expr(Expr *);
static void cstoresym(int, char *);
static Expr *if_stmt(void);

static const int punct[128] = {
	['+'] = T_ADD,
	['-'] = T_SUB,
	['*'] = T_MUL,
	['/'] = T_DIV,
	[';'] = T_SEMI,
	['='] = T_EQU,
	['<'] = T_LT,
	['>'] = T_GT,
	['!'] = T_NOT,
	['{'] = T_LBRACK,
	['}'] = T_RBRACK,
	['('] = T_LPAR,
	[')'] = T_RPAR
};

static const int dpunct[T_EOF + 1][128] = {
	[T_EQU] = {
		['='] = T_DEQU
	},
	[T_GT] = {
		['='] = T_GE
	},
	[T_LT] = {
		['='] = T_LE
	}
};

static const int prec_op[T_EOF + 1] = {
	[T_GT]   = 10,
	[T_LT]   = 10,
	[T_GE]   = 10,
	[T_LE]   = 10,
	[T_ADD]  = 11,
	[T_SUB]  = 11,
	[T_MUL]  = 12,
	[T_DIV]  = 12,
	[T_EOF]  = 0,
	[T_DEQU] = 9,
	[T_INT_LIT] = 0
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
	case 'e':
		if (!strcmp(s, "else")) return T_ELSE;
		break;
	case 'p':
		if (!strcmp(s, "print")) return T_PRINT;
		break;
	case 'i':
		if (!strcmp(s, "int")) return T_INT;
		if (!strcmp(s, "if")) return T_IF;
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
	} if (punct[c]) {
		int t = punct[c];
		if (dpunct[t][c = next_char()])
			t = dpunct[t][c];
		else
			putback_char(c);
		return mktoken(t, 0, NULL);
	}
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

static int
find_symbol(char *s)
{
	for (int i = 0; i < sym_num; ++i)
		if (!strcmp(s, symbol_table[i].name)) return i;
	return -1;
}

static int
new_symbol(void)
{
	if (sym_num + 1 >= SYMSIZE) {
		fprintf(stderr, "Symbol table out of space.\n");
		exit(1);
	}
	return sym_num++;
}

static int
add_symbol(char *s)
{
	int n;

	n = new_symbol();
	symbol_table[n].name = s;
	return n;
}

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
mkexpr(unsigned int op, Expr *left, Expr *right, int int_val, char *var)
{
	Expr *e;

	e = (Expr *)malloc(sizeof(Expr));
	if (!e) {
		fprintf(stderr, "Unable to alloc memory in mkexpr().\n");
		exit(1);
	}
	e->op      = op;
	e->var     = var;
	e->left    = left;
	e->right   = right;
	e->int_val = int_val;
	return e;
}

static Expr *
mkeint(int int_val)
{
	return mkexpr(INT_LIT, NULL, NULL, int_val, NULL);
}

static Expr *
mkebin(int op, Expr *left, Expr *right)
{
	return mkexpr(op, left, right, 0, NULL);
}

static Expr *
mkeun(int op, Expr *e)
{
	return mkexpr(op, e, NULL, 0, NULL);
}

static Expr *
mkevar(char *var)
{
	return mkexpr(VAR, NULL, NULL, 0, var);
}

static Expr *
mkeif(Expr *condition, Expr *ife, Expr *elsee)
{
	Expr *e;
	e = mkebin(IF, ife, elsee);
	e->condition = condition;
	return e;
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
	case T_IDE: {
		char *s = current_token.ide;
		if (find_symbol(s) == -1) {
			fprintf(stderr, "Unknown variable on line %d.\n", linum);
			exit(1);
		}
		next_token();
		return mkevar(s);
	}
	default:
		fprintf(stderr, "Unexpected token on line %d.\n", linum);
		exit(1);
	}
}

static int
op_precedence(int op)
{
	int op_p;

	op_p = prec_op[op];
	if (op_p != 0) return op_p;
	fprintf(stderr, "Syntax error on line %d, unxepected token.\n", linum);
	exit(1);
}

static int
token_op(int op)
{
	if (op && op < T_INT_LIT) return op;
	fprintf(stderr, "Syntax error.\n");
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
		left = mkebin(token_op(ttype), left, right);
		if ((ttype = current_token.type) == T_SEMI || ttype == T_RPAR)
			return left;
	}
	return left;
}

static Expr *
print_stmt(void)
{
	Expr *e;

	assert(T_PRINT, "print");
	e = binexpr(0);
	assert(T_SEMI, ";");
	return mkeun(PRINT, e);
}

static void
var_decl_stmt(void)
{
	char *var;

	assert(T_INT, "int");
	var = current_token.ide;
	assert(T_IDE, "identifier");
	add_symbol(var);
	cglobsym(var);
	assert(T_SEMI, ";");
}

static Expr *
var_assign_stmt(void)
{
	char *var;
	Expr *right, *left;

	var = current_token.ide;
	assert(T_IDE, "identifier");
	if (find_symbol(var) == -1) {
		fprintf(stderr, "Unknown variable on line %d.\n", linum);
		exit(1);
	}
	left = mkevar(var);
	assert(T_EQU, "=");
	right = binexpr(0);
	assert(T_SEMI, ";");
	return mkebin(ASSIGN, left, right);
}

static Expr *
blk_statements(void)
{
	Expr *left, *e;

	e = left = NULL;
	assert(T_LBRACK, "{");
	for (;;) {
		switch (current_token.type) {
		case T_PRINT:
			e = print_stmt();
			break;
		case T_INT:
			var_decl_stmt();
			e = NULL;
			break;
		case T_IDE:
			e = var_assign_stmt();
			break;
		case T_IF:
			e = if_stmt();
			break;
		case T_RBRACK:
			assert(T_RBRACK, "}");
			return left;
		default:
			fprintf(stderr, "Unexpected token.\n");
			exit(1);
		}
		if (e) {
			if (left)
				left = mkebin(DEXPR, left, e);
			else
				left = e;
		}
	}
}

static Expr *
if_stmt(void)
{
	Expr *cond, *ife, *elsee;

	elsee = NULL;
	assert(T_IF, "if");
	assert(T_LPAR, "(");
	cond = binexpr(0);
	assert(T_LPAR, ")");
	ife = blk_statements();
	if (current_token.type == T_ELSE) {
		assert(T_ELSE, "else");
		elsee = blk_statements();
	}
	return mkeif(cond, ife, elsee);
}


/****************************************************************************/
/*                              CODE GENERATION                             */
/****************************************************************************/

static char *reglist[4] = { "%r8", "%r9", "%r10", "%r11" };
static int free_regs[4]  = { 0, 0, 0, 0 };
static FILE *out;
static int nlabel = 0;

static int
calloc_reg(void)
{
	for (int i = 0; i < 4; ++i)
		if (!free_regs[i]) {
			free_regs[i] = 1;
			return i;
		}
	fprintf(stderr, "Unable to allocate a register");
	exit(1);
}

static void
cfree_reg(int reg)
{
	free_regs[reg] = 0;
}

static void
cfree_regs(void)
{
	for (int i = 0; i < 4; ++i)
		cfree_reg(i);
}

static int
cnew_label(void)
{
	return ++nlabel;
}

static void
clabel(int l)
{
	fprintf(out, "L%d:\n", l);
}

static int
cadd(int l, int r)
{
	fprintf(out, "addq %s, %s\n", reglist[r], reglist[l]);
	cfree_reg(r);
	return l;
}
static int
csub(int l, int r)
{
	fprintf(out, "subq %s, %s\n", reglist[r], reglist[l]);
	cfree_reg(r);
	return l;
}

static int
cmul(int l, int r)
{
	fprintf(out, "imulq %s, %s\n", reglist[r], reglist[l]);
	cfree_reg(r);
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
	cfree_reg(r);
	return l;
}

static int
ccompare(int l, int r, char *set)
{
	fprintf(out,
		"cmpq %s, %s\n"
		"%s %sb\n"
		"andq $255, %s\n",
		reglist[l], reglist[r], set, reglist[l], reglist[l]);
	cfree_reg(r);
	return l;
}

static int
cloadint(int n)
{
	int reg;

	reg = calloc_reg();
	fprintf(out, "movq $%d, %s\n", n, reglist[reg]);
	return reg;
}

static int
cloadvar(char *var)
{
	int reg;

	reg = calloc_reg();
	fprintf(out, "movq %s(%%rip), %s\n", var, reglist[reg]);
	return reg;
}

static void
cprintint(int reg)
{
	fprintf(out,
	        "movq %s, %%rdi\n"
	        "call printint\n", reglist[reg]);
}

static void
cstoresym(int reg, char *var)
{
	fprintf(out, "movq %s, %s(%%rip)\n", reglist[reg], var);
}

static void
cglobsym(char *var)
{
	fprintf(out, ".comm %s, 8, 8\n", var);
}

static void
cif(Expr *e)
{
	int label_end, label_else, creg;

	creg = compile_expr(e->condition);
}

static int
compile_expr(Expr *e)
{
	int lreg, rreg;

	if (e->op == DEXPR) {
		compile_expr(e->left);
		cfree_regs();
		compile_expr(e->right);
		cfree_regs();
		return -1;
	}
	if (e->right) rreg = compile_expr(e->right);
	if (e->op == ASSIGN) {
		cstoresym(rreg, e->var);
		return rreg;
	}
	if (e->left) lreg = compile_expr(e->left);

	switch (e->op) {
	case VAR: return cloadvar(e->var);
	case OP_GT: return ccompare(lreg, rreg, "setg");
	case OP_GE: return ccompare(lreg, rreg, "setge");
	case OP_LT: return ccompare(lreg, rreg, "setl");
	case OP_LE: return ccompare(lreg, rreg, "setle");
	case OP_EQU: return ccompare(lreg, rreg, "sete");
	case OP_NEQU: return ccompare(lreg, rreg, "setne");
	case OP_ADD: return cadd(lreg, rreg);
	case OP_SUB: return csub(lreg, rreg);
	case OP_MUL: return cmul(lreg, rreg);
	case OP_DIV: return cdiv(lreg, rreg);
	case INT_LIT: return cloadint(e->int_val);
	case PRINT: cprintint(lreg); return -1;
	default: exit(1);
	}
}

static void
cprolog(void)
{
	fputs(".text\n"
	      ".LC0:\n"
	      ".string \"%d\\n\"\n"
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
	compile_expr(blk_statements());
	cepilog();
	fclose(in);
	fclose(out);
}
