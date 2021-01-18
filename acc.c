#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/** Lexer *********************************************************************/

#define MAX_IDENT_LEN 100

typedef enum {
	T_OP, T_IDENT,
	T_INT, T_CHAR, T_VOID, T_IF, T_ELSE, T_WHILE, T_FOR,
	T_LPAR, T_RPAR, T_LBRA, T_RBRA, T_SEMI,
	T_NUM, T_EOF
} Token;

int is_op(int c);
void next_char(void);
Token is_keyword(void);
void next_token(void);

int lex_int;
char lex_ident[MAX_IDENT_LEN];
int lex_prev_char;
Token lex_token;
FILE *in_file;
const char ops[] = {
	'+', '_', '/', '*', '=', '<', '>'
};

const Token punct[] = {
	[';'] = T_SEMI, ['('] = T_LPAR, [')'] = T_RPAR, ['{'] = T_LBRA,
	['}'] = T_RBRA
};

int
is_op(int c)
{
	for (int i = 0; i < (sizeof(ops) / sizeof(ops[0])); ++i)
		if (ops[i] == c)
			return 1;
	return 0;
}

void
next_char(void)
{
	lex_prev_char = fgetc(in_file);
}

Token
is_keyword(void)
{
	switch (*lex_ident) {
	case 'c':
		if (!strcmp(lex_ident + 1, "har"))
			return T_CHAR;
		break;
	case 'e':
		if (!strcmp(lex_ident + 1, "lse"))
			return T_ELSE;
		break;
	case 'f':
		if (!strcmp(lex_ident + 1, "or"))
			return T_FOR;
		break;
	case 'i':
		if (!strcmp(lex_ident + 1, "nt"))
			return T_INT;
		else if (!strcmp(lex_ident + 1, "f"))
			return T_IF;
		break;
	case 'v':
		if (!strcmp(lex_ident + 1, "oid"))
			return T_VOID;
		break;
	case 'w':
		if (!strcmp(lex_ident + 1, "hile"))
			return T_WHILE;
		break;
	}
	return T_IDENT;
}

void
next_token(void)
{
	while (lex_prev_char == ' ' ||
	       lex_prev_char == '\t' ||
	       lex_prev_char == '\n')
		next_char();
	if (isalpha(lex_prev_char)) {
		int i = 0;
		while (isalnum(lex_prev_char) || lex_prev_char == '_') {
			lex_ident[i++] = lex_prev_char;
			if (i >= MAX_IDENT_LEN)
				exit(1);
			next_char();
		}
		lex_ident[i] = '\0';
		lex_token = is_keyword();
	} else if (isdigit(lex_prev_char)) {
		lex_int = 0;
		while (isdigit(lex_prev_char)) {
			lex_int = lex_int * 10 + lex_prev_char - '0';
			next_char();
		}
		lex_token = T_NUM;
	} else if (is_op(lex_prev_char)) {
		int i = 0;
		while (is_op(lex_prev_char)) {
			lex_ident[i++] = lex_prev_char;
			if (i >= MAX_IDENT_LEN)
				exit(1);
			next_char();
		}
		lex_ident[i] = '\0';
		lex_token = T_OP;
	} else if (lex_prev_char == EOF) {
		lex_token = T_EOF;
	} else {
		if (punct[lex_prev_char]) {
			lex_token = punct[lex_prev_char];
			next_char();
		} else {
			exit(1);
		}
	}
}

/******************************************************************************/

/** Parser ********************************************************************/

#define OP_TABLE_SIZE 10
#define EXPR() binop(OP_TABLE_SIZE - 1)

enum type {
	TY_INT, TY_CHAR, TY_VOID
};

typedef struct expr {
	enum {
		ET_NUM, ET_BINOP, ET_VAR, ET_FUN_CALL
	} type;
	enum type etype;
	enum type cast;
	char binop[MAX_IDENT_LEN];
	struct expr *lexpr;
	struct expr *rexpr;
	char var[MAX_IDENT_LEN];
	int num;
} Expr;

typedef struct statement {
	enum {
		ST_VARDECL, ST_AFFE, ST_IF, ST_WHILE, ST_BLOCK, ST_EXPR
	} type;
	enum type var_type;
	char var[MAX_IDENT_LEN];
	Expr *expr;
	struct statement *bodyl;
	struct statement *bodyr;
} Statement;

typedef struct {
	int len;
	char **ops;
} OpList;

OpList opTable[OP_TABLE_SIZE];
const struct {
	char *op;
	int precedence;
} defaultOps[] = {
	{">", 6}, {">=", 6}, {"<", 6}, {"<=", 6}, {"+", 4}, {"-", 4}, {"*", 3},
	{"/", 3}
};

int op(char *op);
void add_op(char *op, int precedence);
void init_op_table(void);

enum type type(void);

Statement *block(void);

Statement *if_stmt(void);
Statement *while_stmt(void);
Statement *for_stmt(void);
Statement *vardecl(void);
Statement *affectation(void);
Statement *expr_stmt(void);
Statement *statement(void);

Expr *binop(int precedence);
Expr *expr(void);

Statement *(*stmt_fun[])(void) = {
	if_stmt,
	while_stmt,
	vardecl,
	affectation,
	for_stmt
};

int
op(char *op)
{
	return lex_token == T_OP && !strcmp(lex_ident, op);
}

void
add_op(char *op, int precedence)
{
	opTable[precedence].ops = realloc(opTable[precedence].ops,
	                                  ++opTable[precedence].len);
	opTable[precedence].ops[opTable[precedence].len - 1] = op;
}

void
init_op_table(void)
{
	for (int i = 0; i < 10; ++i) {
		opTable[i].ops = malloc(sizeof(char *));
		opTable[i].len = 0;
	}
	for (int i = 0; i < sizeof(defaultOps) / sizeof(defaultOps[0]); ++i)
		add_op(defaultOps[i].op, defaultOps[i].precedence);
}

enum type
type(void)
{
	enum type t;

	if (lex_token == T_INT)
		t = TY_INT;
	else if (lex_token == T_VOID)
		t = TY_VOID;
	else if (lex_token == T_CHAR) {
		t = TY_CHAR;
	}
	else
		return -1;
	next_token();
	return t;
}

Statement *
block(void)
{
	Statement *root, *stmt, *p, buf;

	if (lex_token != T_LBRA)
		return NULL;
	next_token();
	root = malloc(sizeof(Statement));
	p = root;
	stmt = statement();
	while (stmt != NULL) {

		p->bodyl = stmt;
		p->type = ST_BLOCK;
		if ((stmt = statement()) != NULL) {
			p->bodyr = malloc(sizeof(Statement));
			p = p->bodyr;
		}
	}
	if (lex_token != T_RBRA)
		return NULL;
	next_token();
	buf = *p->bodyl;
	*p = buf;
	return root;
}

Statement *
if_stmt(void)
{
	Statement *stmt;

	stmt = malloc(sizeof(Statement));
	if (lex_token != T_IF)
		return NULL;
	next_token();
	if (lex_token != T_LPAR)
		return NULL;
	next_token();
	stmt->expr = expr();
	if (!stmt->expr)
		return NULL;
	if (lex_token != T_RPAR)
		return NULL;
	next_token();
	stmt->bodyl = block();
	if (!stmt->bodyl)
		return NULL;
	if (lex_token == T_ELSE) {
		next_token();
		stmt->bodyr = block();
		if (!stmt->bodyr)
			return NULL;
	} else {
		stmt->bodyr = NULL;
	}
	stmt->type = ST_IF;
	return stmt;
}

Statement *
while_stmt(void)
{
	Statement *stmt;

	stmt = malloc(sizeof(Statement));
	if (lex_token != T_WHILE)
		return NULL;
	next_token();
	if (lex_token != T_LPAR)
		return NULL;
	next_token();
	stmt->expr = expr();
	if (!stmt->expr)
		return NULL;
	if (lex_token != T_RPAR)
		return NULL;
	next_token();
	stmt->bodyl = block();
	if (!stmt->bodyl)
		return NULL;
	stmt->type = ST_WHILE;
	return stmt;
}

Statement *
for_stmt(void)
{
	Statement *while_s, *pre, *post, *stmt;

	while_s = malloc(sizeof(Statement));
	if (lex_token != T_FOR)
		return NULL;
	next_token();
	if (lex_token != T_LPAR)
		return NULL;
	next_token();
	pre = statement();
	if (!pre)
		return NULL;
	while_s->expr = EXPR();
	if (!while_s->expr)
		return NULL;
	if (lex_token != T_SEMI)
		return NULL;
	next_token();
	post = malloc(sizeof(Statement));
	post->type = ST_EXPR;
	post->expr = EXPR();
	if (lex_token != T_RPAR)
		return NULL;
	next_token();
	while_s->type = ST_WHILE;
	while_s->bodyl = malloc(sizeof(Statement));
	while_s->bodyl->type = ST_BLOCK;
	while_s->bodyl->bodyl = block();
	while_s->bodyl->bodyr = post;
	stmt = malloc(sizeof(Statement));
	stmt->type = ST_BLOCK;
	stmt->bodyl = pre;
	stmt->bodyr = while_s;
	return stmt;
}

Statement *
vardecl(void)
{
	Statement *stmt;

	stmt = malloc(sizeof(Statement));
	stmt->var_type = type();
	if (stmt->var_type == -1)
		return NULL;
	stmt->type = ST_VARDECL;
	if (lex_token != T_IDENT)
		return NULL;
	strcpy(stmt->var, lex_ident);
	next_token();
	if (lex_token != T_SEMI)
		return NULL;
	next_token();
	return stmt;
}

Statement *
affectation(void)
{
	Statement *stmt;

	stmt = malloc(sizeof(Statement));
	if (lex_token != T_IDENT)
		return NULL;
	strcpy(stmt->var, lex_ident);
	next_token();
	if (!op("="))
		return NULL;
	next_token();
	stmt->expr = binop(OP_TABLE_SIZE - 1);
	if (!stmt->expr)
		return NULL;
	if (lex_token != T_SEMI)
		return NULL;
	next_token();
	stmt->type = ST_AFFE;
	return stmt;
}


Statement *
expr_stmt(void)
{
	Statement *stmt;

	stmt = malloc(sizeof(Statement));
	stmt->expr = expr();
	if (!stmt->expr)
		return NULL;
	stmt->type = ST_EXPR;
	return stmt;
}

Statement *
statement(void)
{
	Statement *stmt;

	for (int i = 0; i < sizeof(stmt_fun) / sizeof(stmt_fun[0]); ++i) {
		if ((stmt = stmt_fun[i]()))
			return stmt;
	}
	return NULL;
}

Expr *
binop(int precedence)
{
	Expr *lexpr;
	int i;

	if (precedence < 0)
		return expr();
	else
		lexpr = binop(precedence - 1);
	for (;;) {
		for (i = 0; i < opTable[precedence].len; ++i) {
			if (op(opTable[precedence].ops[i])) {
				Expr *e = malloc(sizeof(Expr));
				strcpy(e->binop, lex_ident);
				next_token();
				e->lexpr = lexpr;
				e->rexpr = binop(precedence - 1);
				e->type = ET_BINOP;
				lexpr = e;
				break;
			}
		}
		if (i == opTable[precedence].len)
			return lexpr;
	}
	return lexpr;
}

Expr *
expr(void)
{
	Expr *e;

	e = malloc(sizeof(Expr));
	if (lex_token == T_IDENT) {
		e->type = ET_VAR;
		strcpy(e->var, lex_ident);
	} else if (lex_token == T_NUM) {
		e->type = ET_NUM;
		e->num = lex_int;
	} else {
		return NULL;
	}
	next_token();
	return e;
}

/******************************************************************************/

/** Symbol Table **************************************************************/

typedef struct {
	enum {
		TT_VAR
	} type;
	char name[MAX_IDENT_LEN];
	enum type vtype;
} Symbol;

#define SYMBOL_TABLE_SIZE 4096

struct sym_table_elem {
	struct sym_table_elem *next;
	Symbol elem;
} *symbol_table[SYMBOL_TABLE_SIZE];

unsigned int hash(const char *s);

unsigned int
hash(const char *s)
{
	unsigned int h;

	for (h = 0; *s != '\0'; ++s)
		h = *s ^ (31 * h);
	return h % SYMBOL_TABLE_SIZE;
}

Symbol *hash_search(char *s);
Symbol *hash_new(char *s);

Symbol *
hash_search(char *s)
{
	unsigned int h;

	h = hash(s);
	if (!symbol_table[h]) {
		return NULL;
	} else {
		struct sym_table_elem *p = symbol_table[h];
		while (p) {
			if (!strcmp(s, p->elem.name))
				return &p->elem;
			p = p->next;
		}
		return NULL;
	}
}

Symbol *
hash_new(char *s)
{
	unsigned h;

	h = hash(s);
	if (!symbol_table[h]) {
		symbol_table[h] = malloc(sizeof(struct sym_table_elem));
		symbol_table[h]->next = NULL;
		return &symbol_table[h]->elem;
	} else {
		struct sym_table_elem *p = symbol_table[h];
		while (p->next)
			p = p->next;
		p->next = malloc(sizeof(struct sym_table_elem));
		p = p->next;
		p->next = NULL;
		return &p->elem;
	}
}

/******************************************************************************/

/** Type System ***************************************************************/

enum widen {
	W_RIGHT = 1, W_LEFT, W_NONE
};

int check_numeric(enum type t);
int check_strict(enum type left, enum type right);
int check_widen_left(enum type left, enum type right);
int check_widen_right(enum type left, enum type right);
int check_types(enum type left, enum type right);
int unify_expr_type(Expr *lexpr, Expr *rexpr);
int check_expr_type(Expr *e);
int check_stmt_type(Statement *stmt);

int
check_numeric(enum type t)
{
	return t == TY_INT || t == TY_CHAR;
}

int
check_strict(enum type left, enum type right)
{
	if (left == TY_VOID || right == TY_VOID)
		return 0;
	return left == right ? W_NONE : 0;
}

int
check_widen_left(enum type left, enum type right)
{
	if (check_strict(left, right))
		return W_NONE;
	if (left == TY_CHAR && right == TY_INT) {
		return W_LEFT | TY_INT << 4;
	}
	return 0;
}

int
check_widen_right(enum type left, enum type right)
{
	if (check_strict(left, right))
		return W_NONE;
	if (left == TY_INT && right == TY_CHAR) {
		return W_RIGHT || TY_INT << 4;
	}
	return 0;
}

int
check_types(enum type left, enum type right)
{
	int w;

	w = check_widen_left(left, right);
	if (w == 0)
		w = check_widen_right(left, right);
	return w;
}

int
unify_expr_type(Expr *lexpr, Expr *rexpr)
{
	int w;

	if (!check_expr_type(lexpr))
		return 0;
	if (!check_expr_type(rexpr))
		return 0;
	w = check_types(lexpr->etype, rexpr->etype);
	if (w == 0)
		return 0;
	switch ((enum widen)(w & 0b1111)) {
	case W_LEFT:
		lexpr->cast = w >> 4;
		return 1;
	case W_RIGHT:
		rexpr->cast = w >> 4;
		return 1;
	case W_NONE:
		return 1;
	}
}

int
check_expr_type(Expr *e)
{
	switch (e->type) {
	case ET_NUM:
		e->etype = (char)e->num == e->num ? TY_CHAR : TY_INT;
		return 1;
	case ET_VAR: {
		Symbol *s;
		s = hash_search(e->var);
		if (!s)
			return 0;
		e->etype = s->vtype;
		return 1;
	}
	case ET_BINOP:
		return unify_expr_type(e->lexpr, e->rexpr);
	case ET_FUN_CALL:
		return 1;
	}
}

int
check_stmt_type(Statement *stmt)
{
	switch (stmt->type) {
	case ST_AFFE: {
		Symbol *s = hash_search(stmt->var);
		if (!s)
			exit(1);
		check_expr_type(stmt->expr);
		int w = check_widen_right(s->vtype, stmt->expr->etype);
		if (!w)
			return 0;
		if ((w & 0b1111) != W_NONE) {
			stmt->expr->cast = w >> 4;
		}
		return 1;
	}
	case ST_EXPR:
		return check_expr_type(stmt->expr);
	case ST_IF:
		if (stmt->bodyr && !check_stmt_type(stmt->bodyr))
			return 0;
	case ST_WHILE:
		if (!check_expr_type(stmt->expr) ||
		    !check_numeric(stmt->expr->etype))
			return 0;
		if (!check_stmt_type(stmt->bodyl))
			return 0;
		return 1;
	case ST_BLOCK:
		if (!check_stmt_type(stmt->bodyl) ||
		    !check_stmt_type(stmt->bodyr))
			return 0;
		return 1;
	case ST_VARDECL: {
		Symbol *s = hash_new(stmt->var);
		s->type = TT_VAR;
		s->vtype = stmt->var_type;
		strcpy(s->name, stmt->var);
		return 1;
	}
	}
}

/******************************************************************************/

/** Debug *********************************************************************/

void print_expr(Expr e);
void print_type(enum type type);
void print_statement(Statement stmt);
void print_block(Statement block);

void
print_expr(Expr e)
{
	switch (e.type) {
	case ET_VAR:
		printf("{\"variable\" : \"%s\"}", e.var);
		break;
	case ET_NUM:
		printf("{\"number\" : %d}", e.num);
		break;
	case ET_BINOP:
		printf("{\"binop\" : \"%s\"", e.binop);
		printf(", \"lexpr\" : ");
		print_expr(*e.lexpr);
		printf(", \"rexpr\" : ");
		print_expr(*e.rexpr);
		printf("}");
		break;
	case ET_FUN_CALL:
		break;
	}
}

void
print_type(enum type type)
{
	switch (type) {
	case TY_CHAR:
		printf("\"char\"");
		break;
	case TY_INT:
		printf("\"int\"");
		break;
	case TY_VOID:
		printf("\"void\"");
		break;
	}
}

void
print_statement(Statement stmt)
{
	switch (stmt.type) {
	case ST_AFFE:
		printf("{\"affectation\" : {\"variable\" : \"%s\", \"body\" :",
		       stmt.var);
		print_expr(*stmt.expr);
		printf("}}");
		break;
	case ST_VARDECL:
		printf("{\"variable declaration\" : {\"type\" :");
		print_type(stmt.var_type);
		printf(", \"variable\" : \"%s\"}}", stmt.var);
		break;
	case ST_IF:
		printf("{\"if\": {\"condition\":");
		print_expr(*stmt.expr);
		printf(",\"body\":");
		print_block(*stmt.bodyl);

		if (stmt.bodyr) {
			printf(",\"else\":");
			print_block(*stmt.bodyr);
		}
		printf("}}");
		break;
	case ST_WHILE:
		printf("{\"while\": {\"condition\":");
		print_expr(*stmt.expr);
		printf(",\"body\":");
		print_block(*stmt.bodyl);
		printf("}}");
		break;
	case ST_BLOCK:
		print_statement(*stmt.bodyl);
		printf(",");
		print_statement(*stmt.bodyr);
		break;
	case ST_EXPR:
		printf("{\"expr\":");
		print_expr(*stmt.expr);
		printf("}");
		break;
	}
}

void
print_block(Statement block)
{
	printf("[");
	print_statement(block);
	printf("]");
}

/******************************************************************************/

int
main()
{
	Statement *prog;

	in_file = fopen("test.ac", "r");
	next_char();
	next_token();
	init_op_table();
	prog = block();
	if (!prog)
		return 1;
	if (!check_stmt_type(prog))
		return 1;
	print_block(*prog);
}
