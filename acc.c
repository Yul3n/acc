#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/** Lexer *********************************************************************/

#define MAX_IDENT_LEN 100

typedef enum {
	T_OP, T_IDENT, T_INT, T_IF, T_ELSE, T_WHILE, T_FOR, T_LPAR, T_RPAR,
	T_LBRA, T_RBRA, T_SEMI, T_NUM, T_EOF
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

int
is_op(int c)
{
	return c == '+' || c == '-' || c == '/' || c == '*' || c == '=';
}

void
next_char(void)
{
	lex_prev_char = fgetc(in_file);
}

Token
is_keyword(void)
{
	if (!strcmp(lex_ident, "int")) {
		return T_INT;
	} else if (!strcmp(lex_ident, "if")) {
		return T_IF;
	} else if (!strcmp(lex_ident, "else")) {
		return T_ELSE;
	} else if (!strcmp(lex_ident, "while")) {
		return T_WHILE;
	} else if (!strcmp(lex_ident, "for")) {
		return T_FOR;
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
		switch (lex_prev_char) {
		case ';':
			lex_token = T_SEMI;
			next_char();
			break;
		case '(':
			lex_token = T_LPAR;
			next_char();
			break;
		case ')':
			lex_token = T_RPAR;
			next_char();
			break;
		case '{':
			lex_token = T_LBRA;
			next_char();
			break;
		case '}':
			lex_token = T_RBRA;
			next_char();
			break;
		default:
			exit(1);
		}
	}
}

/******************************************************************************/

/** Parser ********************************************************************/

#define OP_TABLE_SIZE 10
#define EXPR() binop(OP_TABLE_SIZE - 1)

enum type {
	TY_INT
};

typedef struct expr {
	enum {
		ET_NUM, ET_BINOP, ET_VAR
	} type;
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
	add_op("+", 4);
	add_op("-", 4);
	add_op("*", 3);
	add_op("/", 3);
}

enum type
type(void)
{
	if (lex_token != T_INT)
		return -1;
	next_token();
	return TY_INT;
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
	pre = statement();
	if (!pre)
		return NULL;
	while_s->expr = expr();
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
	}
}

void
print_type(enum type type)
{
	switch (type) {
	case TY_INT:
		printf("\"int\"");
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
		print_type(stmt.type);
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
	in_file = fopen("test.ac", "r");
	next_char();
	next_token();
	init_op_table();
	printf("[");
	int i = 0;
	while (lex_token != T_EOF) {
		if (!i)
			i = 1;
		else
			printf(",");
		print_statement(*statement());
	}
	printf("]");
}
