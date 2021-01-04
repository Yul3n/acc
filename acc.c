#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/** Lexer *********************************************************************/

#define MAX_IDENT_LEN 100

typedef enum {
		T_OP, T_IDENT, T_INT, T_SEMI, T_NUM, T_EOF
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
	}
	return T_IDENT;
}

void
next_token(void)
{
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
	}
	exit(1);
}

/******************************************************************************/

/** Parser ********************************************************************/

enum type {
	TY_INT
};

typedef struct expr {
	enum {
		ET_INT, ET_BINOP, ET_VAR
	} type;
	enum {
		BO_ADD, BO_SUB, BO_MUL, BO_DIV
	} binop;
	struct expr *lexpr;
	struct expr *rexpr;
	char var[MAX_IDENT_LEN];
	int num;
} Expr;

typedef struct {
	enum {
		ST_VARDECL, ST_AFFE
	} type;
	enum type var_type;
	char var[MAX_IDENT_LEN];
	Expr *expr;
} Statement;

typedef struct {
	int len;
	char **ops;
} OpList;

int op(char *op);
void add_op(char *op, int precedence);
enum type type(void);

Statement *vardecl(void);
Statement *affectation(void);

Expr *binop(Expr *lexpr);
Expr *expr(void);

int
op(char *op)
{
	return lex_token == T_OP && !strcmp(lex_ident, op);
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
	stmt->expr = expr();
	if (!stmt->expr)
		return NULL;
	if (lex_token != T_SEMI)
		return NULL;
	next_token();
	stmt->type = ST_AFFE;
	return stmt;
}

Expr *
binop(Expr *lexpr)
{
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
		e->type = ET_INT;
		e->num = lex_int;
	} else {
		return NULL;
	}
	next_token();
	return binop(e);
}

/******************************************************************************/
