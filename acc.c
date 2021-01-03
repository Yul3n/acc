#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/** Lexer *********************************************************************/

#define MAX_IDENT_LEN 100

typedef enum {
		T_ADD, T_SUB, T_MUL, T_DIV, T_EQU,
		T_IDENT, T_INT, T_SEMI, T_NUM,
		T_EOF
} Token;

void next_char(void);
Token is_keyword(void);
void next_token(void);

int lex_int;
char lex_ident[MAX_IDENT_LEN];
int lex_prev_char;
Token lex_token;
FILE *in_file;

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
	} else {
		switch (lex_prev_char) {
		case '+':
			next_char();
			lex_token = T_ADD;
			break;
		case '-':
			next_char();
			lex_token = T_SUB;
			break;
		case '*':
			next_char();
			lex_token = T_MUL;
			break;
		}
	}
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
} Expr;

typedef struct {
	enum {
		ST_VARDECL, ST_AFFE
	} type;
	enum type var_type;
	char var[MAX_IDENT_LEN];
	Expr *expr;
} Statement;

Statement *affectation(void);
Expr *expr(void);

Statement *
affectation(void)
{
	Statement *stmt;

	stmt = malloc(sizeof(Statement));
	if (lex_token != T_IDENT)
		return NULL;
	strcpy(stmt->var, lex_ident);
	next_token();
	return stmt;
}

/******************************************************************************/
