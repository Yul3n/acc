#include <stdio.h>
#include <stdlib.h>

typedef struct {
	enum {
		T_ADD, T_SUB, T_MUL, T_DIV, T_EQU,
		T_PRINT, T_INT, T_SEMI,
		T_EOF
	} type;
} Token;
