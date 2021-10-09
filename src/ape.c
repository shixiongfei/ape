/*
 * ape.c
 *
 * Copyright (c) 2021 Xiongfei Shi
 *
 * Author: Xiongfei Shi <xiongfei.shi(a)icloud.com>
 * License: Apache-2.0
 *
 * https://github.com/shixiongfei/ape
 */

#include "ape.h"
#include <stdlib.h>

enum {
  P_DEF,
  P_SET,
  P_COND,
  P_FN,
  P_MACRO,
  P_QUOTE,
  P_AND,
  P_OR,
  P_NOT,
  P_XOR,
  P_DO,
  P_CONS,
  P_CAR,
  P_CDR,
  P_IS,
  P_PRINT,
  P_EQ,
  P_LT,
  P_LTE,
  P_GT,
  P_GTE,
  P_ADD,
  P_SUB,
  P_MUL,
  P_DIV,
  P_MAX
};

static const char *primnames[] = {
    "def", "set!", "cond", "fn",  "macro", "quote", "and",   "or", "not",
    "xor", "do",   "cons", "car", "cdr",   "is?",   "print", "=",  "<",
    "<=",  ">",    ">=",   "+",   "-",     "*",     "/"};

static const char *typenames[] = {
    "pair",   "free",     "nil",   "integer",   "number",    "symbol",
    "string", "function", "macro", "primitive", "cfunction", "pointer"};

typedef union {
  ape_Object *o;
  ape_CFunc f;
  ape_Integer d;
  ape_Number n;
  char c;
} Value;

struct ape_Object {
  Value car, cdr;
};

struct ape_State {
  ape_Alloc alloc;
  void *ud;

  ape_Handlers handlers;
};

#define unused(x) ((void)x)

/*                Pair
 * +---------------+---------------+
 * |      car      |      cdr      |
 * +---------------+---------------+
 */

#define car(x) ((x)->car.o)
#define cdr(x) ((x)->cdr.o)

/*                Tag
 * +---+---+---+---+---+---+---+---+
 * |          Type         |GC | 1 |
 * +---+---+---+---+---+---+---+---+
 * 8   7   6   5   4   3   2   1   0
 */

#define tag(x) ((x)->car.c)
#define type(x) (tag(x) & 0x1 ? tag(x) >> 2 : APE_TPAIR)
#define settype(x, t) (tag(x) = (t) << 2 | 1)

#define isnil(x) ((x) == &nil)

#define integer(x) ((x)->cdr.d)
#define number(x) ((x)->cdr.n)
#define prim(x) ((x)->cdr.c)
#define cfunc(x) ((x)->cdr.f)
#define strbuf(x) (&(x)->car.c + 1)

#define STRBUFSIZE ((int)sizeof(ape_Object *) - 1)
#define GCMARKBIT (0x2)
#define GCSTACKSIZE (256)

static ape_Object nil = {{(void *)(APE_TNIL << 2 | 1)}, {NULL}};

static void *alloc_emul(void *ud, void *ptr, size_t size) {
  unused(ud);

  if (size)
    return realloc(ptr, size);

  free(ptr);
  return NULL;
}

#define ape_realloc(A, p, n) A->alloc(A->ud, p, n)
#define ape_malloc(A, n) ape_realloc(A, NULL, n)
#define ape_free(A, p) ape_realloc(A, p, 0)

ape_State *ape_newstate(ape_Alloc f, void *ud) {
  ape_Alloc alloc = f ? f : alloc_emul;
  ape_State *A = (ape_State *)alloc(ud, NULL, sizeof(ape_State));

  if (!A)
    return NULL;

  A->alloc = alloc;
  A->ud = ud;

  return A;
}

void ape_close(ape_State *A) { ape_free(A, A); }

ape_Handlers *ape_handlers(ape_State *A) { return &A->handlers; }
