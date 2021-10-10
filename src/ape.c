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
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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

#define STRBUFSIZE ((int)sizeof(ape_Object *) - 1)
#define GCMARKBIT (0x2)
#define GCSTACKSIZE (256)
#define CHUNKSIZE (256)

typedef struct ape_Chunk {
  ape_Object objects[CHUNKSIZE];
  struct ape_Chunk *next;
} ape_Chunk;

struct ape_State {
  ape_Alloc alloc;
  void *ud;

  ape_Handlers handlers;

  ape_Chunk *chunks;
  int chunks_count;

  ape_Object *gcstack[GCSTACKSIZE];
  int gcstack_idx;

  ape_Object *calllist;
  ape_Object *freelist;
  ape_Object *symlist;
  ape_Object *t;
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

static void extend_chunks(ape_State *A) {
  ape_Chunk *chunk = (ape_Chunk *)ape_malloc(A, sizeof(ape_Chunk));
  int i;

  if (!chunk)
    ape_error(A, "out of memory");

  /* push to the chunks list */
  chunk->next = A->chunks;
  A->chunks = chunk;
  A->chunks_count += 1;

  /* populate freelist */
  for (i = 0; i < CHUNKSIZE; ++i) {
    ape_Object *obj = &chunk->objects[i];
    settype(obj, APE_TFREE);
    cdr(obj) = A->freelist;
    A->freelist = obj;
  }
}

ape_State *ape_newstate(ape_Alloc f, void *ud) {
  ape_Alloc alloc = f ? f : alloc_emul;
  ape_State *A = (ape_State *)alloc(ud, NULL, sizeof(ape_State));

  if (!A)
    return NULL;

  memset(A, 0, sizeof(ape_State));

  A->alloc = alloc;
  A->ud = ud;

  A->calllist = &nil;
  A->freelist = &nil;
  A->symlist = &nil;

  return A;
}

void ape_close(ape_State *A) {
  ape_Chunk *chunk = A->chunks;

  /* free all chunks */
  while (chunk) {
    ape_Chunk *next = chunk->next;
    ape_free(A, chunk);
    chunk = next;
  }

  ape_free(A, A);
}

ape_Handlers *ape_handlers(ape_State *A) { return &A->handlers; }

void ape_error(ape_State *A, const char *errmsg) {
  ape_Object *cl = A->calllist;

  /* reset context state */
  A->calllist = &nil;

  /* do error handler */
  if (A->handlers.error)
    A->handlers.error(A, errmsg, cl);

  /* error handler returned -- print error and traceback, exit */
  fprintf(stderr, "error: %s\n", errmsg);

  exit(EXIT_FAILURE);
}
