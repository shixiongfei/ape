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

#define STRBUFSIZE ((int)sizeof(ape_Object *) - 1)
#define STRBUFINDEX (STRBUFSIZE - 1)
#define CHUNKSIZE (256)
#define GCSTACKSIZE (256)
#define GCMARKBIT (0x2)
#define FCMARKBIT (0x4)

typedef union {
  ape_Object *o;
  ape_CFunc f;
  ape_Integer d;
  ape_Number n;
  struct {
#if defined(__BYTE_ORDER__) && __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
    char s[STRBUFSIZE];
    char c;
#else
    char c;
    char s[STRBUFSIZE];
#endif
  };
} Value;

/*               Object
 * +---------------+---------------+
 * |      car      |      cdr      |
 * +---------------+---------------+
 */

struct ape_Object {
  Value car, cdr;
};

/*                               Chunks
 * +--------+--------+--------+           +--------+--------+--------+
 * | Object | Object | Object |     +-----+ Object | Object | Object |
 * +--------+--------+--------+     |     +--------+--------+--------+
 * | Object | Object | Object |     |     | Object | Object | Object |
 * +--------+--------+--------+     |     +--------+--------+--------+
 * |        ...etc...         |     |     |        ...etc...         |
 * +--------------------------+     |     +--------------------------+
 * |        next chunk        +-----+     |    next chunk or NULL    |
 * +--------------------------+           +--------------------------+
 */

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

#define car(x) ((x)->car.o)
#define cdr(x) ((x)->cdr.o)

/*                Tag
 * +---+---+---+---+---+---+---+---+
 * |        Type       |FC |GC | 1 |
 * +---+---+---+---+---+---+---+---+
 * 8   7   6   5   4   3   2   1   0
 */

#define tag(x) ((x)->car.c)
#define type(x) (tag(x) & 0x1 ? tag(x) >> 3 : APE_TPAIR)
#define settype(x, t) (tag(x) = (t) << 3 | 1)

#define isnil(x) ((x) == &nil)

#define integer(x) ((x)->cdr.d)
#define number(x) ((x)->cdr.n)
#define prim(x) ((x)->cdr.c)
#define cfunc(x) ((x)->cdr.f)

/* String: Hello, World.
 *                             car                                     cdr
 * +-------------------------------------------------------------+-------------+
 * |           tag                         strbuf                |             |
 * | +--------+---+---+---+ +----+----+----+----+----+----+----+ |             |
 * | | String | 1 | 0 | 1 | | \H | \e | \l | \l | \o | \, | \  | |      +      |
 * | +--------+---+---+---+ +----+----+----+----+----+----+----+ |      |      |
 * |            c             s0   s1   s2   s3   s4   s5   s6   |      |      |
 * +-------------------------------------------------------------+------+------+
 *                                                                      |
 *                              +---------------------------------------+
 *                              |
 * +----------------------------+--------------------------------+-------------+
 * |           tag                         strbuf                |             |
 * | +--------+---+---+---+ +----+----+----+----+----+----+----+ |             |
 * | | String | 0 | 0 | 1 | | \W | \o | \r | \l | \d | \. |  6 | |     nil     |
 * | +--------+---+---+---+ +----+----+----+----+----+----+----+ |             |
 * |            c             s0   s1   s2   s3   s4   s5   s6   |             |
 * +-------------------------------------------------------------+-------------+
 */

#define strbuf(x) ((x)->car.s)
#define stridx(x) ((x)->car.s[STRBUFINDEX])

static ape_Object nil = {{(void *)(APE_TNIL << 3 | 1)}, {NULL}};

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

static int extend_chunks(ape_State *A) {
  ape_Chunk *chunk = (ape_Chunk *)ape_malloc(A, sizeof(ape_Chunk));
  int i;

  if (!chunk)
    return -1;

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
  return 0;
}

static void collect_garbage(ape_State *A) {
  ape_Chunk *chunk;
  int i;

  /* mark */
  for (i = 0; i < A->gcstack_idx; i++) {
    ape_mark(A, A->gcstack[i]);
  }
  ape_mark(A, A->symlist);

  /* sweep and unmark */
  for (chunk = A->chunks; chunk != NULL; chunk = chunk->next) {
    for (i = 0; i < CHUNKSIZE; ++i) {
      ape_Object *obj = &chunk->objects[i];

      if (type(obj) == APE_TFREE)
        continue;

      /* marked */
      if (tag(obj) & GCMARKBIT) {
        tag(obj) &= ~GCMARKBIT;
        continue;
      }

      if (type(obj) == APE_TPTR && A->handlers.gc)
        A->handlers.gc(A, obj);

      settype(obj, APE_TFREE);
      cdr(obj) = A->freelist;
      A->freelist = obj;
    }
  }
}

static ape_Object *alloc(ape_State *A) {
  ape_Object *obj;

  /* do gc if freelist has no more objects */
  if (isnil(A->freelist)) {
    collect_garbage(A);

    if (isnil(A->freelist))
      if (extend_chunks(A) < 0)
        ape_error(A, "out of memory");
  }

  /* get object from freelist and push to the gcstack */
  obj = A->freelist;
  A->freelist = cdr(obj);
  ape_pushgc(A, obj);

  return obj;
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

void ape_pushgc(ape_State *A, ape_Object *obj) {
  if (A->gcstack_idx == GCSTACKSIZE)
    ape_error(A, "gc stack overflow");

  A->gcstack[A->gcstack_idx++] = obj;
}

void ape_restoregc(ape_State *A, int idx) { A->gcstack_idx = idx; }

int ape_savegc(ape_State *A) { return A->gcstack_idx; }

void ape_mark(ape_State *A, ape_Object *obj) {
  ape_Object *car;

loop:
  if (tag(obj) & GCMARKBIT)
    return;

  /* store car before modifying it with GCMARKBIT */
  car = car(obj);
  tag(obj) |= GCMARKBIT;

  switch (type(obj)) {
  case APE_TPAIR:
    ape_mark(A, car);
    /* fall through */
  case APE_TFUNC:
  case APE_TMACRO:
  case APE_TSYMBOL:
  case APE_TSTRING:
    obj = cdr(obj);
    goto loop;

  case APE_TPTR:
    if (A->handlers.mark)
      A->handlers.mark(A, obj);
    break;
  }
}

int ape_type(ape_State *A, ape_Object *obj) {
  unused(A);
  return type(obj);
}

int ape_isnil(ape_State *A, ape_Object *obj) {
  unused(A);
  return isnil(obj);
}

static ape_Object *checktype(ape_State *A, ape_Object *obj, int type) {
  if (type(obj) != type) {
    char buf[64];
    sprintf(buf, "expected %s, got %s", typenames[type], typenames[type(obj)]);
    ape_error(A, buf);
  }
  return obj;
}

ape_Object *ape_cons(ape_State *A, ape_Object *car, ape_Object *cdr) {
  ape_Object *obj = alloc(A);
  car(obj) = car;
  cdr(obj) = cdr;
  return obj;
}

ape_Object *ape_car(ape_State *A, ape_Object *obj) {
  if (isnil(obj))
    return obj;
  return car(checktype(A, obj, APE_TPAIR));
}

ape_Object *ape_cdr(ape_State *A, ape_Object *obj) {
  if (isnil(obj))
    return obj;
  return cdr(checktype(A, obj, APE_TPAIR));
}

ape_Object *ape_list(ape_State *A, ape_Object **objs, int cnt) {
  ape_Object *list = &nil;

  while (cnt--)
    list = ape_cons(A, objs[cnt], list);

  return list;
}

ape_Object *ape_bool(ape_State *A, int b) { return b ? A->t : &nil; }

ape_Object *ape_integer(ape_State *A, ape_Integer d) {
  ape_Object *obj = alloc(A);
  settype(obj, APE_TINTEGER);
  integer(obj) = d;
  return obj;
}

ape_Object *ape_number(ape_State *A, ape_Number n) {
  ape_Object *obj = alloc(A);
  settype(obj, APE_TNUMBER);
  number(obj) = n;
  return obj;
}

static ape_Object *build_string(ape_State *A, ape_Object *tail, int ch) {
  int index;

  if (!tail || (tag(tail) & FCMARKBIT)) {
    ape_Object *obj = ape_cons(A, NULL, &nil);
    settype(obj, APE_TSTRING);

    if (tail) {
      cdr(tail) = obj;
      A->gcstack_idx--;
    }

    tail = obj;
  }

  index = stridx(tail);
  strbuf(tail)[index++] = ch;

  if (index == STRBUFSIZE)
    tag(tail) |= FCMARKBIT;
  else
    stridx(tail) = index;

  return tail;
}

ape_Object *ape_string(ape_State *A, const char *str) {
  return ape_lstring(A, str, strlen(str));
}

ape_Object *ape_lstring(ape_State *A, const char *str, int len) {
  ape_Object *obj = build_string(A, NULL, 0);
  ape_Object *tail = obj;
  int i;

  for (i = 0; i < len; ++i)
    tail = build_string(A, tail, str[i]);

  return obj;
}

ape_Object *ape_symbol(ape_State *A, const char *name) {}

ape_Object *ape_cfunc(ape_State *A, ape_CFunc fn) {
  ape_Object *obj = alloc(A);
  settype(obj, APE_TCFUNC);
  cfunc(obj) = fn;
  return obj;
}

ape_Object *ape_ptr(ape_State *A, void *ptr) {
  ape_Object *obj = alloc(A);
  settype(obj, APE_TPTR);
  cdr(obj) = ptr;
  return obj;
}
