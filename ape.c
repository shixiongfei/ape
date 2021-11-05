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

#include <ctype.h>
#include <float.h>
#include <math.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "ape.h"

/* assumption: pointers are 32 or 64 bit,
   and float/double are IEEE binary32/binary64 */
#if INTPTR_MAX >= INT64_MAX
typedef double floatptr_t;
#define FP_EPSILON DBL_EPSILON
#else
typedef float floatptr_t;
#define FP_EPSILON FLT_EPSILON
#endif

enum {
  P_DEF,
  P_SET,
  P_IF,
  P_FN,
  P_MACRO,
  P_EXPAND,
  P_QUOTE,
  P_QUASIQUOTE,
  P_UNQUOTE,
  P_UNQUOTE_SPLICING,
  P_AND,
  P_OR,
  P_NOT,
  P_DO,
  P_CONS,
  P_CAR,
  P_CDR,
  P_SETCAR,
  P_SETCDR,
  P_TYPE,
  P_VECTOR,
  P_VECSET,
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
    "def",    "set!",        "if",         "fn",       "macro",
    "expand", "quote",       "quasiquote", "unquote",  "unquote-splicing",
    "and",    "or",          "not",        "do",       "cons",
    "car",    "cdr",         "set-car!",   "set-cdr!", "type",
    "vector", "vector-set!", "=",          "<",        "<=",
    ">",      ">=",          "+",          "-",        "*",
    "/",
};

static const char *typenames[] = {
    "pair",   "free",     "nil",   "number",    "symbol",    "string",
    "vector", "function", "macro", "primitive", "cfunction", "pointer",
};

typedef intptr_t slimb_t;
typedef uintptr_t limb_t;

#define STRBUFSIZE ((int)sizeof(ape_Object *) - 1)
#define STRBUFINDEX (STRBUFSIZE - 1)
#if INTPTR_MAX >= INT64_MAX
#define EXPNBUFSIZE 2
#else
#define EXPNBUFSIZE 1
#endif
#define NUMBUFSIZE ((int)sizeof(ape_Object *) - EXPNBUFSIZE - 1)
#define CHUNKSIZE (0xFFFF)
#define GCSTACKSIZE (256)
#define GCMARKBIT (0x2)
#define FCMARKBIT (0x4)
#define SNMARKBIT (0x4)
#define HASHMASK ((((limb_t)1) << (STRBUFSIZE * 8)) - 1)
#define MAXVECSIZE (1 << 24)

typedef union {
  ape_Object *o;
  ape_CFunc f;
  floatptr_t n; /* Alternatives before implementing Decimal */
  limb_t d;     /* TODO: Decimal */
  struct {
#if defined(__BYTE_ORDER__) && __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
    limb_t coefficient : NUMBUFSIZE * 8;
    slimb_t exponent : EXPNBUFSIZE * 8;
    limb_t _ : 8;
#else
    limb_t _ : 8;
    slimb_t exponent : EXPNBUFSIZE * 8;
    limb_t coefficient : NUMBUFSIZE * 8;
#endif
  };
  struct {
#if defined(__BYTE_ORDER__) && __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
    char s[STRBUFSIZE];
    unsigned char c;
#else
    unsigned char c;
    char s[STRBUFSIZE];
#endif
  };
  struct {
#if defined(__BYTE_ORDER__) && __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
    limb_t nx : STRBUFSIZE * 8;
    limb_t __ : 8;
#else
    limb_t __ : 8;
    limb_t nx : STRBUFSIZE * 8;
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
  union {
    ape_Object _; /* memory alignment */
    struct ape_Chunk *next;
  };
} ape_Chunk;

/*                        Symbol List
 *      +--------+--------+        +--------+--------+
 *      |  car   |   cdr  +--------+  car   |   nil  |
 *      +---+----+--------+        +---+----+--------+
 *          |                          |
 * +--------+--------+        +--------+--------+
 * | Symbol |        |        | Symbol |        |
 * +--------+---+----+        +--------+---+----+
 *              |                          |
 *     +--------+--------+        +--------+--------+
 *     | String |  ...   |        | String |  ...   |
 *     +--------+--------+        +--------+--------+
 */

struct ape_State {
  ape_Alloc alloc;
  void *ud;

  ape_Handlers handlers;

  unsigned int symid;
  int next_char;

  ape_Chunk *chunks;
  int chunks_count;

  int gcstack_idx;
  ape_Object *gcstack[GCSTACKSIZE];

  ape_Object *calllist;
  ape_Object *freelist;
  ape_Object *symlist;
  ape_Object *t;
  ape_Object *env;

  ape_Object *primsyms[P_MAX];
  ape_Object *typesyms[APE_TMAX];
};

#define unused(x) ((void)x)

#define car(x) ((x)->car.o)
#define cdr(x) ((x)->cdr.o)

/*                Tag
 * +---+---+---+---+---+---+---+---+
 * |      Type     | ? |FC |GC | 1 |
 * +---+---+---+---+---+---+---+---+
 * 8   7   6   5   4   3   2   1   0
 */

#define tag(x) ((x)->car.c)
#define type(x) (tag(x) & 0x1 ? tag(x) >> 4 : APE_TPAIR)
#define settype(x, t) (tag(x) = (t) << 4 | 1)

#define isnil(x) ((x) == &nil)
#define hash(x) ((x)->car.nx)
#define ptrtype(x) ((x)->car.nx)
#define veclen(x) ((x)->car.nx)

/* TODO: Decimal(Base-10 Number): value = coefficient * 10^exponent
 *
 *                                64Bit Platform
 * +-------------------------------------------------------+-------------------+
 * |             tag                16bit        40bit     |       64bit       |
 * | +--------+---+---+---+---+ +----------+---------------+-----------------+ |
 * | | Number | ? |0/1| 0 | 1 | | exponent |  coefficient            d       | |
 * | +--------+---+---+---+---+ +----------+---------------+-----------------+ |
 * |              sign                                     |                   |
 * +-------------------------------------------------------+-------------------+
 *
 *                                32Bit Platform
 * +-------------------------------------------------------+-------------------+
 * |             tag                8bit         16bit     |       32bit       |
 * | +--------+---+---+---+---+ +----------+---------------+-----------------+ |
 * | | Number | ? |0/1| 0 | 1 | | exponent |  coefficient            d       | |
 * | +--------+---+---+---+---+ +----------+---------------+-----------------+ |
 * |              sign                                     |                   |
 * +-------------------------------------------------------+-------------------+
 */

#define number(x) ((x)->cdr.n)
#define prim(x) ((x)->cdr.c)
#define cfunc(x) ((x)->cdr.f)

/* String: Hello, World.
 *                             car                                       cdr
 * +-----------------------------------------------------------------+---------+
 * |             tag                         strbuf                  |         |
 * | +--------+---+---+---+---+ +----+----+----+----+----+----+----+ |         |
 * | | String | ? | 1 | 0 | 1 | | \H | \e | \l | \l | \o | \, | \  | |    +    |
 * | +--------+---+---+---+---+ +----+----+----+----+----+----+----+ |    |    |
 * |              c               s0   s1   s2   s3   s4   s5   s6   |    |    |
 * +-----------------------------------------------------------------+----+----+
 *                                                                        |
 *                              +-----------------------------------------+
 *                              |
 * +----------------------------+------------------------------------+---------+
 * |             tag                         strbuf                  |         |
 * | +--------+---+---+---+---+ +----+----+----+----+----+----+----+ |         |
 * | | String | ? | 0 | 0 | 1 | | \W | \o | \r | \l | \d | \. |  6 | |   nil   |
 * | +--------+---+---+---+---+ +----+----+----+----+----+----+----+ |         |
 * |              c               s0   s1   s2   s3   s4   s5   s6   |         |
 * +-----------------------------------------------------------------+---------+
 *
 * On 32-bit platforms, strbuf is 3 bytes.
 */

#define strbuf(x) ((x)->car.s)
#define stridx(x) (strbuf(x)[STRBUFINDEX])
#define strcnt(x) (tag(x) & FCMARKBIT ? STRBUFSIZE : stridx(x))

static ape_Object nil = {{(ape_Object *)(APE_TNIL << 4 | 1)}, {NULL}};

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

  memset(chunk, 0, sizeof(ape_Chunk));

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

static double collect_garbage(ape_State *A) {
  ape_Chunk *chunk;
  int i, marked_count = 0;

  if (A->chunks_count == 0)
    return 1.0;

  /* mark */
  for (i = 0; i < A->gcstack_idx; i++)
    ape_mark(A, A->gcstack[i]);

  ape_mark(A, A->symlist);
  ape_mark(A, A->env);

  /* sweep and unmark */
  for (chunk = A->chunks; chunk != NULL; chunk = chunk->next) {
    for (i = 0; i < CHUNKSIZE; ++i) {
      ape_Object *obj = &chunk->objects[i];

      if (type(obj) == APE_TFREE)
        continue;

      /* marked */
      if (tag(obj) & GCMARKBIT) {
        tag(obj) &= ~GCMARKBIT;
        marked_count += 1;
        continue;
      }

      if (type(obj) == APE_TPTR && A->handlers.gc)
        A->handlers.gc(A, obj);

      settype(obj, APE_TFREE);
      cdr(obj) = A->freelist;
      A->freelist = obj;
    }
  }

  return (double)marked_count / ((double)A->chunks_count * CHUNKSIZE);
}

static ape_Object *alloc(ape_State *A) {
  ape_Object *obj;

  /* do gc if freelist has no more objects */
  if (isnil(A->freelist)) {
    double usage = collect_garbage(A);

    /* Expand the capacity when the usage rate reaches 75% */
    if (usage > 0.75)
      if (extend_chunks(A) < 0)
        if (isnil(A->freelist))
          ape_error(A, "out of memory");
  }

  /* get object from freelist and push to the gcstack */
  obj = A->freelist;
  A->freelist = cdr(obj);
  ape_pushgc(A, obj);

  memset(obj, 0, sizeof(ape_Object));
  return obj;
}

extern void stdlib_open(ape_State *A);

static ape_State *ape_init(ape_State *A) {
  int i, top = ape_savegc(A);

  /* init lists */
  A->calllist = &nil;
  A->freelist = &nil;
  A->symlist = &nil;
  A->env = &nil;

  /* init symbol id */
  A->symid = (unsigned int)((uintptr_t)A >> 16); /* random seed */
  A->symid = ((A->symid * 214013L + 2531011L) >> 16) & 0x01ff;

  /* global environment */
  A->env = ape_cons(A, &nil, &nil);

  /* init objects */
  A->t = ape_symbol(A, "true");
  ape_def(A, A->t, A->t, NULL);

  /* init primitive symbols */
  for (i = 0; i < P_MAX; ++i)
    A->primsyms[i] = ape_symbol(A, primnames[i]);

  /* init type name symbols */
  for (i = 0; i < APE_TMAX; ++i)
    A->typesyms[i] = ape_symbol(A, typenames[i]);

  /* register built in primitives */
  for (i = 0; i < P_MAX; ++i) {
    ape_Object *v = alloc(A);

    settype(v, APE_TPRIM);
    prim(v) = i;

    ape_def(A, A->primsyms[i], v, NULL);
    ape_restoregc(A, top);
  }

  stdlib_open(A);
  return A;
}

ape_State *ape_newstate(ape_Alloc f, void *ud) {
  ape_Alloc alloc = f ? f : alloc_emul;
  ape_State *A = (ape_State *)alloc(ud, NULL, sizeof(ape_State));

  if (!A)
    return NULL;

  memset(A, 0, sizeof(ape_State));

  /* init allocator */
  A->alloc = alloc;
  A->ud = ud;

  return ape_init(A);
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

static void raise_error(ape_State *A, const char *errmsg) {
  ape_Object *cl = A->calllist;

  /* reset context state */
  A->calllist = &nil;

  /* do error handler */
  if (A->handlers.error)
    A->handlers.error(A, errmsg, cl);

  /* error handler returned -- print error and traceback, exit */
  fprintf(stderr, "error: %s\n", errmsg);

  for (; !isnil(cl); cl = cdr(cl)) {
    char buf[128];
    ape_tostring(A, car(cl), buf, sizeof(buf));
    fprintf(stderr, "=> %s\n", buf);
  }

  exit(EXIT_FAILURE);
}

int ape_error(ape_State *A, const char *format, ...) {
  char errmsg[64] = {0};
  va_list args;

  va_start(args, format);

  vsnprintf(errmsg, sizeof(errmsg), format, args);
  raise_error(A, errmsg);

  va_end(args);
  return -1;
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

LOOP:
  if (!obj)
    return;

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
  case APE_TVECTOR:
    obj = cdr(obj);
    goto LOOP;

  case APE_TPTR:
    if (A->handlers.mark)
      A->handlers.mark(A, obj);
    break;
  }
}

int ape_length(ape_State *A, ape_Object *obj) {
  int len;

  if (type(obj) == APE_TPAIR) {
    for (len = 0; !isnil(obj); obj = cdr(obj))
      len += 1;
    return len;
  }

  if (type(obj) == APE_TSTRING) {
    for (len = 0; !isnil(obj); obj = cdr(obj))
      len += strcnt(obj);
    return len;
  }

  if (type(obj) == APE_TVECTOR)
    return (int)veclen(obj);

  return ape_error(A, "not an iteratable object");
}

int ape_isnil(ape_State *A, ape_Object *obj) {
  unused(A);
  return isnil(obj);
}

int ape_type(ape_State *A, ape_Object *obj) {
  unused(A);
  return type(obj);
}

int ape_equal(ape_State *A, ape_Object *a, ape_Object *b) {
  unused(A);

  if (a == b)
    return 1;

  if (type(a) != type(b))
    return 0;

  if (type(a) == APE_TNUMBER)
    return fabs(number(a) - number(b)) < FP_EPSILON;

  if (type(a) == APE_TSTRING) {
    for (; !isnil(a); a = cdr(a), b = cdr(b))
      if (car(a) != car(b))
        return 0;

    return a == b;
  }

  return 0;
}

ape_Object *ape_checktype(ape_State *A, ape_Object *obj, int type) {
  if (type(obj) != type)
    ape_error(A, "expected %s, got %s", typenames[type], typenames[type(obj)]);
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
  return car(ape_checktype(A, obj, APE_TPAIR));
}

ape_Object *ape_cdr(ape_State *A, ape_Object *obj) {
  if (isnil(obj))
    return obj;
  return cdr(ape_checktype(A, obj, APE_TPAIR));
}

ape_Object *ape_setcar(ape_State *A, ape_Object *obj, ape_Object *car) {
  car(ape_checktype(A, obj, APE_TPAIR)) = car;
  return obj;
}

ape_Object *ape_setcdr(ape_State *A, ape_Object *obj, ape_Object *cdr) {
  cdr(ape_checktype(A, obj, APE_TPAIR)) = cdr;
  return obj;
}

ape_Object *ape_list(ape_State *A, ape_Object **objs, int cnt) {
  ape_Object *list = &nil;

  while (cnt--)
    list = ape_cons(A, objs[cnt], list);

  return list;
}

ape_Object *ape_true(ape_State *A) { return A->t; }

ape_Object *ape_nil(ape_State *A) {
  unused(A);
  return &nil;
}

ape_Object *ape_bool(ape_State *A, int b) {
  return b ? ape_true(A) : ape_nil(A);
}

ape_Object *ape_integer(ape_State *A, long long n) {
  return ape_number(A, (floatptr_t)n);
}

ape_Object *ape_number(ape_State *A, double n) {
  ape_Object *obj = alloc(A);
  settype(obj, APE_TNUMBER);
  number(obj) = (floatptr_t)n;
  return obj;
}

static ape_Object *build_string(ape_State *A, ape_Object *tail, int ch) {
  int index;

  if (!tail || (tag(tail) & FCMARKBIT)) {
    ape_Object *obj = ape_cons(A, NULL, &nil);
    settype(obj, APE_TSTRING);

    if (!tail)
      return obj;

    cdr(tail) = obj;
    A->gcstack_idx--;

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
  return ape_lstring(A, str, (int)strlen(str));
}

ape_Object *ape_lstring(ape_State *A, const char *str, int len) {
  ape_Object *obj = build_string(A, NULL, 0);
  ape_Object *tail = obj;
  int i;

  for (i = 0; i < len; ++i)
    tail = build_string(A, tail, str[i]);

  return obj;
}

ape_Object *ape_concat(ape_State *A, ape_Object *objs) {
  ape_Object *obj = build_string(A, NULL, 0);
  ape_Object *tail = obj;
  ape_Object *str;
  int i, size;

  while (!isnil(objs)) {
    str = ape_checktype(A, ape_nextarg(A, &objs), APE_TSTRING);

    while (!isnil(str)) {
      size = strcnt(str);

      for (i = 0; i < size; ++i)
        tail = build_string(A, tail, strbuf(str)[i]);

      str = cdr(str);
    }
  }

  return obj;
}

static int strleq(ape_Object *obj, const char *str, int len) {
  int i, j = 0;

  while (!isnil(obj) && j < len) {
    int size = strcnt(obj);

    for (i = 0; i < size && j < len; ++i)
      if (strbuf(obj)[i] != str[j++])
        return 0;

    if (i != size)
      return 0;

    obj = cdr(obj);
  }

  return isnil(obj) && j == len;
}

static int streq(ape_Object *obj, const char *str) {
  return strleq(obj, str, (int)strlen(str));
}

static ape_Object *symbol(ape_State *A, limb_t h, const char *name, int len,
                          int pushlist) {
  ape_Object *obj = alloc(A);

  settype(obj, APE_TSYMBOL);
  hash(obj) = h;
  cdr(obj) = ape_lstring(A, name, len);

  if (pushlist)
    A->symlist = ape_cons(A, obj, A->symlist);

  return obj;
}

static limb_t fast_hash(const char *str, int len) {
  limb_t h = 5381;
  int i;

  for (i = 0; i < len; ++i)
    h = ((h << 5) + h + str[i]) & HASHMASK;

  return h;
}

ape_Object *ape_symbol(ape_State *A, const char *name) {
  ape_Object *obj;
  int len = (int)strlen(name);
  limb_t h = fast_hash(name, len);

  /* try to find in symlist */
  for (obj = A->symlist; !isnil(obj); obj = cdr(obj))
    if (hash(car(obj)) == h && strleq(cdr(car(obj)), name, len))
      return car(obj);

  /* create new symbol, push to symlist and return */
  return symbol(A, h, name, len, 1);
}

/*              Vector(7)
 * +---------------------+----------+
 * | +--------+--------+ |          |
 * | | Vector | length | |          |
 * | +--------+--------+ |          |
 * +---------------------+----+-----+
 *                            |
 *                      +-----+-----+
 *                   +--+     |     +--+
 *                  /   +-----+-----+   \
 *                 /                     \
 *                /                       \
 *               /                         \
 *        +-----+-----+               +-----+-----+
 *        |     |     |               |     |     |
 *        +--+--+--+--+               +--+--+--+--+
 *          /       \                   /       \
 *         /         \                 /         \
 *        /           \               /           \
 * +-----+-----+ +-----+-----+ +-----+-----+ +-----+-----+
 * |  0  |  1  | |  2  |  3  | |  4  |  5  | |  6  | nil |
 * +-----+-----+ +-----+-----+ +-----+-----+ +-----+-----+
 */

static int next_power(int size) {
  if (size < 2)
    return 2;

  /* fast check if power of two */
  if (0 == (size & (size - 1)))
    return size;

  size -= 1;
  size |= size >> 1;
  size |= size >> 2;
  size |= size >> 4;
  size |= size >> 8;
  size |= size >> 16;
  size += 1;

  return size;
}

ape_Object *ape_vector(ape_State *A, int len) {
  ape_Object *obj;

  if (len < 1)
    ape_error(A, "vector length must greater than zero");

  if (len > MAXVECSIZE)
    ape_error(A, "vector too long");

  obj = alloc(A);
  settype(obj, APE_TVECTOR);
  veclen(obj) = len;
  cdr(obj) = &nil;

  return obj;
}

static ape_Object **vector_place(ape_State *A, ape_Object **vec, int dim,
                                 int pos, int build) {
  while (1) {
    if (isnil(*vec)) {
      if (build) {
        int gctop = ape_savegc(A);
        *vec = ape_cons(A, &nil, &nil);
        ape_restoregc(A, gctop);
      } else
        return NULL;
    }

    dim = dim >> 1;

    if (dim < 2)
      return pos & dim ? &cdr(*vec) : &car(*vec);

    if (pos < dim)
      vec = &car(*vec);
    else {
      pos -= dim;
      vec = &cdr(*vec);
    }
  }
  return NULL;
}

ape_Object *ape_vecset(ape_State *A, ape_Object *vec, int pos,
                       ape_Object *obj) {
  int len = (int)veclen(ape_checktype(A, vec, APE_TVECTOR));
  ape_Object **place;

  if (pos >= len)
    ape_error(A, "vector out of range");

  place = vector_place(A, &cdr(vec), next_power(len), pos, 1);

  if (!place)
    ape_error(A, "vector place not found");

  *place = obj;
  return vec;
}

ape_Object *ape_cfunc(ape_State *A, ape_CFunc fn) {
  ape_Object *obj = alloc(A);
  settype(obj, APE_TCFUNC);
  cfunc(obj) = fn;
  return obj;
}

ape_Object *ape_ptr(ape_State *A, void *ptr, int subtype) {
  ape_Object *obj = alloc(A);
  settype(obj, APE_TPTR);
  cdr(obj) = (ape_Object *)ptr;
  ptrtype(obj) = subtype & 0xFFFF;
  return obj;
}

ape_Object *ape_gensym(ape_State *A) {
  char gensym[16] = {0};
  int len = sprintf(gensym, "#:%u", A->symid++);

  /* create new symbol, without push to symlist and return */
  return symbol(A, fast_hash(gensym, len), gensym, len, 0);
}

ape_Object *ape_reverse(ape_State *A, ape_Object *obj) {
  ape_Object *res = &nil;

  for (obj = ape_checktype(A, obj, APE_TPAIR); !isnil(obj); obj = cdr(obj))
    res = ape_cons(A, car(ape_checktype(A, obj, APE_TPAIR)), res);

  /* TODO: string reverse */

  return res;
}

ape_Object *ape_nth(ape_State *A, ape_Object *obj, int idx) {
  int cnt;

  switch (type(obj)) {
  case APE_TPAIR:
    while (idx-- > 0) {
      obj = cdr(obj);

      if (isnil(obj))
        ape_error(A, "index out of range");
    }
    return car(obj);

  case APE_TSTRING:
    while (idx > 0) {
      cnt = strcnt(obj);

      if (cnt == 0 || idx < cnt)
        break;

      obj = cdr(obj);
      idx -= cnt;

      if (isnil(obj))
        ape_error(A, "index out of range");
    }

    if (cnt == 0)
      ape_error(A, "index out of range");

    return ape_lstring(A, strbuf(obj) + idx, 1);

  case APE_TVECTOR:
    cnt = (int)veclen(ape_checktype(A, obj, APE_TVECTOR));
    ape_Object **place;

    if (idx >= cnt)
      ape_error(A, "index out of range");

    place = vector_place(A, &cdr(obj), next_power(cnt), idx, 0);
    return place ? *place : &nil;

  default:
    ape_error(A, "not an iteratable object");
    break;
  }

  return &nil;
}

long long ape_tointeger(ape_State *A, ape_Object *obj) {
  return (long long)number(ape_checktype(A, obj, APE_TNUMBER));
}

double ape_tonumber(ape_State *A, ape_Object *obj) {
  return number(ape_checktype(A, obj, APE_TNUMBER));
}

typedef struct {
  char *p;
  int n;
} CharPtrInt;

static void writebuf(ape_State *A, void *udata, char ch) {
  CharPtrInt *x = (CharPtrInt *)udata;

  unused(A);

  if (x->n) {
    *x->p++ = ch;
    x->n--;
  }
}

int ape_tostring(ape_State *A, ape_Object *obj, char *dst, int size) {
  CharPtrInt x;

  x.p = dst;
  x.n = size - 1;

  ape_write(A, obj, writebuf, &x, 0);
  *x.p = '\0';

  return size - x.n - 1;
}

int ape_ptrtype(ape_State *A, ape_Object *obj) {
  return (int)ptrtype(ape_checktype(A, obj, APE_TPTR));
}

void *ape_toptr(ape_State *A, ape_Object *obj) {
  return cdr(ape_checktype(A, obj, APE_TPTR));
}

static ape_Object rparen = {0};

static ape_Object *reader(ape_State *A, ape_ReadFunc fn, void *udata) {
  static const char *delimiter = "();";
  ape_Object *v, *res, **tail;
  floatptr_t n;
  int ch, top;
  char buf[APE_SYMSIZE] = {0}, *p;

  /* get next character */
  ch = A->next_char ? A->next_char : fn(A, udata);
  A->next_char = 0;

  /* skip whitespace */
  while (ch && isspace(ch))
    ch = fn(A, udata);

  switch (ch) {
  case '\0':
    return NULL;

  case ';':
    do {
      ch = fn(A, udata);
    } while (ch && ch != '\n');

    return reader(A, fn, udata);

  case ')':
    return &rparen;

  case '(':
    res = &nil;
    tail = &res;
    top = ape_savegc(A);
    ape_pushgc(A, res); /* to cause error on too-deep nesting */

    while ((v = reader(A, fn, udata)) != &rparen) {
      if (v == NULL)
        ape_error(A, "unclosed list");

      if (type(v) == APE_TSYMBOL && strleq(cdr(v), ".", 1))
        /* dotted pair */
        *tail = ape_read(A, fn, udata);
      else {
        /* proper pair */
        *tail = ape_cons(A, v, &nil);
        tail = &cdr(*tail);
      }
      ape_restoregc(A, top);
      ape_pushgc(A, res);
    }
    return res;

  case '\'':
    v = ape_read(A, fn, udata);

    if (!v)
      ape_error(A, "stray '''");

    /* Transform: '(...) => (quote (...)) */
    return ape_cons(A, A->primsyms[P_QUOTE], ape_cons(A, v, &nil));

  case '#':
    v = ape_read(A, fn, udata);

    if (!v)
      ape_error(A, "stray '#'");

    /* Transform: #(...) => (vector ...) */
    return ape_cons(A, A->primsyms[P_VECTOR], ape_checktype(A, v, APE_TPAIR));

  case '`':
    v = ape_read(A, fn, udata);

    if (!v)
      ape_error(A, "stray '`'");

    /* Transform: `(...) => (quasiquote (...)) */
    return ape_cons(A, A->primsyms[P_QUASIQUOTE], ape_cons(A, v, &nil));

  case ',':
    ch = fn(A, udata);

    if (ch != '@')
      A->next_char = ch;

    v = ape_read(A, fn, udata);

    if (!v)
      ape_error(A, "stray ','");

    res = ape_cons(A, v, &nil);

    /* Transform: ,@v => (unquote-splicing (v)) */
    if (ch == '@')
      return ape_cons(A, A->primsyms[P_UNQUOTE_SPLICING], res);

    /* Transform: ,v => (unquote (v)) */
    return ape_cons(A, A->primsyms[P_UNQUOTE], res);

  case '"':
    res = build_string(A, NULL, 0);
    v = res;

    ch = fn(A, udata);
    while (ch != '"') {
      if (ch == '\0')
        ape_error(A, "unclosed string");

      if (ch == '\\') {
        ch = fn(A, udata);

        if (ch == '\0')
          ape_error(A, "unclosed string");

        if (strchr("nrt\\\"", ch))
          ch = strchr("n\nr\rt\t\\\\\"\"", ch)[1];
      }

      v = build_string(A, v, ch);
      ch = fn(A, udata);
    }
    return res;

  default:
    p = buf;
    do {
      if (p == buf + sizeof(buf) - 1)
        ape_error(A, "symbol too long");

      *p++ = ch;
      ch = fn(A, udata);
    } while (ch && !strchr(delimiter, ch) && !isspace(ch));

    A->next_char = ch;

    /* try to read as number */
    n = (floatptr_t)strtod(buf, &p);

    if (p != buf && (strchr(delimiter, *p) || isspace(*p)))
      return ape_number(A, n);

    if (!strcmp(buf, "nil"))
      return &nil;

    return ape_symbol(A, buf);
  }

  return NULL;
}

ape_Object *ape_read(ape_State *A, ape_ReadFunc fn, void *udata) {
  ape_Object *obj = reader(A, fn, udata);

  if (obj == &rparen)
    ape_error(A, "stray ')'");

  return obj;
}

static void writestr(ape_State *A, ape_WriteFunc fn, void *udata,
                     const char *str) {
  while (*str)
    fn(A, udata, *str++);
}

void ape_write(ape_State *A, ape_Object *obj, ape_WriteFunc fn, void *udata,
               int strqt) {
  char buf[APE_SYMSIZE];
  int i, len;

  switch (type(obj)) {
  case APE_TNIL:
    writestr(A, fn, udata, "nil");
    break;

  case APE_TNUMBER:
#if INTPTR_MAX >= INT64_MAX
    sprintf(buf, "%.14g", number(obj));
#else
    sprintf(buf, "%.7g", number(obj));
#endif
    writestr(A, fn, udata, buf);
    break;

  case APE_TSYMBOL:
    ape_write(A, cdr(obj), fn, udata, 0);
    break;

  case APE_TPAIR:
    fn(A, udata, '(');

    for (;;) {
      ape_write(A, car(obj), fn, udata, 1);
      obj = cdr(obj);

      if (type(obj) != APE_TPAIR)
        break;

      fn(A, udata, ' ');
    }

    if (!isnil(obj)) {
      writestr(A, fn, udata, " . ");
      ape_write(A, obj, fn, udata, 1);
    }

    fn(A, udata, ')');
    break;

  case APE_TSTRING:
    if (strqt)
      fn(A, udata, '"');

    while (!isnil(obj)) {
      int i, len = strcnt(obj);

      for (i = 0; i < len; ++i) {
        if (strqt && strbuf(obj)[i] == '"')
          fn(A, udata, '\\');

        fn(A, udata, strbuf(obj)[i]);
      }

      obj = cdr(obj);
    }

    if (strqt)
      fn(A, udata, '"');
    break;

  case APE_TVECTOR:
    writestr(A, fn, udata, "#(");

    len = (int)veclen(obj);

    for (i = 0; i < len; ++i) {
      ape_write(A, ape_nth(A, obj, i), fn, udata, 1);

      if (i < (len - 1))
        fn(A, udata, ' ');

      if (len > 20 && i == 9 && i < len - 10) {
        writestr(A, fn, udata, "... ");
        i = len - 11;
      }
    }

    fn(A, udata, ')');
    break;

  default:
    sprintf(buf, "[%s %p]", typenames[type(obj)], (void *)obj);
    writestr(A, fn, udata, buf);
    break;
  }
}

/*                            Environment
 *           +--------+--------+     +--------+--------+
 *           | frames |   +----+-----+ frames |   nil  |
 *           +---+----+--------+     +--------+--------+
 *               |
 *      +--------+--------+     +--------+--------+
 *      | frame1 |   +----+-----+ frame2 |   nil  |
 *      +---+----+--------+     +---+----+--------+
 *          |                       |
 * +--------+--------+     +--------+--------+
 * | Symbol | Object |     | Symbol | Object |
 * +--------+--------+     +--------+--------+
 */

static ape_Object *getbound(ape_Object *sym, ape_Object *env, int recur) {
  ape_Object *frame, *x;

  /* Try to find in environment */
  for (; !isnil(env); env = cdr(env)) {
    frame = car(env);

    for (; !isnil(frame); frame = cdr(frame)) {
      x = car(frame);

      if (car(x) == sym)
        return x;
    }

    if (!recur)
      return &nil;
  }
  return &nil;
}

ape_Object *ape_unbound(ape_State *A, ape_Object *sym, ape_Object *env,
                        int recur) {
  ape_Object *frame, *x, *p = &nil;

  env = env ? env : A->env;
  sym = ape_checktype(A, sym, APE_TSYMBOL);

  /* Try to find in environment */
  for (; !isnil(env); env = cdr(env)) {
    frame = car(env);

    for (; !isnil(frame); p = frame, frame = cdr(frame)) {
      x = car(frame);

      if (car(x) == sym) {
        if (isnil(p))
          car(env) = cdr(frame);
        else
          cdr(p) = cdr(frame);

        return cdr(x);
      }
    }

    if (!recur)
      return &nil;
  }
  return &nil;
}

ape_Object *ape_def(ape_State *A, ape_Object *sym, ape_Object *val,
                    ape_Object *env) {
  ape_Object *var;

  env = env ? env : A->env;
  var = getbound(sym, env, 0);

  if (!isnil(var))
    ape_error(A, "variables cannot be redefined");

  var = ape_cons(A, sym, val);
  car(env) = ape_cons(A, var, car(env));
  return val;
}

ape_Object *ape_set(ape_State *A, ape_Object *sym, ape_Object *val,
                    ape_Object *env) {
  ape_Object *var = getbound(sym, env ? env : A->env, 1);

  if (isnil(var))
    ape_error(A, "unbound variables cannot be set");

  cdr(var) = val;
  return val;
}

ape_Object *ape_nextarg(ape_State *A, ape_Object **args) {
  ape_Object *arg = *args;

  if (type(arg) != APE_TPAIR) {
    if (isnil(arg))
      ape_error(A, "too few arguments");

    ape_error(A, "dotted pair in argument list");
  }

  *args = cdr(arg);
  return car(arg);
}

#define evalarg() ape_eval(A, ape_nextarg(A, &args), env)

static ape_Object *eval_list(ape_State *A, ape_Object *list, ape_Object *env,
                             int *argc) {
  ape_Object *res = &nil;
  ape_Object **tail = &res;
  int cnt = 0;

  while (!isnil(list)) {
    *tail = ape_cons(A, ape_eval(A, ape_nextarg(A, &list), env), &nil);
    tail = &cdr(*tail);
    cnt += 1;
  }

  if (argc)
    *argc = cnt;

  return res;
}

static ape_Object *arith_add(ape_State *A, ape_Object *args, ape_Object *env) {
  floatptr_t res = 0;

  while (!isnil(args))
    res += number(ape_checktype(A, evalarg(), APE_TNUMBER));

  return ape_number(A, res);
}

static ape_Object *arith_sub(ape_State *A, ape_Object *args, ape_Object *env) {
  floatptr_t res;
  ape_Object *x;

  if (isnil(args))
    ape_error(A, "wrong number of operands");

  x = ape_checktype(A, evalarg(), APE_TNUMBER);

  if (isnil(args))
    return ape_number(A, -number(x));

  res = number(x);

  while (!isnil(args))
    res -= number(ape_checktype(A, evalarg(), APE_TNUMBER));

  return ape_number(A, res);
}

static ape_Object *arith_mul(ape_State *A, ape_Object *args, ape_Object *env) {
  floatptr_t res = 1;

  while (!isnil(args))
    res *= number(ape_checktype(A, evalarg(), APE_TNUMBER));

  return ape_number(A, res);
}

static ape_Object *arith_div(ape_State *A, ape_Object *args, ape_Object *env) {
  floatptr_t res;
  ape_Object *x;

  if (isnil(args))
    ape_error(A, "wrong number of operands");

  x = ape_checktype(A, evalarg(), APE_TNUMBER);

  if (isnil(args))
    res = (floatptr_t)1;
  else {
    res = number(x);
    x = ape_checktype(A, evalarg(), APE_TNUMBER);
  }

  do {
    if (fabs(number(x) - (floatptr_t)0) < FP_EPSILON)
      ape_error(A, "division by zero");

    res /= number(x);

    if (isnil(args))
      break;

    x = ape_checktype(A, evalarg(), APE_TNUMBER);
  } while (1);

  return ape_number(A, res);
}

#define arith_compare(A, args, env, op)                                        \
  do {                                                                         \
    res = NULL;                                                                \
    if (isnil(args))                                                           \
      res = A->t;                                                              \
    else {                                                                     \
      va = ape_checktype(A, evalarg(), APE_TNUMBER);                           \
      if (isnil(args))                                                         \
        res = A->t;                                                            \
      else {                                                                   \
        while (!isnil(args)) {                                                 \
          vb = ape_checktype(A, evalarg(), APE_TNUMBER);                       \
          if (!(number(va) op number(vb))) {                                   \
            res = &nil;                                                        \
            break;                                                             \
          }                                                                    \
        }                                                                      \
        if (!res)                                                              \
          res = A->t;                                                          \
      }                                                                        \
    }                                                                          \
  } while (0)

static void args_binds(ape_State *A, ape_Object *syms, ape_Object *args,
                       ape_Object *env) {
  ape_Object *bind;

  while (!isnil(syms)) {
    /* rest arguments */
    if (type(syms) != APE_TPAIR) {
      ape_def(A, syms, args, env);
      return;
    }

    bind = car(syms);

    if (isnil(args)) {
      ape_Object *arg;

      if (type(bind) != APE_TPAIR)
        ape_error(A, "wrong number of arguments");

      arg = cdr(bind);
      /* default argument */
      ape_def(A, car(bind), ape_eval(A, car(arg), env), env);
    } else {
      if (type(bind) == APE_TPAIR)
        bind = car(bind);

      /* bind argument */
      ape_def(A, bind, car(args), env);
    }

    syms = cdr(syms);
    args = cdr(args);
  }
}

static ape_Object *quasiquote(ape_State *A, ape_Object *expr, ape_Object *env) {
  ape_Object *res = &nil;
  ape_Object **tail = &res;

  if (type(expr) != APE_TPAIR)
    return expr;

  while (!isnil(expr)) {
    ape_Object *obj = ape_nextarg(A, &expr);

    if (type(obj) == APE_TPAIR) {
      ape_Object *fn = car(obj);
      ape_Object *args = cdr(obj);

      if (fn == A->primsyms[P_UNQUOTE_SPLICING]) {
        ape_Object *arg = ape_nextarg(A, &args);

        arg = quasiquote(A, ape_cons(A, arg, &nil), env);
        arg = car(ape_checktype(A, arg, APE_TPAIR));

        obj = ape_checktype(A, ape_eval(A, arg, env), APE_TPAIR);

        for (; !isnil(obj); obj = cdr(obj)) {
          /* (x . y) => (x y) */
          if (type(obj) != APE_TPAIR) {
            *tail = ape_cons(A, obj, &nil);
            tail = &cdr(*tail);
            break;
          }

          /* copy list */
          *tail = ape_cons(A, car(obj), &nil);
          tail = &cdr(*tail);
        }

        continue;
      } else if (fn == A->primsyms[P_UNQUOTE]) {
        ape_Object *arg = ape_nextarg(A, &args);

        if (type(arg) == APE_TPAIR) {
          arg = quasiquote(A, ape_cons(A, arg, &nil), env);
          arg = car(ape_checktype(A, arg, APE_TPAIR));
        }

        obj = ape_eval(A, arg, env);
      } else
        obj = quasiquote(A, obj, env);
    }

    *tail = ape_cons(A, obj, &nil);
    tail = &cdr(*tail);
  }

  return res;
}

/*      Function / Macro
 * +------------------+-----+
 * | Function / Macro | cdr |
 * +------------------+--+--+
 *                       |
 *                 +-----+---------+
 *                 | car | do list |
 *                 +--+--+---------+
 *                    |
 *              +-----+------+
 *              | env | args |
 *              +-----+------+
 */

static ape_Object *expand(ape_State *A, ape_Object *macro, ape_Object *args) {
  ape_Object *body, *head, *env;

  body = cdr(macro); /* ((env . args) . (do ...)) */
  head = car(body);  /* (env . args) */

  /* arguments environment */
  env = ape_cons(A, &nil, car(head));
  args_binds(A, cdr(head), args, env);

  /* generate code by macro */
  return ape_eval(A, cdr(body), env);
}

ape_Object *ape_eval(ape_State *A, ape_Object *expr, ape_Object *env) {
  ape_Object *fn, *args;
  ape_Object cl;
  ape_Object *res, *va, *vb; /* registers */
  int gctop, argc, i;

  env = env ? env : A->env;

EVAL:
  if (type(expr) == APE_TSYMBOL) {
    ape_Object *var = getbound(expr, env, 1);

    if (isnil(var))
      ape_error(A, "unbound variables");

    /* Prevent local variables from being accidentally GC */
    ape_pushgc(A, var);
    return cdr(var);
  }

  if (type(expr) != APE_TPAIR)
    return expr;

  car(&cl) = expr;
  cdr(&cl) = A->calllist;
  A->calllist = &cl;

  gctop = ape_savegc(A);

  /* avoid accidental GC */
  ape_pushgc(A, expr);
  ape_pushgc(A, env);

  fn = ape_eval(A, car(expr), env);
  args = cdr(expr);
  res = &nil;

  switch (type(fn)) {
  case APE_TPRIM:
    switch (prim(fn)) {
    case P_DEF:
      va = ape_checktype(A, ape_nextarg(A, &args), APE_TSYMBOL);
      res = ape_def(A, va, evalarg(), env);
      break;
    case P_SET:
      va = ape_checktype(A, ape_nextarg(A, &args), APE_TSYMBOL);
      res = ape_set(A, va, evalarg(), env);
      break;
    case P_IF:
      va = evalarg();
      vb = ape_nextarg(A, &args);

      expr = !isnil(va) ? vb : ape_nextarg(A, &args);

      ape_restoregc(A, gctop);
      A->calllist = cdr(&cl);
      goto EVAL;
    case P_FN:
    case P_MACRO:
      va = ape_cons(A, env, ape_nextarg(A, &args));
      vb = ape_cons(A, A->primsyms[P_DO], args);
      res = alloc(A);
      settype(res, prim(fn) == P_FN ? APE_TFUNC : APE_TMACRO);
      cdr(res) = ape_cons(A, va, vb);
      break;
    case P_EXPAND:
      va = evalarg();
      vb = ape_checktype(A, ape_eval(A, car(va), env), APE_TMACRO);
      res = expand(A, vb, cdr(va));
      break;
    case P_QUOTE:
      res = ape_nextarg(A, &args);
      break;
    case P_QUASIQUOTE:
      res = quasiquote(A, ape_nextarg(A, &args), env);
      break;
    case P_UNQUOTE:
      ape_error(A, "unquote outside a quasiquote");
      break;
    case P_UNQUOTE_SPLICING:
      ape_error(A, "unquote-splicing outside a quasiquote");
      break;
    case P_AND:
      while (!isnil(args) && !isnil(res = evalarg()))
        ;
      break;
    case P_OR:
      while (!isnil(args) && isnil(res = evalarg()))
        ;
      break;
    case P_NOT:
      res = ape_bool(A, isnil(evalarg()));
      break;
    case P_DO:
      if (!isnil(args)) {
        for (; !isnil(cdr(args)); args = cdr(args))
          ape_eval(A, car(args), env);

        expr = car(args);

        ape_restoregc(A, gctop);
        A->calllist = cdr(&cl);
        goto EVAL;
      }
      break;
    case P_CONS:
      va = evalarg();
      res = ape_cons(A, va, evalarg());
      break;
    case P_CAR:
      res = ape_car(A, evalarg());
      break;
    case P_CDR:
      res = ape_cdr(A, evalarg());
      break;
    case P_SETCAR:
      va = evalarg();
      res = ape_setcar(A, va, evalarg());
      break;
    case P_SETCDR:
      va = evalarg();
      res = ape_setcdr(A, va, evalarg());
      break;
    case P_TYPE:
      va = evalarg();
      res = A->typesyms[type(va)];
      break;
    case P_VECTOR:
      va = eval_list(A, args, env, &argc);
      vb = ape_vector(A, argc);

      for (i = 0; i < argc; ++i)
        ape_vecset(A, vb, i, ape_nextarg(A, &va));

      res = vb;
      break;
    case P_VECSET:
      va = ape_checktype(A, evalarg(), APE_TVECTOR);
      vb = ape_checktype(A, evalarg(), APE_TNUMBER);
      res = ape_vecset(A, va, (int)number(vb), evalarg());
      break;
    case P_EQ:
      va = evalarg();
      res = ape_bool(A, ape_equal(A, va, evalarg()));
      break;
    case P_LT:
      arith_compare(A, args, env, <);

      if (!isnil(res) && ape_equal(A, va, vb))
        res = &nil;
      break;
    case P_LTE:
      arith_compare(A, args, env, <);

      if (isnil(res) && ape_equal(A, va, vb))
        res = A->t;
      break;
    case P_GT:
      arith_compare(A, args, env, >);

      if (!isnil(res) && ape_equal(A, va, vb))
        res = &nil;
      break;
    case P_GTE:
      arith_compare(A, args, env, >);

      if (isnil(res) && ape_equal(A, va, vb))
        res = A->t;
      break;
    case P_ADD:
      res = arith_add(A, args, env);
      break;
    case P_SUB:
      res = arith_sub(A, args, env);
      break;
    case P_MUL:
      res = arith_mul(A, args, env);
      break;
    case P_DIV:
      res = arith_div(A, args, env);
      break;
    default:
      ape_error(A, "undefined primitive");
      break;
    }
    break;
  case APE_TCFUNC:
    va = eval_list(A, args, env, &argc);
    res = cfunc(fn)(A, argc, va, env);
    break;
  case APE_TFUNC:
    va = cdr(fn); /* ((env . args) . (do ...)) */
    vb = car(va); /* (env . args)*/

    args = eval_list(A, args, env, NULL);

    /* new local environment */
    env = ape_cons(A, &nil, car(vb));
    args_binds(A, cdr(vb), args, env);

    expr = cdr(va); /* do block */

    ape_restoregc(A, gctop);
    A->calllist = cdr(&cl);
    goto EVAL;
  case APE_TMACRO:
    expr = expand(A, fn, args);

    ape_restoregc(A, gctop);
    A->calllist = cdr(&cl);
    goto EVAL;
  default:
    ape_error(A, "tried to call non-callable value");
    break;
  }

  ape_restoregc(A, gctop);
  ape_pushgc(A, res);
  A->calllist = cdr(&cl);

  return res;
}

ape_Object *ape_load(ape_State *A, const char *file, ape_Object *env) {
  ape_Object *expr;
  FILE *fp;
  int gctop;

  fp = fopen(file, "rb");

  if (!fp)
    ape_error(A, "could not open input file");

  env = env ? env : A->env;
  gctop = ape_savegc(A);

  for (;;) {
    expr = ape_readfp(A, fp);

    if (!expr)
      break;

    ape_eval(A, expr, env);
    ape_restoregc(A, gctop);
  }

  fclose(fp);
  return &nil;
}

static char readbuffer(ape_State *A, void *udata) {
  CharPtrInt *x = (CharPtrInt *)udata;
  int ch = '\0';

  unused(A);

  if (x->n) {
    ch = *x->p++;
    x->n--;
  }

  return ch;
}

ape_Object *ape_readstring(ape_State *A, const char *str) {
  CharPtrInt x;

  x.p = (char *)str;
  x.n = (int)strlen(str);

  return ape_read(A, readbuffer, &x);
}

static char readfp(ape_State *A, void *udata) {
  int ch = fgetc((FILE *)udata);
  unused(A);
  return ch == EOF ? '\0' : ch;
}

ape_Object *ape_readfp(ape_State *A, FILE *fp) {
  return ape_read(A, readfp, fp);
}

static void writefp(ape_State *A, void *udata, char ch) {
  unused(A);
  fputc(ch, (FILE *)udata);
}

void ape_writefp(ape_State *A, ape_Object *obj, FILE *fp) {
  ape_write(A, obj, writefp, fp, 0);
}
