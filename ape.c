/*
 * ape.c
 *
 * Copyright (c) 2021-2024 Xiongfei Shi
 *
 * Author: Xiongfei Shi <xiongfei.shi(a)icloud.com>
 * License: Apache-2.0
 *
 * https://github.com/shixiongfei/ape
 */

#include <ctype.h>
#include <float.h>
#include <limits.h>
#include <math.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "ape.h"

/* assumption: pointers are 32 or 64 bit,
   and float/double are IEEE binary32/binary64 */
#if INTPTR_MAX >= INT64_MAX
typedef double number_t;
#define NUM_EPSILON DBL_EPSILON
#else
typedef float number_t;
#define NUM_EPSILON FLT_EPSILON
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
    "pair",   "forward",  "nil",   "number",    "symbol",    "string",
    "vector", "function", "macro", "primitive", "cfunction", "pointer",
};

typedef intptr_t sword_t;
typedef uintptr_t uword_t;

#define STRBUFSIZE ((int)sizeof(ape_Cell *) - 1)
#define STRBUFINDEX (STRBUFSIZE - 1)

#define MARKBITS 4
#define FCMARKBIT (1 << 1) /* Full Chars */
#define HASHMASK ((1 << 16) - 1)
#define PTRTYPEMASK 0xFFFF
#define SEMISIZE (0x1000)

typedef union {
  ape_Cell *o;
  ape_CFunc f;
  number_t n;
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
    uword_t nx : STRBUFSIZE * 8;
    uword_t __ : 8;
#else
    uword_t __ : 8;
    uword_t nx : STRBUFSIZE * 8;
#endif
  };
} Value;

/*               Object
 * +---------------+---------------+
 * |      car      |      cdr      |
 * +---------------+---------------+
 */

struct ape_Cell {
  Value car, cdr;
};

typedef struct {
  uword_t semi_size;   /* semi space size */
  ape_Cell *to, *from; /* semi space differentiation */
  ape_Cell *head;      /* current head of free memory */
} GC;

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

struct ape_Context {
  ape_State state;

  ape_Alloc alloc;
  void *ud;

  GC gc;
  ape_Handlers handlers;

  unsigned int symid;
  int next_char;

  ape_Cell *calllist;
  ape_Cell *symlist;
  ape_Cell *t;
  ape_Cell *env;

  ape_Cell *primsyms[P_MAX];
  ape_Cell *typesyms[APE_TMAX];
};

#define unused(x) ((void)x)

#define car(x) ((x)->car.o)
#define cdr(x) ((x)->cdr.o)

/*                    Tag
 * +----+----+----+----+----+----+----+----+
 * |        Type       | ?  | ?  |mark| 1  |
 * +----+----+----+----+----+----+----+----+
 * 8    7    6    5    4    3    2    1    0
 */

#define tag(x) ((x)->car.c)
#define type(x) (tag(x) & 0x1 ? tag(x) >> MARKBITS : APE_TPAIR)
#define settype(x, t) (tag(x) = (t) << MARKBITS | 1)

#define isnil(x) ((x) == nil)
#define hash(x) ((x)->car.nx)
#define ptrtype(x) ((x)->car.nx)
#define veclen(x) ((x)->car.nx)

#define number(x) ((x)->cdr.n)
#define prim(x) ((x)->cdr.c)
#define cfunc(x) ((x)->cdr.f)

/* String: Hello, World.
 *                                car                                      cdr
 * +---------------------------------------------------------------------+-----+
 * |             tag                               strbuf                |     |
 * | +------------+---+---+---+---+ +----+----+----+----+----+----+----+ |     |
 * | |   String   |...|...| 1 |...| | \H | \e | \l | \l | \o | \, | \  | |  +  |
 * | +------------+---+---+---+---+ +----+----+----+----+----+----+----+ |  |  |
 * |              c                   s0   s1   s2   s3   s4   s5   s6   |  |  |
 * +---------------------------------------------------------------------+--+--+
 *                                                                          |
 *                                  +---------------------------------------+
 *                                  |
 * +--------------------------------+------------------------------------+-----+
 * |             tag                               strbuf                |     |
 * | +------------+---+---+---+---+ +----+----+----+----+----+----+----+ |     |
 * | |   String   |...|...| 0 |...| | \W | \o | \r | \l | \d | \. | 6  | | nil |
 * | +------------+---+---+---+---+ +----+----+----+----+----+----+----+ |     |
 * |              c                   s0   s1   s2   s3   s4   s5   s6   |     |
 * +---------------------------------------------------------------------+-----+
 *
 * On 32-bit platforms, strbuf is 3 bytes.
 */

#define strbuf(x) ((x)->car.s)
#define stridx(x) (strbuf(x)[STRBUFINDEX])
#define strcnt(x) (tag(x) & FCMARKBIT ? STRBUFSIZE : stridx(x))

#define CTX(A) (A->ctx)

static ape_Cell nilc = {{(ape_Cell *)(APE_TNIL << MARKBITS | 1)}, {NULL}};
static ape_Cell *nil = &nilc;

static void *alloc_emul(void *ud, void *ptr, size_t size) {
  unused(ud);

  if (size)
    return realloc(ptr, size);

  free(ptr);
  return NULL;
}

#define ape_realloc(A, p, n) CTX(A)->alloc(CTX(A)->ud, p, n)
#define ape_malloc(A, n) ape_realloc(A, NULL, n)
#define ape_free(A, p) ape_realloc(A, p, 0)

static void *ape_calloc(ape_State *A, size_t n, size_t s) {
  void *p = ape_malloc(A, n * s);

  if (!p)
    return NULL;

  memset(p, 0, n * s);
  return p;
}

static ape_Cell *handle_gc(ape_State *A, ape_Cell *cell) {
  /* GC the previous C pointer */
  if (type(cell) == APE_TPTR && CTX(A)->handlers.gc)
    CTX(A)->handlers.gc(A, cdr(cell), (int)ptrtype(cell));
  return cell;
}

static void copy_ref(ape_State *A, ape_Cell **p) {
  GC *gc = &CTX(A)->gc;
  ape_Cell *cell = *p;
  ape_Cell *dst;

  if (!cell)
    return;

  if (isnil(cell))
    return;

  /* check if already copied */
  if (type(cell) == APE_TFORWARD) {
    /* install forward ref */
    *p = cdr(cell);
    return;
  }

  dst = handle_gc(A, gc->head++);

  /* copy object */
  car(dst) = car(cell);
  cdr(dst) = cdr(cell);

  /* place forward address at old location */
  settype(cell, APE_TFORWARD);
  cdr(cell) = dst;

  *p = dst;
}

static void copy_roots(ape_State *A, ape_Cell **roots) {
  int i;

  if (roots[0])
    copy_roots(A, (ape_Cell **)roots[0]);

  for (i = 1; roots[i] != APE_GCROOTEND; ++i)
    if (roots[i])
      copy_ref(A, &roots[i]);
}

static void copy_heap(ape_State *A) {
  GC *gc = &CTX(A)->gc;
  ape_Cell *scan, *vec, *p;
  int i;

  /* copy the symbol list */
  copy_ref(A, &CTX(A)->symlist);
  copy_ref(A, &CTX(A)->t);

  /* copy the environment */
  copy_ref(A, &CTX(A)->env);

  /* copy the call list */
  copy_ref(A, &CTX(A)->calllist);

  /* copy the primitive symbols */
  for (i = 0; i < P_MAX; ++i)
    copy_ref(A, &CTX(A)->primsyms[i]);

  /* copy the type name symbols */
  for (i = 0; i < APE_TMAX; ++i)
    copy_ref(A, &CTX(A)->typesyms[i]);

  /* copy the roots */
  copy_roots(A, A->roots);

  /* scan until reaching the head */
  for (scan = gc->from; scan != gc->head; ++scan) {
    switch (type(scan)) {
    case APE_TPAIR:
      copy_ref(A, &car(scan));
      /* fall through */
    case APE_TFUNC:
    case APE_TMACRO:
    case APE_TSYMBOL:
    case APE_TSTRING:
      copy_ref(A, &cdr(scan));
      break;
    case APE_TVECTOR:
      vec = cdr(scan);
      p = vec;
      copy_ref(A, &vec);

      for (i = 1; i < (int)((veclen(scan) >> 1) + (veclen(scan) & 1)); ++i) {
        ape_Cell *cp = p + i;
        copy_ref(A, &cp);
      }

      cdr(scan) = vec;
      break;
    }
  }
}

static double collect_garbage(ape_State *A) {
  GC *gc = &CTX(A)->gc;
  ape_Cell *swap;

  /* swap semi spaces */
  swap = gc->from;
  gc->from = gc->to;
  gc->to = swap;

  /* set head ptrs */
  gc->head = gc->from;

  copy_heap(A);

  /* calculate usage */
  return (double)(gc->head - gc->from) / (double)gc->semi_size;
}

static void free_heap(ape_State *A, ape_Cell *heap, uword_t heap_size) {
  ape_Cell *scan;

  /* Handle objects that need to be GC, such as C pointers */
  for (scan = heap; scan != heap + heap_size; ++scan)
    if (type(scan) != APE_TFORWARD)
      handle_gc(A, scan);

  ape_free(A, heap);
}

static int rescale_heap(ape_State *A, int grow_size) {
  GC *gc = &CTX(A)->gc;
  ape_Cell *from, *to;
  uword_t allocated, semi_size, heap_size;

  allocated = (uword_t)(gc->head - gc->from);
  semi_size = gc->semi_size << 1;
  heap_size = gc->semi_size;

  while (semi_size <= (allocated + grow_size))
    semi_size <<= 1;

  from = gc->from;
  to = gc->to;

  gc->to = (ape_Cell *)ape_calloc(A, semi_size, sizeof(ape_Cell));

  if (!gc->to) {
    gc->to = to;
    return -1;
  }

  gc->from = (ape_Cell *)ape_calloc(A, semi_size, sizeof(ape_Cell));

  if (!gc->from) {
    ape_free(A, gc->to);
    gc->to = to;
    gc->from = from;
    return -1;
  }

  gc->head = gc->from;
  gc->semi_size = semi_size;

  copy_heap(A);
  free_heap(A, from, heap_size);
  free_heap(A, to, heap_size);

  return 0;
}

static ape_Cell *halloc(ape_State *A, int n) {
  GC *gc = &CTX(A)->gc;
  ape_Cell *cells;

  if (gc->head + n > gc->from + gc->semi_size) {
    double usage = collect_garbage(A);

    /* Rescale the semi space when the usage rate reaches 75% */
    if (usage > 0.75)
      if (rescale_heap(A, n) < 0)
        if (gc->head + n > gc->from + gc->semi_size) {
          ape_error(A, "out of memory");
          return NULL;
        }
  }

  cells = gc->head;
  gc->head += n;

  while (n-- > 0) {
    ape_Cell *cell = cells + n;

    if (type(cell) != APE_TFORWARD)
      handle_gc(A, cell);

    memset(cell, 0, sizeof(ape_Cell));
  }

  return cells;
}

#define create_object(A) halloc(A, 1)

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

static ape_Object create_env(ape_State *A, ape_Object parent) {
  return ape_cons(A, nil, parent);
}

static int gc_create(ape_State *A) {
  GC *gc = &CTX(A)->gc;

  /* semi space */
  gc->semi_size = SEMISIZE;
  gc->to = (ape_Cell *)ape_calloc(A, gc->semi_size, sizeof(ape_Cell));

  if (!gc->to)
    return -1;

  gc->from = (ape_Cell *)ape_calloc(A, gc->semi_size, sizeof(ape_Cell));

  if (!gc->from)
    return -1;

  gc->head = gc->from;
  return 0;
}

static void gc_destroy(ape_State *A) {
  GC *gc = &CTX(A)->gc;

  if (gc->to) {
    ape_free(A, gc->to);
    gc->to = NULL;
  }

  if (gc->from) {
    ape_free(A, gc->from);
    gc->from = NULL;
  }

  gc->head = NULL;
  gc->semi_size = 0;
}

extern void stdlib_open(ape_State *A);

static ape_State *ape_init(ape_State *A) {
  int i;

  ape_defvar1(A, v);

  /* init lists */
  CTX(A)->calllist = nil;
  CTX(A)->symlist = nil;

  /* global environment */
  CTX(A)->env = create_env(A, nil);

  /* init symbol id */
  CTX(A)->symid = (unsigned int)((uintptr_t)A >> 16); /* random seed */
  CTX(A)->symid = ((CTX(A)->symid * 214013L + 2531011L) >> 16) & 0x01ff;

  /* init objects */
  CTX(A)->t = ape_symbol(A, "true");
  ape_def(A, CTX(A)->t, CTX(A)->t, NULL);

  /* init primitive symbols */
  for (i = 0; i < P_MAX; ++i)
    CTX(A)->primsyms[i] = ape_symbol(A, primnames[i]);

  /* init type name symbols */
  for (i = 0; i < APE_TMAX; ++i)
    CTX(A)->typesyms[i] = ape_symbol(A, typenames[i]);

  /* register built in primitives */
  for (i = 0; i < P_MAX; ++i) {
    *v = create_object(A);

    settype(*v, APE_TPRIM);
    prim(*v) = i;

    ape_def(A, CTX(A)->primsyms[i], *v, NULL);
  }

  stdlib_open(A);
  return &CTX(A)->state;
}

ape_State *ape_newstate(ape_Alloc f, void *ud) {
  ape_Alloc alloc = f ? f : alloc_emul;
  ape_Context *ctx = (ape_Context *)alloc(ud, NULL, sizeof(ape_Context));
  ape_State *A;

  if (!ctx)
    return NULL;

  memset(ctx, 0, sizeof(ape_Context));

  /* init allocator */
  ctx->alloc = alloc;
  ctx->ud = ud;

  /* init state */
  ctx->state.ctx = ctx;
  A = &ctx->state;

  /* init GC */
  if (gc_create(A) < 0) {
    ape_close(A);
    return NULL;
  }

  return ape_init(A);
}

void ape_close(ape_State *A) {
  gc_destroy(A);
  ape_free(A, CTX(A));
}

ape_Handlers *ape_handlers(ape_State *A) { return &CTX(A)->handlers; }

static void raise_error(ape_State *A, const char *errmsg) {
  ape_Object cl = CTX(A)->calllist;

  /* reset context state */
  CTX(A)->calllist = nil;

  /* flush stdout cache */
  fflush(stdout);

  /* do error handler */
  if (CTX(A)->handlers.error)
    CTX(A)->handlers.error(A, errmsg, cl);

  /* error handler returned -- print error and traceback, exit */
  fprintf(stderr, "error: %s\n", errmsg);

  for (; !isnil(cl); cl = cdr(cl)) {
    char buf[256];
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

int ape_type(ape_State *A, ape_Object obj) {
  unused(A);
  return type(obj);
}

int ape_isnil(ape_State *A, ape_Object obj) {
  unused(A);
  return isnil(obj);
}

int ape_equal(ape_State *A, ape_Object a, ape_Object b) {
  unused(A);

  if (a == b)
    return 1;

  if (type(a) != type(b))
    return 0;

  if (type(a) == APE_TNUMBER)
    return fabs(number(a) - number(b)) < NUM_EPSILON;

  if (type(a) == APE_TSTRING) {
    for (; !isnil(a); a = cdr(a), b = cdr(b))
      if (car(a) != car(b))
        return 0;

    return a == b;
  }

  return 0;
}

ape_Object ape_checktype(ape_State *A, ape_Object obj, int type) {
  if (type(obj) != type) {
    ape_error(A, "expected %s, got %s", typenames[type], typenames[type(obj)]);
    return NULL;
  }
  return obj;
}

ape_Object ape_cons(ape_State *A, ape_Object car, ape_Object cdr) {
  ape_defvar3(A, pair, carp, cdrp);

  *carp = car;
  *cdrp = cdr;
  *pair = create_object(A);
  car(*pair) = *carp;
  cdr(*pair) = *cdrp;
  return *pair;
}

ape_Object ape_car(ape_State *A, ape_Object obj) {
  if (isnil(obj))
    return obj;
  return car(ape_checktype(A, obj, APE_TPAIR));
}

ape_Object ape_cdr(ape_State *A, ape_Object obj) {
  if (isnil(obj))
    return obj;
  return cdr(ape_checktype(A, obj, APE_TPAIR));
}

ape_Object ape_setcar(ape_State *A, ape_Object obj, ape_Object car) {
  car(ape_checktype(A, obj, APE_TPAIR)) = car;
  return obj;
}

ape_Object ape_setcdr(ape_State *A, ape_Object obj, ape_Object cdr) {
  cdr(ape_checktype(A, obj, APE_TPAIR)) = cdr;
  return obj;
}

ape_Object ape_nil(ape_State *A) {
  unused(A);
  return nil;
}

ape_Object ape_true(ape_State *A) { return CTX(A)->t; }

ape_Object ape_bool(ape_State *A, int b) {
  return b ? ape_true(A) : ape_nil(A);
}

ape_Object ape_integer(ape_State *A, long long n) {
  return ape_number(A, (number_t)n);
}

ape_Object ape_number(ape_State *A, double n) {
  ape_defvar1(A, obj);

  *obj = create_object(A);
  settype(*obj, APE_TNUMBER);
  number(*obj) = (number_t)n;
  return *obj;
}

static ape_Object build_string(ape_State *A, ape_Object tail, int ch) {
  int index;

  if (!tail || (tag(tail) & FCMARKBIT)) {
    ape_defvar2(A, obj, tailp);

    *tailp = tail;
    *obj = create_object(A);
    settype(*obj, APE_TSTRING);
    cdr(*obj) = nil;

    if (!*tailp)
      return *obj;

    cdr(*tailp) = *obj;
    tail = *obj;
  }

  index = stridx(tail);
  strbuf(tail)[index++] = ch;

  if (index == STRBUFSIZE)
    tag(tail) |= FCMARKBIT;
  else
    stridx(tail) = index;

  return tail;
}

ape_Object ape_string(ape_State *A, const char *str) {
  return ape_lstring(A, str, (int)strlen(str));
}

ape_Object ape_lstring(ape_State *A, const char *str, int len) {
  int i;
  ape_defvar2(A, obj, tail);

  *obj = build_string(A, NULL, 0);
  *tail = *obj;

  for (i = 0; i < len; ++i)
    *tail = build_string(A, *tail, str[i]);

  return *obj;
}

static int strleq(ape_Object obj, const char *str, int len) {
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

static ape_Object symbol(ape_State *A, uword_t h, const char *name, int len,
                         int pushlist) {
  ape_defvar2(A, obj, str);

  *obj = create_object(A);
  *str = ape_lstring(A, name, len);

  settype(*obj, APE_TSYMBOL);
  hash(*obj) = h;
  cdr(*obj) = *str;

  if (pushlist)
    CTX(A)->symlist = ape_cons(A, *obj, CTX(A)->symlist);

  return *obj;
}

static uword_t fast_hash(const char *str, int len) {
  uword_t h = 5381;
  int i;

  for (i = 0; i < len; ++i)
    h = (h << 5) + h + str[i];

  return h & HASHMASK;
}

ape_Object ape_symbol(ape_State *A, const char *name) {
  ape_Object obj;
  int len = (int)strlen(name);
  uword_t h = fast_hash(name, len);

  /* try to find in symlist */
  for (obj = CTX(A)->symlist; !isnil(obj); obj = cdr(obj))
    if (hash(car(obj)) == h && strleq(cdr(car(obj)), name, len))
      return car(obj);

  /* create new symbol, push to symlist and return */
  return symbol(A, h, name, len, 1);
}

/*                          Vector(5)
 * +---------------------+-------------------------------------------+
 * | +--------+--------+ | +-----+-----+ +-----+-----+ +-----+-----+ |
 * | | Vector | length | | |  0  |  1  | |  2  |  3  | |  4  | nil | |
 * | +--------+--------+ | +-----+-----+ +-----+-----+ +-----+-----+ |
 * +---------------------+-------------------------------------------+
 */

ape_Object ape_vector(ape_State *A, int len) {
  ape_defvar2(A, obj, vec);

  if (len < 1) {
    ape_error(A, "vector length must greater than zero");
    return NULL;
  }

  *obj = create_object(A);
  *vec = halloc(A, (len >> 1) + (len & 1));

  settype(*obj, APE_TVECTOR);
  veclen(*obj) = len;
  cdr(*obj) = *vec;

  return *obj;
}

static ape_Object *vector_place(ape_Object vec, int index) {
  return (index & 1) == 0 ? &car(&vec[index >> 1]) : &cdr(&vec[index >> 1]);
}

ape_Object ape_vecset(ape_State *A, ape_Object vec, int pos, ape_Object obj) {
  int len = (int)veclen(ape_checktype(A, vec, APE_TVECTOR));
  ape_Object *place;

  if (pos >= len) {
    ape_error(A, "vector out of range");
    return NULL;
  }

  place = vector_place(cdr(vec), pos);

  if (!place) {
    ape_error(A, "vector place not found");
    return NULL;
  }

  *place = obj;
  return vec;
}

ape_Object ape_cfunc(ape_State *A, ape_CFunc fn) {
  ape_defvar1(A, obj);

  *obj = create_object(A);
  settype(*obj, APE_TCFUNC);
  cfunc(*obj) = fn;
  return *obj;
}

ape_Object ape_ptr(ape_State *A, void *ptr, int subtype) {
  ape_defvar1(A, obj);

  *obj = create_object(A);
  settype(*obj, APE_TPTR);
  ptrtype(*obj) = subtype & 0xFFFF;
  cdr(*obj) = (ape_Object)ptr;
  return *obj;
}

ape_Object ape_gensym(ape_State *A) {
  char gensym[16] = {0};
  int len = sprintf(gensym, "#:%u", CTX(A)->symid++);

  /* create new symbol, without push to symlist and return */
  return symbol(A, fast_hash(gensym, len), gensym, len, 0);
}

ape_Object ape_strappend(ape_State *A, ape_Object objs) {
  int i, size;
  ape_defvar4(A, objsp, obj, tail, str);

  *objsp = objs;
  *obj = build_string(A, NULL, 0);
  *tail = *obj;

  while (!isnil(*objsp)) {
    *str = ape_checktype(A, ape_nextarg(A, objsp), APE_TSTRING);

    while (!isnil(*str)) {
      size = strcnt(*str);

      for (i = 0; i < size; ++i)
        *tail = build_string(A, *tail, strbuf(*str)[i]);

      *str = cdr(*str);
    }
  }
  return *obj;
}

ape_Object ape_strreverse(ape_State *A, ape_Object obj) {
  int i, len = ape_strlen(A, ape_checktype(A, obj, APE_TSTRING));
  ape_defvar3(A, objp, res, tail);

  *objp = obj;
  *res = build_string(A, NULL, 0);
  *tail = *res;

  for (i = 0; i < len; ++i)
    *tail = build_string(A, *tail, ape_strref(A, *objp, len - i - 1));

  return *res;
}

ape_Object ape_vecref(ape_State *A, ape_Object obj, int idx) {
  int cnt = (int)veclen(ape_checktype(A, obj, APE_TVECTOR));
  ape_Object *place;

  if (idx >= cnt) {
    ape_error(A, "index out of range");
    return NULL;
  }

  place = vector_place(cdr(obj), idx);
  return place ? *place : nil;
}

int ape_strref(ape_State *A, ape_Object obj, int idx) {
  int cnt = 0;

  obj = ape_checktype(A, obj, APE_TSTRING);

  while (idx >= 0) {
    cnt = strcnt(obj);

    if (cnt == 0 || idx < cnt)
      break;

    obj = cdr(obj);
    idx -= cnt;

    if (isnil(obj)) {
      ape_error(A, "index out of range");
      return 0;
    }
  }

  if (cnt == 0) {
    ape_error(A, "index out of range");
    return 0;
  }

  return strbuf(obj)[idx];
}

int ape_strlen(ape_State *A, ape_Object obj) {
  int len;
  obj = ape_checktype(A, obj, APE_TSTRING);

  for (len = 0; !isnil(obj); obj = cdr(obj))
    len += strcnt(obj);

  return len;
}

int ape_veclen(ape_State *A, ape_Object obj) {
  return (int)veclen(ape_checktype(A, obj, APE_TVECTOR));
}

long long ape_tointeger(ape_State *A, ape_Object obj) {
  return (long long)number(ape_checktype(A, obj, APE_TNUMBER));
}

double ape_tonumber(ape_State *A, ape_Object obj) {
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

int ape_tostring(ape_State *A, ape_Object obj, char *dst, int size) {
  CharPtrInt x;

  x.p = dst;
  x.n = size - 1;

  ape_write(A, obj, writebuf, &x, 0);
  *x.p = '\0';

  return size - x.n - 1;
}

int ape_ptrtype(ape_State *A, ape_Object obj) {
  return (int)ptrtype(ape_checktype(A, obj, APE_TPTR));
}

void *ape_toptr(ape_State *A, ape_Object obj) {
  return cdr(ape_checktype(A, obj, APE_TPTR));
}

static ape_Cell rparen = {0};

static ape_Object reader(ape_State *A, ape_ReadFunc fn, void *udata) {
  static const char *delimiter = "();";
  number_t n;
  int ch;
  char buf[APE_SYMSIZE] = {0}, *p;
  ape_defvar4(A, v, res, tail, o);

  /* get next character */
  ch = CTX(A)->next_char ? CTX(A)->next_char : fn(A, udata);
  CTX(A)->next_char = 0;

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
    *res = ape_cons(A, nil, nil);
    *tail = *res;

    while ((*v = reader(A, fn, udata)) != &rparen) {
      if (*v == NULL) {
        ape_error(A, "unclosed list");
        return NULL;
      }

      if (type(*v) == APE_TSYMBOL && strleq(cdr(*v), ".", 1)) {
        /* dotted pair */
        *o = ape_read(A, fn, udata);
        cdr(*tail) = *o;
      } else {
        /* proper pair */
        *o = ape_cons(A, *v, nil);
        cdr(*tail) = *o;
        *tail = cdr(*tail);
      }
    }
    return cdr(*res);

  case '\'':
    *v = ape_read(A, fn, udata);

    if (!*v) {
      ape_error(A, "stray '''");
      return NULL;
    }

    /* Transform: '(...) => (quote (...)) */
    *res = ape_cons(A, *v, nil);
    return ape_cons(A, CTX(A)->primsyms[P_QUOTE], *res);

  case '#':
    *v = ape_read(A, fn, udata);

    if (!*v) {
      ape_error(A, "stray '#'");
      return NULL;
    }

    /* Transform: #(...) => (vector ...) */
    return ape_cons(A, CTX(A)->primsyms[P_VECTOR],
                    ape_checktype(A, *v, APE_TPAIR));

  case '`':
    *v = ape_read(A, fn, udata);

    if (!*v) {
      ape_error(A, "stray '`'");
      return NULL;
    }

    /* Transform: `(...) => (quasiquote (...)) */
    *res = ape_cons(A, *v, nil);
    return ape_cons(A, CTX(A)->primsyms[P_QUASIQUOTE], *res);

  case ',':
    ch = fn(A, udata);

    if (ch != '@')
      CTX(A)->next_char = ch;

    *v = ape_read(A, fn, udata);

    if (!*v) {
      ape_error(A, "stray ','");
      return NULL;
    }

    *res = ape_cons(A, *v, nil);

    /* Transform: ,@v => (unquote-splicing (v)) */
    if (ch == '@')
      return ape_cons(A, CTX(A)->primsyms[P_UNQUOTE_SPLICING], *res);

    /* Transform: ,v => (unquote (v)) */
    return ape_cons(A, CTX(A)->primsyms[P_UNQUOTE], *res);

  case '"':
    *res = build_string(A, NULL, 0);
    *v = *res;

    ch = fn(A, udata);
    while (ch != '"') {
      if (ch == '\0') {
        ape_error(A, "unclosed string");
        return NULL;
      }

      if (ch == '\\') {
        ch = fn(A, udata);

        if (ch == '\0') {
          ape_error(A, "unclosed string");
          return NULL;
        }

        if (strchr("nrt\\\"", ch))
          ch = strchr("n\nr\rt\t\\\\\"\"", ch)[1];
      }

      *v = build_string(A, *v, ch);
      ch = fn(A, udata);
    }
    return *res;

  default:
    p = buf;
    do {
      if (p == buf + sizeof(buf) - 1) {
        ape_error(A, "symbol too long");
        return NULL;
      }

      *p++ = ch;
      ch = fn(A, udata);
    } while (ch && !strchr(delimiter, ch) && !isspace(ch));

    CTX(A)->next_char = ch;

    /* try to read as number */
    n = (number_t)strtod(buf, &p);

    if (p != buf && (strchr(delimiter, *p) || isspace(*p)))
      return ape_number(A, n);

    if (!strcmp(buf, "nil"))
      return nil;

    return ape_symbol(A, buf);
  }
  return NULL;
}

ape_Object ape_read(ape_State *A, ape_ReadFunc fn, void *udata) {
  ape_Object obj = reader(A, fn, udata);

  if (obj == &rparen) {
    ape_error(A, "stray ')'");
    return NULL;
  }
  return obj;
}

static void writestr(ape_State *A, ape_WriteFunc fn, void *udata,
                     const char *str) {
  while (*str)
    fn(A, udata, *str++);
}

void ape_write(ape_State *A, ape_Object obj, ape_WriteFunc fn, void *udata,
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
      ape_write(A, ape_vecref(A, obj, i), fn, udata, 1);

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

#define ENV_RECUR 0x1
#define ENV_CREATE 0x2

static ape_Object *getbound(ape_Object sym, ape_Object env, int flags) {
  ape_Object bound, *frame = NULL;

  /* Try to find in environment */
  for (; !isnil(env); env = cdr(env)) {
    frame = &car(env);

    for (; !isnil(*frame); frame = &cdr(*frame)) {
      bound = car(*frame);

      if (car(bound) == sym)
        return frame;
    }

    if (!(flags & ENV_RECUR))
      return frame;
  }
  return frame;
}

ape_Object ape_unbound(ape_State *A, ape_Object sym, ape_Object env,
                       int recur) {
  ape_Object *frame, bound;

  env = env ? env : CTX(A)->env;
  sym = ape_checktype(A, sym, APE_TSYMBOL);

  frame = getbound(sym, env, recur ? ENV_RECUR : 0);

  if (frame && !isnil(*frame)) {
    bound = car(*frame);
    *frame = cdr(*frame);
    return cdr(bound);
  }
  return nil;
}

ape_Object ape_def(ape_State *A, ape_Object sym, ape_Object val,
                   ape_Object env) {
  ape_Object *frame;
  ape_defvar5(A, bound, symp, valp, envp, va);

  env = env ? env : CTX(A)->env;

  *symp = ape_checktype(A, sym, APE_TSYMBOL);
  *valp = val;
  *envp = env;

  *va = ape_cons(A, *symp, *valp);
  *bound = ape_cons(A, *va, nil);
  frame = getbound(*symp, *envp, ENV_CREATE);

  if (!isnil(*frame)) {
    ape_error(A, "variables cannot be redefined");
    return NULL;
  }

  *frame = *bound;
  return *valp;
}

ape_Object ape_set(ape_State *A, ape_Object sym, ape_Object val,
                   ape_Object env) {
  ape_Object *frame, bound;

  env = env ? env : CTX(A)->env;
  sym = ape_checktype(A, sym, APE_TSYMBOL);

  frame = getbound(sym, env, ENV_RECUR);

  if (!frame || isnil(*frame)) {
    ape_error(A, "unbound variables cannot be set");
    return NULL;
  }

  bound = car(*frame);
  cdr(bound) = val;
  return val;
}

ape_Object ape_nextarg(ape_State *A, ape_Object *args) {
  ape_Object arg = *args;

  if (type(arg) != APE_TPAIR) {
    if (isnil(arg)) {
      ape_error(A, "too few arguments");
      return NULL;
    }

    ape_error(A, "dotted pair in argument list");
    return NULL;
  }

  *args = cdr(arg);
  return car(arg);
}

#define evalarg(args, env) ape_eval(A, ape_nextarg(A, args), *env)

static ape_Object eval_list(ape_State *A, ape_Object list, ape_Object env,
                            int *argc) {
  int cnt = 0;
  ape_defvar6(A, listp, envp, res, tail, va, vb);

  *listp = list;
  *envp = env;
  *res = ape_cons(A, nil, nil);
  *tail = *res;

  while (!isnil(*listp)) {
    *va = evalarg(listp, envp);
    *vb = ape_cons(A, *va, nil);
    cdr(*tail) = *vb;
    *tail = cdr(*tail);
    cnt += 1;
  }

  if (argc)
    *argc = cnt;

  return cdr(*res);
}

static ape_Object arith_add(ape_State *A, ape_Object args, ape_Object env) {
  number_t res = 0;
  ape_defvar2(A, argsp, envp);

  *argsp = args;
  *envp = env;

  while (!isnil(*argsp))
    res += number(ape_checktype(A, evalarg(argsp, envp), APE_TNUMBER));

  return ape_number(A, res);
}

static ape_Object arith_sub(ape_State *A, ape_Object args, ape_Object env) {
  number_t res;
  ape_defvar3(A, x, argsp, envp);

  *argsp = args;
  *envp = env;

  if (isnil(*argsp)) {
    ape_error(A, "wrong number of operands");
    return NULL;
  }

  *x = ape_checktype(A, evalarg(argsp, envp), APE_TNUMBER);

  if (isnil(*argsp))
    return ape_number(A, -number(*x));

  res = number(*x);

  while (!isnil(*argsp))
    res -= number(ape_checktype(A, evalarg(argsp, envp), APE_TNUMBER));

  return ape_number(A, res);
}

static ape_Object arith_mul(ape_State *A, ape_Object args, ape_Object env) {
  number_t res = 1;
  ape_defvar2(A, argsp, envp);

  *argsp = args;
  *envp = env;

  while (!isnil(*argsp))
    res *= number(ape_checktype(A, evalarg(argsp, envp), APE_TNUMBER));

  return ape_number(A, res);
}

static ape_Object arith_div(ape_State *A, ape_Object args, ape_Object env) {
  number_t res;
  ape_defvar3(A, x, argsp, envp);

  *argsp = args;
  *envp = env;

  if (isnil(*argsp)) {
    ape_error(A, "wrong number of operands");
    return NULL;
  }

  *x = ape_checktype(A, evalarg(argsp, envp), APE_TNUMBER);

  if (isnil(*argsp))
    res = (number_t)1;
  else {
    res = number(*x);
    *x = ape_checktype(A, evalarg(argsp, envp), APE_TNUMBER);
  }

  do {
    if (fabs(number(*x) - (number_t)0) < NUM_EPSILON) {
      ape_error(A, "division by zero");
      return NULL;
    }

    res /= number(*x);

    if (isnil(*argsp))
      break;

    *x = ape_checktype(A, evalarg(argsp, envp), APE_TNUMBER);
  } while (1);

  return ape_number(A, res);
}

#define arith_compare(A, args, env, op)                                        \
  do {                                                                         \
    *res = NULL;                                                               \
    if (isnil(*args))                                                          \
      *res = CTX(A)->t;                                                        \
    else {                                                                     \
      *va = ape_checktype(A, evalarg(args, env), APE_TNUMBER);                 \
      if (isnil(*args))                                                        \
        *res = CTX(A)->t;                                                      \
      else {                                                                   \
        while (!isnil(*args)) {                                                \
          *vb = ape_checktype(A, evalarg(args, env), APE_TNUMBER);             \
          if (!(number(*va) op number(*vb))) {                                 \
            *res = nil;                                                        \
            break;                                                             \
          }                                                                    \
        }                                                                      \
        if (!*res)                                                             \
          *res = CTX(A)->t;                                                    \
      }                                                                        \
    }                                                                          \
  } while (0)

static void args_binds(ape_State *A, ape_Object syms, ape_Object args,
                       ape_Object env) {
  ape_defvar6(A, bind, symsp, argp, argsp, envp, va);

  *symsp = syms;
  *argsp = args;
  *envp = env;

  while (!isnil(*symsp)) {
    /* rest arguments */
    if (type(*symsp) != APE_TPAIR) {
      ape_def(A, *symsp, *argsp, *envp);
      return;
    }

    *bind = car(*symsp);

    if (isnil(*argsp)) {
      if (type(*bind) != APE_TPAIR) {
        ape_error(A, "wrong number of arguments");
        return;
      }

      *argp = cdr(*bind);
      /* default argument */
      *va = ape_eval(A, car(*argp), *envp);
      ape_def(A, car(*bind), *va, *envp);
    } else {
      if (type(*bind) == APE_TPAIR)
        *bind = car(*bind);

      /* bind argument */
      ape_def(A, *bind, car(*argsp), *envp);
    }

    *symsp = cdr(*symsp);
    *argsp = cdr(*argsp);
  }
}

static ape_Object quasiquote(ape_State *A, ape_Object expr, ape_Object env) {
  ape_defvar9(A, exprp, envp, res, tail, obj, arg, args, fn, va);

  if (type(expr) != APE_TPAIR)
    return expr;

  *exprp = expr;
  *envp = env;
  *res = ape_cons(A, nil, nil);
  *tail = *res;

  while (!isnil(*exprp)) {
    *obj = ape_nextarg(A, exprp);

    if (type(*obj) == APE_TPAIR) {
      *fn = car(*obj);
      *args = cdr(*obj);

      if (*fn == CTX(A)->primsyms[P_UNQUOTE_SPLICING]) {
        *arg = ape_nextarg(A, args);

        *va = ape_cons(A, *arg, nil);
        *arg = quasiquote(A, *va, *envp);
        *arg = car(ape_checktype(A, *arg, APE_TPAIR));

        *obj = ape_checktype(A, ape_eval(A, *arg, *envp), APE_TPAIR);

        for (; !isnil(*obj); *obj = cdr(*obj)) {
          /* (x . y) => (x y) */
          if (type(*obj) != APE_TPAIR) {
            *va = ape_cons(A, *obj, nil);
            cdr(*tail) = *va;
            *tail = cdr(*tail);
            break;
          }

          /* copy list */
          *va = ape_cons(A, car(*obj), nil);
          cdr(*tail) = *va;
          *tail = cdr(*tail);
        }

        continue;
      } else if (*fn == CTX(A)->primsyms[P_UNQUOTE]) {
        *arg = ape_nextarg(A, args);

        if (type(*arg) == APE_TPAIR) {
          *va = ape_cons(A, *arg, nil);
          *arg = quasiquote(A, *va, *envp);
          *arg = car(ape_checktype(A, *arg, APE_TPAIR));
        }

        *obj = ape_eval(A, *arg, *envp);
      } else
        *obj = quasiquote(A, *obj, *envp);
    }

    *va = ape_cons(A, *obj, nil);
    cdr(*tail) = *va;
    *tail = cdr(*tail);
  }
  return cdr(*res);
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

static ape_Object expand(ape_State *A, ape_Object macro, ape_Object args) {
  ape_defvar5(A, macrop, argsp, body, head, env);

  *macrop = macro;
  *argsp = args;

  /* ((env . args) . (do ...)) */
  *body = cdr(ape_checktype(A, *macrop, APE_TMACRO));
  /* (env . args) */
  *head = car(*body);

  /* arguments environment */
  *env = create_env(A, car(*head));
  args_binds(A, cdr(*head), *argsp, *env);

  /* generate code by macro */
  return ape_eval(A, cdr(*body), *env);
}

ape_Object ape_eval(ape_State *A, ape_Object expr, ape_Object env) {
  int argc, i;
  ape_defvar8(A, exprp, envp, fn, args, res, va, vb, vc);

  env = env ? env : CTX(A)->env;

  *exprp = expr;
  *envp = env;

EVAL:
  if (type(*exprp) == APE_TSYMBOL) {
    ape_Object *frame, bound;

    frame = getbound(*exprp, *envp, ENV_RECUR);

    if (!frame || isnil(*frame)) {
      ape_error(A, "unbound variables");
      return NULL;
    }

    bound = car(*frame);
    return cdr(bound);
  }

  if (type(*exprp) != APE_TPAIR)
    return *exprp;

  CTX(A)->calllist = ape_cons(A, *exprp, CTX(A)->calllist);

  *fn = ape_eval(A, car(*exprp), *envp);
  *args = cdr(*exprp);
  *res = nil;

  switch (type(*fn)) {
  case APE_TPRIM:
    switch (prim(*fn)) {
    case P_DEF:
      *va = ape_nextarg(A, args);
      *vb = evalarg(args, envp);
      *res = ape_def(A, *va, *vb, *envp);
      break;
    case P_SET:
      *va = ape_nextarg(A, args);
      *vb = evalarg(args, envp);
      *res = ape_set(A, *va, *vb, *envp);
      break;
    case P_IF:
      *va = evalarg(args, envp);
      *vb = ape_nextarg(A, args);

      *exprp = !isnil(*va) ? *vb : ape_nextarg(A, args);

      CTX(A)->calllist = cdr(CTX(A)->calllist);
      goto EVAL;
    case P_FN:
    case P_MACRO:
      *va = ape_cons(A, *envp, ape_nextarg(A, args));
      *vb = ape_cons(A, CTX(A)->primsyms[P_DO], *args);
      *res = create_object(A);
      settype(*res, prim(*fn) == P_FN ? APE_TFUNC : APE_TMACRO);
      *vc = ape_cons(A, *va, *vb);
      cdr(*res) = *vc;
      break;
    case P_EXPAND:
      *va = evalarg(args, envp);
      *vb = ape_eval(A, car(*va), *envp);
      *res = expand(A, *vb, cdr(*va));
      break;
    case P_QUOTE:
      *res = ape_nextarg(A, args);
      break;
    case P_QUASIQUOTE:
      *res = quasiquote(A, ape_nextarg(A, args), *envp);
      break;
    case P_UNQUOTE:
      ape_error(A, "unquote outside a quasiquote");
      break;
    case P_UNQUOTE_SPLICING:
      ape_error(A, "unquote-splicing outside a quasiquote");
      break;
    case P_AND:
      while (!isnil(*args) && !isnil(*res = evalarg(args, envp)))
        ;
      break;
    case P_OR:
      while (!isnil(*args) && isnil(*res = evalarg(args, envp)))
        ;
      break;
    case P_NOT:
      *res = ape_bool(A, isnil(evalarg(args, envp)));
      break;
    case P_DO:
      if (!isnil(*args)) {
        for (; !isnil(cdr(*args)); *args = cdr(*args))
          ape_eval(A, car(*args), *envp);

        *exprp = car(*args);

        CTX(A)->calllist = cdr(CTX(A)->calllist);
        goto EVAL;
      }
      break;
    case P_CONS:
      *va = evalarg(args, envp);
      *vb = evalarg(args, envp);
      *res = ape_cons(A, *va, *vb);
      break;
    case P_CAR:
      *res = ape_car(A, evalarg(args, envp));
      break;
    case P_CDR:
      *res = ape_cdr(A, evalarg(args, envp));
      break;
    case P_SETCAR:
      *va = evalarg(args, envp);
      *vb = evalarg(args, envp);
      *res = ape_setcar(A, *va, *vb);
      break;
    case P_SETCDR:
      *va = evalarg(args, envp);
      *vb = evalarg(args, envp);
      *res = ape_setcdr(A, *va, *vb);
      break;
    case P_TYPE:
      *va = evalarg(args, envp);
      *res = CTX(A)->typesyms[type(*va)];
      break;
    case P_VECTOR:
      *va = eval_list(A, *args, *envp, &argc);
      *res = ape_vector(A, argc);

      for (i = 0; i < argc; ++i)
        ape_vecset(A, *res, i, ape_nextarg(A, va));
      break;
    case P_VECSET:
      *va = evalarg(args, envp);
      *vb = ape_checktype(A, evalarg(args, envp), APE_TNUMBER);
      *vc = evalarg(args, envp);
      *res = ape_vecset(A, *va, (int)number(*vb), *vc);
      break;
    case P_EQ:
      *va = evalarg(args, envp);
      *vb = evalarg(args, envp);
      *res = ape_bool(A, ape_equal(A, *va, *vb));
      break;
    case P_LT:
      arith_compare(A, args, envp, <);

      if (!isnil(*res) && ape_equal(A, *va, *vb))
        *res = nil;
      break;
    case P_LTE:
      arith_compare(A, args, envp, <);

      if (isnil(*res) && ape_equal(A, *va, *vb))
        *res = CTX(A)->t;
      break;
    case P_GT:
      arith_compare(A, args, envp, >);

      if (!isnil(*res) && ape_equal(A, *va, *vb))
        *res = nil;
      break;
    case P_GTE:
      arith_compare(A, args, envp, >);

      if (isnil(*res) && ape_equal(A, *va, *vb))
        *res = CTX(A)->t;
      break;
    case P_ADD:
      *res = arith_add(A, *args, *envp);
      break;
    case P_SUB:
      *res = arith_sub(A, *args, *envp);
      break;
    case P_MUL:
      *res = arith_mul(A, *args, *envp);
      break;
    case P_DIV:
      *res = arith_div(A, *args, *envp);
      break;
    default:
      ape_error(A, "undefined primitive");
      break;
    }
    break;
  case APE_TCFUNC:
    *va = eval_list(A, *args, *envp, &argc);
    *res = cfunc(*fn)(A, argc, *va, *envp);
    break;
  case APE_TFUNC:
    *va = cdr(*fn); /* ((env . args) . (do ...)) */
    *vb = car(*va); /* (env . args)*/

    *args = eval_list(A, *args, *envp, NULL);

    /* new local environment */
    *envp = create_env(A, car(*vb));
    args_binds(A, cdr(*vb), *args, *envp);

    *exprp = cdr(*va); /* do block */

    CTX(A)->calllist = cdr(CTX(A)->calllist);
    goto EVAL;
  case APE_TMACRO:
    *exprp = expand(A, *fn, *args);

    CTX(A)->calllist = cdr(CTX(A)->calllist);
    goto EVAL;
  default:
    ape_error(A, "tried to call non-callable value");
    break;
  }

  CTX(A)->calllist = cdr(CTX(A)->calllist);
  return *res;
}

ape_Object ape_load(ape_State *A, const char *file, ape_Object env) {
  FILE *fp;
  ape_defvar2(A, expr, envp);

  fp = fopen(file, "rb");

  if (!fp) {
    ape_error(A, "could not open input file");
    return NULL;
  }

  env = env ? env : CTX(A)->env;
  *envp = env;

  for (;;) {
    *expr = ape_readfp(A, fp);

    if (!*expr)
      break;

    ape_eval(A, *expr, *envp);
  }

  fclose(fp);
  return nil;
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

ape_Object ape_readstring(ape_State *A, const char *str) {
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

ape_Object ape_readfp(ape_State *A, FILE *fp) {
  return ape_read(A, readfp, fp);
}

static void writefp(ape_State *A, void *udata, char ch) {
  unused(A);
  fputc(ch, (FILE *)udata);
}

void ape_writefp(ape_State *A, ape_Object obj, FILE *fp) {
  ape_write(A, obj, writefp, fp, 0);
}
