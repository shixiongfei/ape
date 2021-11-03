/*
 * ape_stdlib.c
 *
 * Copyright (c) 2021 Xiongfei Shi
 *
 * Author: Xiongfei Shi <xiongfei.shi(a)icloud.com>
 * License: Apache-2.0
 *
 * https://github.com/shixiongfei/ape
 */

#include <float.h>
#include <math.h>

#include "ape.h"

typedef struct Function {
  const char *name;
  ape_CFunc func;
} Function;

static ape_Object *caar(ape_State *A, ape_Object *args, ape_Object *env) {
  return ape_car(A, ape_car(A, ape_nextarg(A, &args)));
}

static ape_Object *cadr(ape_State *A, ape_Object *args, ape_Object *env) {
  return ape_car(A, ape_cdr(A, ape_nextarg(A, &args)));
}

static ape_Object *cdar(ape_State *A, ape_Object *args, ape_Object *env) {
  return ape_cdr(A, ape_car(A, ape_nextarg(A, &args)));
}

static ape_Object *cddr(ape_State *A, ape_Object *args, ape_Object *env) {
  return ape_cdr(A, ape_cdr(A, ape_nextarg(A, &args)));
}

static ape_Object *caaar(ape_State *A, ape_Object *args, ape_Object *env) {
  return ape_car(A, caar(A, args, env));
}

static ape_Object *caadr(ape_State *A, ape_Object *args, ape_Object *env) {
  return ape_car(A, cadr(A, args, env));
}

static ape_Object *cadar(ape_State *A, ape_Object *args, ape_Object *env) {
  return ape_car(A, cdar(A, args, env));
}

static ape_Object *caddr(ape_State *A, ape_Object *args, ape_Object *env) {
  return ape_car(A, cddr(A, args, env));
}

static ape_Object *cdaar(ape_State *A, ape_Object *args, ape_Object *env) {
  return ape_cdr(A, caar(A, args, env));
}

static ape_Object *cdadr(ape_State *A, ape_Object *args, ape_Object *env) {
  return ape_cdr(A, cadr(A, args, env));
}

static ape_Object *cddar(ape_State *A, ape_Object *args, ape_Object *env) {
  return ape_cdr(A, cdar(A, args, env));
}

static ape_Object *cdddr(ape_State *A, ape_Object *args, ape_Object *env) {
  return ape_cdr(A, cddr(A, args, env));
}

static ape_Object *caaaar(ape_State *A, ape_Object *args, ape_Object *env) {
  return ape_car(A, caaar(A, args, env));
}

static ape_Object *caaadr(ape_State *A, ape_Object *args, ape_Object *env) {
  return ape_car(A, caadr(A, args, env));
}

static ape_Object *caadar(ape_State *A, ape_Object *args, ape_Object *env) {
  return ape_car(A, cadar(A, args, env));
}

static ape_Object *caaddr(ape_State *A, ape_Object *args, ape_Object *env) {
  return ape_car(A, caddr(A, args, env));
}

static ape_Object *cadaar(ape_State *A, ape_Object *args, ape_Object *env) {
  return ape_car(A, cdaar(A, args, env));
}

static ape_Object *cadadr(ape_State *A, ape_Object *args, ape_Object *env) {
  return ape_car(A, cdadr(A, args, env));
}

static ape_Object *caddar(ape_State *A, ape_Object *args, ape_Object *env) {
  return ape_car(A, cddar(A, args, env));
}

static ape_Object *cadddr(ape_State *A, ape_Object *args, ape_Object *env) {
  return ape_car(A, cdddr(A, args, env));
}

static ape_Object *cdaaar(ape_State *A, ape_Object *args, ape_Object *env) {
  return ape_cdr(A, caaar(A, args, env));
}

static ape_Object *cdaadr(ape_State *A, ape_Object *args, ape_Object *env) {
  return ape_cdr(A, caadr(A, args, env));
}

static ape_Object *cdadar(ape_State *A, ape_Object *args, ape_Object *env) {
  return ape_cdr(A, cadar(A, args, env));
}

static ape_Object *cdaddr(ape_State *A, ape_Object *args, ape_Object *env) {
  return ape_cdr(A, caddr(A, args, env));
}

static ape_Object *cddaar(ape_State *A, ape_Object *args, ape_Object *env) {
  return ape_cdr(A, cdaar(A, args, env));
}

static ape_Object *cddadr(ape_State *A, ape_Object *args, ape_Object *env) {
  return ape_cdr(A, cdadr(A, args, env));
}

static ape_Object *cdddar(ape_State *A, ape_Object *args, ape_Object *env) {
  return ape_cdr(A, cddar(A, args, env));
}

static ape_Object *cddddr(ape_State *A, ape_Object *args, ape_Object *env) {
  return ape_cdr(A, cdddr(A, args, env));
}

static ape_Object *unbound(ape_State *A, ape_Object *args, ape_Object *env) {
  return ape_unbound(A, ape_nextarg(A, &args), env, 1);
}

static ape_Object *eval(ape_State *A, ape_Object *args, ape_Object *env) {
  return ape_eval(A, ape_nextarg(A, &args), env);
}

static ape_Object *load(ape_State *A, ape_Object *args, ape_Object *env) {
  char filename[260] = {0};
  ape_tostring(A, ape_nextarg(A, &args), filename, sizeof(filename) - 1);
  return ape_load(A, filename, env);
}

static ape_Object *list(ape_State *A, ape_Object *args, ape_Object *env) {
  return args;
}

static ape_Object *concat(ape_State *A, ape_Object *args, ape_Object *env) {
  return ape_concat(A, args);
}

static ape_Object *length(ape_State *A, ape_Object *args, ape_Object *env) {
  return ape_integer(A, ape_length(A, ape_nextarg(A, &args)));
}

static ape_Object *reverse(ape_State *A, ape_Object *args, ape_Object *env) {
  return ape_reverse(A, ape_nextarg(A, &args));
}

static ape_Object *nth(ape_State *A, ape_Object *args, ape_Object *env) {
  int index = (int)ape_tointeger(A, ape_nextarg(A, &args));
  return ape_nth(A, ape_nextarg(A, &args), index);
}

static ape_Object *print(ape_State *A, ape_Object *args, ape_Object *env) {
  while (!ape_isnil(A, args)) {
    ape_writefp(A, ape_nextarg(A, &args), stdout);

    if (!ape_isnil(A, args))
      printf(" ");
  }
  printf("\n");
  return ape_nil(A);
}

static ape_Object *gensym(ape_State *A, ape_Object *args, ape_Object *env) {
  return ape_gensym(A);
}

static ape_Object *rem(ape_State *A, ape_Object *args, ape_Object *env) {
  long long a, b;

  a = ape_tointeger(A, ape_nextarg(A, &args));
  b = ape_tointeger(A, ape_nextarg(A, &args));

  return ape_integer(A, a % b);
}

static ape_Object *round_(ape_State *A, ape_Object *args, ape_Object *env) {
  double x = ape_tonumber(A, ape_nextarg(A, &args));
  int n = ape_isnil(A, args) ? 0 : (int)ape_tointeger(A, ape_nextarg(A, &args));
  double p, y, z;

  if (n == 0)
    return ape_number(A, round(x));

  p = pow(10, n);
  y = x * p;
  z = round(y);

  if ((fabs(z - y) - 0.5) < DBL_EPSILON)
    return ape_number(A, (2.0 * round(y / 2.0)) / p);

  return ape_number(A, z / p);
}

static const char defmacro[] = {"                                              \
(def defmacro (macro (name args . body)                                        \
  `(def ,name (macro ,args ,@body))))"};

static const char defn[] = {"                                                  \
(defmacro defn (name args . body)                                              \
  `(def ,name (fn ,args ,@body)))"};

static const char let[] = {"                                                   \
(defmacro let (binds . body)                                                   \
  ((fn ()                                                                      \
     (def args nil)                                                            \
     (def vals nil)                                                            \
     (while binds                                                              \
       (set! args (cons (car binds) args))                                     \
       (set! vals (cons (cadr binds) vals))                                    \
       (set! binds (cddr binds)))                                              \
     `((fn ,(reverse args)                                                     \
         ,@body)                                                               \
       ,@(reverse vals)))))"};

static const char cond[] = {"                                                  \
(defmacro cond clauses                                                         \
  `(if ,(car clauses)                                                          \
       ,(cadr clauses)                                                         \
     ,(if (not (cdddr clauses))                                                \
          (caddr clauses)                                                      \
        `(cond ,@(cddr clauses)))))"};

static const char apply[] = {"                                                 \
(defn apply (op args)                                                          \
  (eval (cons op args)))"};

static const char when[] = {"                                                  \
(defmacro when (test . body)                                                   \
  `(if ,test (do ,@body) nil))"};

static const char unless[] = {"                                                \
(defmacro unless (test . body)                                                 \
  `(if (not ,test) (do ,@body) nil))"};

static const char while_[] = {"                                                \
(defmacro while (test . body)                                                  \
  `(when ,test                                                                 \
     ,@body                                                                    \
     (while ,test ,@body)))"};

static const char for_[] = {"                                                  \
(defmacro for (item list . body)                                               \
  (let (iter (gensym))                                                         \
    `(let (,iter ,list)                                                        \
       (while ,iter                                                            \
         (let (,item (car ,iter))                                              \
           (set! ,iter (cdr ,iter))                                            \
           ,@body)))))"};

static const char map[] = {"                                                   \
(defn map (proc list)                                                          \
  (def res nil)                                                                \
  (for x list                                                                  \
    (set! res (cons (proc x) res)))                                            \
  (reverse res))"};

static const char filter[] = {"                                                \
(defn filter (proc list)                                                       \
  (def res nil)                                                                \
  (for x list                                                                  \
    (when (proc x)                                                             \
      (set! res (cons x res))))                                                \
  (reverse res))"};

static const char reduce[] = {"                                                \
(defn reduce (proc accum list)                                                 \
  (for x list                                                                  \
    (set! accum (proc accum x)))                                               \
  accum)"};

void stdlib_open(ape_State *A) {
  const Function cfuncs[] = {
      {"caar", caar},       {"cadr", cadr},     {"cdar", cdar},
      {"cddr", cddr},       {"caaar", caaar},   {"caadr", caadr},
      {"cadar", cadar},     {"caddr", caddr},   {"cdaar", cdaar},
      {"cdadr", cdadr},     {"cddar", cddar},   {"cdddr", cdddr},
      {"caaaar", caaaar},   {"caaadr", caaadr}, {"caadar", caadar},
      {"caaddr", caaddr},   {"cadaar", cadaar}, {"cadadr", cadadr},
      {"caddar", caddar},   {"cadddr", cadddr}, {"cdaaar", cdaaar},
      {"cdaadr", cdaadr},   {"cdadar", cdadar}, {"cdaddr", cdaddr},
      {"cddaar", cddaar},   {"cddadr", cddadr}, {"cdddar", cdddar},
      {"cddddr", cddddr},   {"eval", eval},     {"load", load},
      {"list", list},       {"concat", concat}, {"length", length},
      {"reverse", reverse}, {"nth", nth},       {"print", print},
      {"gensym", gensym},   {"rem", rem},       {"round", round_},
      {NULL, NULL}};
  const char *stdlib[] = {defmacro, defn, let, cond,   apply,  when, unless,
                          while_,   for_, map, filter, reduce, NULL};
  int gctop = ape_savegc(A);

  /* c libs */
  for (const Function *func = cfuncs; func->name && func->func; ++func) {
    ape_def(A, ape_symbol(A, func->name), ape_cfunc(A, func->func), NULL);
    ape_restoregc(A, gctop);
  }

  /* ape libs */
  for (const char **lib = stdlib; *lib; ++lib) {
    ape_Object *expr = ape_readstring(A, *lib);
    ape_eval(A, expr, NULL);
    ape_restoregc(A, gctop);
  }
}
