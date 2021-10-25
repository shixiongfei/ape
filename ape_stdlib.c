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

#include "ape.h"

typedef struct Function {
  const char *name;
  ape_CFunc func;
} Function;

static ape_Object *cadr(ape_State *A, ape_Object *args) {
  return ape_car(A, ape_cdr(A, ape_nextarg(A, &args)));
}

static ape_Object *cddr(ape_State *A, ape_Object *args) {
  return ape_cdr(A, ape_cdr(A, ape_nextarg(A, &args)));
}

static ape_Object *caddr(ape_State *A, ape_Object *args) {
  return ape_car(A, cddr(A, args));
}

static ape_Object *cdddr(ape_State *A, ape_Object *args) {
  return ape_cdr(A, cddr(A, args));
}

static ape_Object *cadddr(ape_State *A, ape_Object *args) {
  return ape_car(A, cdddr(A, args));
}

static ape_Object *cddddr(ape_State *A, ape_Object *args) {
  return ape_cdr(A, cdddr(A, args));
}

static ape_Object *eval(ape_State *A, ape_Object *args) {
  return ape_eval(A, ape_nextarg(A, &args));
}

static ape_Object *list(ape_State *A, ape_Object *args) { return args; }

static ape_Object *length(ape_State *A, ape_Object *args) {
  return ape_integer(A, ape_length(A, ape_nextarg(A, &args)));
}

static ape_Object *reverse(ape_State *A, ape_Object *args) {
  return ape_reverse(A, ape_nextarg(A, &args));
}

static ape_Object *nth(ape_State *A, ape_Object *args) {
  int index = (int)ape_tointeger(A, ape_nextarg(A, &args));
  return ape_nth(A, ape_nextarg(A, &args), index);
}

static ape_Object *print(ape_State *A, ape_Object *args) {
  while (!ape_isnil(A, args)) {
    ape_writefp(A, ape_nextarg(A, &args), stdout);

    if (!ape_isnil(A, args))
      printf(" ");
  }
  printf("\n");
  return ape_nil(A);
}

static ape_Object *gensym(ape_State *A, ape_Object *args) {
  return ape_gensym(A);
}

static ape_Object *unquote(ape_State *A, ape_Object *args) {
  ape_error(A, "unquote outside a quasiquote");
  return ape_nil(A);
}

static ape_Object *unquote_splicing(ape_State *A, ape_Object *args) {
  ape_error(A, "unquote-splicing outside a quasiquote");
  return ape_nil(A);
}

static ape_Object *rem(ape_State *A, ape_Object *args) {
  ape_Integer a, b;

  a = ape_tointeger(A, ape_nextarg(A, &args));
  b = ape_tointeger(A, ape_nextarg(A, &args));

  return ape_integer(A, a % b);
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
(defn reduce (proc list accum)                                                 \
  (for x list                                                                  \
    (set! accum (proc accum x)))                                               \
  accum)"};

void stdlib_open(ape_State *A) {
  const Function cfuncs[] = {
      {"cadr", cadr},       {"cddr", cddr},
      {"caddr", caddr},     {"cdddr", cdddr},
      {"cadddr", cadddr},   {"cddddr", cddddr},
      {"eval", eval},       {"list", list},
      {"length", length},   {"reverse", reverse},
      {"nth", nth},         {"print", print},
      {"unquote", unquote}, {"unquote-splicing", unquote_splicing},
      {"gensym", gensym},   {"rem", rem},
      {NULL, NULL}};
  const char *stdlib[] = {defmacro, defn, let, cond,   when,   unless,
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
    ape_eval(A, expr);
    ape_restoregc(A, gctop);
  }
}
