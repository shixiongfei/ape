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

#define CFUNC(fn)                                                              \
  { APE_STR(fn), fn }

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

static const char defmacro[] = {"                                              \
(def defmacro (macro (name args . body)                                        \
  (list 'def name (cons 'macro (cons args body)))))"};

static const char defn[] = {"                                                  \
(defmacro defn (name args . body)                                              \
  (list 'def name (cons 'fn (cons args body))))"};

static const char cond[] = {"                                                  \
(defmacro cond clauses                                                         \
  (list 'if (car clauses)                                                      \
            (cadr clauses)                                                     \
          (if (not (cdddr clauses))                                            \
              (caddr clauses)                                                  \
            (cons 'cond (cddr clauses)))))"};

void stdlib_open(ape_State *A) {
  const Function cfuncs[] = {CFUNC(cadr),  CFUNC(cddr),   CFUNC(caddr),
                             CFUNC(cdddr), CFUNC(cadddr), CFUNC(cddddr),
                             {NULL, NULL}};
  const char *stdlib[] = {defmacro, defn, cond, NULL};
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
