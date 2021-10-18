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

static const char defmacro[] = {"                                              \
(def defmacro (macro (name args . body)                                        \
  (list 'def name (cons 'macro (cons args body)))))"};

static const char defn[] = {"                                                  \
(defmacro defn (name args . body)                                              \
  (list 'def name (cons 'fn (cons args body))))"};

static const char cadr[] = {"(defn cadr (lst) (car (cdr lst)))"};
static const char cddr[] = {"(defn cddr (lst) (cdr (cdr lst)))"};
static const char caddr[] = {"(defn caddr (lst) (car (cddr lst)))"};
static const char cdddr[] = {"(defn cdddr (lst) (cdr (cddr lst)))"};
static const char cadddr[] = {"(defn cadddr (lst) (car (cdddr lst)))"};
static const char cddddr[] = {"(defn cddddr (lst) (cdr (cdddr lst)))"};

static const char cond[] = {"                                                  \
(defmacro cond clauses                                                         \
  (list 'if (car clauses)                                                      \
            (cadr clauses)                                                     \
          (if (not (cdddr clauses))                                            \
              (caddr clauses)                                                  \
            (cons 'cond (cddr clauses)))))"};

void stdlib_open(ape_State *A) {
  const char *stdlib[] = {defmacro, defn,   cadr,   cddr, caddr,
                          cdddr,    cadddr, cddddr, cond, NULL};
  int gctop = ape_savegc(A);

  for (const char **lib = stdlib; *lib; lib++) {
    ape_Object *expr = ape_readstring(A, *lib);
    ape_eval(A, expr);

    ape_restoregc(A, gctop);
  }
}
