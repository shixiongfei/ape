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

static const char cond[] = {"                                                  \
(defmacro cond clauses                                                         \
  (if (cdr clauses)                                                            \
      (list 'if (car clauses)                                                  \
                (car (cdr clauses))                                            \
               (apply cond (cdr (cdr clauses))))                               \
    (car clauses)))"};

void stdlib_open(ape_State *A) {
  const char *stdlib[] = {defmacro, defn, cond, NULL};
  int gctop = ape_savegc(A);

  for (const char **lib = stdlib; *lib; lib++) {
    ape_Object *expr = ape_readstring(A, *lib);
    ape_eval(A, expr);

    ape_restoregc(A, gctop);
  }
}
