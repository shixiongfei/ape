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

static const char defn[] = {"\
(def defn (macro (name args . body) \
  (list 'def name (cons 'fn (cons args body))))) "};

void stdlib_open(ape_State *A) {
  const char *stdlib[] = {defn, NULL};
  int gctop = ape_savegc(A);

  for (const char **lib = stdlib; *lib; lib++) {
    ape_Object *expr = ape_readstring(A, *lib);
    ape_eval(A, expr);

    ape_restoregc(A, gctop);
  }
}
