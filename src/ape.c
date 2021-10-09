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
#include <stdlib.h>

#define ape_unused(x) ((void)x)

struct ape_State {
  ape_Alloc alloc;
  void *ud;
};

#define ape_realloc(A, p, n) A->alloc(A->ud, p, n)
#define ape_malloc(A, n) ape_realloc(A, NULL, n)
#define ape_free(A, p) ape_realloc(A, p, 0)

static void *alloc_emul(void *ud, void *ptr, size_t size) {
  ape_unused(ud);

  if (size)
    return realloc(ptr, size);

  free(ptr);
  return NULL;
}

ape_State *ape_newstate(ape_Alloc f, void *ud) {
  ape_Alloc alloc = f ? f : alloc_emul;
  ape_State *A = (ape_State *)alloc(ud, NULL, sizeof(ape_State));

  if (!A)
    return NULL;

  A->alloc = alloc;
  A->ud = ud;

  return A;
}

void ape_close(ape_State *A) { ape_free(A, A); }
