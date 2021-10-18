/*
 * main.c
 *
 * Copyright (c) 2021 Xiongfei Shi
 *
 * Author: Xiongfei Shi <xiongfei.shi(a)icloud.com>
 * License: Apache-2.0
 *
 * https://github.com/shixiongfei/ape
 */

#include <setjmp.h>
#include <stdio.h>

#include "ape.h"

static long prompt = 0;
static jmp_buf toplevel;

static void on_error(ape_State *A, const char *msg, ape_Object *call_list) {
  fprintf(stderr, "error: %s\n", msg);
  longjmp(toplevel, -1);
}

int main(int argc, char *argv[]) {
  ape_State *A = ape_newstate(NULL, NULL);
  FILE *fp = stdin;
  ape_Object *expr;
  int gctop;

  printf("Ape v%s\n\n", APE_VERSION);

  /* init input file */
  if (argc > 1) {
    fp = fopen(argv[1], "rb");

    if (!fp)
      ape_error(A, "could not open input file");
  }

  if (fp == stdin)
    ape_handlers(A)->error = on_error;

  gctop = ape_savegc(A);
  setjmp(toplevel);

  /* repl */
  for (;;) {
    ape_restoregc(A, gctop);
    prompt += 1;

    if (fp == stdin)
      printf("%ld| in> ", prompt);

    expr = ape_readfp(A, fp);

    if (!expr)
      break;

    expr = ape_eval(A, expr);

    if (fp == stdin) {
      printf("%ld|out> ", prompt);
      ape_writefp(A, expr, stdout);
      printf("\n");
    }
  }

  if (fp != stdin)
    fclose(fp);

  ape_close(A);
  return 0;
}
