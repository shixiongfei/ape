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

static void on_error(ape_State *A, const char *errmsg, ape_Object *calllist) {
  ape_Object *cl = calllist;

  fprintf(stderr, "error: %s\n", errmsg);

  for (; !ape_isnil(A, cl); cl = ape_cdr(A, cl)) {
    char buf[64];
    ape_tostring(A, ape_car(A, cl), buf, sizeof(buf));
    fprintf(stderr, "=> %s\n", buf);
  }

  longjmp(toplevel, -1);
}

int main(int argc, char *argv[]) {
  ape_State *A = ape_newstate(NULL, NULL);
  FILE *fp = stdin;
  ape_Object *expr;
  int gctop;

  /* init input file */
  if (argc > 1) {
    fp = fopen(argv[1], "rb");

    if (!fp)
      ape_error(A, "could not open input file");
  }

  if (fp == stdin) {
    ape_handlers(A)->error = on_error;

    printf("Ape v%s\n\n", APE_VERSION);
  }

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

    if (fp == stdin) {
      printf("%ld|out> ", prompt);
      /* Maybe eval will take a long time.
         Let's display the prompt first. */
      fflush(stdout);
    }

    expr = ape_eval(A, expr);

    if (fp == stdin) {
      ape_writefp(A, expr, stdout);
      printf("\n");
    }
  }

  if (fp != stdin)
    fclose(fp);

  ape_close(A);
  return 0;
}
