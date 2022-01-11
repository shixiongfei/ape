/*
 * main.c
 *
 * Copyright (c) 2021-2022 Xiongfei Shi
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

static void on_error(ape_State *A, const char *errmsg, ape_Object calllist) {
  ape_Object cl = calllist;

  fprintf(stderr, "error: %s\n", errmsg);

  for (; !ape_isnil(A, cl); cl = ape_cdr(A, cl)) {
    char buf[128];
    ape_tostring(A, ape_car(A, cl), buf, sizeof(buf));
    fprintf(stderr, "=> %s\n", buf);
  }

  longjmp(toplevel, -1);
}

static int do_file(const char *filename) {
  ape_State *A = ape_newstate(NULL, NULL);
  ape_load(A, filename, NULL);
  ape_close(A);
  return 0;
}

static int do_repl(void) {
  ape_State *A = ape_newstate(NULL, NULL);
  ape_Object expr;

  ape_handlers(A)->error = on_error;
  printf("Welcome to Ape v%s\n", APE_RELEASE);

  setjmp(toplevel);

  /* repl */
  for (;;) {
    printf("%ld| in> ", ++prompt);

    expr = ape_readfp(A, stdin);

    if (!expr)
      break;

    printf("%ld|out> ", prompt);
    /* Maybe eval will take a long time.
       Let's display the prompt first. */
    fflush(stdout);

    expr = ape_eval(A, expr, NULL);

    ape_writefp(A, expr, stdout);
    printf("\n");
  }

  ape_close(A);
  return 0;
}

int main(int argc, char *argv[]) {
  if (argc > 1)
    return do_file(argv[1]);
  return do_repl();
}
