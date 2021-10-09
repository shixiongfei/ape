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

#include "ape.h"
#include <stdio.h>

int main(int argc, char *argv[]) {
  ape_State *A = ape_newstate(NULL, NULL);

  printf("Ape v%s\n", APE_VERSION);

  ape_close(A);
  return 0;
}
