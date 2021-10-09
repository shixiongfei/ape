/*
 * ape.h
 *
 * Copyright (c) 2021 Xiongfei Shi
 *
 * Author: Xiongfei Shi <xiongfei.shi(a)icloud.com>
 * License: Apache-2.0
 *
 * https://github.com/shixiongfei/ape
 */

#ifndef __APE_H__
#define __APE_H__

#include <stddef.h>
#include <stdint.h>

#define APE_MAJOR 0
#define APE_MINOR 1
#define APE_PATCH 0

#define APE__STR(x) #x
#define APE_STR(x) APE__STR(x)

#define APE_VERMAJOR APE_STR(APE_MAJOR)
#define APE_VERMINOR APE_STR(APE_MINOR)
#define APE_VERPATCH APE_STR(APE_PATCH)

#define APE_VERNUM ((APE_MAJOR * 100) + APE_MINOR)
#define APE_VERFULL ((APE_VERNUM * 100) + APE_PATCH)
#define APE_VERSION (APE_VERMAJOR "." APE_VERMINOR "." APE_VERPATCH)

#define APE_API extern

#ifdef __cplusplus
extern "C" {
#endif

typedef int64_t ape_Integer;
typedef double ape_Number;
typedef struct ape_Object ape_Object;
typedef struct ape_State ape_State;
typedef ape_Object *(*ape_CFunc)(ape_State *A, ape_Object *args);

typedef void (*ape_ErrorFunc)(ape_State *A, const char *errmsg, ape_Object *cl);
typedef void (*ape_WriteFunc)(ape_State *A, void *udata, char ch);
typedef char (*ape_ReadFunc)(ape_State *A, void *udata);

typedef void *(*ape_Alloc)(void *ud, void *ptr, size_t size);

typedef struct {
  ape_ErrorFunc error;
  ape_CFunc mark;
  ape_CFunc gc;
} ape_Handlers;

enum {
  APE_TPAIR,
  APE_TFREE,
  APE_TNIL,
  APE_INTEGER,
  APE_TNUMBER,
  APE_TSYMBOL,
  APE_TSTRING,
  APE_TFUNC,
  APE_TMACRO,
  APE_TPRIM,
  APE_TCFUNC,
  APE_TPTR
};

APE_API ape_State *ape_newstate(ape_Alloc f, void *ud);
APE_API void ape_close(ape_State *A);

APE_API ape_Handlers *ape_handlers(ape_State *A);

#ifdef __cplusplus
};
#endif

#endif /* __APE_H__ */
