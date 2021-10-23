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

#include <stdint.h>
#include <stdio.h>

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

/* assumption: pointers are 32 or 64 bit,
   and float/double are IEEE binary32/binary64 */
#if UINTPTR_MAX > (1ULL << 32)
typedef double floatptr_t;
#else
typedef float floatptr_t;
#endif

#ifdef __cplusplus
extern "C" {
#endif

typedef intptr_t ape_Integer;
typedef floatptr_t ape_Number;
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
  APE_TINTEGER,
  APE_TNUMBER,
  APE_TSYMBOL,
  APE_TSTRING,
  APE_TFUNC,
  APE_TMACRO,
  APE_TPRIM,
  APE_TCFUNC,
  APE_TPTR,
};

APE_API ape_State *ape_newstate(ape_Alloc f, void *ud);
APE_API void ape_close(ape_State *A);

APE_API ape_Handlers *ape_handlers(ape_State *A);
APE_API int ape_error(ape_State *A, const char *format, ...);

APE_API void ape_pushgc(ape_State *A, ape_Object *obj);
APE_API void ape_restoregc(ape_State *A, int idx);
APE_API int ape_savegc(ape_State *A);
APE_API void ape_mark(ape_State *A, ape_Object *obj);

APE_API int ape_length(ape_State *A, ape_Object *obj);
APE_API int ape_isnil(ape_State *A, ape_Object *obj);
APE_API int ape_type(ape_State *A, ape_Object *obj);

APE_API ape_Object *ape_cons(ape_State *A, ape_Object *car, ape_Object *cdr);
APE_API ape_Object *ape_car(ape_State *A, ape_Object *obj);
APE_API ape_Object *ape_cdr(ape_State *A, ape_Object *obj);
APE_API ape_Object *ape_setcar(ape_State *A, ape_Object *obj, ape_Object *car);
APE_API ape_Object *ape_setcdr(ape_State *A, ape_Object *obj, ape_Object *cdr);
APE_API ape_Object *ape_list(ape_State *A, ape_Object **objs, int cnt);
APE_API ape_Object *ape_true(ape_State *A);
APE_API ape_Object *ape_nil(ape_State *A);
APE_API ape_Object *ape_bool(ape_State *A, int b);
APE_API ape_Object *ape_integer(ape_State *A, ape_Integer d);
APE_API ape_Object *ape_number(ape_State *A, ape_Number n);
APE_API ape_Object *ape_string(ape_State *A, const char *str);
APE_API ape_Object *ape_lstring(ape_State *A, const char *str, int len);
APE_API ape_Object *ape_symbol(ape_State *A, const char *name);
APE_API ape_Object *ape_cfunc(ape_State *A, ape_CFunc fn);
APE_API ape_Object *ape_ptr(ape_State *A, void *ptr);
APE_API ape_Object *ape_gensym(ape_State *A);
APE_API ape_Object *ape_reverse(ape_State *A, ape_Object *obj);
APE_API ape_Object *ape_nth(ape_State *A, ape_Object *obj, int idx);

APE_API ape_Integer ape_tointeger(ape_State *A, ape_Object *obj);
APE_API ape_Number ape_tonumber(ape_State *A, ape_Object *obj);
APE_API int ape_tostring(ape_State *A, ape_Object *obj, char *dst, int size);
APE_API void *ape_toptr(ape_State *A, ape_Object *obj);

APE_API ape_Object *ape_read(ape_State *A, ape_ReadFunc fn, void *udata);
APE_API void ape_write(ape_State *A, ape_Object *obj, ape_WriteFunc fn,
                       void *udata, int strqt);

APE_API ape_Object *ape_def(ape_State *A, ape_Object *sym, ape_Object *val,
                            ape_Object *env);
APE_API ape_Object *ape_set(ape_State *A, ape_Object *sym, ape_Object *val,
                            ape_Object *env);
APE_API ape_Object *ape_nextarg(ape_State *A, ape_Object **args);
APE_API ape_Object *ape_eval(ape_State *A, ape_Object *expr);

APE_API ape_Object *ape_readstring(ape_State *A, const char *str);
APE_API ape_Object *ape_readfp(ape_State *A, FILE *fp);
APE_API void ape_writefp(ape_State *A, ape_Object *obj, FILE *fp);

#ifdef __cplusplus
};
#endif

#endif /* __APE_H__ */
