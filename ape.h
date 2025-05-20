/*
 * ape.h
 *
 * Copyright (c) 2021-2025 Xiongfei Shi
 *
 * Author: Xiongfei Shi <xiongfei.shi(a)icloud.com>
 * License: Apache-2.0
 *
 * https://github.com/shixiongfei/ape
 */

#ifndef __APE_H__
#define __APE_H__

#include <stdio.h>

#define APE_MAJOR 0
#define APE_MINOR 3
#define APE_PATCH 2

#define APE__STR(x) #x
#define APE_STR(x) APE__STR(x)

#define APE_VERMAJOR APE_STR(APE_MAJOR)
#define APE_VERMINOR APE_STR(APE_MINOR)
#define APE_VERPATCH APE_STR(APE_PATCH)

#define APE_VERNUM ((APE_MAJOR * 100) + APE_MINOR)
#define APE_VERFULL ((APE_VERNUM * 100) + APE_PATCH)
#define APE_VERSION APE_VERMAJOR "." APE_VERMINOR
#define APE_RELEASE APE_VERSION "." APE_VERPATCH

#define APE_API extern

#define APE_SYMSIZE 64

#define APE__PASTE(name, id) name##id
#define APE_PASTE(name, id) APE__PASTE(name, id)

#define APE_GCROOTEND ((void *)-1)
#define APE_GCUNIQUE(name) APE_PASTE(name, __LINE__)

#define APE_GCDEFROOTS(A, size)                                                \
  ape_Object APE_GCUNIQUE(roots_)[size + 2] = {NULL};                          \
  ape_State APE_GCUNIQUE(state_) = {A->ctx, APE_GCUNIQUE(roots_)}

#define APE_GCDEFVAR(var, idx)                                                 \
  ape_Object *var = (ape_Object *)(APE_GCUNIQUE(roots_) + idx);

#define APE_GCROOTS(A, size)                                                   \
  APE_GCUNIQUE(roots_)[0] = (ape_Object)A->roots;                              \
  APE_GCUNIQUE(roots_)[size + 1] = APE_GCROOTEND;                              \
  A = &APE_GCUNIQUE(state_)

#define ape_defvar1(A, var1)                                                   \
  APE_GCDEFROOTS(A, 1);                                                        \
  APE_GCDEFVAR(var1, 1);                                                       \
  APE_GCROOTS(A, 1)

#define ape_defvar2(A, var1, var2)                                             \
  APE_GCDEFROOTS(A, 2);                                                        \
  APE_GCDEFVAR(var1, 1);                                                       \
  APE_GCDEFVAR(var2, 2);                                                       \
  APE_GCROOTS(A, 2)

#define ape_defvar3(A, var1, var2, var3)                                       \
  APE_GCDEFROOTS(A, 3);                                                        \
  APE_GCDEFVAR(var1, 1);                                                       \
  APE_GCDEFVAR(var2, 2);                                                       \
  APE_GCDEFVAR(var3, 3);                                                       \
  APE_GCROOTS(A, 3)

#define ape_defvar4(A, var1, var2, var3, var4)                                 \
  APE_GCDEFROOTS(A, 4);                                                        \
  APE_GCDEFVAR(var1, 1);                                                       \
  APE_GCDEFVAR(var2, 2);                                                       \
  APE_GCDEFVAR(var3, 3);                                                       \
  APE_GCDEFVAR(var4, 4);                                                       \
  APE_GCROOTS(A, 4)

#define ape_defvar5(A, var1, var2, var3, var4, var5)                           \
  APE_GCDEFROOTS(A, 5);                                                        \
  APE_GCDEFVAR(var1, 1);                                                       \
  APE_GCDEFVAR(var2, 2);                                                       \
  APE_GCDEFVAR(var3, 3);                                                       \
  APE_GCDEFVAR(var4, 4);                                                       \
  APE_GCDEFVAR(var5, 5);                                                       \
  APE_GCROOTS(A, 5)

#define ape_defvar6(A, var1, var2, var3, var4, var5, var6)                     \
  APE_GCDEFROOTS(A, 6);                                                        \
  APE_GCDEFVAR(var1, 1);                                                       \
  APE_GCDEFVAR(var2, 2);                                                       \
  APE_GCDEFVAR(var3, 3);                                                       \
  APE_GCDEFVAR(var4, 4);                                                       \
  APE_GCDEFVAR(var5, 5);                                                       \
  APE_GCDEFVAR(var6, 6);                                                       \
  APE_GCROOTS(A, 6)

#define ape_defvar7(A, var1, var2, var3, var4, var5, var6, var7)               \
  APE_GCDEFROOTS(A, 7);                                                        \
  APE_GCDEFVAR(var1, 1);                                                       \
  APE_GCDEFVAR(var2, 2);                                                       \
  APE_GCDEFVAR(var3, 3);                                                       \
  APE_GCDEFVAR(var4, 4);                                                       \
  APE_GCDEFVAR(var5, 5);                                                       \
  APE_GCDEFVAR(var6, 6);                                                       \
  APE_GCDEFVAR(var7, 7);                                                       \
  APE_GCROOTS(A, 7)

#define ape_defvar8(A, var1, var2, var3, var4, var5, var6, var7, var8)         \
  APE_GCDEFROOTS(A, 8);                                                        \
  APE_GCDEFVAR(var1, 1);                                                       \
  APE_GCDEFVAR(var2, 2);                                                       \
  APE_GCDEFVAR(var3, 3);                                                       \
  APE_GCDEFVAR(var4, 4);                                                       \
  APE_GCDEFVAR(var5, 5);                                                       \
  APE_GCDEFVAR(var6, 6);                                                       \
  APE_GCDEFVAR(var7, 7);                                                       \
  APE_GCDEFVAR(var8, 8);                                                       \
  APE_GCROOTS(A, 8)

#define ape_defvar9(A, var1, var2, var3, var4, var5, var6, var7, var8, var9)   \
  APE_GCDEFROOTS(A, 9);                                                        \
  APE_GCDEFVAR(var1, 1);                                                       \
  APE_GCDEFVAR(var2, 2);                                                       \
  APE_GCDEFVAR(var3, 3);                                                       \
  APE_GCDEFVAR(var4, 4);                                                       \
  APE_GCDEFVAR(var5, 5);                                                       \
  APE_GCDEFVAR(var6, 6);                                                       \
  APE_GCDEFVAR(var7, 7);                                                       \
  APE_GCDEFVAR(var8, 8);                                                       \
  APE_GCDEFVAR(var9, 9);                                                       \
  APE_GCROOTS(A, 9)

#ifdef __cplusplus
extern "C" {
#endif

typedef struct ape_Cell ape_Cell, *ape_Object;
typedef struct ape_Context ape_Context;

typedef struct ape_State {
  ape_Context *ctx;
  ape_Object *roots;
} ape_State;

typedef ape_Object (*ape_CFunc)(ape_State *A, int argc, ape_Object args,
                                ape_Object env);
typedef void (*ape_GCFunc)(ape_State *A, void *ptr, int subtype);
typedef void (*ape_ErrorFunc)(ape_State *A, const char *errmsg, ape_Object cl);
typedef void (*ape_WriteFunc)(ape_State *A, void *udata, char ch);
typedef char (*ape_ReadFunc)(ape_State *A, void *udata);

typedef void *(*ape_Alloc)(void *ud, void *ptr, size_t size);

typedef struct {
  ape_ErrorFunc error;
  ape_GCFunc gc;
} ape_Handlers;

enum {
  APE_TPAIR,
  APE_TFORWARD,
  APE_TNIL,
  APE_TNUMBER,
  APE_TSYMBOL,
  APE_TSTRING,
  APE_TVECTOR,
  APE_TFUNC,
  APE_TMACRO,
  APE_TPRIM,
  APE_TCFUNC,
  APE_TPTR,
  APE_TMAX
};

APE_API ape_State *ape_newstate(ape_Alloc f, void *ud);
APE_API void ape_close(ape_State *A);

APE_API ape_Handlers *ape_handlers(ape_State *A);
APE_API int ape_error(ape_State *A, const char *format, ...);

APE_API int ape_type(ape_State *A, ape_Object obj);
APE_API int ape_isnil(ape_State *A, ape_Object obj);
APE_API int ape_equal(ape_State *A, ape_Object a, ape_Object b);

APE_API ape_Object ape_checktype(ape_State *A, ape_Object obj, int type);
APE_API ape_Object ape_cons(ape_State *A, ape_Object car, ape_Object cdr);
APE_API ape_Object ape_car(ape_State *A, ape_Object obj);
APE_API ape_Object ape_cdr(ape_State *A, ape_Object obj);
APE_API ape_Object ape_setcar(ape_State *A, ape_Object obj, ape_Object car);
APE_API ape_Object ape_setcdr(ape_State *A, ape_Object obj, ape_Object cdr);
APE_API ape_Object ape_nil(ape_State *A);
APE_API ape_Object ape_true(ape_State *A);
APE_API ape_Object ape_bool(ape_State *A, int b);
APE_API ape_Object ape_integer(ape_State *A, long long n);
APE_API ape_Object ape_number(ape_State *A, double n);
APE_API ape_Object ape_string(ape_State *A, const char *str);
APE_API ape_Object ape_lstring(ape_State *A, const char *str, int len);
APE_API ape_Object ape_symbol(ape_State *A, const char *name);
APE_API ape_Object ape_vector(ape_State *A, int len);
APE_API ape_Object ape_vecset(ape_State *A, ape_Object vec, int pos,
                              ape_Object obj);
APE_API ape_Object ape_cfunc(ape_State *A, ape_CFunc fn);
APE_API ape_Object ape_ptr(ape_State *A, void *ptr, int subtype);
APE_API ape_Object ape_gensym(ape_State *A);
APE_API ape_Object ape_strappend(ape_State *A, ape_Object objs);
APE_API ape_Object ape_strreverse(ape_State *A, ape_Object obj);

APE_API ape_Object ape_vecref(ape_State *A, ape_Object obj, int idx);
APE_API int ape_strref(ape_State *A, ape_Object obj, int idx);

APE_API int ape_strlen(ape_State *A, ape_Object obj);
APE_API int ape_veclen(ape_State *A, ape_Object obj);

APE_API long long ape_tointeger(ape_State *A, ape_Object obj);
APE_API double ape_tonumber(ape_State *A, ape_Object obj);
APE_API int ape_tostring(ape_State *A, ape_Object obj, char *dst, int size);
APE_API int ape_ptrtype(ape_State *A, ape_Object obj);
APE_API void *ape_toptr(ape_State *A, ape_Object obj);

APE_API ape_Object ape_read(ape_State *A, ape_ReadFunc fn, void *udata);
APE_API void ape_write(ape_State *A, ape_Object obj, ape_WriteFunc fn,
                       void *udata, int strqt);

APE_API ape_Object ape_unbound(ape_State *A, ape_Object sym, ape_Object env,
                               int recur);
APE_API ape_Object ape_def(ape_State *A, ape_Object sym, ape_Object val,
                           ape_Object env);
APE_API ape_Object ape_set(ape_State *A, ape_Object sym, ape_Object val,
                           ape_Object env);
APE_API ape_Object ape_nextarg(ape_State *A, ape_Object *args);
APE_API ape_Object ape_eval(ape_State *A, ape_Object expr, ape_Object env);
APE_API ape_Object ape_load(ape_State *A, const char *file, ape_Object env);

APE_API ape_Object ape_readstring(ape_State *A, const char *str);
APE_API ape_Object ape_readfp(ape_State *A, FILE *fp);
APE_API void ape_writefp(ape_State *A, ape_Object obj, FILE *fp);

#ifdef __cplusplus
};
#endif

#endif /* __APE_H__ */
