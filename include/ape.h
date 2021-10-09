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

typedef struct ape_State ape_State;

typedef void *(*ape_Alloc)(void *ud, void *ptr, size_t size);

APE_API ape_State *ape_newstate(ape_Alloc f, void *ud);
APE_API void ape_close(ape_State *A);

#ifdef __cplusplus
};
#endif

#endif /* __APE_H__ */
