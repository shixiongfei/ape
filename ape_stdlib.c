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

#include <float.h>
#include <math.h>
#include <stdlib.h>

#include "ape.h"

typedef struct Function {
  const char *name;
  ape_CFunc func;
} Function;

static ape_Object caar(ape_State *A, int argc, ape_Object args,
                       ape_Object env) {
  return ape_car(A, ape_car(A, ape_nextarg(A, &args)));
}

static ape_Object cadr(ape_State *A, int argc, ape_Object args,
                       ape_Object env) {
  return ape_car(A, ape_cdr(A, ape_nextarg(A, &args)));
}

static ape_Object cdar(ape_State *A, int argc, ape_Object args,
                       ape_Object env) {
  return ape_cdr(A, ape_car(A, ape_nextarg(A, &args)));
}

static ape_Object cddr(ape_State *A, int argc, ape_Object args,
                       ape_Object env) {
  return ape_cdr(A, ape_cdr(A, ape_nextarg(A, &args)));
}

static ape_Object caaar(ape_State *A, int argc, ape_Object args,
                        ape_Object env) {
  return ape_car(A, caar(A, argc, args, env));
}

static ape_Object caadr(ape_State *A, int argc, ape_Object args,
                        ape_Object env) {
  return ape_car(A, cadr(A, argc, args, env));
}

static ape_Object cadar(ape_State *A, int argc, ape_Object args,
                        ape_Object env) {
  return ape_car(A, cdar(A, argc, args, env));
}

static ape_Object caddr(ape_State *A, int argc, ape_Object args,
                        ape_Object env) {
  return ape_car(A, cddr(A, argc, args, env));
}

static ape_Object cdaar(ape_State *A, int argc, ape_Object args,
                        ape_Object env) {
  return ape_cdr(A, caar(A, argc, args, env));
}

static ape_Object cdadr(ape_State *A, int argc, ape_Object args,
                        ape_Object env) {
  return ape_cdr(A, cadr(A, argc, args, env));
}

static ape_Object cddar(ape_State *A, int argc, ape_Object args,
                        ape_Object env) {
  return ape_cdr(A, cdar(A, argc, args, env));
}

static ape_Object cdddr(ape_State *A, int argc, ape_Object args,
                        ape_Object env) {
  return ape_cdr(A, cddr(A, argc, args, env));
}

static ape_Object caaaar(ape_State *A, int argc, ape_Object args,
                         ape_Object env) {
  return ape_car(A, caaar(A, argc, args, env));
}

static ape_Object caaadr(ape_State *A, int argc, ape_Object args,
                         ape_Object env) {
  return ape_car(A, caadr(A, argc, args, env));
}

static ape_Object caadar(ape_State *A, int argc, ape_Object args,
                         ape_Object env) {
  return ape_car(A, cadar(A, argc, args, env));
}

static ape_Object caaddr(ape_State *A, int argc, ape_Object args,
                         ape_Object env) {
  return ape_car(A, caddr(A, argc, args, env));
}

static ape_Object cadaar(ape_State *A, int argc, ape_Object args,
                         ape_Object env) {
  return ape_car(A, cdaar(A, argc, args, env));
}

static ape_Object cadadr(ape_State *A, int argc, ape_Object args,
                         ape_Object env) {
  return ape_car(A, cdadr(A, argc, args, env));
}

static ape_Object caddar(ape_State *A, int argc, ape_Object args,
                         ape_Object env) {
  return ape_car(A, cddar(A, argc, args, env));
}

static ape_Object cadddr(ape_State *A, int argc, ape_Object args,
                         ape_Object env) {
  return ape_car(A, cdddr(A, argc, args, env));
}

static ape_Object cdaaar(ape_State *A, int argc, ape_Object args,
                         ape_Object env) {
  return ape_cdr(A, caaar(A, argc, args, env));
}

static ape_Object cdaadr(ape_State *A, int argc, ape_Object args,
                         ape_Object env) {
  return ape_cdr(A, caadr(A, argc, args, env));
}

static ape_Object cdadar(ape_State *A, int argc, ape_Object args,
                         ape_Object env) {
  return ape_cdr(A, cadar(A, argc, args, env));
}

static ape_Object cdaddr(ape_State *A, int argc, ape_Object args,
                         ape_Object env) {
  return ape_cdr(A, caddr(A, argc, args, env));
}

static ape_Object cddaar(ape_State *A, int argc, ape_Object args,
                         ape_Object env) {
  return ape_cdr(A, cdaar(A, argc, args, env));
}

static ape_Object cddadr(ape_State *A, int argc, ape_Object args,
                         ape_Object env) {
  return ape_cdr(A, cdadr(A, argc, args, env));
}

static ape_Object cdddar(ape_State *A, int argc, ape_Object args,
                         ape_Object env) {
  return ape_cdr(A, cddar(A, argc, args, env));
}

static ape_Object cddddr(ape_State *A, int argc, ape_Object args,
                         ape_Object env) {
  return ape_cdr(A, cdddr(A, argc, args, env));
}

static ape_Object istype(ape_State *A, ape_Object obj, int type) {
  return ape_bool(A, ape_type(A, obj) == type);
}

static ape_Object nilp(ape_State *A, int argc, ape_Object args,
                       ape_Object env) {
  return ape_bool(A, ape_isnil(A, ape_nextarg(A, &args)));
}

static ape_Object pairp(ape_State *A, int argc, ape_Object args,
                        ape_Object env) {
  return istype(A, ape_nextarg(A, &args), APE_TPAIR);
}

static ape_Object numberp(ape_State *A, int argc, ape_Object args,
                          ape_Object env) {
  return istype(A, ape_nextarg(A, &args), APE_TNUMBER);
}

static ape_Object symbolp(ape_State *A, int argc, ape_Object args,
                          ape_Object env) {
  return istype(A, ape_nextarg(A, &args), APE_TSYMBOL);
}

static ape_Object stringp(ape_State *A, int argc, ape_Object args,
                          ape_Object env) {
  return istype(A, ape_nextarg(A, &args), APE_TSTRING);
}

static ape_Object vectorp(ape_State *A, int argc, ape_Object args,
                          ape_Object env) {
  return istype(A, ape_nextarg(A, &args), APE_TVECTOR);
}

static ape_Object fnp(ape_State *A, int argc, ape_Object args, ape_Object env) {
  return istype(A, ape_nextarg(A, &args), APE_TFUNC);
}

static ape_Object macrop(ape_State *A, int argc, ape_Object args,
                         ape_Object env) {
  return istype(A, ape_nextarg(A, &args), APE_TMACRO);
}

static ape_Object primitivep(ape_State *A, int argc, ape_Object args,
                             ape_Object env) {
  return istype(A, ape_nextarg(A, &args), APE_TPRIM);
}

static ape_Object cfnp(ape_State *A, int argc, ape_Object args,
                       ape_Object env) {
  return istype(A, ape_nextarg(A, &args), APE_TCFUNC);
}

static ape_Object ptrp(ape_State *A, int argc, ape_Object args,
                       ape_Object env) {
  return istype(A, ape_nextarg(A, &args), APE_TPTR);
}

static ape_Object print(ape_State *A, int argc, ape_Object args,
                        ape_Object env) {
  while (!ape_isnil(A, args)) {
    ape_writefp(A, ape_nextarg(A, &args), stdout);

    if (!ape_isnil(A, args))
      printf(" ");
  }
  printf("\n");
  return ape_nil(A);
}

static ape_Object eval(ape_State *A, int argc, ape_Object args,
                       ape_Object env) {
  return ape_eval(A, ape_nextarg(A, &args), env);
}

static ape_Object load(ape_State *A, int argc, ape_Object args,
                       ape_Object env) {
  char filename[260] = {0};
  ape_tostring(A, ape_nextarg(A, &args), filename, sizeof(filename) - 1);
  return ape_load(A, filename, env);
}

static ape_Object number(ape_State *A, int argc, ape_Object args,
                         ape_Object env) {
  ape_Object str = ape_checktype(A, ape_nextarg(A, &args), APE_TSTRING);
  int len = ape_length(A, str);
  char *p, buf[APE_SYMSIZE] = {0};
  double n;

  if (len >= APE_SYMSIZE)
    ape_error(A, "number string too long");

  ape_tostring(A, str, buf, sizeof(buf) - 1);
  n = strtod(buf, &p);

  if ((int)(p - buf) != len)
    ape_error(A, "not a number string");

  return ape_number(A, n);
}

static ape_Object string(ape_State *A, int argc, ape_Object args,
                         ape_Object env) {
  ape_Object obj = ape_nextarg(A, &args);
  char buf[APE_SYMSIZE] = {0};

  switch (ape_type(A, obj)) {
  case APE_TSTRING:
    return obj;
  case APE_TNUMBER:
  case APE_TSYMBOL:
    ape_tostring(A, obj, buf, sizeof(buf) - 1);
    return ape_string(A, buf);
  default:
    ape_error(A, "type cannot convert to string");
    break;
  }
  return ape_nil(A);
}

static ape_Object string_tolist(ape_State *A, int argc, ape_Object args,
                                ape_Object env) {
  ape_Object str = ape_checktype(A, ape_nextarg(A, &args), APE_TSTRING);
  ape_Object list = ape_nil(A);
  int i = ape_length(A, str);
  int gctop = ape_savegc(A);

  while (i--) {
    ape_restoregc(A, gctop);
    list = ape_cons(A, ape_nth(A, str, i), list);
  }
  return list;
}

static ape_Object string_tovector(ape_State *A, int argc, ape_Object args,
                                  ape_Object env) {
  ape_Object str = ape_checktype(A, ape_nextarg(A, &args), APE_TSTRING);
  ape_Object vec = ape_vector(A, ape_length(A, str));
  int i;

  for (i = 0; i < ape_length(A, vec); ++i)
    ape_vecset(A, vec, i, ape_nth(A, str, i));

  return vec;
}

static ape_Object list(ape_State *A, int argc, ape_Object args,
                       ape_Object env) {
  return args;
}

static ape_Object concat(ape_State *A, int argc, ape_Object args,
                         ape_Object env) {
  return ape_concat(A, args);
}

static ape_Object length(ape_State *A, int argc, ape_Object args,
                         ape_Object env) {
  return ape_integer(A, ape_length(A, ape_nextarg(A, &args)));
}

static ape_Object reverse(ape_State *A, int argc, ape_Object args,
                          ape_Object env) {
  return ape_reverse(A, ape_nextarg(A, &args));
}

static ape_Object nth(ape_State *A, int argc, ape_Object args, ape_Object env) {
  int index = (int)ape_tointeger(A, ape_nextarg(A, &args));
  return ape_nth(A, ape_nextarg(A, &args), index);
}

static ape_Object append(ape_State *A, int argc, ape_Object args,
                         ape_Object env) {
  return ape_append(A, args);
}

static ape_Object assoc(ape_State *A, int argc, ape_Object args,
                        ape_Object env) {
  ape_Object k = ape_nextarg(A, &args);
  ape_Object l = ape_checktype(A, ape_nextarg(A, &args), APE_TPAIR);
  ape_Object p;

  for (; !ape_isnil(A, l); l = ape_cdr(A, l)) {
    p = ape_car(A, l);

    if (ape_equal(A, k, ape_car(A, p)))
      return p;
  }
  return ape_nil(A);
}

static ape_Object get(ape_State *A, int argc, ape_Object args, ape_Object env) {
  ape_Object l = ape_checktype(A, ape_nextarg(A, &args), APE_TPAIR);
  ape_Object k = ape_nextarg(A, &args);

  while (!ape_isnil(A, l)) {
    if (ape_equal(A, k, ape_car(A, l)))
      return ape_car(A, ape_cdr(A, l));

    l = ape_cdr(A, ape_cdr(A, l));
  }
  return !ape_isnil(A, args) ? ape_nextarg(A, &args) : ape_nil(A);
}

static ape_Object symbol(ape_State *A, int argc, ape_Object args,
                         ape_Object env) {
  ape_Object str = ape_checktype(A, ape_nextarg(A, &args), APE_TSTRING);
  char buf[APE_SYMSIZE] = {0};

  if (ape_length(A, str) >= APE_SYMSIZE)
    ape_error(A, "symbol too long");

  ape_tostring(A, str, buf, sizeof(buf) - 1);
  return ape_symbol(A, buf);
}

static ape_Object gensym(ape_State *A, int argc, ape_Object args,
                         ape_Object env) {
  return ape_gensym(A);
}

static ape_Object make_vector(ape_State *A, int argc, ape_Object args,
                              ape_Object env) {
  return ape_vector(A, (int)ape_tointeger(A, ape_nextarg(A, &args)));
}

static ape_Object vector_tolist(ape_State *A, int argc, ape_Object args,
                                ape_Object env) {
  ape_Object vec = ape_checktype(A, ape_nextarg(A, &args), APE_TVECTOR);
  ape_Object list = ape_nil(A);
  int i = ape_length(A, vec);
  int gctop = ape_savegc(A);

  while (i--) {
    ape_restoregc(A, gctop);
    list = ape_cons(A, ape_nth(A, vec, i), list);
  }
  return list;
}

static ape_Object list_tovector(ape_State *A, int argc, ape_Object args,
                                ape_Object env) {
  ape_Object list = ape_checktype(A, ape_nextarg(A, &args), APE_TPAIR);
  ape_Object vec = ape_vector(A, ape_length(A, list));
  int i;

  for (i = 0; i < ape_length(A, vec); ++i)
    ape_vecset(A, vec, i, ape_nextarg(A, &list));

  return vec;
}

static ape_Object rem(ape_State *A, int argc, ape_Object args, ape_Object env) {
  double a, b;

  a = ape_tonumber(A, ape_nextarg(A, &args));
  b = ape_tonumber(A, ape_nextarg(A, &args));

  return ape_number(A, fmod(a, b));
}

static ape_Object round_(ape_State *A, int argc, ape_Object args,
                         ape_Object env) {
  double x = ape_tonumber(A, ape_nextarg(A, &args));
  int n = ape_isnil(A, args) ? 0 : (int)ape_tointeger(A, ape_nextarg(A, &args));
  double p, y, z;

  if (n == 0)
    return ape_number(A, round(x));

  p = pow(10, n);
  y = x * p;
  z = round(y);

  if ((fabs(z - y) - 0.5) < DBL_EPSILON)
    return ape_number(A, (2.0 * round(y / 2.0)) / p);

  return ape_number(A, z / p);
}

static ape_Object nanp(ape_State *A, int argc, ape_Object args,
                       ape_Object env) {
  return ape_bool(A, isnan(ape_tonumber(A, ape_nextarg(A, &args))));
}

static ape_Object infp(ape_State *A, int argc, ape_Object args,
                       ape_Object env) {
  return ape_bool(A, isinf(ape_tonumber(A, ape_nextarg(A, &args))));
}

static ape_Object abs_(ape_State *A, int argc, ape_Object args,
                       ape_Object env) {
  return ape_number(A, fabs(ape_tonumber(A, ape_nextarg(A, &args))));
}

static ape_Object acos_(ape_State *A, int argc, ape_Object args,
                        ape_Object env) {
  return ape_number(A, acos(ape_tonumber(A, ape_nextarg(A, &args))));
}

static ape_Object acosh_(ape_State *A, int argc, ape_Object args,
                         ape_Object env) {
  return ape_number(A, acosh(ape_tonumber(A, ape_nextarg(A, &args))));
}

static ape_Object asin_(ape_State *A, int argc, ape_Object args,
                        ape_Object env) {
  return ape_number(A, asin(ape_tonumber(A, ape_nextarg(A, &args))));
}

static ape_Object asinh_(ape_State *A, int argc, ape_Object args,
                         ape_Object env) {
  return ape_number(A, asinh(ape_tonumber(A, ape_nextarg(A, &args))));
}

static ape_Object atan_(ape_State *A, int argc, ape_Object args,
                        ape_Object env) {
  return ape_number(A, atan(ape_tonumber(A, ape_nextarg(A, &args))));
}

static ape_Object atan2_(ape_State *A, int argc, ape_Object args,
                         ape_Object env) {
  double y = ape_tonumber(A, ape_nextarg(A, &args));
  double x = ape_tonumber(A, ape_nextarg(A, &args));
  return ape_number(A, atan2(y, x));
}

static ape_Object atanh_(ape_State *A, int argc, ape_Object args,
                         ape_Object env) {
  return ape_number(A, atanh(ape_tonumber(A, ape_nextarg(A, &args))));
}

static ape_Object cos_(ape_State *A, int argc, ape_Object args,
                       ape_Object env) {
  return ape_number(A, cos(ape_tonumber(A, ape_nextarg(A, &args))));
}

static ape_Object cosh_(ape_State *A, int argc, ape_Object args,
                        ape_Object env) {
  return ape_number(A, cosh(ape_tonumber(A, ape_nextarg(A, &args))));
}

static ape_Object sin_(ape_State *A, int argc, ape_Object args,
                       ape_Object env) {
  return ape_number(A, sin(ape_tonumber(A, ape_nextarg(A, &args))));
}

static ape_Object sinh_(ape_State *A, int argc, ape_Object args,
                        ape_Object env) {
  return ape_number(A, sinh(ape_tonumber(A, ape_nextarg(A, &args))));
}

static ape_Object tan_(ape_State *A, int argc, ape_Object args,
                       ape_Object env) {
  return ape_number(A, tan(ape_tonumber(A, ape_nextarg(A, &args))));
}

static ape_Object tanh_(ape_State *A, int argc, ape_Object args,
                        ape_Object env) {
  return ape_number(A, tanh(ape_tonumber(A, ape_nextarg(A, &args))));
}

static ape_Object exp_(ape_State *A, int argc, ape_Object args,
                       ape_Object env) {
  return ape_number(A, exp(ape_tonumber(A, ape_nextarg(A, &args))));
}

static ape_Object log_(ape_State *A, int argc, ape_Object args,
                       ape_Object env) {
  return ape_number(A, log(ape_tonumber(A, ape_nextarg(A, &args))));
}

static ape_Object log10_(ape_State *A, int argc, ape_Object args,
                         ape_Object env) {
  return ape_number(A, log10(ape_tonumber(A, ape_nextarg(A, &args))));
}

static ape_Object pow_(ape_State *A, int argc, ape_Object args,
                       ape_Object env) {
  double x = ape_tonumber(A, ape_nextarg(A, &args));
  double y = ape_tonumber(A, ape_nextarg(A, &args));
  return ape_number(A, pow(x, y));
}

static ape_Object sqrt_(ape_State *A, int argc, ape_Object args,
                        ape_Object env) {
  return ape_number(A, sqrt(ape_tonumber(A, ape_nextarg(A, &args))));
}

static ape_Object ceil_(ape_State *A, int argc, ape_Object args,
                        ape_Object env) {
  return ape_number(A, ceil(ape_tonumber(A, ape_nextarg(A, &args))));
}

static ape_Object floor_(ape_State *A, int argc, ape_Object args,
                         ape_Object env) {
  return ape_number(A, floor(ape_tonumber(A, ape_nextarg(A, &args))));
}

static ape_Object unbound(ape_State *A, int argc, ape_Object args,
                          ape_Object env) {
  return ape_unbound(A, ape_nextarg(A, &args), env, 1);
}

static const char defmacro[] = {"                                              \
(def defmacro (macro (name args . body)                                        \
  `(def ,name (macro ,args ,@body))))"};

static const char defn[] = {"                                                  \
(defmacro defn (name args . body)                                              \
  `(def ,name (fn ,args ,@body)))"};

static const char let[] = {"                                                   \
(defmacro let (binds . body)                                                   \
  ((fn ()                                                                      \
     (def args nil)                                                            \
     (def vals nil)                                                            \
     (while binds                                                              \
       (set! args (cons (car binds) args))                                     \
       (set! vals (cons (cadr binds) vals))                                    \
       (set! binds (cddr binds)))                                              \
     `((fn ,(reverse args)                                                     \
         ,@body)                                                               \
       ,@(reverse vals)))))"};

static const char cond[] = {"                                                  \
(defmacro cond clauses                                                         \
  `(if ,(car clauses)                                                          \
       ,(cadr clauses)                                                         \
     ,(if (not (cdddr clauses))                                                \
          (caddr clauses)                                                      \
        `(cond ,@(cddr clauses)))))"};

static const char apply[] = {"                                                 \
(defn apply (op args)                                                          \
  (eval (cons op args)))"};

static const char when[] = {"                                                  \
(defmacro when (test . body)                                                   \
  `(if ,test (do ,@body) nil))"};

static const char unless[] = {"                                                \
(defmacro unless (test . body)                                                 \
  `(if (not ,test) (do ,@body) nil))"};

static const char while_[] = {"                                                \
(defmacro while (test . body)                                                  \
  `(when ,test                                                                 \
     ,@body                                                                    \
     (while ,test ,@body)))"};

static const char for_[] = {"                                                  \
(defmacro for (item list . body)                                               \
  (let (iter (gensym))                                                         \
    `(let (,iter ,list)                                                        \
       (while ,iter                                                            \
         (let (,item (car ,iter))                                              \
           (set! ,iter (cdr ,iter))                                            \
           ,@body)))))"};

static const char map[] = {"                                                   \
(defn map (proc list)                                                          \
  (def res nil)                                                                \
  (for x list                                                                  \
    (set! res (cons (proc x) res)))                                            \
  (reverse res))"};

static const char filter[] = {"                                                \
(defn filter (proc list)                                                       \
  (def res nil)                                                                \
  (for x list                                                                  \
    (when (proc x)                                                             \
      (set! res (cons x res))))                                                \
  (reverse res))"};

static const char reduce[] = {"                                                \
(defn reduce (proc accum list)                                                 \
  (for x list                                                                  \
    (set! accum (proc accum x)))                                               \
  accum)"};

static const char acons[] = {"                                                 \
(defn acons (a b l)                                                            \
  (cons (cons a b) l))"};

static const char push[] = {"                                                  \
(defmacro push (val place)                                                     \
  `(set! ,place (cons ,val ,place)))"};

static const char pop[] = {"                                                   \
(defmacro pop (place)                                                          \
  (let (gx (gensym))                                                           \
    `(let (,gx (car ,place))                                                   \
       (set! ,place (cdr ,place))                                              \
       ,gx)))"};

void stdlib_open(ape_State *A) {
  const Function cfuncs[] = {
      {"caar", caar},
      {"cadr", cadr},
      {"cdar", cdar},
      {"cddr", cddr},
      {"caaar", caaar},
      {"caadr", caadr},
      {"cadar", cadar},
      {"caddr", caddr},
      {"cdaar", cdaar},
      {"cdadr", cdadr},
      {"cddar", cddar},
      {"cdddr", cdddr},
      {"caaaar", caaaar},
      {"caaadr", caaadr},
      {"caadar", caadar},
      {"caaddr", caaddr},
      {"cadaar", cadaar},
      {"cadadr", cadadr},
      {"caddar", caddar},
      {"cadddr", cadddr},
      {"cdaaar", cdaaar},
      {"cdaadr", cdaadr},
      {"cdadar", cdadar},
      {"cdaddr", cdaddr},
      {"cddaar", cddaar},
      {"cddadr", cddadr},
      {"cdddar", cdddar},
      {"cddddr", cddddr},
      {"nil?", nilp},
      {"pair?", pairp},
      {"number?", numberp},
      {"symbol?", symbolp},
      {"string?", stringp},
      {"vector?", vectorp},
      {"fn?", fnp},
      {"macro?", macrop},
      {"primitive?", primitivep},
      {"cfn?", cfnp},
      {"ptr?", ptrp},
      {"print", print},
      {"eval", eval},
      {"load", load},
      {"number", number},
      {"string", string},
      {"string->list", string_tolist},
      {"string->vector", string_tovector},
      {"list", list},
      {"concat", concat},
      {"length", length},
      {"reverse", reverse},
      {"nth", nth},
      {"append", append},
      {"assoc", assoc},
      {"get", get},
      {"symbol", symbol},
      {"gensym", gensym},
      {"make-vector", make_vector},
      {"vector->list", vector_tolist},
      {"list->vector", list_tovector},
      {"rem", rem},
      {"round", round_},
      {"nan?", nanp},
      {"inf?", infp},
      {"abs", abs_},
      {"acos", acos_},
      {"acosh", acosh_},
      {"asin", asin_},
      {"asinh", asinh_},
      {"atan", atan_},
      {"atan2", atan2_},
      {"atanh", atanh_},
      {"cos", cos_},
      {"cosh", cosh_},
      {"sin", sin_},
      {"sinh", sinh_},
      {"tan", tan_},
      {"tanh", tanh_},
      {"exp", exp_},
      {"log", log_},
      {"log10", log10_},
      {"pow", pow_},
      {"sqrt", sqrt_},
      {"ceil", ceil_},
      {"floor", floor_},
      {"unbound", unbound},
      {NULL, NULL},
  };
  const char *stdlib[] = {
      defmacro, defn, let,    cond,   apply, when, unless, while_,
      for_,     map,  filter, reduce, acons, push, pop,    NULL,
  };
  int gctop = ape_savegc(A);

  /* c libs */
  for (const Function *func = cfuncs; func->name && func->func; ++func) {
    ape_def(A, ape_symbol(A, func->name), ape_cfunc(A, func->func), NULL);
    ape_restoregc(A, gctop);
  }

  /* ape libs */
  for (const char **lib = stdlib; *lib; ++lib) {
    ape_Object expr = ape_readstring(A, *lib);
    ape_eval(A, expr, NULL);
    ape_restoregc(A, gctop);
  }
}
