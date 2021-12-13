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

static ape_Object error(ape_State *A, int argc, ape_Object args,
                        ape_Object env) {
  char buf[128] = {0};
  ape_tostring(A, ape_nextarg(A, &args), buf, sizeof(buf) - 1);
  ape_error(A, buf);
  return ape_nil(A);
}

static ape_Object nilp(ape_State *A, int argc, ape_Object args,
                       ape_Object env) {
  return ape_bool(A, ape_isnil(A, ape_nextarg(A, &args)));
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
  int len = ape_strlen(A, str);
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

static ape_Object string_append(ape_State *A, int argc, ape_Object args,
                         ape_Object env) {
  return ape_strappend(A, args);
}

static ape_Object string_length(ape_State *A, int argc, ape_Object args,
                                ape_Object env) {
  return ape_integer(A, ape_strlen(A, ape_nextarg(A, &args)));
}

static ape_Object vector_length(ape_State *A, int argc, ape_Object args,
                                ape_Object env) {
  return ape_integer(A, ape_veclen(A, ape_nextarg(A, &args)));
}

static ape_Object string_ref(ape_State *A, int argc, ape_Object args,
                             ape_Object env) {
  int index = (int)ape_tointeger(A, ape_nextarg(A, &args));
  return ape_strref(A, ape_nextarg(A, &args), index);
}

static ape_Object vector_ref(ape_State *A, int argc, ape_Object args,
                             ape_Object env) {
  int index = (int)ape_tointeger(A, ape_nextarg(A, &args));
  return ape_vecref(A, ape_nextarg(A, &args), index);
}

static ape_Object string_reverse(ape_State *A, int argc, ape_Object args,
                          ape_Object env) {
  return ape_strreverse(A, ape_nextarg(A, &args));
}

static ape_Object symbol(ape_State *A, int argc, ape_Object args,
                         ape_Object env) {
  ape_Object str = ape_checktype(A, ape_nextarg(A, &args), APE_TSTRING);
  char buf[APE_SYMSIZE] = {0};

  if (ape_strlen(A, str) >= APE_SYMSIZE)
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

static const char list[] = {"                                                  \
(def list (fn x x))"};

static const char defmacro[] = {"                                              \
(def defmacro (macro (name args . body)                                        \
  `(def ,name (macro ,args ,@body))))"};

static const char defn[] = {"                                                  \
(defmacro defn (name args . body)                                              \
  `(def ,name (fn ,args ,@body)))"};

static const char caar[] = {"                                                  \
(defn caar (list)                                                              \
  (car (car list)))"};

static const char cadr[] = {"                                                  \
(defn cadr (list)                                                              \
  (car (cdr list)))"};

static const char cdar[] = {"                                                  \
(defn cdar (list)                                                              \
  (cdr (car list)))"};

static const char cddr[] = {"                                                  \
(defn cddr (list)                                                              \
  (cdr (cdr list)))"};

static const char caaar[] = {"                                                 \
(defn caaar (list)                                                             \
  (car (caar list)))"};

static const char caadr[] = {"                                                 \
(defn caadr (list)                                                             \
  (car (cadr list)))"};

static const char cadar[] = {"                                                 \
(defn cadar (list)                                                             \
  (car (cdar list)))"};

static const char caddr[] = {"                                                 \
(defn caddr (list)                                                             \
  (car (cddr list)))"};

static const char cdaar[] = {"                                                 \
(defn cdaar (list)                                                             \
  (cdr (caar list)))"};

static const char cdadr[] = {"                                                 \
(defn cdadr (list)                                                             \
  (cdr (cadr list)))"};

static const char cddar[] = {"                                                 \
(defn cddar (list)                                                             \
  (cdr (cdar list)))"};

static const char cdddr[] = {"                                                 \
(defn cdddr (list)                                                             \
  (cdr (cddr list)))"};

static const char caaaar[] = {"                                                \
(defn caaaar (list)                                                            \
  (car (caaar list)))"};

static const char caaadr[] = {"                                                \
(defn caaadr (list)                                                            \
  (car (caadr list)))"};

static const char caadar[] = {"                                                \
(defn caadar (list)                                                            \
  (car (cadar list)))"};

static const char caaddr[] = {"                                                \
(defn caaddr (list)                                                            \
  (car (caddr list)))"};

static const char cadaar[] = {"                                                \
(defn cadaar (list)                                                            \
  (car (cdaar list)))"};

static const char cadadr[] = {"                                                \
(defn cadadr (list)                                                            \
  (car (cdadr list)))"};

static const char caddar[] = {"                                                \
(defn caddar (list)                                                            \
  (car (cddar list)))"};

static const char cadddr[] = {"                                                \
(defn cadddr (list)                                                            \
  (car (cdddr list)))"};

static const char cdaaar[] = {"                                                \
(defn cdaaar (list)                                                            \
  (cdr (caaar list)))"};

static const char cdaadr[] = {"                                                \
(defn cdaadr (list)                                                            \
  (cdr (caadr list)))"};

static const char cdadar[] = {"                                                \
(defn cdadar (list)                                                            \
  (cdr (cadar list)))"};

static const char cdaddr[] = {"                                                \
(defn cdaddr (list)                                                            \
  (cdr (caddr list)))"};

static const char cddaar[] = {"                                                \
(defn cddaar (list)                                                            \
  (cdr (cdaar list)))"};

static const char cddadr[] = {"                                                \
(defn cddadr (list)                                                            \
  (cdr (cdadr list)))"};

static const char cdddar[] = {"                                                \
(defn cdddar (list)                                                            \
  (cdr (cddar list)))"};

static const char cddddr[] = {"                                                \
(defn cddddr (list)                                                            \
  (cdr (cdddr list)))"};

static const char pairp[] = {"                                                 \
(defn pair? (p)                                                                \
  (= (type p) 'pair))"};

static const char numberp[] = {"                                               \
(defn number? (n)                                                              \
  (= (type n) 'number))"};

static const char symbolp[] = {"                                               \
(defn symbol? (s)                                                              \
  (= (type s) 'symbol))"};

static const char stringp[] = {"                                               \
(defn string? (s)                                                              \
  (= (type s) 'string))"};

static const char vectorp[] = {"                                               \
(defn vector? (v)                                                              \
  (= (type v) 'vector))"};

static const char fnp[] = {"                                                   \
(defn fn? (f)                                                                  \
  (= (type f) 'function))"};

static const char macrop[] = {"                                                \
(defn macro? (m)                                                               \
  (= (type m) 'macro))"};

static const char primitivep[] = {"                                            \
(defn primitive? (p)                                                           \
  (= (type p) 'primitive))"};

static const char cfnp[] = {"                                                  \
(defn cfn? (f)                                                                 \
  (= (type f) 'cfunction))"};

static const char ptrp[] = {"                                                  \
(defn ptr? (p)                                                                 \
  (= (type p) 'pointer))"};

static const char length[] = {"                                                \
(defn length (list)                                                            \
  (def cnt 0)                                                                  \
  (while list                                                                  \
    (set! list (cdr list))                                                     \
    (set! cnt (+ cnt 1)))                                                      \
  cnt)"};

static const char reverse[] = {"                                               \
(defn reverse (list)                                                           \
  (def res nil)                                                                \
  (while list                                                                  \
    (set! res (cons (car list) res))                                           \
    (set! list (cdr list)))                                                    \
  res)"};

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
  (eval (cons op                                                               \
              (map (fn (arg)                                                   \
                     (list 'quote arg))                                        \
                   args))))"};

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

static const char append[] = {"                                                \
(defn append (list . more)                                                     \
  (if (not more) list                                                          \
    (if (not list)                                                             \
        (apply append more)                                                    \
      (cons (car list)                                                         \
            (apply append (cons (cdr list) more))))))"};

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

static const char assoc[] = {"                                                 \
(defn assoc (key list)                                                         \
  (let (p (car list))                                                          \
    (cond                                                                      \
      (not list) nil                                                           \
      (= (car p) key) p                                                        \
      (assoc key (cdr list)))))"};

static const char get[] = {"                                                   \
(defn get (list key (default nil))                                             \
  (cond                                                                        \
    (not list) default                                                         \
    (= (car list) key) (cadr list)                                             \
    (get (cddr list) key default)))"};

static const char push[] = {"                                                  \
(defmacro push (val place)                                                     \
  `(set! ,place (cons ,val ,place)))"};

static const char pop[] = {"                                                   \
(defmacro pop (place)                                                          \
  (let (gx (gensym))                                                           \
    `(let (,gx (car ,place))                                                   \
       (set! ,place (cdr ,place))                                              \
       ,gx)))"};

static const char list_ref[] = {"                                              \
(defn list-ref (index list)                                                    \
  (while (and list (< 0 index))                                                \
    (set! index (- index 1))                                                   \
    (set! list (cdr list)))                                                    \
  (if (= index 0)                                                              \
      (car list)                                                               \
    (error \"index out of range\")))"};

static const char string_tolist[] = {"                                         \
(defn string->list (string)                                                    \
  (def i 0)                                                                    \
  (def len (string-length string))                                             \
  (def res nil)                                                                \
  (while (< i len)                                                             \
    (set! res (cons (string-ref i string) res))                                \
    (set! i (+ i 1)))                                                          \
  (reverse res))"};

static const char string_tovector[] = {"                                       \
(defn string->vector (string)                                                  \
  (def i 0)                                                                    \
  (def len (string-length string))                                             \
  (def res (make-vector len))                                                  \
  (while (< i len)                                                             \
    (vector-set! res i (string-ref i string))                                  \
    (set! i (+ i 1)))                                                          \
  res)"};

static const char vector_tolist[] = {"                                         \
(defn vector->list (vector)                                                    \
  (def i 0)                                                                    \
  (def len (vector-length vector))                                             \
  (def res nil)                                                                \
  (while (< i len)                                                             \
    (set! res (cons (vector-ref i vector) res))                                \
    (set! i (+ i 1)))                                                          \
  (reverse res))"};

static const char list_tovector[] = {"                                         \
(defn list->vector (list)                                                      \
  (def i 0)                                                                    \
  (def len (length list))                                                      \
  (def res (make-vector len))                                                  \
  (while list                                                                  \
    (vector-set! res i (car list))                                             \
    (set! list (cdr list))                                                     \
    (set! i (+ i 1)))                                                          \
  res)"};

void stdlib_open(ape_State *A) {
  const Function cfuncs[] = {
      {"error", error},
      {"nil?", nilp},
      {"print", print},
      {"eval", eval},
      {"load", load},
      {"number", number},
      {"string", string},
      {"string-append", string_append},
      {"string-length", string_length},
      {"vector-length", vector_length},
      {"string-ref", string_ref},
      {"vector-ref", vector_ref},
      {"string-reverse", string_reverse},
      {"symbol", symbol},
      {"gensym", gensym},
      {"make-vector", make_vector},
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
      list,          defmacro,
      defn,          caar,
      cadr,          cdar,
      cddr,          caaar,
      caadr,         cadar,
      caddr,         cdaar,
      cdadr,         cddar,
      cdddr,         caaaar,
      caaadr,        caadar,
      caaddr,        cadaar,
      cadadr,        caddar,
      cadddr,        cdaaar,
      cdaadr,        cdadar,
      cdaddr,        cddaar,
      cddadr,        cdddar,
      cddddr,        pairp,
      numberp,       symbolp,
      stringp,       vectorp,
      fnp,           macrop,
      primitivep,    cfnp,
      ptrp,          length,
      reverse,       let,
      cond,          apply,
      when,          unless,
      while_,        for_,
      append,        map,
      filter,        reduce,
      acons,         assoc,
      get,           push,
      pop,           list_ref,
      string_tolist, string_tovector,
      vector_tolist, list_tovector,
      NULL,
  };

  /* c libs */
  for (const Function *func = cfuncs; func->name && func->func; ++func)
    ape_def(A, ape_symbol(A, func->name), ape_cfunc(A, func->func), NULL);

  /* ape libs */
  for (const char **lib = stdlib; *lib; ++lib) {
    ape_Object expr = ape_readstring(A, *lib);
    ape_eval(A, expr, NULL);
  }
}
