#include "caml/mlvalues.h"
#include "caml/memory.h"
#include "caml/alloc.h"
#include <string.h>

typedef uintnat unativeint_t;
#ifdef _MSC_VER
typedef unsigned char uint8_t;
#endif
#define caml_copy_int16 Val_int
#define caml_copy_int8 Val_int

#define LOADPRIM(type) \
  do {\
    CAMLparam1(ptr);\
    u##type##_t res;\
    memcpy(&res, (void*)Nativeint_val(ptr), sizeof res);\
    CAMLreturn(caml_copy_##type(res));\
  } while (0)

CAMLprim value caml_load_int8(value ptr) {
  LOADPRIM(int8);
}

CAMLprim value caml_load_int16(value ptr) {
  LOADPRIM(int16);
}

CAMLprim value caml_load_int32(value ptr) {
  LOADPRIM(int32);
}

CAMLprim value caml_load_int64(value ptr) {
  LOADPRIM(int64);
}

CAMLprim value caml_load_nativeint(value ptr) {
  LOADPRIM(nativeint);
}

#define caml_int8_val  Val_int
#define caml_int16_val Val_int
#define caml_int32_val Int32_val
#define caml_int64_val Int64_val

#define SETPRIM(type)                                                   \
  do {                                                                  \
    CAMLparam2(ptr,newval);                                             \
    u##type##_t val = caml_##type##_val(newval);                        \
    memcpy((void*)Nativeint_val(ptr), &val, sizeof val);                \
    CAMLreturn(Val_unit);                                               \
  } while(0)

CAMLprim value caml_set_int8(value ptr, value newval) {
  SETPRIM(int8);
}

CAMLprim value caml_set_int16(value ptr, value newval) {
  SETPRIM(int16);
}

CAMLprim value caml_set_int32(value ptr, value newval) {
  SETPRIM(int32);
}

CAMLprim value caml_set_int64(value ptr, value newval) {
  SETPRIM(int64);
}
