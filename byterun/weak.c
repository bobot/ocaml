/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*              Damien Doligez, projet Para, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1997 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* Operations on weak arrays and ephemerons (named ephe here)*/

#include <string.h>

#include "caml/alloc.h"
#include "caml/fail.h"
#include "caml/major_gc.h"
#include "caml/memory.h"
#include "caml/mlvalues.h"
#include "caml/weak_private.h"
#include "caml/weak.h"

value caml_ephe_list_head = 0;

static value ephe_dummy = 0;
value caml_ephe_none = (value) &ephe_dummy;

#define Assert_valid_ephemeron(eph) do{                                 \
    Assert (Is_in_heap (eph));                                          \
    Assert (Tag_val(eph) == Abstract_tag);                              \
    Assert (CAML_EPHE_FIRST_KEY <= Wosize_val (eph));                   \
}while(0)

#define Assert_valid_offset(eph,offset) do{                             \
    Assert_valid_ephemeron(eph);                                        \
    Assert (0 <= offset);                                               \
    Assert (offset < Wosize_val (eph) - CAML_EPHE_FIRST_KEY);           \
}while(0)

CAMLexport mlsize_t caml_ephemeron_key_length(value eph){
  return Wosize_val (eph) - CAML_EPHE_FIRST_KEY;
}

#if defined (NATIVE_CODE) && defined (NO_NAKED_POINTERS)
/** The minor heap is considered alive.
    Outside minor and major heap, x must be black.
*/
static inline int Is_Dead_during_clean(value x){
  Assert (x != caml_ephe_none); Assert (caml_gc_phase == Phase_clean);
  return Is_block (x) && !Is_young (x) && Is_white_val(x);
}
/** The minor heap doesn't have to be marked, outside they should
    already be black
*/
static inline int Must_be_Marked_during_mark(value x){
  Assert (x != caml_ephe_none); Assert (caml_gc_phase == Phase_mark);
  return Is_block (x) && !Is_young (x);
}
#else
static inline int Is_Dead_during_clean(value x){
  Assert (x != caml_ephe_none); Assert (caml_gc_phase == Phase_clean);
  return Is_block (x) && Is_in_heap (x) && Is_white_val(x);
}
static inline int Must_be_Marked_during_mark(value x){
  Assert (x != caml_ephe_none); Assert (caml_gc_phase == Phase_mark);
  return Is_block (x) && Is_in_heap (x);
}
#endif

/* [len] is a value that represents a number of words (fields) */
CAMLexport value caml_ephemeron_create (mlsize_t len)
{
  mlsize_t size, i;
  value res;

  Assert(len <= Max_wosize - CAML_EPHE_FIRST_KEY);
  size = len + CAML_EPHE_FIRST_KEY;
  res = caml_alloc_shr (size, Abstract_tag);
  for (i = 1; i < size; i++) Field (res, i) = caml_ephe_none;
  Field (res, CAML_EPHE_LINK_OFFSET) = caml_ephe_list_head;
  caml_ephe_list_head = res;
  return res;
}

CAMLprim value caml_ephe_create (value len)
{
  intnat size = Long_val(len);
  if (size < 0 || size > (Max_wosize - CAML_EPHE_FIRST_KEY))
    caml_invalid_argument ("Weak.create");
  return caml_ephemeron_create(size);
}

CAMLprim value caml_weak_create (value len)
{
  return caml_ephe_create(len);
}

/**
   Specificity of the cleaning phase (Phase_clean):

   The dead keys must be removed from the ephemerons and data removed
   when one the keys is dead. Here we call it cleaning the ephemerons.
   A specific phase of the GC is dedicated to this, Phase_clean. This
   phase is just after the mark phase, so the white values are dead
   values. It iterates the function caml_ephe_clean through all the
   ephemerons.

   However the GC is incremental and ocaml code can run on the middle
   of this cleaning phase. In order to respect the semantic of the
   ephemerons concerning dead values, the getter and setter must work
   as if the cleaning of all the ephemerons have been done at once.

   - key getter: Even if a dead key have not yet been replaced by
     caml_ephe_none, getting it should return none.
   - key setter: If we replace a dead key we need to set the data to
     caml_ephe_none and clean the ephemeron.

     This two cases are dealt by a call to do_check_key_clean that
     trigger the cleaning of the ephemerons when the accessed key is
     dead. This test is fast.

     In the case of value getter and value setter, there is no fast
     test because the removing of the data depend of the deadliness of the keys.
     We must always try to clean the ephemerons.

 */

#define None_val (Val_int(0))
#define Some_tag 0

/* If we are in Phase_clean we need to check if the key
   that is going to disappear is dead and so should trigger a cleaning
 */
static void do_check_key_clean(value ar, mlsize_t offset){
                                   Assert ( offset >= 2);
  if (caml_gc_phase == Phase_clean){
    value elt = Field (ar, offset);
    if (elt != caml_ephe_none && Is_Dead_during_clean(elt)){
      Field(ar,offset) = caml_ephe_none;
      Field(ar,CAML_EPHE_DATA_OFFSET) = caml_ephe_none;
    };
  };
}

/* If we are in Phase_clean we need to do as if the key is empty when
   it will be cleaned during this phase */
static inline int is_ephe_key_none(value ar, mlsize_t offset){
  value elt = Field (ar, offset);
  if (elt == caml_ephe_none){
    return 1;
  }else if (caml_gc_phase == Phase_clean && Is_Dead_during_clean(elt)){
    Field(ar,offset) = caml_ephe_none;
    Field(ar,CAML_EPHE_DATA_OFFSET) = caml_ephe_none;
    return 1;
  } else {
    return 0;
  }
}

/** The offset start here at 0 */
static inline mlsize_t raise_if_invalid_offset(value ar, value n,
                                               char* msg){
  intnat offset = Long_val (n);
  if (offset < 0 || offset >= Wosize_val (ar) - CAML_EPHE_FIRST_KEY){
    caml_invalid_argument (msg);
  }
  return offset;
}

static void do_set (value ar, mlsize_t offset, value v)
{
  if (Is_block (v) && Is_young (v)){
    /* modified version of caml_modify */
    value old = Field (ar, offset);
    Field (ar, offset) = v;
    if (!(Is_block (old) && Is_young (old))){
      add_to_ephe_ref_table (&caml_ephe_ref_table, ar, offset);
    }
  }else{
    Field (ar, offset) = v;
  }
}

CAMLexport void caml_ephemeron_set_key(value ar, mlsize_t offset, value k){
  Assert_valid_offset(ar,offset);

  offset += CAML_EPHE_FIRST_KEY;

  do_check_key_clean(ar,offset);
  do_set (ar, offset, k);
}

CAMLprim value caml_ephe_set_key (value ar, value n, value el)
{
  mlsize_t offset = raise_if_invalid_offset(ar,n,"Weak.set");

  caml_ephemeron_set_key(ar,offset,el);
  return Val_unit;
}

CAMLexport void caml_ephemeron_unset_key(value ar, mlsize_t offset){
  Assert_valid_offset(ar,offset);

  offset += CAML_EPHE_FIRST_KEY;

  do_check_key_clean(ar,offset);
  Field (ar, offset) = caml_ephe_none;
}

CAMLprim value caml_ephe_unset_key (value ar, value n)
{
  mlsize_t offset = raise_if_invalid_offset (ar,n,"Weak.set");

  caml_ephemeron_unset_key(ar,offset);
  return Val_unit;
}

value caml_ephe_set_key_option (value ar, value n, value el)
{
  if (Is_block (el)){
                                              Assert (Wosize_val (el) == 1);
    caml_ephe_set_key(ar,n,Field (el, 0));
  }else{
                                              Assert (el == None_val);
    caml_ephe_unset_key(ar,n);
  }
  return Val_unit;
}

CAMLprim value caml_weak_set (value ar, value n, value el){
  return caml_ephe_set_key_option(ar,n,el);
}

CAMLexport void caml_ephemeron_set_data (value ar, value el)
{
  Assert_valid_ephemeron(ar);

  if (caml_gc_phase == Phase_clean){
    /* During this phase since we don't know which ephemeron have been
       cleaned we always need to check it. */
    caml_ephe_clean(ar);
  };
  do_set (ar, CAML_EPHE_DATA_OFFSET, el);
}

CAMLprim value caml_ephe_set_data (value ar, value el)
{
  caml_ephemeron_set_data (ar, el);
  return Val_unit;
}

CAMLexport void caml_ephemeron_unset_data (value ar)
{
  Assert_valid_ephemeron(ar);

  Field (ar, CAML_EPHE_DATA_OFFSET) = caml_ephe_none;
}

CAMLprim value caml_ephe_unset_data (value ar)
{
  caml_ephemeron_unset_data (ar);
  return Val_unit;
}

static value optionalize(int status, value *x) {
  CAMLparam0();
  CAMLlocal2(res,v);
  if(status) {
    v = *x;
    res = caml_alloc_small (1, Some_tag);
    Field (res, 0) = v;
  } else {
    res = None_val;
  }
  CAMLreturn(res);
}

CAMLexport int caml_ephemeron_get_key (value ar, mlsize_t offset, value *key)
{
  value elt;
  Assert_valid_offset(ar,offset);

  offset += CAML_EPHE_FIRST_KEY;

  if (is_ephe_key_none(ar, offset)){
    return 0;
  }else{
    elt = Field (ar, offset);
    if (caml_gc_phase == Phase_mark && Must_be_Marked_during_mark(elt)){
      caml_darken (elt, NULL);
    }
    *key = elt;
    return 1;
  }
}

CAMLprim value caml_ephe_get_key (value ar, value n)
{
  mlsize_t offset = raise_if_invalid_offset (ar,n,"Weak.get_key");

  value data;
  return optionalize(caml_ephemeron_get_key(ar,offset,&data),&data);
}

CAMLprim value caml_weak_get (value ar, value n){
  return caml_ephe_get_key(ar, n);
}

CAMLexport int caml_ephemeron_get_data (value ar, value *data)
{
  value elt;
  Assert_valid_ephemeron(ar);

  if(caml_gc_phase == Phase_clean) caml_ephe_clean(ar);
  elt = Field (ar, CAML_EPHE_DATA_OFFSET);
  if (elt == caml_ephe_none){
    return 0;
  }else{
    if (caml_gc_phase == Phase_mark && Must_be_Marked_during_mark(elt)){
      caml_darken (elt, NULL);
    }
    *data = elt;
    return 1;
  }
}

CAMLprim value caml_ephe_get_data (value ar)
{
  value data;
  return optionalize(caml_ephemeron_get_data(ar,&data),&data);
}


static inline void copy_value(value src, value dst){
  if (Tag_val (src) < No_scan_tag){
    mlsize_t i;
    for (i = 0; i < Wosize_val (src); i++){
      value f = Field (src, i);
      if (caml_gc_phase == Phase_mark && Must_be_Marked_during_mark(f)){
        caml_darken (f, NULL);
      }
      caml_modify (&Field (dst, i), f);
    }
  }else{
    memmove (Bp_val (dst), Bp_val (src), Bosize_val (src));
  }
};

CAMLexport int caml_ephemeron_get_key_copy(value ar, mlsize_t offset,
                                           value *key)
{
  CAMLparam1(ar);
  value elt = Val_unit, v; /* Caution: they are NOT a local root. */
  Assert_valid_offset(ar,offset);

  offset += CAML_EPHE_FIRST_KEY;

  while(1) {
    if(is_ephe_key_none(ar, offset)) CAMLreturn(0);
    v = Field (ar, offset);
    if(!(Is_block (v) && Is_in_heap_or_young(v))) {
      *key = v;
      CAMLreturn(1);
    }
    if (elt != Val_unit &&
        Wosize_val(v) == Wosize_val(elt) && Tag_val(v) == Tag_val(elt)) {
      copy_value(v,elt);
      *key = elt;
      CAMLreturn(1);
    }
    elt = caml_alloc (Wosize_val (v), Tag_val (v));
    /* The GC may erase, move or even change v during this call to
       caml_alloc. */
  }
}

CAMLprim value caml_ephe_get_key_copy (value ar, value n)
{
  mlsize_t offset = raise_if_invalid_offset (ar,n,"Weak.get_copy");

  value key;
  return optionalize(caml_ephemeron_get_key_copy(ar, offset, &key), &key);
}

CAMLprim value caml_weak_get_copy (value ar, value n){
  return caml_ephe_get_key_copy(ar,n);
}

CAMLexport int caml_ephemeron_get_data_copy (value ar, value *data)
{
  CAMLparam1 (ar);
  value elt = Val_unit,v; /* Caution: they are NOT a local root. */
  Assert_valid_ephemeron(ar);

  while(1) {
    if (caml_gc_phase == Phase_clean) caml_ephe_clean(ar);
    v = Field (ar, CAML_EPHE_DATA_OFFSET);
    if (v == caml_ephe_none) CAMLreturn(0);
    if (!(Is_block (v) && Is_in_heap_or_young(v))) {
      *data = v;
      CAMLreturn(1);
    }
    if (elt != Val_unit &&
        Wosize_val(v) == Wosize_val(elt) && Tag_val(v) == Tag_val(elt)) {
      copy_value(v,elt);
      *data = elt;
      CAMLreturn(1);
    }
    elt = caml_alloc (Wosize_val (v), Tag_val (v));
    /* The GC may erase, move or even change v during this call to
       caml_alloc. */
  }
}


CAMLprim value caml_ephe_get_data_copy (value ar)
{
  value data;
  return optionalize(caml_ephemeron_get_data_copy(ar,&data),&data);
}

CAMLexport int caml_ephemeron_check_key(value ar, mlsize_t offset){
  Assert_valid_offset(ar,offset);

  offset += CAML_EPHE_FIRST_KEY;
  return !is_ephe_key_none(ar, offset);
}

CAMLprim value caml_ephe_check_key (value ar, value n)
{
  mlsize_t offset = raise_if_invalid_offset(ar, n, "Weak.check");

  return Val_bool (caml_ephemeron_check_key(ar, offset));
}

CAMLprim value caml_weak_check (value ar, value n)
{
  return caml_ephe_check_key(ar,n);
}

CAMLexport int caml_ephemeron_check_data (value ar)
{
  Assert_valid_ephemeron(ar);

  if(caml_gc_phase == Phase_clean) caml_ephe_clean(ar);
  return Field (ar, CAML_EPHE_DATA_OFFSET) != caml_ephe_none;
}

CAMLprim value caml_ephe_check_data (value ar)
{
  return Val_bool (caml_ephemeron_check_data(ar));
}

CAMLexport void caml_ephemeron_blit_key(value ars, mlsize_t offset_s,
                                        value ard, mlsize_t offset_d,
                                        mlsize_t length)
{
  if (length == 0) return;
  long i; /** long because the second for-loop stop with i == -1 */
  Assert_valid_offset(ars,offset_s);
  Assert_valid_offset(ard,offset_d);
  Assert(length <= Wosize_val(ars) - CAML_EPHE_FIRST_KEY);
  Assert(length <= Wosize_val(ard) - CAML_EPHE_FIRST_KEY);
  Assert(offset_s <= Wosize_val(ars) - CAML_EPHE_FIRST_KEY - length);
  Assert(offset_d <= Wosize_val(ard) - CAML_EPHE_FIRST_KEY - length);

  offset_s += CAML_EPHE_FIRST_KEY;
  offset_d += CAML_EPHE_FIRST_KEY;

  if (caml_gc_phase == Phase_clean){
    caml_ephe_clean(ars);
    caml_ephe_clean(ard);
  }
  if (offset_d < offset_s){
    for (i = 0; i < length; i++){
      do_set (ard, offset_d + i, Field (ars, offset_s + i));
    }
  }else{
    for (i = length - 1; i >= 0; i--){
      do_set (ard, offset_d + i,  Field (ars, offset_s + i));
    }
  }
}

CAMLprim value caml_ephe_blit_key (value ars, value ofs,
                                   value ard, value ofd, value len)
{
  if (Long_val(len) == 0) return Val_unit;
  mlsize_t offset_s = raise_if_invalid_offset(ars, ofs, "Weak.blit");
  mlsize_t offset_d = raise_if_invalid_offset(ard, ofd, "Weak.blit");
  mlsize_t length = Long_val (len);

  caml_ephemeron_blit_key(ars,offset_s,ard,offset_d,length);
  return Val_unit;
}

CAMLprim value caml_weak_blit (value ars, value ofs,
                      value ard, value ofd, value len)
{
  return caml_ephe_blit_key (ars, ofs, ard, ofd, len);
}

CAMLexport void caml_ephemeron_blit_data (value ars, value ard)
{
  Assert_valid_ephemeron(ars);
  Assert_valid_ephemeron(ard);

  if(caml_gc_phase == Phase_clean) {
    caml_ephe_clean(ars);
    caml_ephe_clean(ard);
  };
  do_set (ard, CAML_EPHE_DATA_OFFSET, Field (ars, CAML_EPHE_DATA_OFFSET));
}

CAMLprim value caml_ephe_blit_data (value ars, value ard)
{
  caml_ephemeron_blit_data(ars, ard);
  return Val_unit;
}
