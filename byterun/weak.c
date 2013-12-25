/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*             Damien Doligez, projet Para, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1997 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* Operations on weak arrays and ephemerons (named ephe here)*/

#include <string.h>

#include "alloc.h"
#include "fail.h"
#include "major_gc.h"
#include "memory.h"
#include "mlvalues.h"

value caml_ephe_list_head = 0;

static value ephe_dummy = 0;
value caml_ephe_none = (value) &ephe_dummy;

/** The first field 0:  weak list;
       second field 1:  data;
       others       2..:  keys;

    A weak pointer is an ephemeron with the data at caml_ephe_none
 */

CAMLprim value caml_ephe_create (value len)
{
  mlsize_t size, i;
  value res;

  size = Long_val (len) + 1 /* weak_list */ + 1 /* the value */;
  if (size <= 0 || size > Max_wosize) caml_invalid_argument ("Weak.create");
  res = caml_alloc_shr (size, Abstract_tag);
  for (i = 1; i < size; i++) Field (res, i) = caml_ephe_none;
  Field (res, 0) = caml_ephe_list_head;
  caml_ephe_list_head = res;
  return res;
}

CAMLprim value caml_weak_create (value len)
{
  return caml_ephe_create(len);
}

void caml_ephe_clean (value v){
  value child;
  int release_data = 0;
  mlsize_t size, i;
  header_t hd;
                                    Assert(caml_gc_phase == Phase_clean);

  hd = Hd_val (v);
  size = Wosize_hd (hd);
  for (i = 2; i < size; i++){
    child = Field (v, i);
  ephemeron_again:
    if (child != caml_ephe_none
        && Is_block (child) && Is_in_heap (child)){
      if (Tag_val (child) == Forward_tag){
        value f = Forward_val (child);
        if (Is_block (f)) {
          if (!Is_in_value_area(f) || Tag_val (f) == Forward_tag
              || Tag_val (f) == Lazy_tag || Tag_val (f) == Double_tag){
            /* Do not short-circuit the pointer. */
          }else{
            Field (v, i) = child = f;
            goto ephemeron_again;
          }
        }
      }
      if (Is_white_val (child)){
        release_data = 1;
        Field (v, i) = caml_ephe_none;
      }
    }
  }

  child = Field (v, 1);
  if(child != caml_ephe_none){
      if (release_data){
        Field (v, 1) = caml_ephe_none;
      } else {
        //The mark phase must have marked it
        Assert( !(Is_block (child) && Is_in_heap (child)
                  && Is_white_val (child)) );
      }
  }
}


#define None_val (Val_int(0))
#define Some_tag 0

/* If we are in Phase_clean we need to check if the key
   that is going to disappear is dead and so should trigger a cleaning
   That don't work for data because a data can be alive even if a key is dead
   and so the data should be removed from the ephemeron.
 */
static void do_check_clean(value ar, mlsize_t offset){
  value elt = Field (ar, offset);
  if (caml_gc_phase == Phase_clean &&
      Is_block (elt) && Is_in_heap (elt) && Is_white_val(elt)){
    caml_ephe_clean(ar);
  };
}

static void do_set (value ar, mlsize_t offset, value v)
{
  if (Is_block (v) && Is_young (v)){
    /* modified version of Modify */
    value old = Field (ar, offset);
    Field (ar, offset) = v;
    if (!(Is_block (old) && Is_young (old))){
      if (caml_ephe_ref_table.ptr >= caml_ephe_ref_table.limit){
        CAMLassert (caml_ephe_ref_table.ptr == caml_ephe_ref_table.limit);
        caml_realloc_ephe_ref_table (&caml_ephe_ref_table);
      }
      struct caml_ephe_ref_elt *ephe_ref = caml_ephe_ref_table.ptr++;
      ephe_ref->ephe = ar;
      ephe_ref->offset = offset;
    }
  }else{
    Field (ar, offset) = v;
  }
}

CAMLprim value caml_ephe_set_key (value ar, value n, value el)
{
  mlsize_t offset = Long_val (n) + 2;
                                                   Assert (Is_in_heap (ar));
  if (offset < 2 || offset >= Wosize_val (ar)){
    caml_invalid_argument ("Weak.set");
  }
  do_check_clean(ar,offset);
  do_set (ar, offset, el);
  return Val_unit;
}

CAMLprim value caml_ephe_unset_key (value ar, value n)
{
  mlsize_t offset = Long_val (n) + 2;
                                                   Assert (Is_in_heap (ar));
  if (offset < 2 || offset >= Wosize_val (ar)){
    caml_invalid_argument ("Weak.set");
  }
  do_check_clean(ar,offset);
  Field (ar, offset) = caml_ephe_none;
  return Val_unit;
}

value caml_ephe_set_key_option (value ar, value n, value el)
{
  mlsize_t offset = Long_val (n) + 2;
                                                   Assert (Is_in_heap (ar));
  if (offset < 2 || offset >= Wosize_val (ar)){
    caml_invalid_argument ("Weak.set");
  }
  do_check_clean(ar,offset);
  if (el != None_val && Is_block (el)){
                                              Assert (Wosize_val (el) == 1);
    do_set (ar, offset, Field (el, 0));
  }else{
    Field (ar, offset) = caml_ephe_none;
  }
  return Val_unit;
}

CAMLprim value caml_weak_set (value ar, value n, value el){
  return caml_ephe_set_key_option(ar,n,el);
}

CAMLprim value caml_ephe_set_data (value ar, value el)
{
                                                   Assert (Is_in_heap (ar));
  if (caml_gc_phase == Phase_clean){
    // During this phase since we don't know which ephemeron have been cleaned
    // we always need to check it.
    caml_ephe_clean(ar);
  };
  do_set (ar, 1, el);
  return Val_unit;
}

CAMLprim value caml_ephe_unset_data (value ar)
{
                                                   Assert (Is_in_heap (ar));
  Field (ar, 1) = caml_ephe_none;
  return Val_unit;
}

int caml_is_ephe_none(value ar, value elt){
  if (elt == caml_ephe_none){
    return 1;
  }else if (caml_gc_phase == Phase_clean &&
            Is_block (elt) && Is_in_heap (elt) && Is_white_val(elt)){
    caml_ephe_clean(ar);
    return 1;
  } else {
    return 0;
  }
}

#define Setup_for_gc
#define Restore_after_gc

CAMLprim value caml_ephe_get_key (value ar, value n)
{
  CAMLparam2 (ar, n);
  mlsize_t offset = Long_val (n) + 2;
  CAMLlocal2 (res, elt);
                                                   Assert (Is_in_heap (ar));
  if (offset < 2 || offset >= Wosize_val (ar)){
    caml_invalid_argument ("Weak.get_key");
  }
  elt = Field (ar, offset);
  if (caml_is_ephe_none(ar, elt)){
    res = None_val;
  }else{
    if (caml_gc_phase == Phase_mark && Is_block (elt) && Is_in_heap (elt)){
      caml_darken (elt, NULL);
    }
    res = caml_alloc_small (1, Some_tag);
    Field (res, 0) = elt;
  }
  CAMLreturn (res);
}

CAMLprim value caml_weak_get (value ar, value n){
  return caml_ephe_get_key(ar, n);
}

CAMLprim value caml_ephe_get_data (value ar)
{
  CAMLparam1 (ar);
  mlsize_t offset = 1;
  CAMLlocal2 (res, elt);
                                                   Assert (Is_in_heap (ar));
  elt = Field (ar, offset);
  if(caml_gc_phase == Phase_clean) caml_ephe_clean(ar);
  if (caml_is_ephe_none(ar, elt)){
    res = None_val;
  }else{
    if (caml_gc_phase == Phase_mark && Is_block (elt) && Is_in_heap (elt)){
      caml_darken (elt, NULL);
    }
    res = caml_alloc_small (1, Some_tag);
    Field (res, 0) = elt;
  }
  CAMLreturn (res);
}

#undef Setup_for_gc
#undef Restore_after_gc

CAMLprim value caml_ephe_get_key_copy (value ar, value n)
{
  CAMLparam2 (ar, n);
  mlsize_t offset = Long_val (n) + 2;
  CAMLlocal2 (res, elt);
  value v;  /* Caution: this is NOT a local root. */
                                                   Assert (Is_in_heap (ar));
  if (offset < 1 || offset >= Wosize_val (ar)){
    caml_invalid_argument ("Weak.get_copy");
  }

  v = Field (ar, offset);
  if (caml_is_ephe_none(ar, v)) CAMLreturn (None_val);
  if (Is_block (v) && Is_in_heap_or_young(v)) {
    elt = caml_alloc (Wosize_val (v), Tag_val (v));
          /* The GC may erase or move v during this call to caml_alloc. */
    v = Field (ar, offset);
    if (caml_is_ephe_none(ar, v)) CAMLreturn (None_val);
    if (Tag_val (v) < No_scan_tag){
      mlsize_t i;
      for (i = 0; i < Wosize_val (v); i++){
        value f = Field (v, i);
        if (caml_gc_phase == Phase_mark && Is_block (f) && Is_in_heap (f)){
          caml_darken (f, NULL);
        }
        Modify (&Field (elt, i), f);
      }
    }else{
      memmove (Bp_val (elt), Bp_val (v), Bosize_val (v));
    }
  }else{
    elt = v;
  }
  res = caml_alloc_small (1, Some_tag);
  Field (res, 0) = elt;

  CAMLreturn (res);
}

CAMLprim value caml_weak_get_copy (value ar, value n){
  return caml_ephe_get_key_copy(ar,n);
}

CAMLprim value caml_ephe_get_data_copy (value ar)
{
  CAMLparam1 (ar);
  mlsize_t offset = 1;
  CAMLlocal2 (res, elt);
  value v;  /* Caution: this is NOT a local root. */
                                                   Assert (Is_in_heap (ar));

  v = Field (ar, offset);
  if(caml_gc_phase == Phase_clean) caml_ephe_clean(ar);
  if (caml_is_ephe_none(ar, v)) CAMLreturn (None_val);
  if (Is_block (v) && Is_in_heap_or_young(v)) {
    elt = caml_alloc (Wosize_val (v), Tag_val (v));
          /* The GC may erase or move v during this call to caml_alloc. */
    v = Field (ar, offset);
    if (caml_is_ephe_none(ar, v)) CAMLreturn (None_val);
    if (Tag_val (v) < No_scan_tag){
      mlsize_t i;
      for (i = 0; i < Wosize_val (v); i++){
        value f = Field (v, i);
        if (caml_gc_phase == Phase_mark && Is_block (f) && Is_in_heap (f)){
          caml_darken (f, NULL);
        }
        Modify (&Field (elt, i), f);
      }
    }else{
      memmove (Bp_val (elt), Bp_val (v), Bosize_val (v));
    }
  }else{
    elt = v;
  }
  res = caml_alloc_small (1, Some_tag);
  Field (res, 0) = elt;

  CAMLreturn (res);
}

CAMLprim value caml_ephe_check_key (value ar, value n)
{
  mlsize_t offset = Long_val (n) + 2;
                                                   Assert (Is_in_heap (ar));
  if (offset < 2 || offset >= Wosize_val (ar)){
    caml_invalid_argument ("Weak.check");
  }
  return Val_bool (!caml_is_ephe_none(ar, Field (ar, offset)));
}

CAMLprim value caml_weak_check (value ar, value n)
{
  return caml_ephe_check_key(ar,n);
}

CAMLprim value caml_ephe_check_data (value ar)
{
  if(caml_gc_phase == Phase_clean) caml_ephe_clean(ar);
  return Val_bool (!caml_is_ephe_none(ar, Field (ar, 1)));
}

CAMLprim value caml_ephe_blit_key (value ars, value ofs,
                               value ard, value ofd, value len)
{
  mlsize_t offset_s = Long_val (ofs) + 2;
  mlsize_t offset_d = Long_val (ofd) + 2;
  mlsize_t length = Long_val (len);
  long i;
                                                   Assert (Is_in_heap (ars));
                                                   Assert (Is_in_heap (ard));
  if (offset_s < 1 || offset_s + length > Wosize_val (ars)){
    caml_invalid_argument ("Weak.blit");
  }
  if (offset_d < 1 || offset_d + length > Wosize_val (ard)){
    caml_invalid_argument ("Weak.blit");
  }
  if (caml_gc_phase == Phase_clean) caml_ephe_clean(ars);
  if (offset_d < offset_s){
    for (i = 0; i < length; i++){
      do_set (ard, offset_d + i, Field (ars, offset_s + i));
    }
  }else{
    for (i = length - 1; i >= 0; i--){
      do_set (ard, offset_d + i,  Field (ars, offset_s + i));
    }
  }
  return Val_unit;
}

CAMLprim value caml_ephe_blit_data (value ars, value ard)
{
  if(caml_gc_phase == Phase_clean) {
    caml_ephe_clean(ars);
    caml_ephe_clean(ard);
  };
  do_set (ard, 1, Field (ars, 1));
  return Val_unit;
}

CAMLprim value caml_weak_blit (value ars, value ofs,
                      value ard, value ofd, value len)
{
  return caml_ephe_blit_key (ars, ofs, ard, ofd, len);
}
