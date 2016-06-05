/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*           Damien Doligez, projet Moscova, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 2000 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* Handling of finalised values. */

#include "caml/callback.h"
#include "caml/fail.h"
#include "caml/mlvalues.h"
#include "caml/roots.h"
#include "caml/signals.h"
#include "caml/finalise.h"

/* final flags */
#define FINAL_CALLED_WITHOUT_VALUE 1

struct final {
  value fun;
  value val;
  int offset;
  int flags;
};

static struct final *final_table = NULL;
static uintnat old = 0, young = 0, size = 0;
/* [0..old) : finalisable set, the values are in the major heap
   [old..young) : recent set, the values could be in the minor heap
   [young..size) : free space

   The element of the finalisable set are moved to the finalising set
   below when the value are unreachable (for the first or last time
   depending on the flag).
*/

struct to_do {
  struct to_do *next;
  int size;
  struct final item[1];  /* variable size */
};

static struct to_do *to_do_hd = NULL;
static struct to_do *to_do_tl = NULL;
/*
  to_do_hd: head of the list of finalisation functions that can be run.
  to_do_tl: tail of the list of finalisation functions that can be run.

  It is the finalising set.
*/


/* [size] is a number of elements for the [to_do.item] array */
static void alloc_to_do (int size)
{
  struct to_do *result = malloc (sizeof (struct to_do)
                                 + size * sizeof (struct final));
  if (result == NULL) caml_fatal_error ("out of memory");
  result->next = NULL;
  result->size = size;
  if (to_do_tl == NULL){
    to_do_hd = result;
    to_do_tl = result;
  }else{
    Assert (to_do_tl->next == NULL);
    to_do_tl->next = result;
    to_do_tl = result;
  }
}

/* Find white finalisable values, move them to the finalising set, and
   darken them.
*/
void caml_final_update (int flags)
{
  uintnat i, j, k;
  uintnat todo_count = 0;

  Assert (old <= young);
  for (i = 0; i < old; i++){
    Assert (Is_block (final_table[i].val));
    Assert (Is_in_heap (final_table[i].val));
    if (final_table[i].flags == flags &&
        Is_white_val (final_table[i].val)){
      ++ todo_count;
    }
  }

  /** invariant:
      - 0 <= j <= i /\ 0 <= k <= i /\ 0 <= k <= todo_count
      - i : index in final_table, before i all the values are black
      (alive or in the minor heap) or the finalizer have been copied
      in to_do_tl.
      - j : index in final_table, before j all the values are black
      (alive or in the minor heap), next available slot.
      - k : index in to_do_tl, next available slot.
  */
  if (todo_count > 0){
    alloc_to_do (todo_count);
    j = k = 0;
    for (i = 0; i < old; i++){
      Assert (Is_block (final_table[i].val));
      Assert (Is_in_heap (final_table[i].val));
      Assert (Tag_val (final_table[i].val) != Forward_tag);
      if (final_table[i].flags == flags &&
          Is_white_val (final_table[i].val)){
        to_do_tl->item[k++] = final_table[i];
      }else{
        final_table[j++] = final_table[i];
      }
    }
    CAMLassert (i == old);
    old = j;
    for(;i < young; i++){
      final_table[j++] = final_table[i];
    }
    young = j;
    to_do_tl->size = k;
    for (i = 0; i < k; i++){
      if((final_table[i].flags & FINAL_CALLED_WITHOUT_VALUE) == 0) {
        /* Note that item may already be dark due to multiple entries in
           the final table. */
        caml_darken (to_do_tl->item[i].val, NULL);
      }
    }
  }
}

void caml_final_update_mark_phase (void)
{
  return caml_final_update(0);
}

void caml_final_update_clean_phase (void)
{
  return caml_final_update(FINAL_CALLED_WITHOUT_VALUE);
}

/** update for values in minor_gc that have the flag FINAL_CALLED_WITHOUT_VALUE
    after minor GC.
    All the value of finalizer that doesn't have the FINAL_CALLED_WITHOUT_VALUE
    flag are already black since caml_final_do_young_roots.
 */
void caml_final_update_minor_phase ()
{
  uintnat i, j, k;
  uintnat todo_count = 0;

  Assert (old <= young);
  for (i = old; i < young; i++){
    Assert (Is_block (final_table[i].val));
    if (Is_young(final_table[i].val) &&
        (final_table[i].flags & FINAL_CALLED_WITHOUT_VALUE) &&
        Hd_val (v) == 0 /** Is_white_val in minor heap */ ){
      ++ todo_count;
    }
  }

  /** invariant:
      - 0 <= j <= i /\ 0 <= k <= i /\ 0 <= k <= todo_count
      - i : index in final_table, before i all the values are in the
      major heap or black or the finalizer have been copied in
      to_do_tl.
      - j : index in final_table, before j all the values are in the
      major heap or black or the finalizer have been copied in
      to_do_tl, next available slot.
      - k : index in to_do_tl, next available slot.
  */
  if (todo_count > 0){
    alloc_to_do (todo_count);
    j = old;
    k = 0;
    for (i = old; i < young; i++){
      Assert (Is_block (final_table[i].val));
      Assert (Is_in_heap (final_table[i].val));
      Assert (Tag_val (final_table[i].val) != Forward_tag);
      if (Is_young(final_table[i].val) &&
          (final_table[i].flags & FINAL_CALLED_WITHOUT_VALUE) &&
          Hd_val (v) == 0 /** Is_white_val in minor heap */ ){
        to_do_tl->item[k++] = final_table[i];
      }else{
        final_table[j++] = final_table[i];
      }
    }
    CAMLassert (i == young);
    young = j;
    to_do_tl->size = k;
  }
}



static int running_finalisation_function = 0;

/* Call the finalisation functions for the finalising set.
   Note that this function must be reentrant.
*/
void caml_final_do_calls (void)
{
  struct final f;
  value res;

  if (running_finalisation_function) return;
  if (to_do_hd != NULL){
    if (caml_finalise_begin_hook != NULL) (*caml_finalise_begin_hook) ();
    caml_gc_message (0x80, "Calling finalisation functions.\n", 0);
    while (1){
      while (to_do_hd != NULL && to_do_hd->size == 0){
        struct to_do *next_hd = to_do_hd->next;
        free (to_do_hd);
        to_do_hd = next_hd;
        if (to_do_hd == NULL) to_do_tl = NULL;
      }
      if (to_do_hd == NULL) break;
      Assert (to_do_hd->size > 0);
      -- to_do_hd->size;
      f = to_do_hd->item[to_do_hd->size];
      running_finalisation_function = 1;
      if(f.flags & FINAL_CALLED_WITHOUT_VALUE){
        res = caml_callback_exn (f.fun, Val_unit);
      } else {
        res = caml_callback_exn (f.fun, f.val + f.offset);
      }
      running_finalisation_function = 0;
      if (Is_exception_result (res)) caml_raise (Extract_exception (res));
    }
    caml_gc_message (0x80, "Done calling finalisation functions.\n", 0);
    if (caml_finalise_end_hook != NULL) (*caml_finalise_end_hook) ();
  }
}

/* Call a scanning_action [f] on [x]. */
#define Call_action(f,x) (*(f)) ((x), &(x))

/* Call [*f] on the closures of the finalisable set and
   the closures and values of the finalising set.
   This is called by the major GC through [caml_darken_all_roots].
*/
void caml_final_do_strong_roots (scanning_action f)
{
  uintnat i;
  struct to_do *todo;

  Assert (old <= young);
  for (i = 0; i < young; i++) Call_action (f, final_table[i].fun);

  for (todo = to_do_hd; todo != NULL; todo = todo->next){
    for (i = 0; i < todo->size; i++){
      Call_action (f, todo->item[i].fun);
      if((final_table[i].flags & FINAL_CALLED_WITHOUT_VALUE) == 0) {
        Call_action (f, todo->item[i].val);
      }
    }
  }
}

/* Call [*f] on the values of the finalisable set.
   This is called directly by the compactor.
*/
void caml_final_do_weak_roots (scanning_action f)
{
  uintnat i;

  CAMLassert (old <= young);
  for (i = 0; i < young; i++) Call_action (f, final_table[i].val);
}

/* Call [*f] on the closures and values of the recent set.
   This is called by the minor GC through [caml_oldify_local_roots].
*/
void caml_final_do_young_roots (scanning_action f)
{
  uintnat i;

  Assert (old <= young);
  for (i = old; i < young; i++){
    Call_action (f, final_table[i].fun);
    if((final_table[i].flags & FINAL_CALLED_WITHOUT_VALUE) == 0) {
      Call_action (f, final_table[i].val);
    }
  }
}

/* Empty the recent set into the finalisable set.
   This is called at the end of each minor collection.
   The minor heap must be empty when this is called.
*/
void caml_final_empty_young (void)
{
  old = young;
}

/* Put (f,v) in the recent set. */
static value caml_final_register_aux (value f, value v, value flags)
{
  if (!Is_block (v)
      || !Is_in_heap_or_young(v)
      || Tag_val (v) == Lazy_tag
      || Tag_val (v) == Double_tag
      || Tag_val (v) == Forward_tag) {
    caml_invalid_argument ("Gc.finalise");
  }
  Assert (old <= young);

  if (young >= size){
    if (final_table == NULL){
      uintnat new_size = 30;
      final_table = caml_stat_alloc (new_size * sizeof (struct final));
      Assert (old == 0);
      Assert (young == 0);
      size = new_size;
    }else{
      uintnat new_size = size * 2;
      final_table = caml_stat_resize (final_table,
                                      new_size * sizeof (struct final));
      size = new_size;
    }
  }
  Assert (young < size);
  final_table[young].fun = f;
  if (Tag_val (v) == Infix_tag){
    final_table[young].offset = Infix_offset_val (v);
    final_table[young].val = v - Infix_offset_val (v);
    final_table[young].flags = Long_val(flags);
  }else{
    final_table[young].offset = 0;
    final_table[young].val = v;
    final_table[young].flags = Long_val(flags);
  }
  ++ young;

  return Val_unit;
}

CAMLprim value caml_final_register (value f, value v)
{
  return caml_final_register_aux(f,v,0);
}

CAMLprim value caml_final_register_called_without_value (value f, value v)
{
  return caml_final_register_aux(f,v,FINAL_CALLED_WITHOUT_VALUE);
}

CAMLprim value caml_final_release (value unit)
{
  running_finalisation_function = 0;
  return Val_unit;
}
