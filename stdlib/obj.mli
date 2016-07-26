(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Operations on internal representations of values.

   Not for the casual user.
*)

type t

external repr : 'a -> t = "%identity"
external obj : t -> 'a = "%identity"
external magic : 'a -> 'b = "%identity"
val [@inline always] is_block : t -> bool
external is_int : t -> bool = "%obj_is_int"
external tag : t -> int = "caml_obj_tag"
external size : t -> int = "%obj_size"
external reachable_words : t -> int = "caml_obj_reachable_words"
  (**
     Computes the total size (in words, including the headers) of all
     heap blocks accessible from the argument.  Statically
     allocated blocks are excluded.

     @Since 4.04
  *)

external field : t -> int -> t = "%obj_field"

(** When using flambda:

    [set_field] MUST NOT be called on immutable blocks.  (Blocks allocated
    in C stubs, or with [new_block] below, are always considered mutable.)

    The same goes for [set_double_field] and [set_tag].  However, for
    [set_tag], in the case of immutable blocks where the middle-end optimizers
    never see code that discriminates on their tag (for example records), the
    operation should be safe.  Such uses are nonetheless discouraged.

    For experts only:
    [set_field] et al can be made safe by first wrapping the block in
    [Sys.opaque_identity], so any information about its contents will not
    be propagated.
*)
external set_field : t -> int -> t -> unit = "%obj_set_field"
external set_tag : t -> int -> unit = "caml_obj_set_tag"

val [@inline always] double_field : t -> int -> float  (* @since 3.11.2 *)
val [@inline always] set_double_field : t -> int -> float -> unit
  (* @since 3.11.2 *)
external new_block : int -> int -> t = "caml_obj_block"
external dup : t -> t = "caml_obj_dup"
external truncate : t -> int -> unit = "caml_obj_truncate"
external add_offset : t -> Int32.t -> t = "caml_obj_add_offset"
         (* @since 3.12.0 *)

val first_non_constant_constructor_tag : int
val last_non_constant_constructor_tag : int

val lazy_tag : int
val closure_tag : int
val object_tag : int
val infix_tag : int
val forward_tag : int
val no_scan_tag : int
val abstract_tag : int
val string_tag : int   (* both [string] and [bytes] *)
val double_tag : int
val double_array_tag : int
val custom_tag : int
val final_tag : int
  [@@ocaml.deprecated "Replaced by custom_tag."]

val int_tag : int
val out_of_heap_tag : int
val unaligned_tag : int   (* should never happen @since 3.11.0 *)

val extension_constructor : 'a -> extension_constructor
val [@inline always] extension_name : extension_constructor -> string
val [@inline always] extension_id : extension_constructor -> int

(** The following two functions are deprecated.  Use module {!Marshal}
    instead. *)

val marshal : t -> bytes
  [@@ocaml.deprecated "Use Marshal.to_bytes instead."]
val unmarshal : bytes -> int -> t * int
  [@@ocaml.deprecated "Use Marshal.from_bytes and Marshal.total_size instead."]

module Ephemeron: sig
  (** Ephemeron with arbitrary arity and untyped *)

  type obj_t = t
  (** alias for {!Obj.t} *)

  type t
  (** an ephemeron cf {!Ephemeron} *)

  val create: int -> t
  (** [create n] returns an ephemeron with [n] keys.
      All the keys and the data are initially empty *)

  val length: t -> int
  (** return the number of keys *)

  val get_key: t -> int -> obj_t option
  (** Same as {!Ephemeron.K1.get_key} *)

  val get_key_copy: t -> int -> obj_t option
  (** Same as {!Ephemeron.K1.get_key_copy} *)

  val set_key: t -> int -> obj_t -> unit
  (** Same as {!Ephemeron.K1.set_key} *)

  val unset_key: t -> int -> unit
  (** Same as {!Ephemeron.K1.unset_key} *)

  val check_key: t -> int -> bool
  (** Same as {!Ephemeron.K1.check_key} *)

  val blit_key : t -> int -> t -> int -> int -> unit
  (** Same as {!Ephemeron.K1.blit_key} *)

  val get_data: t -> obj_t option
  (** Same as {!Ephemeron.K1.get_data} *)

  val get_data_copy: t -> obj_t option
  (** Same as {!Ephemeron.K1.get_data_copy} *)

  val set_data: t -> obj_t -> unit
  (** Same as {!Ephemeron.K1.set_data} *)

  val unset_data: t -> unit
  (** Same as {!Ephemeron.K1.unset_data} *)

  val check_data: t -> bool
  (** Same as {!Ephemeron.K1.check_data} *)

  val blit_data : t -> t -> unit
  (** Same as {!Ephemeron.K1.blit_data} *)
end

module Pointer : sig
(** Operations on raw pointers.

   This module provides operations on pointers, which represent raw machine
   addresses.  Any external pointer can be stored in a {!Nativeint.t} and
   presented back to external code.  Furthermore, pointers can be
   dereferenced, yielding the contents of the memory location they point to.

   It is almost always a very bad idea to expose pointers in library module
   interfaces -- they are intended for FFI work, such as accessing C data
   structures from OCaml without copying.  Libraries that use pointers
   should use them only to access data allocated by foreign allocators.

   Most users should not use pointers directly.  They are intended for use
   by higher-level libraries (such as ocaml-ctypes) as a faster alternative
   to C externals for accessing individual words of foreign data structures.

   If you control where the data is allocated, use the {!Bigarray} module
   instead, or use other libraries that provide safe abstractions.  The
   {!Bigarray} module ensures that the data lives as long as the bigarray is
   alive, and that it is freed when no longer accessible by OCaml code.
   Like raw pointers, the data contained in a bigarray can be accessed by
   other code without copying.

   Other libraries that provide higher-level functionality for manipulating
   foreign data include ocaml-ctypes and cstruct.

   Pointers are {e highly} unsafe.  There is no check (and indeed no way to
   check) that a pointer actually points to the location in memory that it
   claims to.  In fact, pointers are even less safe than C pointers, because
   OCaml pointers are untyped, while C pointers are typed.It is the
   responsibilty of any code that uses pointers to ensure that such use is safe,
   even given invalid or malicious input.  If you fail to do so, you will get a
   crash at best.  If the process is processing untrusted data, a misuse of
   pointers is likely to result in corruption of memory that allows for an
   attacker to execute arbitrary code.  Note that this applies to the rest of
   the {!Obj} module as well.

   @since 4.04.0
 *)

  external load8 : nativeint -> char = "%load8"
  (** Load a byte from the pointer.  The result is placed in an OCaml [char] *)

  external aligned_load16 : nativeint -> int = "%aligned_load16"
  (** Load 16 bits from a pointer.  The pointer must point to an even address,
      but this is not checked. *)

  external aligned_load32 : nativeint -> int32 = "%aligned_load32"
  (** Load 32 bits from a pointer.  The pointer must point to an address that
      is divisible by 4, but this is not checked. *)

  external aligned_load64 : nativeint -> int64 = "%aligned_load64"
  (** Load 64 bits from a pointer.  The pointer must point to an address that
      is divisible by 8, but this is not checked. *)

  (* val aligned_loadnative : nativeint -> nativeint *)
  (* (\** Load a machine word from a pointer.  The pointer must point to an *)
  (*     address that is a multiple of the size of a machine word in bytes, *)
  (*     but this is not checked. *\) *)

  external unaligned_load16 : nativeint -> int = "%unaligned_load16"
  (** Load 16 bits from a pointer.  The pointer need not be aligned. *)

  external unaligned_load32 : nativeint -> int32 = "%unaligned_load32"
  (** Load 32 bits from a pointer.  The pointer need not be aligned. *)

  external unaligned_load64 : nativeint -> int64 = "%unaligned_load64"
  (** Load 64 bits from a pointer.  The pointer need not be aligned. *)

  (* val unaligned_loadnative : nativeint -> nativeint *)
  (** Load a machine word from a pointer.  The pointer need not be aligned. *)

end

module Repr : sig
(** Operations on ocaml values.

    This module is very similar to the previous one except it works on
    OCaml values. These functions are GC friendly compared to the
    previous one. They take one ocaml value and a memory offset.

    @since 4.04.0
*)

  external load8 : t -> int -> char = "%string_unsafe_get"
  (** Load a byte from a value and an offset. The result is placed
      in an OCaml [char] *)

  external unaligned_load16 : t -> int -> int = "%caml_string_get16u"
  (** Load 16 bits from a value and an offset. The resulting pointer
      need not be aligned. *)

  external unaligned_load32 : t -> int -> int32 = "%caml_string_get32u"
  (** Load 32 bits from a value and an offset. The resulting pointer
      need not be aligned. *)

  external unaligned_load64 : t -> int -> int64 = "%caml_string_get64u"
  (** Load 64 bits from a value and an offset. The resulting pointer
      need not be aligned. *)

  val unaligned_loadnative : t -> int -> nativeint
  (** Load a machine word from a value and an offset. The resulting
      pointer need not be aligned. *)

  external set8 : t -> int -> char -> unit = "%string_unsafe_set"
  (** Set a byte from a value and an offset. The result is placed
      in an OCaml [char] *)

  external unaligned_set16 : t -> int -> int -> unit = "%caml_string_set16u"
  (** Set 16 bits from a value and an offset. The resulting pointer
      need not be aligned. *)

  external unaligned_set32 : t -> int -> int32 -> unit = "%caml_string_set32u"
  (** Set 32 bits from a value and an offset. The resulting pointer
      need not be aligned. *)

  external unaligned_set64 : t -> int -> int64 -> unit = "%caml_string_set64u"
  (** Set 64 bits from a value and an offset. The resulting pointer
      need not be aligned. *)

  val unaligned_setnative : t -> int -> nativeint -> unit
  (** Set a machine word from a value and an offset. The resulting
      pointer need not be aligned. *)

end
