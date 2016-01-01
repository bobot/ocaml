(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(** Operations on internal representations of values.

   Not for the casual user.
*)

type t

external repr : 'a -> t = "%identity"
external obj : t -> 'a = "%identity"
external magic : 'a -> 'b = "%identity"
external is_block : t -> bool = "caml_obj_is_block"
external is_int : t -> bool = "%obj_is_int"
external tag : t -> int = "caml_obj_tag"
external set_tag : t -> int -> unit = "caml_obj_set_tag"
external size : t -> int = "%obj_size"
external field : t -> int -> t = "%obj_field"
external set_field : t -> int -> t -> unit = "%obj_set_field"
val double_field : t -> int -> float  (* @since 3.11.2 *)
val set_double_field : t -> int -> float -> unit  (* @since 3.11.2 *)
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
val extension_name : extension_constructor -> string
val extension_id : extension_constructor -> int

(** The following two functions are deprecated.  Use module {!Marshal}
    instead. *)

val marshal : t -> bytes
  [@@ocaml.deprecated "Use Marshal.to_bytes instead."]
val unmarshal : bytes -> int -> t * int
  [@@ocaml.deprecated "Use Marshal.from_bytes and Marshal.total_size instead."]

module Ephemeron: sig
  (** Ephemeron with arbitrary arity and untyped *)

  type eph
  (** an ephemeron cf {!Ephemeron} *)

  val create: int -> eph
  (** [create n] returns an ephemeron with [n] keys.
      All the keys and the data are initially empty *)
  val length: eph -> int
  (** return the number of keys *)

  val get_key: eph -> int -> t option
  (** Same as {!Ephemeron.K1.get_key} *)
  val get_key_copy: eph -> int -> t option
  (** Same as {!Ephemeron.K1.get_key_copy} *)
  val set_key: eph -> int -> t -> unit
  (** Same as {!Ephemeron.K1.set_key} *)
  val unset_key: eph -> int -> unit
  (** Same as {!Ephemeron.K1.unset_key} *)
  val check_key: eph -> int -> bool
  (** Same as {!Ephemeron.K1.check_key} *)
  val blit_key : eph -> int -> eph -> int -> int -> unit
  (** Same as {!Ephemeron.K1.blit_key} *)

  val get_data: eph -> t option
  (** Same as {!Ephemeron.K1.get_data} *)
  val get_data_copy: eph -> t option
  (** Same as {!Ephemeron.K1.get_data_copy} *)
  val set_data: eph -> t -> unit
  (** Same as {!Ephemeron.K1.set_data} *)
  val unset_data: eph -> unit
  (** Same as {!Ephemeron.K1.unset_data} *)
  val check_data: eph -> bool
  (** Same as {!Ephemeron.K1.check_data} *)
  val blit_data : eph -> eph -> unit
  (** Same as {!Ephemeron.K1.blit_data} *)
end
