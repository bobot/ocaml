(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Damien Doligez, projet Para, INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 1997 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(** Ephemerons and weak table *)

(** Ephemerons and weak table

    Ephemerons are defined in a language agnostic way in this paper:
    B. Hayes, Ephemerons: a New Finalization Mechanism, OOPSLA'9

    Ephemerons hold some keys and one data. They are all boxed ocaml values and
    suffer of the same limitation than weak pointers.

    The keys of an ephemerons have the same behavior than weak
    pointers according to the garbage collector.

    The keys and data of an ephemeron are said to be full if it points to a
    value, empty if the value have never been set or was erased by the GC.

    The data is considered by the garbage collector alive if all the
    full keys are alive and if the ephemeron is alive.
    When one of the keys is not considered alive anymore, the data is
    emptied from the ephemeron even if the data is alive for another
    reason.
*)

module type SeededS = sig
  include Hashtbl.SeededS
  val stats_alive: 'a t -> Hashtbl.statistics
  (** same as {!stats} but only count the alive bindings *)
end

module K1 : sig
  type ('k,'d) t (** an ephemeron with one key *)

  val create: unit -> ('k,'d) t
  (** [Ephemeron.K1.create ()] creates an ephemeron with one key. The
      data and key are empty *)

  val get_key: ('k,'d) t -> 'k option
  (** [Ephemeron.K1.get_key eph] returns [None] if the key of [eph] is
      empty, [Some x] (where [x] is the key) if it is full. *)

  val get_key_copy: ('k,'d) t -> 'k option
  (** [Ephemeron.K1.get_key eph] returns [None] if the key of [eph] is
      empty, [Some x] (where [x] is a (shallow) copy of the key) if
      it is full. This function as the same GC friendliness as {!Weak.get_copy}
  *)

  val set_key: ('k,'d) t -> 'k -> unit
  (** [Ephemeron.K1.set_key eph el] sets the key of [eph] to be a
      (full) key to [el]
  *)

  val unset_key: ('k,'d) t -> unit
  (** [Ephemeron.K1.unset_key eph el] sets the key of [eph] to be an
      empty key. Since there is only one key, the ephemeron behave like a
      references on the data.
  *)

  val check_key: ('k,'d) t -> bool
  (** [Ephemeron.K1.check_key eph] returns [true] if the key of the [eph]
      is full, [false] if it is empty. Note that even if
      [Ephemeron.K1.check_key eph] returns [true], a subsequent
      {!Ephemeron.K1.get_key}[eph] can return [None].*)


  val blit_key : ('k,_) t -> ('k,_) t -> unit
  (** [Ephemeron.K1.blit_key eph1 eph2] sets the key of [eph2] with
      the key of [eph1]. Contrary to using [Ephemeron.K1.get_key]
      followed by [Ephemeron.K1.set_key] or [Ephemeon.K1.unset_key]
      this function does not prevent the incremental GC from erasing
      the value in its current cycle. *)

  val get_data: ('k,'d) t -> 'd option
  val get_data_copy: ('k,'d) t -> 'd option
  val set_data: ('k,'d) t -> 'd -> unit
  val unset_data: ('k,'d) t -> unit
  val check_data: ('k,'d) t -> bool

  module MakeSeeded (H:Hashtbl.SeededHashedType) : SeededS with type key = H.t
end

module K2 : sig
  type ('k1,'k2,'d) t (** an ephemeron with two keys *)

  val create: unit -> ('k1,'k2,'d) t

  val get_key1: ('k1,'k2,'d) t -> 'k1 option
  val get_key1_copy: ('k1,'k2,'d) t -> 'k1 option
  val set_key1: ('k1,'k2,'d) t -> 'k1 -> unit
  val unset_key1: ('k1,'k2,'d) t -> unit
  val check_key1: ('k1,'k2,'d) t ->  bool

  val get_key2: ('k1,'k2,'d) t -> 'k2 option
  val get_key2_copy: ('k1,'k2,'d) t -> 'k2 option
  val set_key2: ('k1,'k2,'d) t -> 'k2 -> unit
  val unset_key2: ('k1,'k2,'d) t -> unit
  val check_key2: ('k1,'k2,'d) t -> bool

  val blit_key1  : ('k1,_,_) t -> ('k1,_,_) t -> unit
  val blit_key2  : (_,'k2,_) t -> (_,'k2,_) t -> unit
  val blit_key12 : ('k1,'k2,_) t -> ('k1,'k2,_) t -> unit

  val get_data: ('k1,'k2,'d) t -> 'd option
  val get_data_copy: ('k1,'k2,'d) t -> 'd option
  val set_data: ('k1,'k2,'d) t -> 'd -> unit
  val check_data: ('k1,'k2,'d) t -> bool

  module MakeSeeded
      (H1:Hashtbl.SeededHashedType)
      (H2:Hashtbl.SeededHashedType) :
    SeededS with type key = H1.t * H2.t
end

module Obj: sig
    (** This module define very low-level untyped use of ephemeron

        Not for the casual user.
    *)

  type t
    (** an ephemeron *)

  val create: int -> t
  val length: t -> int
    (** number of keys *)

  val get_key: t -> int -> Obj.t option
  val get_key_copy: t -> int -> Obj.t option
  val set_key: t -> int -> Obj.t -> unit
  val unset_key: t -> int -> unit
  val check_key: t -> int -> bool
  val blit_key : t -> int -> t -> int -> int -> unit

  val get_data: t -> Obj.t option
  val get_data_copy: t -> Obj.t option
  val set_data: t -> Obj.t -> unit
  val unset_data: t -> unit
  val check_data: t -> bool
  val blit_data : t -> t -> unit

  type equal =
  | ETrue | EFalse
  | EDead (** the garbage collector reclaimed the data *)

  module MakeSeeded(H: sig
    type t
    type 'a container
    val create: t -> 'a -> 'a container
    val hash: int -> t -> int
    val equal: t -> 'a container -> equal
    val get_key: 'a container -> t option
    val get_data: 'a container -> 'a option
    val set_data: 'a container -> 'a -> unit
    val check_key: 'a container -> bool
  end) : SeededS with type key = H.t

end
