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
    full keys are alive and if the ephemeron is alive. When one of the
    keys is not considered alive anymore by the GC, the data is
    emptied from the ephemeron even if the data is alive for another
    reason.

    The ephemerons complicate the notion of liveness of values, because
    it is not anymore an equivalence with the reachability from root
    value by usual pointers (not weak and not ephemerons). The notion
    of liveness is constructed by the least fixpoint of:
    A value is alive if:
     - it is a root value
     - it is reachable from alive value by usual pointers
     - it is the data of an ephemeron with all its full keys alive

*)

module type S = sig
  include Hashtbl.S
  val stats_alive: 'a t -> Hashtbl.statistics
  (** same as {!Hashtbl.SeededS.stats} but only count the alive bindings *)
end
(** The output signature of the functor {!K1.Make} and {!K2.Make}.
    These hashtables are weak in the keys. If all the keys of a binding are
    alive the binding is kept, but if one of the keys of the binding
    is dead then the binding is removed.
*)

module type SeededS = sig
  include Hashtbl.SeededS
  val stats_alive: 'a t -> Hashtbl.statistics
  (** same as {!Hashtbl.SeededS.stats} but only count the alive bindings *)
end
(** The output signature of the functor {!K1.MakeSeeded} and {!K2.MakeSeeded}.
*)

module K1 : sig
  type ('k,'d) t (** an ephemeron with one key *)

  val create: unit -> ('k,'d) t
  (** [Ephemeron.K1.create ()] creates an ephemeron with one key. The
      data and key are empty *)

  val get_key: ('k,'d) t -> 'k option
  (** [Ephemeron.K1.get_key eph] returns [None] if the key of [eph] is
      empty, [Some x] (where [x] is the key) if it is full. *)

  val get_key_copy: ('k,'d) t -> 'k option
  (** [Ephemeron.K1.get_key_copy eph] returns [None] if the key of [eph] is
      empty, [Some x] (where [x] is a (shallow) copy of the key) if
      it is full. This function has the same GC friendliness as {!Weak.get_copy}
  *)

  val set_key: ('k,'d) t -> 'k -> unit
  (** [Ephemeron.K1.set_key eph el] sets the key of [eph] to be a
      (full) key to [el]
  *)

  val unset_key: ('k,'d) t -> unit
  (** [Ephemeron.K1.unset_key eph el] sets the key of [eph] to be an
      empty key. Since there is only one key, the ephemeron start
      behaving like a references on the data. *)

  val check_key: ('k,'d) t -> bool
  (** [Ephemeron.K1.check_key eph] returns [true] if the key of the [eph]
      is full, [false] if it is empty. Note that even if
      [Ephemeron.K1.check_key eph] returns [true], a subsequent
      {!Ephemeron.K1.get_key}[eph] can return [None].
  *)


  val blit_key : ('k,_) t -> ('k,_) t -> unit
  (** [Ephemeron.K1.blit_key eph1 eph2] sets the key of [eph2] with
      the key of [eph1]. Contrary to using [Ephemeron.K1.get_key]
      followed by [Ephemeron.K1.set_key] or [Ephemeon.K1.unset_key]
      this function does not prevent the incremental GC from erasing
      the value in its current cycle. *)

  val get_data: ('k,'d) t -> 'd option
  (** [Ephemeron.K1.get_data eph] returns [None] if the data of [eph] is
      empty, [Some x] (where [x] is the data) if it is full. *)

  val get_data_copy: ('k,'d) t -> 'd option
  (** [Ephemeron.K1.get_data_copy eph] returns [None] if the data of [eph] is
      empty, [Some x] (where [x] is a (shallow) copy of the data) if
      it is full. This function has the same GC friendliness as {!Weak.get_copy}
  *)

  val set_data: ('k,'d) t -> 'd -> unit
  (** [Ephemeron.K1.set_data eph el] sets the data of [eph] to be a
      (full) data to [el]
  *)

  val unset_data: ('k,'d) t -> unit
  (** [Ephemeron.K1.unset_key eph el] sets the key of [eph] to be an
      empty key. The ephemeron start behaving like a weak pointer.
  *)

  val check_data: ('k,'d) t -> bool
  (** [Ephemeron.K1.check_data eph] returns [true] if the data of the [eph]
      is full, [false] if it is empty. Note that even if
      [Ephemeron.K1.check_data eph] returns [true], a subsequent
      {!Ephemeron.K1.get_data}[eph] can return [None].
  *)

  val blit_data : (_,'d) t -> (_,'d) t -> unit
  (** [Ephemeron.K1.blit_data eph1 eph2] sets the data of [eph2] with
      the data of [eph1]. Contrary to using [Ephemeron.K1.get_data]
      followed by [Ephemeron.K1.set_data] or [Ephemeon.K1.unset_data]
      this function does not prevent the incremental GC from erasing
      the value in its current cycle. *)

  module Make (H:Hashtbl.HashedType) : S with type key = H.t
  (** Functor building an implementation of a weak hash table *)

  module MakeSeeded (H:Hashtbl.SeededHashedType) : SeededS with type key = H.t
  (** Functor building an implementation of a weak hash table.
      The seed is similar to the one of {!Hashtbl.MakeSeeded}. *)

end

module K2 : sig
  type ('k1,'k2,'d) t (** an ephemeron with two keys *)

  val create: unit -> ('k1,'k2,'d) t
  (** Same as {!Ephemeron.K1.create} *)

  val get_key1: ('k1,'k2,'d) t -> 'k1 option
  (** Same as {!Ephemeron.K1.get_key} *)
  val get_key1_copy: ('k1,'k2,'d) t -> 'k1 option
  (** Same as {!Ephemeron.K1.get_key_copy} *)
  val set_key1: ('k1,'k2,'d) t -> 'k1 -> unit
  (** Same as {!Ephemeron.K1.set_key} *)
  val unset_key1: ('k1,'k2,'d) t -> unit
  (** Same as {!Ephemeron.K1.unset_key} *)
  val check_key1: ('k1,'k2,'d) t ->  bool
  (** Same as {!Ephemeron.K1.check_key} *)

  val get_key2: ('k1,'k2,'d) t -> 'k2 option
  (** Same as {!Ephemeron.K1.get_key} *)
  val get_key2_copy: ('k1,'k2,'d) t -> 'k2 option
  (** Same as {!Ephemeron.K1.get_key_copy} *)
  val set_key2: ('k1,'k2,'d) t -> 'k2 -> unit
  (** Same as {!Ephemeron.K1.get_key} *)
  val unset_key2: ('k1,'k2,'d) t -> unit
  (** Same as {!Ephemeron.K1.unset_key} *)
  val check_key2: ('k1,'k2,'d) t -> bool
  (** Same as {!Ephemeron.K1.check_key} *)

  val blit_key1  : ('k1,_,_) t -> ('k1,_,_) t -> unit
  (** Same as {!Ephemeron.K1.blit_key} *)
  val blit_key2  : (_,'k2,_) t -> (_,'k2,_) t -> unit
  (** Same as {!Ephemeron.K1.blit_key} *)
  val blit_key12 : ('k1,'k2,_) t -> ('k1,'k2,_) t -> unit
  (** Same as {!Ephemeron.K1.blit_key} *)

  val get_data: ('k1,'k2,'d) t -> 'd option
  (** Same as {!Ephemeron.K1.get_data} *)
  val get_data_copy: ('k1,'k2,'d) t -> 'd option
  (** Same as {!Ephemeron.K1.get_data_copy} *)
  val set_data: ('k1,'k2,'d) t -> 'd -> unit
  (** Same as {!Ephemeron.K1.set_data} *)
  val check_data: ('k1,'k2,'d) t -> bool
  (** Same as {!Ephemeron.K1.check_data} *)
  val blit_data: ('k1,'k2,'d) t -> ('k1,'k2,'d) t -> unit
  (** Same as {!Ephemeron.K1.blit_data} *)

  module Make
      (H1:Hashtbl.HashedType)
      (H2:Hashtbl.HashedType) :
    S with type key = H1.t * H2.t
  (** Functor building an implementation of a weak hash table *)

  module MakeSeeded
      (H1:Hashtbl.SeededHashedType)
      (H2:Hashtbl.SeededHashedType) :
    SeededS with type key = H1.t * H2.t
  (** Functor building an implementation of a weak hash table.
      The seed is similar to the one of {!Hashtbl.MakeSeeded}. *)

end

module Obj: sig
    (** This module define very low-level untyped use of ephemeron

        Not for the casual user.
    *)

  type t
    (** an ephemeron *)

  val create: int -> t
  (** Same as {!Ephemeron.K1.create} *)
  val length: t -> int
  (** return the number of keys *)

  val get_key: t -> int -> Obj.t option
  (** Same as {!Ephemeron.K1.get_key} *)
  val get_key_copy: t -> int -> Obj.t option
  (** Same as {!Ephemeron.K1.get_key_copy} *)
  val set_key: t -> int -> Obj.t -> unit
  (** Same as {!Ephemeron.K1.set_key} *)
  val unset_key: t -> int -> unit
  (** Same as {!Ephemeron.K1.unset_key} *)
  val check_key: t -> int -> bool
  (** Same as {!Ephemeron.K1.check_key} *)
  val blit_key : t -> int -> t -> int -> int -> unit
  (** Same as {!Ephemeron.K1.blit_key} *)

  val get_data: t -> Obj.t option
  (** Same as {!Ephemeron.K1.get_data} *)
  val get_data_copy: t -> Obj.t option
  (** Same as {!Ephemeron.K1.get_data_copy} *)
  val set_data: t -> Obj.t -> unit
  (** Same as {!Ephemeron.K1.set_data} *)
  val unset_data: t -> unit
  (** Same as {!Ephemeron.K1.unset_data} *)
  val check_data: t -> bool
  (** Same as {!Ephemeron.K1.check_data} *)
  val blit_data : t -> t -> unit
  (** Same as {!Ephemeron.K1.blit_data} *)

  type equal =
  | ETrue | EFalse
  | EDead (** the garbage collector reclaimed the data *)

  module MakeSeeded(H:
  sig
    type t
    (** keys *)
    type 'a container
    (** contains keys and the associated data *)

    val hash: int -> t -> int
    (** same as {!Hashtbl.SeededHashedType} *)
    val equal: t -> 'a container -> equal
    (** equality predicate used to compare a key with the one in a
        container. Can return [EDead] if the keys in the container are
        dead *)

    val create: t -> 'a -> 'a container
    (** [create key data] creates a container from
        some initials keys and one data *)
    val get_key: 'a container -> t option
    (** [get_key cont] returns the keys if they are all alive *)
    val get_data: 'a container -> 'a option
    (** [get_data cont] return the data if it is alive *)
    val set_data: 'a container -> 'a -> unit
    (** [set_data cont] modify the data *)
    val check_key: 'a container -> bool
    (** [check_key cont] checks if all the keys contains in the data
        are alive *)
  end) : SeededS with type key = H.t
  (** Functor building an implementation of an hash table that use the container
      for keeping the information given *)

end
