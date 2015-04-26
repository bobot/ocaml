(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            François Bobot, CEA                                      *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(** Regroups all the function relative to inline aassembly *)

type reg = string

type boxing =
  | Boxed (** ocaml value (can be an ocaml [int]) *)
  (** unboxed *)
  | Float

type vreg = {
  reg   : reg option;
  name  : string;
  boxing: boxing;
}

type effect =
  | EReg of reg
  | EVreg of string
  | EMemory

type label =
  | LEnd
  | LLabel of string

type ('input,'id,'branch) t = {
  inputs   : (vreg * 'input) list;
  outputs  : (vreg * 'id) list;
  effects  : effect list;
  asmcode  : string;
  branches : (label * 'branch) list;
}
(** An inline asm. The ['output] is the type of the binder for the outputs.
    The ['expr] is the type of the expression that gives the input and
    the type of the expression executed after the execution of the inline asm
    (end of the asm or after jump)
*)

val print:
  (Format.formatter -> 'input -> unit) ->
  (Format.formatter -> 'output -> unit) ->
  (Format.formatter -> 'branch -> unit) ->
  Format.formatter -> ('input,'output,'branch) t -> unit

val map_branches : ('a -> 'b) -> ('input,'output,'a) t -> ('input,'output,'b) t
val iter_branches: ('a -> unit) -> ('input,'output,'a) t -> unit

val map_inputs : ('a -> 'b) -> ('a,'output,'branch) t -> ('b,'output,'branch) t
val iter_inputs: ('a -> unit) -> ('a,'output,'branch) t -> unit

val map_exprs : ('a -> 'b) -> ('a,'output,'a) t -> ('b,'output,'b) t
val iter_exprs : ('a -> unit) -> ('a,'output,'a) t -> unit
(** map branches and inputs *)

val map_outputs : ('a -> 'b) -> ('input,'a,'branch) t -> ('input,'b,'branch) t
val iter_outputs: ('a -> unit) -> ('input,'a,'branch) t -> unit
