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

type boxing =
  | Boxed (** ocaml value (can be an ocaml [int]) *)
  (** unboxed *)
  | Float

(** 'reg really correspond to the virtual register found
    in asmcomp.
 *)
type 'reg vreg = {
  reg   : 'reg option;
  name  : string;
  boxing: boxing;
}

type 'reg effect =
  | EReg of 'reg
  | EVreg of string
  | EMemory

type label =
  | LEnd
  | LLabel of string

type asmcode =
  | AText of string
  | AVar of string

type ('reg, 'input,'id,'branch) t = {
  inputs   : ('reg vreg * 'input) list;
  outputs  : ('reg vreg * 'id) list;
  effects  : 'reg effect list;
  asmcode  : asmcode list;
  branches : (label * 'branch) list;
}
(** An inline asm. The ['output] is the type of the binder for the outputs.
    The ['expr] is the type of the expression that gives the input and
    the type of the expression executed after the execution of the inline asm
    (end of the asm or after jump)
*)

val print:
  (Format.formatter -> 'reg -> unit) ->
  (Format.formatter -> 'input -> unit) ->
  (Format.formatter -> 'output -> unit) ->
  (Format.formatter -> 'branch -> unit) ->
  Format.formatter -> ('reg,'input,'output,'branch) t -> unit

val emit:
  ('reg -> 'a) -> ('label -> 'a) -> (string -> 'a) ->
  ('reg,unit,unit,'label) t -> 'a list

val map_branches :
  ('a -> 'b) -> ('reg,'input,'output,'a) t -> ('reg,'input,'output,'b) t
val iter_branches:
  ('a -> unit) -> ('reg,'input,'output,'a) t -> unit
val exists_branches :
  ('a -> bool) -> ('reg,'input,'output,'a) t -> bool
val fold_branches :
  ('acc -> 'branch -> 'acc) -> 'acc -> ('reg,'input,'output,'branch) t -> 'acc
val fold_map_branches :
  ('acc -> 'a -> 'acc * 'b) ->
  'acc -> ('reg,'input,'output,'a) t ->
  'acc * ('reg,'input,'output,'b) t

val map_inputs :
  ('a -> 'b) -> ('reg,'a,'output,'branch) t -> ('reg,'b,'output,'branch) t
val iter_inputs:
  ('a -> unit) -> ('reg,'a,'output,'branch) t -> unit
val exists_inputs :
  ('a -> bool) -> ('reg,'a,'output,'branch) t -> bool

val map_exprs :
  ('a -> 'b) -> ('reg,'a,'output,'a) t -> ('reg,'b,'output,'b) t
(** map branches and inputs *)
val iter_exprs :
  ('a -> unit) -> ('reg,'a,'output,'a) t -> unit
val exists_exprs :
  ('a -> bool) -> ('reg,'a,'output,'a) t -> bool

val map_outputs :
  ('a -> 'b) -> ('reg,'input,'a,'branch) t -> ('reg,'input,'b,'branch) t
val iter_outputs:
  ('a -> unit) -> ('reg,'input,'a,'branch) t -> unit
val exists_outputs:
  ('a -> bool) -> ('reg,'input,'a,'branch) t -> bool

val map_regs :
  ('a -> 'b) -> ('a,'input,'output,'branch) t -> ('b,'input,'output,'branch) t

val size: ('reg,'input,'output,'branch) t -> int
(** number of newline '\n' *)

val get_inputs_reg: ('reg,unit,unit,'branch) t -> 'reg list
val get_outputs_reg: ('reg,unit,unit,'branch) t -> 'reg list
val get_effects_reg: ('reg,unit,unit,'branch) t -> 'reg list
(** To be used only when all the register are known *)
