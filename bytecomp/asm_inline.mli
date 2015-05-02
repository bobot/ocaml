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
open Asm_inline_types

val is_asm_primitive : string -> bool

(** Initial Parsing *)

open Typedtree

type is_asm_application_result =
  | Ok of (string,expression,Ident.t,expression) t *
          (Asttypes.arg_label * expression option * optional) list
  (** The asm inline specification have been parsed correctly.
      The remaining arguments are return.
  *)
  | Expr of expression
  (** The result is directly an expression (eg. arch ) *)
  | Badly_placed_asm_primitive
  (** not an asm inline specific but yet an asm primitive *)
  | Not_complete of Location.t
  (** asm inline specification with variables or partial application *)
  | Other_primitive

val is_asm_application: expression ->  is_asm_application_result
