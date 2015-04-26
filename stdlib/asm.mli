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

(** Arch *)

type arch = X86 | AMD64 | ARM | ARM64 | POWER | Byte

val pp_arch: Format.formatter -> arch -> unit

external arch: arch = "%asm_arch"

(** Inline Asm *)

type 'reg input
type ('reg,'app,'res) output
type 'app callback

exception Wrong_architecture of arch
(** requested architecture *)

type amd64_reg = [ `RAX | `RCX | `RDX | `RBX | `RSP | `RBP | `RSI | `RDI]

external amd64:
  input:amd64_reg input list ->
  string ->
  effect:[amd64_reg | `VReg of string | `Memory] list ->
  output:(amd64_reg,'app,'res) output ->
  label:([`End | `Label of string] * 'app) list ->
  'res = "%asm_amd64"

type x86_reg = [ `EAX ]

external x86:
  input:x86_reg input list ->
  string ->
  effect:[x86_reg | `VReg of string | `Memory] list ->
  output:(x86_reg,'app,'res) output ->
  label:([`End | `Label of string] * 'app) list ->
  'res = "%asm_x86"

(** {2 Inputs} *)

external input_value : ?force_reg:'reg -> string -> 'a -> 'reg input
  = "%asm_input_value"

external input_float : ?force_reg:'reg -> string -> float -> 'reg input
  = "%asm_input_float"

(** {2 Outputs} *)

external output_value: ?force_reg:'reg -> string ->
  ('reg, 'app, 'res) output -> ('reg, 'arg -> 'app, 'res) output
  = "%asm_output_value"

external output_float: ?force_reg:'reg -> string ->
  ('reg, 'app, 'res) output -> ('reg, float -> 'app, 'res) output
  = "%asm_output_float"

external output_end  : ('reg,unit -> 'res,'res) output
  = "%asm_output_end"
