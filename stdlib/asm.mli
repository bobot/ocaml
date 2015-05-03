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

type arch = I386 | AMD64 | ARM | ARM64 | POWER | Byte

val pp_arch: Format.formatter -> arch -> unit

external arch: arch = "%asm_arch"

(** Inline Asm *)

type 'reg input
type ('reg,'app,'res) output
type 'app callback

exception Wrong_architecture of arch
(** requested architecture *)

type amd64_reg = [ `RAX | `RBC | `RDI | `RSI | `RDX | `RCX | `R8 | `R9 | `R12
                 | `R13 | `R10 | `R11
                 | `XMM0 | `XMM1 | `XMM2 | `XMM3 | `XMM4 | `XMM5 | `XMM6 | `XMM7
                 | `XMM8 | `XMM9 | `XMM10 | `XMM11 | `XMM12 | `XMM13 | `XMM14
                 | `XMM15 ]

external amd64:
  input:amd64_reg input list ->
  string ->
  effect:[amd64_reg | `VReg of string | `Memory] list ->
  output:(amd64_reg,'app,'res) output ->
  label:([`End | `Label of string] * 'app) list ->
  'res = "%asm_amd64"


type i386_reg = [ `EAX | `EBX | `ECX | `EDX | `ESI | `EDI | `EBP | `TOS ]

external i386:
  input:i386_reg input list ->
  string ->
  effect:[i386_reg | `VReg of string | `Memory] list ->
  output:(i386_reg,'app,'res) output ->
  label:([`End | `Label of string] * 'app) list ->
  'res = "%asm_i386"

(** {2 Inputs} *)

external ivalue : ?force_reg:'reg -> string -> 'a -> 'reg input
  = "%asm_input_value"

external ifloat : ?force_reg:'reg -> string -> float -> 'reg input
  = "%asm_input_float"

(** {2 Outputs} *)

external ovalue: ?force_reg:'reg -> string ->
  ('reg, 'app, 'res) output -> ('reg, 'arg -> 'app, 'res) output
  = "%asm_output_value"

external ofloat: ?force_reg:'reg -> string ->
  ('reg, 'app, 'res) output -> ('reg, float -> 'app, 'res) output
  = "%asm_output_float"

external oend  : ('reg,unit -> 'res,'res) output
  = "%asm_output_end"
