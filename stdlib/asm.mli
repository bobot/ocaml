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
(** [pp_arch fmt arch] pretty print the architecture name *)

external arch: arch = "%asm_arch"
(** [arch] is the current architecture. In bytecode it is always [Byte]. *)

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
(** The registers that can be used for amd64.
    `RBP is not usable because it can store the frame pointer in some
    configuration
*)


external amd64:
  input:amd64_reg input list ->
  string ->
  effect:[amd64_reg | `VReg of string | `Memory] list ->
  output:(amd64_reg,'app,'res) output ->
  label:([`End | `Label of string] * 'app) list ->
  'res = "%asm_amd64"
(** [amd64 ~input asm ~effect ~output ~label] call the assembly code
    [asm] after replacing the "%name" with the registers allocated for
    the input or output with the corresponding name or with the label
    corresponding to the execution of the associated expression.

    ['res] is the return type of all the branch defined in [label].

    @raise Wrong_architecture(AMD64) if it have been compiled not in
    native amd64.
*)

type i386_reg = [ `EAX | `EBX | `ECX | `EDX | `ESI | `EDI | `EBP | `TOS ]

external i386:
  input:i386_reg input list ->
  string ->
  effect:[i386_reg | `VReg of string | `Memory] list ->
  output:(i386_reg,'app,'res) output ->
  label:([`End | `Label of string] * 'app) list ->
  'res = "%asm_i386"
(** same as {!Asm.amd64} but for i386 *)

(** {2 Inputs} *)

external ivalue : ?force_reg:'reg -> string -> 'a -> 'reg input
  = "%asm_input_value"
(** [ivalue name value] put the boxed value [value] in the register named [name]
    (with the %) in the assembly *)


external ifloat : ?force_reg:'reg -> string -> float -> 'reg input
  = "%asm_input_float"
(** [ifloat name value] unbox the given float [value] in the register
    named [name] (with the %) in the assembly *)

(** {2 Outputs} *)

external ovalue: ?force_reg:'reg -> string ->
  ('reg, 'app, 'res) output -> ('reg, 'arg -> 'app, 'res) output
  = "%asm_output_value"
(** [ovalue name ...] add a binder which will be binded with the boxed value
    named [name]. If name is the same than one of the input they will
    share the register.
*)

external ofloat: ?force_reg:'reg -> string ->
  ('reg, 'app, 'res) output -> ('reg, float -> 'app, 'res) output
  = "%asm_output_float"
(** [ofloat name ...] add a binder which will be binded with the boxed
    version of the float named [name].If name is the same than one of
    the input they will share the register.
*)

external oend  : ('reg,unit -> 'res,'res) output
  = "%asm_output_end"
(** [oend] end the list of argument with the unit argument *)
