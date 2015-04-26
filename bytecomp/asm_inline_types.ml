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



let print print_input print_output print_branch ppf asm =
  let print_boxing ppf = function
    | Boxed -> Format.fprintf ppf "Boxed"
    | Float -> Format.fprintf ppf "Float" in
  let print_vreg ppf vreg =
    match vreg.reg with
    | None     -> Format.fprintf ppf "%s[%a]" vreg.name print_boxing vreg.boxing
    | Some reg -> Format.fprintf ppf "%s[%a](%s)"
                    vreg.name print_boxing vreg.boxing reg  in
  let print_input ppf (vreg, inp) =
    Format.fprintf ppf "%a -> %a" print_input inp print_vreg vreg in
  let print_output ppf (vreg, out) =
    Format.fprintf ppf "%a -> %a" print_vreg vreg print_output out in
  let print_effect ppf = function
    | EReg reg -> Format.pp_print_string ppf reg
    | EVreg vreg -> Format.pp_print_string ppf vreg
    | EMemory -> Format.pp_print_string ppf "memory"
  in
  let print_label ppf = function
    | LEnd -> Format.fprintf ppf "LEnd"
    | LLabel lab -> Format.fprintf ppf "L %s" lab
  in
  let print_branch ppf (label, br) =
    Format.fprintf ppf "%a: %a" print_label label print_branch br in
  let pp_sep ppf () = Format.fprintf ppf ";@ " in
  Format.fprintf ppf "ASM{@[<hv>\
                      @[inputs =@ %a;@]@,\
                      @[outputs=@ %a;@]@,\
                      @[effects=@ %a;@]@,\
                      @[asmcode=@ %S;@]@,\
                      @[<hov 2>branches=@ %a@]\
                      @]"
    (Format.pp_print_list ~pp_sep print_input) asm.inputs
    (Format.pp_print_list ~pp_sep print_output) asm.outputs
    (Format.pp_print_list ~pp_sep print_effect) asm.effects
    asm.asmcode
    (Format.pp_print_list ~pp_sep print_branch) asm.branches

let list_map_snd f l = List.map (fun (a,b) -> (a,f b)) l
let list_iter_snd f l = List.iter (fun (_,b) -> f b) l

let map_branches f asm  = { asm with branches = list_map_snd f asm.branches }
let iter_branches f asm = list_iter_snd f asm.branches


let map_inputs f asm  = { asm with inputs = list_map_snd f asm.inputs }
let iter_inputs f asm = list_iter_snd f asm.inputs

let map_exprs f asm  = { asm with branches = list_map_snd f asm.branches;
                              inputs = list_map_snd f asm.inputs}
let iter_exprs f asm = list_iter_snd f asm.branches; list_iter_snd f asm.inputs

let map_outputs f asm  = { asm with outputs = list_map_snd f asm.outputs }
let iter_outputs f asm = list_iter_snd f asm.outputs
