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

type vreg = {
  reg : reg option;
  name: string;
}

type effect =
  | EReg of reg
  | EMemory

type label =
  | LEnd
  | Label of string

type ('input,'id,'branch) t = {
  inputs   : (vreg * 'input) list;
  outputs  : (vreg * 'id) list;
  effects  : effect list;
  asmcode  : string;
  branches : (label * 'branch) list;
}



let print print_input print_output print_branch ppf asm =
  let print_vreg ppf vreg =
    match vreg.reg with
    | None     -> Format.fprintf ppf "%s" vreg.name
    | Some reg -> Format.fprintf ppf "%s(%s)" vreg.name reg in
  let print_input ppf (vreg, inp) =
    Format.fprintf ppf "%a -> %a" print_input inp print_vreg vreg in
  let print_output ppf (vreg, out) =
    Format.fprintf ppf "%a -> %a" print_vreg vreg print_output out in
  let print_effect ppf = function
    | EReg reg -> Format.pp_print_string ppf reg
    | EMemory -> Format.pp_print_string ppf "memory"
  in
  let print_label ppf = function
    | LEnd -> Format.fprintf ppf "LEnd"
    | Label lab -> Format.fprintf ppf "L %s" lab
  in
  let print_branch ppf (label, br) =
    Format.fprintf ppf "%a: %a" print_label label print_branch br in
  let pp_sep ppf () = Format.fprintf ppf ";@ " in
  Format.fprintf ppf "ASM{\
                      @[inputs =@,%a@];@,\
                      @[outputs=@,%a@];@,\
                      @[effects=@,%a@];@,\
                      @[asmcode=@,%S@];@,\
                      @[branches=@,%a@]\
                     "
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


let asm_primitives =
  let h = Hashtbl.create 20 in
  let l = [] in
  List.iter (fun (s,f) -> Hashtbl.add h s f) l;
  h

let is_asm_primitive s = Hashtbl.mem asm_primitives s

type is_asm_application_result =
  | Ok of (Typedtree.expression,Ident.t,Typedtree.expression) t
  (** The asm inline specification correctly have been parsed *)
  | Expr of Typedtree.expression
  (** The result is directly an expression (eg. arch ) *)
  | Badly_placed_asm_primitive
  (** not an asm inline specific but yet an asm primitive *)
  | Not_complete of Location.t
  (** asm inline specification with variables or partial application *)
  | Other_primitive


let is_asm_application s args =
  let _ = EReg "rax" in
  let _ = EMemory in let _ = LEnd in let _ = Label "goto" in
  Other_primitive
