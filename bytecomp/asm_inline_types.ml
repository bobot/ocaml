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

type boxing =
  | Boxed (** ocaml value (can be an ocaml [int]) *)
  (** unboxed *)
  | Float

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

type ('reg,'input,'id,'branch) t = {
  inputs   : ('reg vreg * 'input) list;
  outputs  : ('reg vreg * 'id) list;
  effects  : 'reg effect list;
  asmcode  : asmcode list;
  branches : (label * 'branch) list;
}



let print print_reg print_input print_output print_branch ppf asm =
  let print_boxing ppf = function
    | Boxed -> Format.fprintf ppf "Boxed"
    | Float -> Format.fprintf ppf "Float" in
  let print_vreg ppf vreg =
    match vreg.reg with
    | None     -> Format.fprintf ppf "%s[%a]" vreg.name print_boxing vreg.boxing
    | Some reg -> Format.fprintf ppf "%s[%a](%a)"
                    vreg.name print_boxing vreg.boxing print_reg reg  in
  let print_input ppf (vreg, inp) =
    Format.fprintf ppf "%a -> %a" print_input inp print_vreg vreg in
  let print_output ppf (vreg, out) =
    Format.fprintf ppf "%a -> %a" print_vreg vreg print_output out in
  let print_effect ppf = function
    | EReg reg -> print_reg ppf reg
    | EVreg vreg -> Format.pp_print_string ppf vreg
    | EMemory -> Format.pp_print_string ppf "memory"
  in
  let print_label ppf = function
    | LEnd -> Format.fprintf ppf "LEnd"
    | LLabel lab -> Format.fprintf ppf "L %s" lab
  in
  let print_branch ppf (label, br) =
    Format.fprintf ppf "%a: %a" print_label label print_branch br in
  let rec print_asmcode ppf = function
    | [] -> ()
    | AText s::l->
        Format.fprintf ppf "AText:%s" s;
        (* Format.pp_print_string ppf s; *)
        print_asmcode ppf l
    | AVar s::l ->
        Format.fprintf ppf "AVar:%s" s;
        (* Format.pp_print_string ppf s; *)
        print_asmcode ppf l
  in
  let pp_sep ppf () = Format.fprintf ppf ";@ " in
  Format.fprintf ppf "ASM{@[<hv>\
                      @[inputs =@ %a;@]@,\
                      @[outputs=@ %a;@]@,\
                      @[effects=@ %a;@]@,\
                      @[asmcode=@ %a;@]@,\
                      @[<hov 2>branches=@ %a@]\
                      @]"
    (Format.pp_print_list ~pp_sep print_input) asm.inputs
    (Format.pp_print_list ~pp_sep print_output) asm.outputs
    (Format.pp_print_list ~pp_sep print_effect) asm.effects
    print_asmcode asm.asmcode
    (Format.pp_print_list ~pp_sep print_branch) asm.branches

let emit conv_reg conv_label conv_text asm =
  let aux = function
    | AVar s ->
        let rec find_label = function
          | [] -> invalid_arg (Printf.sprintf "inline_asm unbounded var: %s" s)
          | (LLabel s', lab)::_ when s = s' -> conv_label lab
          | _::l -> find_label l in
        let rec find_reg next_find = function
          | [] -> next_find ()
          | ({name = s'; reg = Some reg},_)::_ when s = s' -> conv_reg reg
          | _::l -> find_reg next_find l in
        find_reg (fun () -> find_reg (fun () -> find_label asm.branches)
                     asm.inputs) asm.outputs
    | AText s -> conv_text s in
  List.map aux asm.asmcode

let list_map_snd f l = List.map (fun (a,b) -> (a,f b)) l
let list_iter_snd f l = List.iter (fun (_,b) -> f b) l
let list_exists_snd f l = List.exists (fun (_,b) -> f b) l
let list_fold_snd f acc l = List.fold_left (fun acc (_,b) -> f acc b) acc l
let list_fold_map_snd f acc l =
  List.fold_left (fun (acc,l') (lab,b) ->
      let (acc, b') = f acc b in
      (acc,(lab,b')::l')
    ) (acc,[]) l

let map_branches f asm  = { asm with branches = list_map_snd f asm.branches }
let iter_branches f asm = list_iter_snd f asm.branches
let exists_branches f asm = list_exists_snd f asm.branches
let fold_branches f acc asm = list_fold_snd f acc asm.branches
let fold_map_branches f acc asm =
  let (acc,l) = list_fold_map_snd f acc asm.branches in
  acc, {asm with branches = List.rev l}


let map_inputs f asm  = { asm with inputs = list_map_snd f asm.inputs }
let iter_inputs f asm = list_iter_snd f asm.inputs
let exists_inputs f asm = list_exists_snd f asm.inputs

let map_exprs f asm  = { asm with branches = list_map_snd f asm.branches;
                              inputs = list_map_snd f asm.inputs}
let iter_exprs f asm = list_iter_snd f asm.branches; list_iter_snd f asm.inputs
let exists_exprs f asm =
  list_exists_snd f asm.branches || list_exists_snd f asm.inputs

let map_outputs f asm  = { asm with outputs = list_map_snd f asm.outputs }
let iter_outputs f asm = list_iter_snd f asm.outputs
let exists_outputs f asm = list_exists_snd f asm.outputs

let map_vreg f vreg = {vreg with
                       reg = match vreg.reg with
                         | None -> None
                         | Some v -> Some (f v)}

let map_regs f asm =
  { asm with
    outputs = List.map (fun (v,o) -> (map_vreg f v,o)) asm.outputs;
    inputs = List.map (fun (v,i) -> (map_vreg f v,i)) asm.inputs;
    effects = List.map (function
        | EReg r -> EReg (f r)
        | EVreg x -> EVreg x
        | EMemory -> EMemory) asm.effects;
  }
let size asm =
  let rec aux acc = function
    | [] -> 0
    | AVar s::l -> aux acc l
    | AText s::l ->
        let size = ref acc in
        for i = 0 to String.length s - 1 do
          if s.[i] = '\n' then incr size;
        done;
        aux (!size) l
  in
  aux 0 asm.asmcode


let get = function
  | None -> assert false (** all virtual register should be known *)
  | Some r -> r

let get_inputs_reg asm = List.map (fun ({reg},_)-> get reg) asm.inputs
let get_outputs_reg asm = List.map (fun ({reg},_)-> get reg) asm.outputs
let get_effects_reg asm =
  List.fold_left (fun acc -> function
      | EReg reg -> reg::acc
      | EVreg _ -> assert false (** all virtual register should be known *)
      | EMemory -> acc) [] asm.effects
