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

open Asm_inline_types

let asm_arch_supported = ["amd64";"x86"]

let asm_input_primitive = [
  "%asm_input_value", Boxed;
  "%asm_input_float", Float;
]


let asm_output_primitive = [
  "%asm_output_value", Boxed;
  "%asm_output_float", Float;
]


let asm_other_primitive = [
  "%asm_arch";
  "%asm_output_end";
]

let is_supported_arch =
  let offset = String.length "%asm_" in
  fun s ->
    let arch = String.sub s offset (String.length s - offset) in
    if List.mem arch asm_arch_supported
    then Some arch
    else None

let asm_primitives =
  let h = Hashtbl.create 20 in
  List.iter (fun s -> Hashtbl.add h ("%asm_"^s) ()) asm_arch_supported;
  List.iter (fun (s,_) -> Hashtbl.add h s ()) asm_input_primitive;
  List.iter (fun (s,_) -> Hashtbl.add h s ()) asm_output_primitive;
  List.iter (fun s -> Hashtbl.add h s ()) asm_other_primitive;
  h

let is_asm_primitive s =
  Hashtbl.mem asm_primitives s

open Typedtree
open Asttypes
open Types
open Primitive

type is_asm_application_result =
  | Ok of (string,expression,Ident.t,expression) t *
          (Asttypes.arg_label * expression option * optional) list
  (** The asm inline specification correctly have been parsed *)
  | Expr of expression
  (** The result is directly an expression (eg. arch ) *)
  | Badly_placed_asm_primitive
  (** not an asm inline specific but yet an asm primitive *)
  | Not_complete of Location.t
  (** asm inline specification with variables or partial application *)
  | Other_primitive

let dumb_loc = Location.in_file "asm_inline.ml"

let mk_construct cst args =
  Ast_helper.Exp.construct
    ~loc:dumb_loc
    { txt = Longident.Ldot(Longident.Lident("Asm"),cst);
      loc = dumb_loc; }
    args

let arch_of_string s =
  match s with
  | "amd64" -> "AMD64"
  | "x86" -> "X86"
  | "arm" -> "ARM"
  | "arm64" -> "ARM64"
  | "power" -> "POWER"
  | _ -> assert false (** absurd: all the architecture should be listed *)

let current_arch env =
  let cst =
    if not (!Clflags.native_code)
    then "Byte"
    else arch_of_string Config.architecture
  in
  Typecore.type_expression env (mk_construct cst None)

exception Error of is_asm_application_result
let raise_not_complete e =
  raise (Error(Not_complete e.exp_loc))

(** Extract name and applied expression from Texp_ident and Texp_apply *)
let extract_name e =
  match e.exp_desc with
  | Texp_ident(_, _, {val_kind = Val_prim p}) -> p.prim_name, []
  | Texp_apply({ exp_desc = Texp_ident(_, _, {val_kind = Val_prim p})},
               oargs) ->
      if List.length oargs < p.prim_arity
      then raise_not_complete e
      else p.prim_name, oargs
  | _ -> assert false (** absurd: should not be used with other constructors *)

let parse_vreg reg name boxing =
  let reg = match reg with
    | (_,None,_) -> assert false (** absurd *)
    | (_,Some {exp_desc = Texp_construct(_,{cstr_name = "None"},[])},_) -> None
    | (_,Some {exp_desc = Texp_variant(reg,arg)}, _) ->
        assert (arg = None); (** By typing: constant variant *)
        Some (String.lowercase_ascii reg)
    | (_,Some e,_) -> raise_not_complete e
  in
  let name = match name with
    | (_,None,_) ->
        assert false (** absurd: by typing not an optional argument *)
    | (_,Some {exp_desc = Texp_constant (Const_string (name,_))}, _) ->
        name
    | (_,Some e,_) -> raise_not_complete e
  in
  { reg; name; boxing }

(** return the list of variable that will receive the output and
    the list of outputs couple
*)
let rec parse_outputs = function
  | (_,None,_) -> assert false (** absurd: by typing *)
  | (_,Some e, _) ->
      match extract_name e with
      | "%asm_output_end", [] -> [],[]
      | pname, reg::name::outputs::other_args
        when List.mem_assoc pname asm_output_primitive ->
          assert (other_args = []); (** by typing, can't be more applied *)
          let boxing = List.assoc pname asm_output_primitive in
          let vars,outputs = parse_outputs outputs in
          let vreg = parse_vreg reg name boxing in
          let var = Ident.create "asm_out" in
          let typ = match (Btype.repr e.exp_type).desc with
            | Tconstr (_,[_;ty;_],_) ->
                begin match (Btype.repr ty).desc with
                | Tarrow(_,typ,_,_) -> typ
                | _ -> assert false (** absurd: must be ['arg -> _] *)
                end
            | _ -> assert false (** absurd: must be [(_,'arg -> _,_) output] *)
          in
          (var,typ)::vars,(vreg,var)::outputs
      | _ -> raise_not_complete e


let parse_asmcode asmcode =
  match asmcode with
  | (_,None,_) -> assert false (** absurd: by typing *)
  | (_,Some {exp_desc = Texp_constant (Const_string (name,_))}, _) ->
      let l = Lexer.asmcode (Lexing.from_string name) in
      List.map (function
          | `Text s -> AText s
          | `Var s -> AVar s) l
  | (_,Some e,_) -> raise_not_complete e

(** fold_right on the element of the list *)
let rec parse_list parse inputs acc =
  match inputs.exp_desc with
  | Texp_construct(_,{cstr_name = "::"},[elt;inputs]) ->
      parse elt (parse_list parse inputs acc)
  | Texp_construct(_,{cstr_name = "[]"},[]) -> acc
  | _ -> raise_not_complete inputs

let parse_inputs inputs =
  let parse elt acc =
    begin match extract_name elt with
    | pname, reg::name::value::other_args
      when List.mem_assoc pname asm_input_primitive ->
        assert (other_args = []); (** by typing, can't be more applied *)
        let boxing = List.assoc pname asm_input_primitive in
        let value =
          match value with
          | (_,None,_) -> assert false (** absurd: no optional argument *)
          | (_,Some v, _) -> v
        in
        let vreg = parse_vreg reg name boxing in
        (vreg,value)::acc
    | _ -> raise_not_complete elt
    end in
  match inputs with
  | (_,None,_) -> assert false (** absurd: by typing *)
  | (_,Some e,_) -> parse_list parse e []

let parse_effects  effects  =
  let parse elt acc =
    match elt.exp_desc with
    | Texp_variant("VReg",
                   Some {exp_desc = Texp_constant (Const_string(vreg,_))})
      -> (EVreg vreg)::acc
    | Texp_variant("Memory",None) -> EMemory::acc
    | Texp_variant(reg,None) -> EReg(String.lowercase_ascii reg)::acc
    | _ -> raise_not_complete elt
  in
  match effects with
  | (_,None,_) -> assert false (** absurd: by typing *)
  | (_,Some e,_) -> parse_list parse e []

let parse_branches apply branches =
  let parse elt acc =
    match elt.exp_desc with
    | Texp_tuple([label; br]) ->
        let label = match label.exp_desc with
          | Texp_variant("End",None) -> LEnd
          | Texp_variant("Label",
                         Some {exp_desc = Texp_constant(Const_string(lab,_))})
            -> LLabel lab
          | _ -> raise_not_complete label
        in
        (label,apply br)::acc
    | _ -> raise_not_complete elt
  in
  match branches with
  | (_,None,_) -> assert false (** absurd: by typing *)
  | (_,Some e,_) -> parse_list parse e []

let parse_branches vars env return_type branches =
  let dumb_exp exp_desc exp_type =
    { exp_desc; exp_loc = dumb_loc; exp_type;
      exp_extra = []; exp_env = env; exp_attributes = [];
    }
  in
  let args =
    List.fold_left (fun acc (var,typ) ->
        (Nolabel,Some (dumb_exp (Texp_ident(Path.Pident var,
                                       mknoloc (Longident.Lident "asm_out"),
                                       {val_type = typ;
                                        val_kind = Val_reg;
                                        val_loc = dumb_loc;
                                        val_attributes = []})) typ),Required)
        ::acc
      ) [(Nolabel,
          Some (Typecore.type_expression env
                  (Ast_helper.Exp.construct
                     (mknoloc (Longident.Lident "()")) None))
         ,Required)] vars
  in
  let apply f_exp =
    { exp_desc = Texp_apply(f_exp,args);
      exp_loc = f_exp.exp_loc;
      exp_extra = [];
      exp_type = return_type;
      exp_env = env;
      exp_attributes = [];
    }
  in
  parse_branches apply branches

let is_asm_application e =
  let env = e.exp_env in
  match extract_name e with
  | "%asm_arch", [] ->
      Expr { e with exp_desc = (current_arch e.exp_env).exp_desc}
  | p_name, inputs::asmcode::effects::outputs::branches::other ->
      begin match is_supported_arch p_name with
      | None -> Badly_placed_asm_primitive
      | Some arch
        when not (!Clflags.native_code) || arch <> Config.architecture ->
          (** build [Pervasives.raise (Asm.Wrong_architecture Asm.AMD64)] *)
          let wrong_architecture =
            mk_construct "Wrong_architecture"
              (Some (mk_construct (arch_of_string arch) None))
          in
          let exp_raise =
            Ast_helper.Exp.ident
              { txt = Longident.Ldot(Longident.Lident("Pervasives"),"raise");
                loc = dumb_loc }
          in
          let raise_apply =
            Ast_helper.Exp.apply ~loc:e.exp_loc
              exp_raise [Nolabel,wrong_architecture] in
          let raise_apply = Typecore.type_expression env raise_apply in
          if other = []
          then Expr raise_apply
          else Expr { e with exp_desc = Texp_apply(raise_apply,other) }
      | Some _ ->
          let vars, outputs = parse_outputs outputs in
          (** vars are the variable that bind the output and that must
              be applied to each branch *)
          Ok ({ inputs = parse_inputs inputs;
                asmcode = parse_asmcode asmcode;
                effects = parse_effects effects;
                outputs = outputs;
                branches = parse_branches vars e.exp_env e.exp_type branches;
              }, other)
      end
  | _ -> Badly_placed_asm_primitive

let is_asm_application e =
  try
    is_asm_application e
  with Error r -> r
