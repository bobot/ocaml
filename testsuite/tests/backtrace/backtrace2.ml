(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Gallium, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2008 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* A test for stack backtraces *)

exception Error of string

let test_Error msg =
  let rec f msg n =
    if n = 0 then raise(Error msg) else 1 + f msg (n-1) in
  let exception_raised_internally () =
    try Hashtbl.find (Hashtbl.create 3) 0
    with Not_found -> false in
  try
    f msg 5
  with Error "a" -> print_string "a"; print_newline(); 0
     | Error "b" as exn -> print_string "b"; print_newline(); raise exn
     | Error "c" -> raise (Error "c")
     (** [Error "d"] not catched *)
     (** Test reraise when an exception is used in the middle of the exception
         handler. *)
     | Error "e" as exn ->
         print_string "e"; print_newline ();
         ignore (exception_raised_internally ()); raise exn
     (** Test reraise of backtrace when a `when` clause use exceptions.

     *)
     | Error "f" when exception_raised_internally () ->
         assert false (** absurd: when false *)
     | Error "f" as exn -> print_string "f"; print_newline(); raise exn

let test_Not_found () =
  let rec aux n =
    if n = 0 then raise Not_found else 1 + aux (n-1)
  in
  try aux 5
  (** Test the raise to reraise heuristic with included try_with.
      It correctly doesn't translate it as a reraise (wrong backtrace) *)
  with exn ->
    print_string "test_Not_found"; print_newline();
    (try Hashtbl.find (Hashtbl.create 3) 0 with Not_found -> raise exn)

exception Localized of exn

let test_localized () =
  let rec aux n =
    if n = 0 then raise Not_found else 1 + aux (n-1)
  in
  try aux 5
  with exn rec bt ->
    Printexc.reraise_raw_backtrace (Localized exn) bt

let run g args =
  try
    ignore (g args.(0)); print_string "No exception\n"
  with exn ->
    Printf.printf "Uncaught exception %s\n" (Printexc.to_string exn);
    Printexc.print_backtrace stdout;
    flush stdout

let _ =
  Printexc.record_backtrace true;
  run test_Error [| "a" |];
  run test_Error [| "b" |];
  run test_Error [| "c" |];
  run test_Error [| "d" |];
  run test_Error [| "e" |];
  run test_Error [| "f" |];
  run test_Error [| |];
  run test_Not_found  [| () |];
  run test_localized  [| () |];
  ()
