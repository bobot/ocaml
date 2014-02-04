(*************************************************************************)
(*                                                                       *)
(*                                OCaml                                  *)
(*                                                                       *)
(*         Damien Doligez, projet Gallium, INRIA Rocquencourt            *)
(*                                                                       *)
(*   Copyright 2008 Institut National de Recherche en Informatique et    *)
(*   en Automatique.  All rights reserved.  This file is distributed     *)
(*   under the terms of the Q Public License version 1.0.                *)
(*                                                                       *)
(*************************************************************************)

let debug = false

open Printf
open Ephemeron

let is_true test s b = printf "%s %s: %s\n" test s (if b then "OK" else "FAIL")
let is_false test s b = is_true test s (not b)

let is_data_value test eph (v:int) =
  match K1.get_data_copy eph with
  | Some x ->
      if !x = v
      then printf "%s data set: OK\n" test
      else printf "%s data set: FAIL(bad value %i)\n" test (!x)
  | None -> printf "%s data set: FAIL\n" test

let is_key_value test eph (v:int) =
  match K1.get_key_copy eph with
  | Some x ->
      if !x = v
      then printf "%s key set: OK\n" test
      else printf "%s key set: FAIL(bad value %i)\n" test (!x)
  | None -> printf "%s key unset: FAIL\n" test

let is_key_unset test eph =
  is_false test "key unset" (K1.check_key eph)

let is_data_unset test eph =
  is_false test "data unset" (K1.check_data eph)

let ra = ref (ref 1)
let rb = ref (ref (ref 2))

(** test: key alive data dangling *)
let () =
  let test = "test1" in
  Gc.minor ();
  Gc.full_major ();
  let eph : (int ref, int ref) K1.t = K1.create () in
  K1.set_key eph (!ra);
  K1.set_data eph (ref 42);
  is_key_value test eph 1;
  is_data_value test eph 42;
  Gc.minor ();
  is_key_value test eph 1;
  is_data_value test eph 42;
  Gc.full_major ();
  is_key_value test eph 1;
  is_data_value test eph 42;
  ra := ref 12;
  Gc.full_major ();
  is_key_unset test eph;
  is_data_unset test eph

(** test: key dangling data dangling *)
let () =
  let test = "test2" in
  Gc.minor ();
  Gc.full_major ();
  let eph : (int ref, int ref) K1.t = K1.create () in
  K1.set_key eph (ref 125);
  K1.set_data eph (ref 42);
  is_key_value test eph 125;
  is_data_value test eph 42;
  ra := ref 13;
  Gc.minor ();
  is_key_unset test eph;
  is_data_unset test eph


(** test: key dangling data alive *)
let () =
  let test = "test3" in
  Gc.minor ();
  Gc.full_major ();
  let eph : (int ref, int ref) K1.t = K1.create () in
  K1.set_key eph (ref 125);
  K1.set_data eph (!ra);
  is_key_value test eph 125;
  is_data_value test eph 13;
  ra := ref 14;
  Gc.minor ();
  is_key_unset test eph;
  is_data_unset test eph

(** test: key alive but one away, data dangling *)
let () =
  let test = "test4" in
  Gc.minor ();
  Gc.full_major ();
  let eph : (int ref, int ref) K1.t = K1.create () in
  rb := ref (ref 3);
  K1.set_key eph (!(!rb));
  K1.set_data eph (ref 43);
  is_key_value test eph 3;
  is_data_value test eph 43;
  Gc.minor ();
  Gc.minor ();
  is_key_value test eph 3;
  is_data_value test eph 43

(** test: key dangling but one away, data dangling *)
let () =
  let test = "test5" in
  Gc.minor ();
  Gc.full_major ();
  let eph : (int ref, int ref) K1.t = K1.create () in
  rb := ref (ref 3);
  K1.set_key eph (!(!rb));
  K1.set_data eph (ref 43);
  is_key_value test eph 3;
  is_data_value test eph 43;
  !rb := ref 4;
  Gc.minor ();
  Gc.minor ();
  is_key_unset test eph;
  is_data_unset test eph
