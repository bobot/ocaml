open Bigarray

(* Test harness *)

let error_occurred = ref false

let function_tested = ref ""

let testing_function s =
    function_tested := s;
    print_newline();
    print_string s;
    print_newline()

let test test_number answer correct_answer =
 flush stdout;
 flush stderr;
 if answer <> correct_answer then begin
   Printf.eprintf "*** Bad result (%s, test %d)\n" !function_tested test_number;
   flush stderr;
   error_occurred := true
 end else begin
   Printf.printf " %d..." test_number
 end

(* Tests *)

let tests () =
  testing_function "map_file";
  let mapped_file = Filename.temp_file "bigarray" ".data" in
  begin
    let fd =
     Unix.openfile mapped_file
                   [Unix.O_RDWR; Unix.O_TRUNC; Unix.O_CREAT] 0o666 in
    let a =
      array1_of_genarray (Unix.map_file fd float64 c_layout true [|10000|])
    in
    Unix.close fd;
    for i = 0 to 9999 do a.{i} <- float i done;
    let fd = Unix.openfile mapped_file [Unix.O_RDONLY] 0 in
    let b =
      array2_of_genarray
        (Unix.map_file fd float64 fortran_layout false [|100; -1|])
    in
    Unix.close fd;
    let ok = ref true in
    for i = 0 to 99 do
      for j = 0 to 99 do
        if b.{j+1,i+1} <> float (100 * i + j) then ok := false
      done
    done;
    test 1 !ok true;
    b.{50,50} <- (-1.0);
    let fd = Unix.openfile mapped_file [Unix.O_RDONLY] 0 in
    let c =
      array2_of_genarray (Unix.map_file fd float64 c_layout false [|-1; 100|])
    in
    Unix.close fd;
    let ok = ref true in
    for i = 0 to 99 do
      for j = 0 to 99 do
        if c.{i,j} <> float (100 * i + j) then ok := false
      done
    done;
    test 2 !ok true;
    let fd = Unix.openfile mapped_file [Unix.O_RDONLY] 0 in
    let c =
      array2_of_genarray
        (Unix.map_file fd ~pos:800L float64 c_layout false [|-1; 100|])
    in
    Unix.close fd;
    let ok = ref true in
    for i = 1 to 99 do
      for j = 0 to 99 do
        if c.{i-1,j} <> float (100 * i + j) then ok := false
      done
    done;
    test 3 !ok true;
    let fd = Unix.openfile mapped_file [Unix.O_RDONLY] 0 in
    let c =
      array2_of_genarray
        (Unix.map_file fd ~pos:79200L float64 c_layout false [|-1; 100|])
    in
    Unix.close fd;
    let ok = ref true in
    for j = 0 to 99 do
      if c.{0,j} <> float (100 * 99 + j) then ok := false
    done;
    test 4 !ok true
  end;
  (* Force garbage collection of the mapped bigarrays above, otherwise
     Win32 doesn't let us erase the file.  Notice the begin...end above
     so that the VM doesn't keep stack references to the mapped bigarrays. *)
  Gc.full_major();
  Sys.remove mapped_file;

  ()
  [@@inline never]


(********* End of test *********)

let _ =
  tests ();
  print_newline();
  if !error_occurred then begin
    prerr_endline "************* TEST FAILED ****************"; exit 2
  end else
    exit 0
