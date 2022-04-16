(*************************************************************************)
(*                                                                       *)
(*                              OByteLib                                 *)
(*                                                                       *)
(*                            Benoit Vaugon                              *)
(*                                                                       *)
(*    This file is distributed under the terms of the CeCILL license.    *)
(*    See file ../LICENSE-en.                                            *)
(*                                                                       *)
(*************************************************************************)

open Tools

type t = Value.t array

let print symb oc data =
  let buf = Buffer.create 16 in
  let print_global i value =
    Printf.bprintf buf "%4d:  %a" i Value.bprint value;
    begin
      let ident_opt = if i >= Array.length symb then None else symb.(i) in
      match ident_opt with
      | None -> ()
      | Some ident ->
        let s = Printf.sprintf "  (* %s *)" (Ident.name ident) in
        let sz = Buffer.length buf in
        let ofs = max 0 (78 - sz - (String.length s)) in
        for _i = 0 to ofs do Buffer.add_char buf ' ' done;
        Buffer.add_string buf s;
    end;
    Printf.fprintf oc "%s\n" (Buffer.contents buf);
    Buffer.clear buf in
  Array.iteri print_global data

let read index ic =
  let (offset, _) = Index.find_section index Section.DATA in
  let tbl = seek_in ic offset; (input_value ic : Obj.t array) in
  Array.map Value.of_obj tbl

let to_objs data =
  Array.map (Value.make_to_obj ()) data

let write oc data =
  output_value oc (to_objs data)

let fix_std_exceptions globals =
  let global_nb = Array.length globals in
  if global_nb < 12 then
    fail "invalid DATA: no place for standard exceptions" global_nb;
  let repl ind exn =
    let e = Obj.repr exn in
    let e = if Obj.tag e <> 0 then e else (assert (Obj.size e = 2); Obj.field e 0) in
    if e <> globals.(ind) then
      fail "invalid DATA: incompatible standard exception %d" ind;
    globals.(ind) <- Obj.repr exn in
  Array.iteri repl [|
    Out_of_memory;
    Sys_error "";
    Failure "";
    Invalid_argument "";
    End_of_file;
    Division_by_zero;
    Not_found;
    Match_failure ("", -1, -1);
    Stack_overflow;
    Sys_blocked_io;
    Assert_failure ("", -1, -1);
    Undefined_recursive_module ("", -1, -1);
 |]
