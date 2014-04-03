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

type entry = { section: Section.t; offset: int; length: int }
type t = entry list

let read_int ic offset =
  seek_in ic offset;
  input_binary_int ic

let read_name ic offset =
  let buf = String.create 4 in
  seek_in ic offset;
  really_input ic buf 0 4; buf

let read ic =
  let file_size = in_channel_length ic in
  let magic_size = Version.magic_size in
  let index_size = read_int ic (file_size - magic_size - 4) in
  let rec f ind next_ofs rem =
    if ind = -1 then rem else
      let descr_ofs = file_size - magic_size - 4 - (index_size - ind) lsl 3 in
      let name = read_name ic descr_ofs in
      let secty = Section.of_string name in
      let length = read_int ic (descr_ofs + 4) in
      let new_ofs = next_ofs - length in
      let entry = { section = secty; offset = new_ofs; length } in
      f (ind - 1) new_ofs (entry :: rem) in
  f (index_size - 1) (file_size - magic_size - 4 - index_size lsl 3) []

let write oc index =
  let rec f rest size = match rest with
    | { section; offset = _; length } :: tl ->
      output_string oc (Section.to_string section);
      output_binary_int oc length;
      f tl (size + 1);
    | [] ->
      output_binary_int oc size in
  f index 0

let find_section index sec =
  try
    let entry = List.find (fun entry -> entry.section = sec) index in
    (entry.offset, entry.length)
  with Not_found ->
    fail "section %s not found" (Section.to_string sec)

let print oc index =
  let print_descr { section; offset; length } =
    Printf.fprintf oc "  %s  %8d  %8d\n" (Section.to_string section)
      offset length in
  Printf.fprintf oc "<name>    <addr>    <size>\n";
  Printf.fprintf oc "--------------------------\n";
  List.iter print_descr index
