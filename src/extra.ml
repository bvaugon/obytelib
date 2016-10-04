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

type t = string

let empty = ""

let read index ic =
  let () = seek_in ic 0 in
  let s = input_line ic in
  if String.length s < 3 || s.[0] <> '#' || s.[1] <> '!' then seek_in ic 0;
  let start = pos_in ic in
  let stop = match index with
    | [] -> Tools.fail "empty index"
    | entry0 :: rest ->
      List.fold_left (fun acc entry -> min entry.Index.offset acc)
        entry0.Index.offset rest in
  let length = stop - start in
  if length < 0 then Tools.fail "incompatible index";
  let extra = Bytes.create length in
  really_input ic extra 0 length;
  Bytes.to_string extra

let write oc extra =
  output_string oc extra

let size extra =
  String.length extra
