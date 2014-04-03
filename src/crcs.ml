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

type entry = { name: string; hash: string }
type t = entry list

let empty = []

let print oc crcs =
  let pp_entry { name; hash } =
    Printf.fprintf oc "%s  %s\n" (Digest.to_hex hash) name in
  List.iter pp_entry (List.sort compare crcs)

let read index ic =
  let (offset, _length) = Index.find_section index Section.CRCS in
  seek_in ic offset;
  (input_value ic : t)

let write oc crcs =
  output_value oc (crcs : t)
