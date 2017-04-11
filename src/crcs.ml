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

type entry = string * Digest.t option
type t = entry list

let empty = []

let print oc crcs =
  let pp_entry (name, hash) =
    Printf.fprintf oc "%s  %s\n" (match hash with Some h -> Digest.to_hex h | None -> "-") name in
  List.iter pp_entry (List.sort compare crcs)

let read index ic =
  let (offset, _length) = Index.find_section index Section.CRCS in
  seek_in ic offset;
  let crcs = (input_value ic : t) in
  crcs

let write oc crcs =
  output_value oc (crcs : t)
