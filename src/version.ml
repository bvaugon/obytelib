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

type t = V008 | V010 | V011 | V023 | V025 | V026 | V027

let versions = [ V008; V010; V011; V023; V025 ; V026 ; V027 ]

let to_string v = match v with
  | V008 -> "008"
  | V010 -> "010"
  | V011 -> "011"
  | V023 -> "023"
  | V025 -> "025"
  | V026 -> "026"
  | V027 -> "027"

let to_magic v =
  "Caml1999X" ^ to_string v

let magic_size =
  let len = String.length (to_magic V008) in
  let check_len v = String.length (to_magic v) = len in
  assert (List.for_all check_len versions); len

let of_magic s =
  List.find (fun v -> to_magic v = s) versions

let read ic =
  let file_size = in_channel_length ic in
  if file_size < magic_size then fail "too short file";
  let () = seek_in ic (file_size - magic_size) in
  let magic_string = Bytes.create magic_size in
  let () = really_input ic magic_string 0 magic_size in
  let magic_string = Bytes.to_string magic_string in
  try of_magic magic_string
  with Not_found -> fail "unknown magic string: %S" magic_string

let write oc v =
  output_string oc (to_magic v)
