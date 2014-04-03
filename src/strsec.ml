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

module Make(S : sig val section : Section.t end) = struct
  type t = string array

  let empty = [||]

  let print oc strs =
    let print_str i str = Printf.fprintf oc "%4d:  %s\n" i str in
    Array.iteri print_str strs

(***)

  let read index ic =
    try
      let (offset, length) =
        try Index.find_section index S.section
        with Failure _ -> raise Exit in
      let buf = Buffer.create 16 in
      let rec f i res =
        if i <> length then
          let c = input_char ic in
          if int_of_char c <> 0 then (
            Buffer.add_char buf c;
            f (i + 1) res
          ) else (
            let name = Buffer.contents buf in
            Buffer.clear buf;
            f (i + 1) (name :: res)
          )
        else if Buffer.length buf <> 0 then
          Tools.fail "unexpected end of %s section"
            (Section.to_string S.section)
        else
          res in
      seek_in ic offset;
      Array.of_list (List.rev (f 0 []))
    with Exit -> empty
      
  let write oc strs =
    Array.iter (Printf.fprintf oc "%s\000") strs
end
