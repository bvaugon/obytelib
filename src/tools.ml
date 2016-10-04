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

let fail fmt = Printf.ksprintf failwith fmt

let bprint_list op sep cl pp_x buf x_list =
  let open Printf in
  let rec f buf l = match l with
    | [] -> ()
    | [ last ] -> bprintf buf "%a" pp_x last
    | x :: rest -> bprintf buf "%a%s %a" pp_x x sep f rest in
  match x_list with
  | [] -> bprintf buf "%s%s" op cl
  | _  -> bprintf buf "%s %a %s" op f x_list cl

let bprint_array op sep cl pp_x buf x_array =
  bprint_list op sep cl pp_x buf (Array.to_list x_array)

let bprint_mlarray pp_x buf x_array =
  bprint_array "[" ";" "]" pp_x buf x_array

let to_string_of_bprint bprint x =
  let buf = Buffer.create 16 in
  bprint buf x;
  Buffer.contents buf

let input_binary_int_rev =
  let buf4 = Bytes.create 4 in
  fun ic ->
    let () = really_input ic buf4 0 4 in
    let n =
      (int_of_char (Bytes.get buf4 0)) lor (int_of_char (Bytes.get buf4 1) lsl 8) lor
        (int_of_char (Bytes.get buf4 2) lsl 16) lor (int_of_char (Bytes.get buf4 3) lsl 24) in
    match Sys.word_size with
    | 32 -> n
    | 64 -> (n lsl 32) asr 32
    | ws -> fail "unsupported architecture: sizeof(word) = %d" ws

let find_object_method o tag =
  let rec bin_search tbl tag li hi =
    if li >= hi then Obj.field tbl (li - 1) else
      let mi = ((li + hi) lsr 1) lor 1 in
      let tag' = Obj.field tbl mi in
      if tag < tag' then bin_search tbl tag li (mi - 2)
      else bin_search tbl tag mi hi
  in
  let tbl = Obj.field o 0 in
  let hi = ((Obj.obj (Obj.field tbl 0) : int) lsl 1) lor 1 in
  bin_search tbl (Obj.repr tag) 3 hi
