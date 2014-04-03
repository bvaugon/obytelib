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

type t =
  | Int         of int
  | Int32       of Int32.t
  | Int64       of Int64.t
  | Nativeint   of Nativeint.t
  | Float       of float
  | Float_array of float array
  | String      of string
  | Block       of int * t array

(***)

let rec bprint buf value =
  let open Printf in
  match value with
  | Int n          -> bprintf buf "%d" n
  | Int32 n        -> bprintf buf "%ldl" n
  | Int64 n        -> bprintf buf "%LdL" n
  | Nativeint n    -> bprintf buf "%ndn" n
  | Float f        -> bprintf buf "%F" f
  | Float_array t  -> bprint_mlarray (fun buf f -> bprintf buf "%F" f) buf t
  | String s       -> bprintf buf "%S" s
  | Block (tag, b) -> bprint_array (sprintf "[%d|" tag) ";" "]" bprint buf b

let to_string value =
  let buf = Buffer.create 16 in
  bprint buf value;
  Buffer.contents buf

(***)

let rec of_obj obj =
  let tag = Obj.tag obj in
  if tag = Obj.lazy_tag then fail "unexpected lazy block";
  if tag = Obj.closure_tag then fail "unexpected closure";
  if tag = Obj.object_tag then fail "unexpected object";
  if tag = Obj.infix_tag then fail "unexpected closure";
  if tag = Obj.abstract_tag then fail "unexpected abstract block";
  if tag = Obj.string_tag then String (Obj.obj obj : string)
  else if tag = Obj.double_tag then Float (Obj.obj obj : float)
  else if tag = Obj.double_array_tag then Float_array (Obj.obj obj : float array)
  else if tag = Obj.custom_tag then
    let key = Obj.field obj 0 in
    if key = Obj.field (Obj.repr 0l) 0 then Int32 (Obj.obj obj : int32)
    else if key = Obj.field (Obj.repr 0L) 0 then Int64 (Obj.obj obj : int64)
    else if key = Obj.field (Obj.repr 0n) 0 then Nativeint (Obj.obj obj : nativeint)
    else fail "unknown custom block"
  else if tag = Obj.int_tag then Int (Obj.obj obj : int)
  else if tag = Obj.out_of_heap_tag then fail "unexpected block out of heap"
  else if tag >= Obj.no_scan_tag then fail "unexpected block"
  else (
    assert (tag < Obj.no_scan_tag);
    let size = Obj.size obj in
    let tab = Array.init size (fun i -> of_obj (Obj.field obj i)) in
    Block (tag, tab)
  )

(***)

let make_to_obj () =
  let float_htbl = Hashtbl.create 16 in
  let int32_htbl = Hashtbl.create 16 in
  let int64_htbl = Hashtbl.create 16 in
  let nativeint_htbl = Hashtbl.create 16 in
  let unify htbl v =
    try Hashtbl.find htbl v with Not_found ->
      let o = Obj.repr v in
      Hashtbl.add htbl v o;
      o in
  let rec to_obj value = match value with
    | Int n          -> Obj.repr n
    | Int32 n        -> unify int32_htbl n
    | Int64 n        -> unify int64_htbl n
    | Nativeint n    -> unify nativeint_htbl n
    | Float f        -> unify float_htbl f
    | Float_array t  -> Obj.repr t
    | String s       -> Obj.repr s
    | Block (tag, b) ->
      let len = Array.length b in
      let blk = Obj.new_block tag len in
      for i = 0 to len - 1 do Obj.set_field blk i (to_obj b.(i)) done;
      blk in
  to_obj

(***)
