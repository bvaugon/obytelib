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

type 'a stack = {
  max_size     : int;
  min_size     : int;
  default      : 'a;
  mutable size : int;
  mutable tbl  : 'a array;
}

let create max_size init_size default = {
  max_size;
  min_size = init_size;
  default;
  size = 0;
  tbl = Array.make init_size default
}

let size stack = stack.size

(***)

let acc stack n =
  if n < 0 || n >= stack.size then invalid_arg "Astack.acc";
  let pos = stack.size - 1 - n in
  stack.tbl.(pos)

let assign stack n v =
  if n < 0 || n >= stack.size then invalid_arg "Astack.assign";
  let pos = stack.size - 1 - n in
  stack.tbl.(pos) <- v

let push stack x =
  let len = Array.length stack.tbl in
  let pos = stack.size in
  if pos >= len then (
    let new_len = min (len * 2) stack.max_size in
    if new_len <= len then raise Stack_overflow;
    let new_tbl = Array.make new_len stack.default in
    Array.blit stack.tbl 0 new_tbl 0 pos;
    stack.tbl <- new_tbl;
  );
  stack.tbl.(pos) <- x;
  stack.size <- pos + 1

let popn stack n =
  if n < 0 || n > stack.size then invalid_arg "Astack.popn";
  if n <> 0 then
    let new_size = stack.size - n in
    let len = Array.length stack.tbl in
    let new_len = len / 2 in
    if new_len >= stack.min_size && new_size < len / 4 then (
      let new_tbl = Array.make new_len stack.default in
      Array.blit stack.tbl 0 new_tbl 0 new_size;
      stack.tbl <- new_tbl;
    ) else (
      Array.fill stack.tbl new_size n stack.default;
    );
    stack.size <- new_size

let pop stack =
  let v = acc stack 0 in
  popn stack 1; v

(***)
