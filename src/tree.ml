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

type ('a, 'b) t = Empty | Node of ('a, 'b) t * 'a * 'b * ('a, 'b) t * int

let rec fold f acc tree = match tree with
  | Empty -> acc
  | Node (left, k, v, right, _h) -> fold f (f k v (fold f acc left)) right

let rec iter f tree = match tree with
  | Empty -> ()
  | Node (left, k, v, right, _h) -> iter f left; f k v; iter f right

let height tree = match tree with
  | Empty -> 0
  | Node (_left, _k, _v, _right, h) -> h
