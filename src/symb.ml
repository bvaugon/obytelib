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

type marshaled_value = {
  num_cnt: int;
  num_tbl: (Ident.t, int) Tree.t;
}

type t = Ident.t option array

let empty = [||]
              
(******************************************************************************)

let maxind_of_tree tree =
  Tree.fold (fun _ident ind acc -> max ind acc) (-1) tree
    
let table_of_tree tree =
  let size = maxind_of_tree tree + 1 in
  let table = Array.make size None in
  let store ident ind =
    assert (table.(ind) = None);
    table.(ind) <- Some ident in
  Tree.iter store tree;
  table
    
let tree_of_table table =
  let aggr_idents acc ident_opt =
    match ident_opt with
    | None -> acc
    | Some ident -> ident :: acc in
  let sorted_tbl = Array.of_list (Array.fold_left aggr_idents [] table) in
  let () = Array.sort compare sorted_tbl in
  let rec make_tree i j =
    if i >= j then Tree.Empty else
      let m = (i + j) / 2 in
      let left = make_tree i m and right = make_tree (m + 1) j in
      let h = max (Tree.height left) (Tree.height right) + 1 in
      Tree.Node (left, sorted_tbl.(m), m, right, h) in
  make_tree 0 (Array.length sorted_tbl)

(******************************************************************************)

let print oc table =
  Array.iteri (fun ind ident_opt ->
    match ident_opt with
    | None -> ()
    | Some ident -> Printf.fprintf oc "%4d: %a\n" ind Ident.print ident
  ) table

(***)

let read version index ic =
  let (offset, _length) = Index.find_section index Section.SYMB in
  let () = seek_in ic offset in
  let mv = (input_value ic : marshaled_value) in
  let table = table_of_tree mv.num_tbl in
  assert (mv.num_cnt = Array.length table);
  Array.iter (Ident.check_opt version) table;
  table

(***)

let write version oc table =
  Array.iter (Ident.check_opt version) table;
  let tree = tree_of_table table in
  let num_cnt = maxind_of_tree tree + 1 in
  let marshaled_value = { num_cnt = num_cnt; num_tbl = tree } in
  output_value oc marshaled_value

(******************************************************************************)
