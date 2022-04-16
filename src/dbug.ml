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

type 'a ident_data = {
  ident: Ident.t;
  data: 'a;
  previous: 'a ident_data option
}

type 'a ident_tbl =
    Empty
  | Node of 'a ident_tbl * 'a ident_data * 'a ident_tbl * int

type compilation_env =
  { ce_stack: int ident_tbl;
    ce_heap: int ident_tbl;
    ce_rec: int ident_tbl }

type location_t = {
  loc_start: Lexing.position;
  loc_end: Lexing.position;
  loc_ghost: bool;
}

type env_summary
type subst_t
type types_type_expr

type debug_event =
  { mutable ev_pos: int;                (* Position in bytecode *)
    ev_module: string;                  (* Name of defining module *)
    ev_loc: location_t;                 (* Location in source file *)
    ev_kind: debug_event_kind;          (* Before/after event *)
    ev_info: debug_event_info;          (* Extra information *)
    ev_typenv: env_summary;             (* Typing environment *)
    ev_typsubst: subst_t;               (* Substitution over types *)
    ev_compenv: compilation_env;        (* Compilation environment *)
    ev_stacksize: int;                  (* Size of stack frame *)
    ev_repr: debug_event_repr }         (* Position of the representative *)

and debug_event_kind =
    Event_before
  | Event_after of types_type_expr
  | Event_pseudo

and debug_event_info =
    Event_function
  | Event_return of int
  | Event_other

and debug_event_repr =
    Event_none
  | Event_parent of int ref
  | Event_child of int ref

type t = (int * debug_event list * string list) array

let empty = [||]

let print_position oc =
  Lexing.(fun { pos_fname; pos_lnum; pos_bol; pos_cnum } ->
    Printf.fprintf oc
      "{ pos_fname = %S; pos_lnum = %d; pos_bol = %d; pos_cnum = %d }"
      pos_fname pos_lnum pos_bol pos_cnum)

let print_location oc { loc_start; loc_end; loc_ghost } =
  Printf.fprintf oc "{ loc_start = %a; loc_end = %a; loc_ghost = %B }"
    print_position loc_start print_position loc_end loc_ghost

let print_event oc event =
  Printf.fprintf oc
    "{ ev_pos = %d; ev_module = %S; ev_loc = %a; ev_stack_size = %d }"
    event.ev_pos event.ev_module print_location event.ev_loc event.ev_stacksize

let print oc dbug =
  let nb = Array.length dbug in
  for i = 0 to nb - 1 do
    let (n, l, d) = dbug.(i) in
    Printf.fprintf oc "%d:\n" n;
    List.iter (Printf.fprintf oc "  %a\n" print_event) l;
    Printf.fprintf oc "  ---\n";
    List.iter (Printf.fprintf oc "  %S\n") d;
    if i < nb - 1 then Printf.printf "\n";
  done
  
let read index ic =
  try
    let (offset, length) =
      try Index.find_section index Section.DBUG
      with Failure _ -> raise Exit in
    if length = 0 then raise Exit;
    seek_in ic offset;
    let event_nb = input_binary_int ic in
    Array.init event_nb (fun _ ->
      let n = input_binary_int ic in
      let l = (input_value ic : debug_event list) in
      let d = (input_value ic : string list) in
      (n, l, d)
    )
  with Exit -> empty

let write oc dbug =
  output_binary_int oc (Array.length dbug);
  Array.iter (fun (n, l, d) ->
    output_binary_int oc n;
    output_value oc (l : debug_event list);
    output_value oc (d : string list)
  ) dbug
