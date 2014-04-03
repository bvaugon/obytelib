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

type location = {
  loc_start: Lexing.position;
  loc_end: Lexing.position;
  loc_ghost: bool;
}

type fake_debug_event_kind
type fake_debug_event_info
type fake_env_summary
type fake_subst
type fake_compilation_env
type fake_debug_event_repr

type debug_event = {
  mutable ev_pos: int;                (* Position in bytecode           *)
  ev_module: string;                  (* Name of defining module        *)
  ev_loc: location;                   (* Location in source file        *)
  ev_kind: fake_debug_event_kind;     (* Before/after event             *)
  ev_info: fake_debug_event_info;     (* Extra information              *)
  ev_typenv: fake_env_summary;        (* Typing environment             *)
  ev_typsubst: fake_subst;            (* Substitution over types        *)
  ev_compenv: fake_compilation_env;   (* Compilation environment        *)
  ev_stacksize: int;                  (* Size of stack frame            *)
  ev_repr: fake_debug_event_repr;     (* Position of the representative *)
}

type t = (int * debug_event list) array

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
    let (n, l) = dbug.(i) in
    Printf.fprintf oc "%d:\n" n;
    List.iter (Printf.fprintf oc "  %a\n" print_event) l;
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
      (n, l))
  with Exit -> empty

let write oc dbug =
  output_binary_int oc (Array.length dbug);
  Array.iter (fun (n, l) ->
    output_binary_int oc n;
    output_value oc (l : debug_event list)) dbug
