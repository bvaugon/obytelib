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

type t = Instr.t array

let kosaraju_gen get_nexts instrs =
  let instr_nb = Array.length instrs in
  let colors = Array.make instr_nb (-1) in
  let nexts = Array.mapi get_nexts instrs in
  let nexts = Array.map (List.filter (fun ind -> ind < instr_nb)) nexts in
  let preds = Array.make instr_nb [] in
  let rec run color ind =
    if colors.(ind) = -1 then (
      colors.(ind) <- color;
      List.iter (run color) nexts.(ind);
      List.iter (run color) preds.(ind);
    ) in
  Array.iteri (fun ind ->
    List.iter (fun ind' ->
      preds.(ind') <- ind :: preds.(ind')
    )
  ) nexts;
  for ind = 0 to instr_nb - 1 do
    if colors.(ind) = -1 then run ind ind;
  done;
  colors

let kosaraju instrs =
  kosaraju_gen Instr.get_nexts instrs

(***)
  
let print_gen get_ptrs get_nexts bprint_instr data symb prim oc instrs =
  let open Printf in
  let colors = kosaraju_gen get_nexts instrs in
  let margin_sz = 8 in
  let instr_nb = Array.length instrs in
  let labels = Array.make instr_nb (-1) in
  Array.iter (fun instr ->
    List.iter (fun ptr ->
      labels.(ptr) <- 0) (get_ptrs instr)) instrs;
  begin
    let cnt = ref 1 in
    for i = 0 to instr_nb - 1 do
      if labels.(i) = 0 then (
        labels.(i) <- !cnt;
        incr cnt;
      )
    done;
  end;
  let buf = Buffer.create 16 in
  let data_nb = Array.length data in
  let symb_nb = Array.length symb in
  let prim_nb = Array.length prim in
  let pp_ptr buf ptr = bprintf buf "L%d" labels.(ptr) in
  let pp_cfun buf idx =
    if idx >= prim_nb then bprintf buf "#%d" idx
    else bprintf buf "%S" prim.(idx) in
  let pp_data buf ind =
    let ident_opt = if ind >= symb_nb then None else symb.(ind) in
    let data_opt = if ind >= data_nb then None else Some data.(ind) in
    match ident_opt, data_opt with
    | None, (Some (Value.Int 0) | None) -> bprintf buf "#%d" ind
    | None, Some d -> Value.bprint buf d
    | Some ident, _     -> bprintf buf "{%s}" (Ident.name ident) in
  for i = 0 to instr_nb - 1 do
    if i > 0 && colors.(i - 1) <> colors.(i) then fprintf oc "\n";
    fprintf oc "pc=%-4d " i;
    if labels.(i) <> -1 then bprintf buf "%a: " pp_ptr i;
    while Buffer.length buf < margin_sz do Buffer.add_char buf ' ' done;
    bprint_instr pp_ptr pp_cfun pp_data buf instrs.(i);
    Buffer.add_char buf '\n';
    output_string oc (Buffer.contents buf);
    Buffer.clear buf;
  done

let print data symb prim oc instrs =
  print_gen Instr.get_ptrs Instr.get_nexts Instr.bprint data symb prim oc instrs

(***)

(* Reduce envacc by the number of code pointers in the corresponding closure *)
(* Used to convert code having version >= V029 *)
let normalise_envaccs version instrs =
  match version with
  | Version.V008 | Version.V010 | Version.V011
  | Version.V022 | Version.V023 | Version.V025
  | Version.V026 | Version.V027 | Version.V028 ->
    ()
  | Version.V029 | Version.V030 | Version.V031 ->
    let open Instr in
    let colors = kosaraju instrs in
    let code_ptr_nb = Hashtbl.create 16 in
    Array.iter (fun instr ->
      match instr with
      | CLOSURE (_, ptr) ->
        Hashtbl.add code_ptr_nb colors.(ptr) 1
      | CLOSUREREC (_, _, o, t) ->
        let delta = 1 + Array.length t in
        Hashtbl.add code_ptr_nb colors.(o) delta;
        Array.iteri (fun i ptr -> Hashtbl.add code_ptr_nb colors.(ptr) (delta - i - 1)) t;
      | _ ->
        ()
    ) instrs;
    let compute_new_ind pc old_ind =
      match Hashtbl.find_all code_ptr_nb colors.(pc) with
      | [] -> assert false
      | [ delta ] -> old_ind - delta
      | _ :: _ :: _ -> assert false in
    let make_envacc pc old_ind =
      match compute_new_ind pc old_ind with
      | 1 -> ENVACC1
      | 2 -> ENVACC2
      | 3 -> ENVACC3
      | 4 -> ENVACC4
      | n -> assert (n >= 5); ENVACC n in
    let make_pushenvacc pc old_ind =
      match compute_new_ind pc old_ind with
      | 1 -> PUSHENVACC1
      | 2 -> PUSHENVACC2
      | 3 -> PUSHENVACC3
      | 4 -> PUSHENVACC4
      | n -> assert (n >= 5); PUSHENVACC n in
    let make_offsetclosure old_ofs =
      assert (old_ofs mod 3 = 0);
      match old_ofs / 3 * 2 with
      | -2  -> OFFSETCLOSUREM2
      | +2  -> OFFSETCLOSURE2
      | ofs -> OFFSETCLOSURE ofs in
    let make_pushoffsetclosure old_ofs =
      assert (old_ofs mod 3 = 0);
      match old_ofs / 3 * 2 with
      | -2  -> PUSHOFFSETCLOSUREM2
      | +2  -> PUSHOFFSETCLOSURE2
      | ofs -> PUSHOFFSETCLOSURE ofs in
    for pc = 0 to Array.length instrs - 1 do
      match instrs.(pc) with
      | ENVACC1               -> instrs.(pc) <- make_envacc pc 1
      | ENVACC2               -> instrs.(pc) <- make_envacc pc 2
      | ENVACC3               -> instrs.(pc) <- make_envacc pc 3
      | ENVACC4               -> instrs.(pc) <- make_envacc pc 4
      | ENVACC n              -> instrs.(pc) <- make_envacc pc n
      | PUSHENVACC1           -> instrs.(pc) <- make_pushenvacc pc 1
      | PUSHENVACC2           -> instrs.(pc) <- make_pushenvacc pc 2
      | PUSHENVACC3           -> instrs.(pc) <- make_pushenvacc pc 3
      | PUSHENVACC4           -> instrs.(pc) <- make_pushenvacc pc 4
      | PUSHENVACC n          -> instrs.(pc) <- make_pushenvacc pc n
      | OFFSETCLOSUREM2       -> instrs.(pc) <- make_offsetclosure (-3)
      | OFFSETCLOSURE2        -> instrs.(pc) <- make_offsetclosure 3
      | OFFSETCLOSURE ofs     -> instrs.(pc) <- make_offsetclosure ofs
      | PUSHOFFSETCLOSUREM2   -> instrs.(pc) <- make_pushoffsetclosure (-3)
      | PUSHOFFSETCLOSURE2    -> instrs.(pc) <- make_pushoffsetclosure 3
      | PUSHOFFSETCLOSURE ofs -> instrs.(pc) <- make_pushoffsetclosure ofs
      | _            -> ()
    done

(***)

let read version index ic =
  let open Instr in
  let (offset, length) = Index.find_section index Section.CODE in
  if length mod 4 <> 0 then
    fail "invalid CODE section: size (= %d) is not a multiple of 4" length;
  seek_in ic offset;
  let inds = ref [] in
  let instrs = ref [] in
  let word_nb = length / 4 in
  let word_ind = ref 0 in
  let next_word () =
    if !word_ind = word_nb then raise End_of_file;
    incr word_ind;
    input_binary_int_rev ic in
  begin try
    while !word_ind < word_nb do
      inds := !word_ind :: !inds;
      instrs := Instr.read version next_word :: !instrs;
    done;
  with End_of_file ->
    fail "truncated CODE section"
  end;
  let inds = Array.of_list (List.rev !inds) in
  let instrs = Array.of_list (List.rev !instrs) in
  let ptr_map = Array.make word_nb (-1) in
  let remap_ptr ind ofs ptr = ptr_map.(inds.(ind) + ptr + ofs) in
  let remap_ptrs ind ofs ptrs = Array.map (remap_ptr ind ofs) ptrs in
  let remap_instr ind instr = match instr with
    | PUSH_RETADDR ptr        -> PUSH_RETADDR (remap_ptr ind 1 ptr)
    | CLOSURE (n, ptr)        -> CLOSURE (n, remap_ptr ind 2 ptr)
    | CLOSUREREC (f, v, o, t) -> CLOSUREREC (f, v, remap_ptr ind 3 o, remap_ptrs ind 3 t)
    | BRANCH ptr              -> BRANCH (remap_ptr ind 1 ptr)
    | BRANCHIF ptr            -> BRANCHIF (remap_ptr ind 1 ptr)
    | BRANCHIFNOT ptr         -> BRANCHIFNOT (remap_ptr ind 1 ptr)
    | SWITCH (n, ptrs)        -> SWITCH (n, remap_ptrs ind 2 ptrs)
    | PUSHTRAP ptr            -> PUSHTRAP (remap_ptr ind 1 ptr)
    | BEQ (n, ptr)            -> BEQ (n, remap_ptr ind 2 ptr)
    | BNEQ (n, ptr)           -> BNEQ (n, remap_ptr ind 2 ptr)
    | BLTINT (n, ptr)         -> BLTINT (n, remap_ptr ind 2 ptr)
    | BLEINT (n, ptr)         -> BLEINT (n, remap_ptr ind 2 ptr)
    | BGTINT (n, ptr)         -> BGTINT (n, remap_ptr ind 2 ptr)
    | BGEINT (n, ptr)         -> BGEINT (n, remap_ptr ind 2 ptr)
    | BULTINT (n, ptr)        -> BULTINT (n, remap_ptr ind 2 ptr)
    | BUGEINT (n, ptr)        -> BUGEINT (n, remap_ptr ind 2 ptr)
    | _ -> instr in
  Array.iteri (fun i ind -> ptr_map.(ind) <- i) inds;
  let instrs = Array.mapi remap_instr instrs in
  normalise_envaccs version instrs;
  instrs

(***)

let write version oc code =
  let instr_nb = Array.length code in
  let ptr_map = Array.make instr_nb (-1) in
  let compute_ptrs =
    let addr = ref 0 in
    fun ind _w ->
      if ptr_map.(ind) = -1 then ptr_map.(ind) <- !addr;
      incr addr in
  let write_code _ind w =
    output_byte oc (w land 0xFF);
    output_byte oc ((w asr 8) land 0xFF);
    output_byte oc ((w asr 16) land 0xFF);
    output_byte oc ((w asr 24) land 0xFF) in
  let export write_word ind instr =
    Instr.write version
      (write_word ind)
      (fun delta ptr -> write_word ind (ptr_map.(ptr) - ptr_map.(ind) - delta))
      instr in
  Array.iteri (export compute_ptrs) code;
  Array.iteri (export write_code) code

(***)

let size version code =
  let counter = ref 0 in
  Array.iter (fun instr ->
    Instr.write version (fun _word -> incr counter) (fun _delta _ptr -> incr counter) instr
  ) code;
  !counter * 4

(***)
