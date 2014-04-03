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

type unop = NOT | NEG | OFFSET of int | VECTLENGTH | ISINT
type binop = ADD | SUB | MUL | DIV | MOD | AND | OR | XOR | LSL | LSR | ASR
type compop = EQ | NEQ | LT | LE | GT | GE | ULT | UGE

type t =
  | ACC            of int
  | PUSH
  | POP            of int
  | ASSIGN         of int
  | ENVACC         of int
  | PUSH_RETADDR   of int
  | APPLY          of int
  | APPTERM        of int * int
  | RETURN         of int
  | RESTART
  | GRAB           of int
  | CLOSURE        of int * int
  | CLOSUREREC     of int * int array
  | OFFSETCLOSURE  of int
  | GETGLOBAL      of int
  | SETGLOBAL      of int
  | ATOM           of int
  | MAKEBLOCK      of int * int
  | MAKEFLOATBLOCK of int
  | GETFIELD       of int
  | GETFLOATFIELD  of int
  | SETFIELD       of int
  | SETFLOATFIELD  of int
  | GETVECTITEM
  | SETVECTITEM
  | GETSTRINGCHAR
  | SETSTRINGCHAR
  | BRANCH         of int
  | BRANCHIF       of int
  | BRANCHIFNOT    of int
  | SWITCH         of int array * int array
  | PUSHTRAP       of int
  | POPTRAP
  | RAISE
  | RERAISE
  | RAISE_NOTRACE
  | CHECK_SIGNALS
  | C_CALL         of int * int
  | CONSTINT       of int
  | UNAPP          of unop
  | BINAPP         of binop
  | COMPARE        of compop
  | COMPBRANCH     of compop * int * int
  | OFFSETREF      of int
  | GETMETHOD
  | GETPUBMET      of int
  | GETDYNMET
  | STOP

(***)

let bprint_unop buf unop =
  let open Printf in
  match unop with
  | NOT        -> bprintf buf "NOT"
  | NEG        -> bprintf buf "NEG"
  | OFFSET n   -> bprintf buf "OFFSET %d" n
  | VECTLENGTH -> bprintf buf "VECTLENGTH"
  | ISINT      -> bprintf buf "ISINT"

let bprint_binop buf binop =
  let open Printf in
  match binop with
  | ADD -> bprintf buf "ADD"
  | SUB -> bprintf buf "SUB"
  | MUL -> bprintf buf "MUL"
  | DIV -> bprintf buf "DIV"
  | MOD -> bprintf buf "MOD"
  | AND -> bprintf buf "AND"
  | OR  -> bprintf buf "OR"
  | XOR -> bprintf buf "XOR"
  | LSL -> bprintf buf "LSL"
  | LSR -> bprintf buf "LSR"
  | ASR -> bprintf buf "ASR"

let bprint_compop buf compop =
  let open Printf in
  match compop with
  | EQ  -> bprintf buf "EQ"
  | NEQ -> bprintf buf "NEQ"
  | LT  -> bprintf buf "LT"
  | LE  -> bprintf buf "LE"
  | GT  -> bprintf buf "GT"
  | GE  -> bprintf buf "GE"
  | ULT -> bprintf buf "ULT"
  | UGE -> bprintf buf "UGE"

let bprint pp_ptr pp_cfun pp_data buf instr =
  let open Printf in
  let open Tools in
  match instr with
  | ACC n                 -> bprintf buf "ACC %d" n
  | PUSH                  -> bprintf buf "PUSH"
  | POP n                 -> bprintf buf "POP %d" n
  | ASSIGN n              -> bprintf buf "ASSIGN %d" n
  | ENVACC n              -> bprintf buf "ENVACC %d" n
  | PUSH_RETADDR ptr      -> bprintf buf "PUSH_RETADDR %a" pp_ptr ptr
  | APPLY n               -> bprintf buf "APPLY %d" n
  | APPTERM (n, s)        -> bprintf buf "APPTERM %d %d" n s
  | RETURN n              -> bprintf buf "RETURN %d" n
  | RESTART               -> bprintf buf "RESTART"
  | GRAB n                -> bprintf buf "GRAB %d" n
  | CLOSURE (n, ptr)      -> bprintf buf "CLOSURE %d %a" n pp_ptr ptr
  | CLOSUREREC (n, ptrs)  -> bprintf buf "CLOSUREREC %d %a" n (bprint_mlarray pp_ptr) ptrs
  | OFFSETCLOSURE n       -> bprintf buf "OFFSETCLOSURE %d" n
  | GETGLOBAL n           -> bprintf buf "GETGLOBAL %a" pp_data n
  | SETGLOBAL n           -> bprintf buf "SETGLOBAL %a" pp_data n
  | ATOM tag              -> bprintf buf "ATOM %d" tag
  | MAKEBLOCK (tag, sz)   -> bprintf buf "MAKEBLOCK %d %d" tag sz
  | MAKEFLOATBLOCK sz     -> bprintf buf "MAKEFLOATBLOCK %d" sz
  | GETFIELD ind          -> bprintf buf "GETFIELD %d" ind
  | GETFLOATFIELD ind     -> bprintf buf "GETFLOATFIELD %d" ind
  | SETFIELD ind          -> bprintf buf "SETFIELD %d" ind
  | SETFLOATFIELD ind     -> bprintf buf "SETFLOATFIELD %d" ind
  | GETVECTITEM           -> bprintf buf "GETVECTITEM"
  | SETVECTITEM           -> bprintf buf "SETVECTITEM"
  | GETSTRINGCHAR         -> bprintf buf "GETSTRINGCHAR" 
  | SETSTRINGCHAR         -> bprintf buf "SETSTRINGCHAR"
  | BRANCH ptr            -> bprintf buf "BRANCH %a" pp_ptr ptr
  | BRANCHIF ptr          -> bprintf buf "BRANCHIF %a" pp_ptr ptr
  | BRANCHIFNOT ptr       -> bprintf buf "BRANCHIFNOT %a" pp_ptr ptr
  | SWITCH (iptrs, pptrs) -> bprintf buf "SWITCH %a %a" (bprint_mlarray pp_ptr) iptrs (bprint_mlarray pp_ptr) pptrs
  | PUSHTRAP ptr          -> bprintf buf "PUSHTRAP %a" pp_ptr ptr
  | POPTRAP               -> bprintf buf "POPTRAP"
  | RAISE                 -> bprintf buf "RAISE"
  | RERAISE               -> bprintf buf "RERAISE"
  | RAISE_NOTRACE         -> bprintf buf "RAISE_NOTRACE"
  | CHECK_SIGNALS         -> bprintf buf "CHECK_SIGNALS"
  | C_CALL (narg, idx)    -> bprintf buf "C_CALL %d %a" narg pp_cfun idx
  | CONSTINT n            -> bprintf buf "CONSTINT %d" n
  | UNAPP unop            -> bprintf buf "UNAPP %a" bprint_unop unop
  | BINAPP binop          -> bprintf buf "BINAPP %a" bprint_binop binop
  | COMPARE compop        -> bprintf buf "COMPARE %a" bprint_compop compop
  | COMPBRANCH (op,n,ptr) -> bprintf buf "COMPBRANCH %a %d %a" bprint_compop op n pp_ptr ptr
  | OFFSETREF n           -> bprintf buf "OFFSETREF %d" n
  | GETMETHOD             -> bprintf buf "GETMETHOD"
  | GETPUBMET tag         -> bprintf buf "GETPUBMET %d" tag
  | GETDYNMET             -> bprintf buf "GETDYNMET"
  | STOP                  -> bprintf buf "STOP"

let string_of_unop = Tools.to_string_of_bprint bprint_unop
let string_of_binop = Tools.to_string_of_bprint bprint_binop
let string_of_compop = Tools.to_string_of_bprint bprint_compop

let to_string =
  let pp_ptr buf ptr = Printf.bprintf buf "%d" ptr in
  let pp_cfun buf idx = Printf.bprintf buf "%d" idx in
  let pp_data buf ind = Printf.bprintf buf "%d" ind in
  Tools.to_string_of_bprint (bprint pp_ptr pp_cfun pp_data)

(***)

let get_ptrs instr = match instr with
  | PUSH_RETADDR ptr | CLOSURE (_, ptr) | BRANCH ptr | BRANCHIF ptr
  | BRANCHIFNOT ptr | PUSHTRAP ptr | COMPBRANCH (_, _, ptr) -> [ ptr ]
  | CLOSUREREC (_, p) -> Array.to_list p
  | SWITCH (iptrs, pptrs) -> Array.to_list iptrs @ Array.to_list pptrs
  | _ -> []

let get_nexts ind instr = match instr with
  | STOP | RETURN _ | APPTERM _ | RAISE | RERAISE | RAISE_NOTRACE -> []
  | GRAB _ -> [ ind - 1; ind + 1 ]
  | BRANCH ptr -> [ ptr ]
  | BRANCHIF ptr | BRANCHIFNOT ptr | PUSH_RETADDR ptr
  | PUSHTRAP ptr -> [ ind + 1; ptr ]
  | COMPBRANCH (_, _, ptr) -> [ ind + 1; ptr ]
  | SWITCH (iptrs, pptrs) -> Array.to_list iptrs @ Array.to_list pptrs
  | _ -> [ ind + 1 ]

(***)
