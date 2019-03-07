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
open Normalised_instr

type t = Normalised_instr.t array

let print data symb prim oc instrs =
  Code.print_gen get_ptrs get_nexts bprint data symb prim oc instrs

let kosaraju instrs =
  Code.kosaraju_gen Normalised_instr.get_nexts instrs

let of_code instrs =
  let instr_nb = Array.length instrs in
  let ptr_map = Array.make instr_nb (-1) in
  let normed_instrs = ref [] in
  let normed_instrs_ind = ref 0 in
  let put ni = normed_instrs := ni :: !normed_instrs; incr normed_instrs_ind in
  let convert_instr ind instr =
    ptr_map.(ind) <- !normed_instrs_ind;
    match instr with
    | Instr.ACC0                      -> put (ACC 0)
    | Instr.ACC1                      -> put (ACC 1)
    | Instr.ACC2                      -> put (ACC 2)
    | Instr.ACC3                      -> put (ACC 3)
    | Instr.ACC4                      -> put (ACC 4)
    | Instr.ACC5                      -> put (ACC 5)
    | Instr.ACC6                      -> put (ACC 6)
    | Instr.ACC7                      -> put (ACC 7)
    | Instr.ACC n                     -> put (ACC n)
    | Instr.PUSH                      -> put PUSH
    | Instr.PUSHACC0                  -> put PUSH; put (ACC 0)
    | Instr.PUSHACC1                  -> put PUSH; put (ACC 1)
    | Instr.PUSHACC2                  -> put PUSH; put (ACC 2)
    | Instr.PUSHACC3                  -> put PUSH; put (ACC 3)
    | Instr.PUSHACC4                  -> put PUSH; put (ACC 4)
    | Instr.PUSHACC5                  -> put PUSH; put (ACC 5)
    | Instr.PUSHACC6                  -> put PUSH; put (ACC 6)
    | Instr.PUSHACC7                  -> put PUSH; put (ACC 7)
    | Instr.PUSHACC n                 -> put PUSH; put (ACC n)
    | Instr.POP n                     -> put (POP n)
    | Instr.ASSIGN n                  -> put (ASSIGN n)
    | Instr.ENVACC1                   -> put (ENVACC 1)
    | Instr.ENVACC2                   -> put (ENVACC 2)
    | Instr.ENVACC3                   -> put (ENVACC 3)
    | Instr.ENVACC4                   -> put (ENVACC 4)
    | Instr.ENVACC n                  -> put (ENVACC n)
    | Instr.PUSHENVACC1               -> put PUSH; put (ENVACC 1)
    | Instr.PUSHENVACC2               -> put PUSH; put (ENVACC 2)
    | Instr.PUSHENVACC3               -> put PUSH; put (ENVACC 3)
    | Instr.PUSHENVACC4               -> put PUSH; put (ENVACC 4)
    | Instr.PUSHENVACC n              -> put PUSH; put (ENVACC n)
    | Instr.PUSH_RETADDR ptr          -> put (PUSH_RETADDR ptr)
    | Instr.APPLY n                   -> put (APPLY n)
    | Instr.APPLY1                    -> put (APPLY 1)
    | Instr.APPLY2                    -> put (APPLY 2)
    | Instr.APPLY3                    -> put (APPLY 3)
    | Instr.APPTERM (n, s)            -> put (APPTERM (n, s))
    | Instr.APPTERM1 s                -> put (APPTERM (1, s))
    | Instr.APPTERM2 s                -> put (APPTERM (2, s))
    | Instr.APPTERM3 s                -> put (APPTERM (3, s))
    | Instr.RETURN n                  -> put (RETURN n)
    | Instr.RESTART                   -> put RESTART
    | Instr.GRAB n                    -> put (GRAB n)
    | Instr.CLOSURE (n, ptr)          -> put (CLOSURE (n, ptr))
    | Instr.CLOSUREREC (f, v, o, t) ->
      if f <> Array.length t + 1 then
        fail "invalid bytecode: at instruction index %d: \
              incompatible CLOSUREREC first and fourth parameters" ind;
      put (CLOSUREREC (v, Array.concat [ [| o |]; t ]));
    | Instr.OFFSETCLOSUREM2           -> put (OFFSETCLOSURE (-1))
    | Instr.OFFSETCLOSURE0            -> put (OFFSETCLOSURE 0)
    | Instr.OFFSETCLOSURE2            -> put (OFFSETCLOSURE 1)
    | Instr.OFFSETCLOSURE n ->
      if n mod 2 <> 0 then
        fail "invalid bytecode: at instruction index %d: \
              invalid OFFSETCLOSURE parameter: %d (not a multiple of 2)"
          ind n;
      put (OFFSETCLOSURE (n / 2));
    | Instr.PUSHOFFSETCLOSUREM2       -> put PUSH; put (OFFSETCLOSURE (-1))
    | Instr.PUSHOFFSETCLOSURE0        -> put PUSH; put (OFFSETCLOSURE 0)
    | Instr.PUSHOFFSETCLOSURE2        -> put PUSH; put (OFFSETCLOSURE 1)
    | Instr.PUSHOFFSETCLOSURE n ->
      if n mod 2 <> 0 then
        fail "invalid bytecode: at instruction index %d: \
              invalid PUSHOFFSETCLOSURE parameter: %d (not a multiple of 2)"
          ind n;
      put PUSH;
      put (OFFSETCLOSURE (n / 2));
    | Instr.GETGLOBAL n               -> put (GETGLOBAL n)
    | Instr.PUSHGETGLOBAL n           -> put PUSH; put (GETGLOBAL n)
    | Instr.GETGLOBALFIELD (n, p)     -> put (GETGLOBAL n); put (GETFIELD p)
    | Instr.PUSHGETGLOBALFIELD (n, p) -> put PUSH; put (GETGLOBAL n); put (GETFIELD p)
    | Instr.SETGLOBAL n               -> put (SETGLOBAL n)
    | Instr.ATOM0                     -> put (ATOM 0)
    | Instr.ATOM tag                  -> put (ATOM tag)
    | Instr.PUSHATOM0                 -> put PUSH; put (ATOM 0)
    | Instr.PUSHATOM tag              -> put PUSH; put (ATOM tag)
    | Instr.MAKEBLOCK (tag, sz)       -> put (MAKEBLOCK (tag, sz))
    | Instr.MAKEBLOCK1 tag            -> put (MAKEBLOCK (tag, 1))
    | Instr.MAKEBLOCK2 tag            -> put (MAKEBLOCK (tag, 2))
    | Instr.MAKEBLOCK3 tag            -> put (MAKEBLOCK (tag, 3))
    | Instr.MAKEFLOATBLOCK sz         -> put (MAKEFLOATBLOCK sz)
    | Instr.GETFIELD0                 -> put (GETFIELD 0)
    | Instr.GETFIELD1                 -> put (GETFIELD 1)
    | Instr.GETFIELD2                 -> put (GETFIELD 2)
    | Instr.GETFIELD3                 -> put (GETFIELD 3)
    | Instr.GETFIELD n                -> put (GETFIELD n)
    | Instr.GETFLOATFIELD n           -> put (GETFLOATFIELD n)
    | Instr.SETFIELD0                 -> put (SETFIELD 0)
    | Instr.SETFIELD1                 -> put (SETFIELD 1)
    | Instr.SETFIELD2                 -> put (SETFIELD 2)
    | Instr.SETFIELD3                 -> put (SETFIELD 3)
    | Instr.SETFIELD n                -> put (SETFIELD n)
    | Instr.SETFLOATFIELD n           -> put (SETFLOATFIELD n)
    | Instr.VECTLENGTH                -> put (UNAPP VECTLENGTH)
    | Instr.GETVECTITEM               -> put GETVECTITEM
    | Instr.SETVECTITEM               -> put SETVECTITEM
    | Instr.GETBYTESCHAR              -> put GETBYTESCHAR
    | Instr.SETBYTESCHAR              -> put SETBYTESCHAR
    | Instr.GETSTRINGCHAR             -> put GETSTRINGCHAR
    | Instr.BRANCH ptr                -> put (BRANCH ptr)
    | Instr.BRANCHIF ptr              -> put (BRANCHIF ptr)
    | Instr.BRANCHIFNOT ptr           -> put (BRANCHIFNOT ptr)
    | Instr.SWITCH (n, ptrs) ->
      let size_tag = n lsr 16 and size_long = n land 0xFFFF in
      let iptrs = Array.sub ptrs 0 size_long
      and pptrs = Array.sub ptrs size_long size_tag in
      put (SWITCH (iptrs, pptrs))
    | Instr.BOOLNOT                   -> put (UNAPP NOT)
    | Instr.PUSHTRAP ptr              -> put (PUSHTRAP ptr)
    | Instr.POPTRAP                   -> put POPTRAP
    | Instr.RAISE                     -> put RAISE
    | Instr.RERAISE                   -> put RERAISE
    | Instr.RAISE_NOTRACE             -> put RAISE_NOTRACE
    | Instr.CHECK_SIGNALS             -> put CHECK_SIGNALS
    | Instr.C_CALL1 idx               -> put (C_CALL (1, idx))
    | Instr.C_CALL2 idx               -> put (C_CALL (2, idx))
    | Instr.C_CALL3 idx               -> put (C_CALL (3, idx))
    | Instr.C_CALL4 idx               -> put (C_CALL (4, idx))
    | Instr.C_CALL5 idx               -> put (C_CALL (5, idx))
    | Instr.C_CALLN (narg, idx)       -> put (C_CALL (narg, idx))
    | Instr.CONST0                    -> put (CONSTINT 0)
    | Instr.CONST1                    -> put (CONSTINT 1)
    | Instr.CONST2                    -> put (CONSTINT 2)
    | Instr.CONST3                    -> put (CONSTINT 3)
    | Instr.CONSTINT n                -> put (CONSTINT n)
    | Instr.PUSHCONST0                -> put PUSH; put (CONSTINT 0)
    | Instr.PUSHCONST1                -> put PUSH; put (CONSTINT 1)
    | Instr.PUSHCONST2                -> put PUSH; put (CONSTINT 2)
    | Instr.PUSHCONST3                -> put PUSH; put (CONSTINT 3)
    | Instr.PUSHCONSTINT n            -> put PUSH; put (CONSTINT n)
    | Instr.NEGINT                    -> put (UNAPP NEG)
    | Instr.ADDINT                    -> put (BINAPP ADD)
    | Instr.SUBINT                    -> put (BINAPP SUB)
    | Instr.MULINT                    -> put (BINAPP MUL)
    | Instr.DIVINT                    -> put (BINAPP DIV)
    | Instr.MODINT                    -> put (BINAPP MOD)
    | Instr.ANDINT                    -> put (BINAPP AND)
    | Instr.ORINT                     -> put (BINAPP OR)
    | Instr.XORINT                    -> put (BINAPP XOR)
    | Instr.LSLINT                    -> put (BINAPP LSL)
    | Instr.LSRINT                    -> put (BINAPP LSR)
    | Instr.ASRINT                    -> put (BINAPP ASR)
    | Instr.EQ                        -> put (COMPARE EQ)
    | Instr.NEQ                       -> put (COMPARE NEQ)
    | Instr.LTINT                     -> put (COMPARE LT)
    | Instr.LEINT                     -> put (COMPARE LE)
    | Instr.GTINT                     -> put (COMPARE GT)
    | Instr.GEINT                     -> put (COMPARE GE)
    | Instr.OFFSETINT n               -> put (UNAPP (OFFSET n))
    | Instr.OFFSETREF n               -> put (OFFSETREF n)
    | Instr.ISINT                     -> put (UNAPP ISINT)
    | Instr.GETMETHOD                 -> put GETMETHOD
    | Instr.BEQ (n, ptr)              -> put (COMPBRANCH (EQ, n, ptr))
    | Instr.BNEQ (n, ptr)             -> put (COMPBRANCH (NEQ, n, ptr))
    | Instr.BLTINT (n, ptr)           -> put (COMPBRANCH (LT, n, ptr))
    | Instr.BLEINT (n, ptr)           -> put (COMPBRANCH (LE, n, ptr))
    | Instr.BGTINT (n, ptr)           -> put (COMPBRANCH (GT, n, ptr))
    | Instr.BGEINT (n, ptr)           -> put (COMPBRANCH (GE, n, ptr))
    | Instr.ULTINT                    -> put (COMPARE ULT)
    | Instr.UGEINT                    -> put (COMPARE UGE)
    | Instr.BULTINT (n, ptr)          -> put (COMPBRANCH (ULT, n, ptr))
    | Instr.BUGEINT (n, ptr)          -> put (COMPBRANCH (UGE, n, ptr))
    | Instr.GETPUBMET (tag, cache) ->
      if cache <> 0 then
        fail "invalid bytecode: at instruction index %d: \
              invalid GETPUBMET second parameter: %d (should be 0)"
          ind cache;
      put (GETPUBMET tag);
    | Instr.GETDYNMET                 -> put GETDYNMET
    | Instr.STOP                      -> put STOP
    | Instr.EVENT ->
      fail "invalid bytecode: at index %d: unexpected instruction EVENT" ind
    | Instr.BREAK ->
      fail "invalid bytecode: at index %d: unexpected instruction BREAK" ind in
  let remap_ptrs ptrs = Array.map (fun ptr -> ptr_map.(ptr)) ptrs in
  let remap_normed_instr normed_instr = match normed_instr with
    | PUSH_RETADDR ptr      -> PUSH_RETADDR ptr_map.(ptr)
    | CLOSURE (n, ptr)      -> CLOSURE (n, ptr_map.(ptr))
    | CLOSUREREC (n, ptrs)  -> CLOSUREREC (n, remap_ptrs ptrs)
    | BRANCH ptr            -> BRANCH ptr_map.(ptr)
    | BRANCHIF ptr          -> BRANCHIF ptr_map.(ptr)
    | BRANCHIFNOT ptr       -> BRANCHIFNOT ptr_map.(ptr)
    | SWITCH (iptrs, pptrs) -> SWITCH (remap_ptrs iptrs, remap_ptrs pptrs)
    | PUSHTRAP ptr          -> PUSHTRAP ptr_map.(ptr)
    | COMPBRANCH (op,n,ptr) -> COMPBRANCH (op, n, ptr_map.(ptr))
    | _ -> normed_instr in
  Array.iteri convert_instr instrs;
  Array.map remap_normed_instr (Array.of_list (List.rev !normed_instrs))

let to_code normed_instrs =
  let normed_instr_nb = Array.length normed_instrs in
  let ptr_map = Array.make normed_instr_nb (-1) in
  let pointed = 
    let pointed = Array.make normed_instr_nb false in
    Array.iter (fun normed_instr ->
      List.iter (fun ptr -> pointed.(ptr) <- true)
        (get_ptrs normed_instr)) normed_instrs;
    pointed in
  let instrs = ref [] in
  let instrs_ind = ref 0 in
  let put instr = instrs := instr :: !instrs; incr instrs_ind in
  let normed_instr_ind = ref 0 in
  let get () =
    let ind = !normed_instr_ind in
    ptr_map.(ind) <- !instrs_ind;
    normed_instr_ind := ind + 1;
    normed_instrs.(ind) in
  let has_unpointed_next () =
    let ind = !normed_instr_ind in
    ind < normed_instr_nb && not pointed.(ind) in
  let rec default normed_instr = match normed_instr with
    | ACC 0                 -> put Instr.ACC0
    | ACC 1                 -> put Instr.ACC1
    | ACC 2                 -> put Instr.ACC2
    | ACC 3                 -> put Instr.ACC3
    | ACC 4                 -> put Instr.ACC4
    | ACC 5                 -> put Instr.ACC5
    | ACC 6                 -> put Instr.ACC6
    | ACC 7                 -> put Instr.ACC7
    | ACC n                 -> put (Instr.ACC n)
    | PUSH                  -> after_push ()
    | POP n                 -> put (Instr.POP n)
    | ASSIGN n              -> put (Instr.ASSIGN n)
    | ENVACC 1              -> put Instr.ENVACC1
    | ENVACC 2              -> put Instr.ENVACC2
    | ENVACC 3              -> put Instr.ENVACC3
    | ENVACC 4              -> put Instr.ENVACC4
    | ENVACC n              -> put (Instr.ENVACC n)
    | PUSH_RETADDR ptr      -> put (Instr.PUSH_RETADDR ptr)
    | APPLY 1               -> put Instr.APPLY1
    | APPLY 2               -> put Instr.APPLY2
    | APPLY 3               -> put Instr.APPLY3
    | APPLY n               -> put (Instr.APPLY n)
    | APPTERM (1, s)        -> put (Instr.APPTERM1 s)
    | APPTERM (2, s)        -> put (Instr.APPTERM2 s)
    | APPTERM (3, s)        -> put (Instr.APPTERM3 s)
    | APPTERM (n, s)        -> put (Instr.APPTERM (n, s))
    | RETURN n              -> put (Instr.RETURN n)
    | RESTART               -> put Instr.RESTART
    | GRAB n                -> put (Instr.GRAB n)
    | CLOSURE (n, ptr)      -> put (Instr.CLOSURE (n, ptr))
    | CLOSUREREC (n, ptrs)  ->
      let f = Array.length ptrs in
      if f = 0 then
        fail "invalid bytecode: at instruction index %d: \
              invalid CLOSUREREC second parameter: empty array"
          !normed_instr_ind;
      let o = ptrs.(0) in
      let t = Array.sub ptrs 1 (f - 1) in
      put (Instr.CLOSUREREC (f, n, o, t))
    | OFFSETCLOSURE (-1)    -> put Instr.OFFSETCLOSUREM2
    | OFFSETCLOSURE 0       -> put Instr.OFFSETCLOSURE0
    | OFFSETCLOSURE 1       -> put Instr.OFFSETCLOSURE2
    | OFFSETCLOSURE n       -> put (Instr.OFFSETCLOSURE (2 * n))
    | GETGLOBAL n           -> after_getglobal n
    | SETGLOBAL n           -> put (Instr.SETGLOBAL n)
    | ATOM 0                -> put Instr.ATOM0
    | ATOM tag              -> put (Instr.ATOM tag)
    | MAKEBLOCK (tag, 1)    -> put (Instr.MAKEBLOCK1 tag)
    | MAKEBLOCK (tag, 2)    -> put (Instr.MAKEBLOCK2 tag)
    | MAKEBLOCK (tag, 3)    -> put (Instr.MAKEBLOCK3 tag)
    | MAKEBLOCK (tag, sz)   -> put (Instr.MAKEBLOCK (tag, sz))
    | MAKEFLOATBLOCK sz     -> put (Instr.MAKEFLOATBLOCK sz)
    | GETFIELD 0            -> put Instr.GETFIELD0
    | GETFIELD 1            -> put Instr.GETFIELD1
    | GETFIELD 2            -> put Instr.GETFIELD2
    | GETFIELD 3            -> put Instr.GETFIELD3
    | GETFIELD ind          -> put (Instr.GETFIELD ind)
    | GETFLOATFIELD ind     -> put (Instr.GETFLOATFIELD ind)
    | SETFIELD 0            -> put Instr.SETFIELD0
    | SETFIELD 1            -> put Instr.SETFIELD1
    | SETFIELD 2            -> put Instr.SETFIELD2
    | SETFIELD 3            -> put Instr.SETFIELD3
    | SETFIELD ind          -> put (Instr.SETFIELD ind)
    | SETFLOATFIELD ind     -> put (Instr.SETFLOATFIELD ind)
    | UNAPP VECTLENGTH      -> put Instr.VECTLENGTH
    | GETVECTITEM           -> put Instr.GETVECTITEM
    | SETVECTITEM           -> put Instr.SETVECTITEM
    | GETBYTESCHAR          -> put Instr.GETBYTESCHAR
    | SETBYTESCHAR          -> put Instr.SETBYTESCHAR
    | GETSTRINGCHAR         -> put Instr.GETSTRINGCHAR
    | BRANCH ptr            -> put (Instr.BRANCH ptr)
    | BRANCHIF ptr          -> put (Instr.BRANCHIF ptr)
    | BRANCHIFNOT ptr       -> put (Instr.BRANCHIFNOT ptr)
    | SWITCH (iptrs, pptrs) ->
      let size_tag = Array.length pptrs and size_long = Array.length iptrs in
      let n = (size_tag lsl 16) lor size_long in
      if size_long > 0xFFFF then
        fail "invalid bytecode: at instruction index %d: \
              invalid SWITCH first parameter: too many pointers"
          !normed_instr_ind;
      put (Instr.SWITCH (n, Array.concat [ iptrs; pptrs ]))
    | UNAPP NOT             -> put Instr.BOOLNOT
    | PUSHTRAP ptr          -> put (Instr.PUSHTRAP ptr)
    | POPTRAP               -> put Instr.POPTRAP
    | RAISE                 -> put Instr.RAISE
    | RERAISE               -> put Instr.RERAISE
    | RAISE_NOTRACE         -> put Instr.RAISE_NOTRACE
    | CHECK_SIGNALS         -> put Instr.CHECK_SIGNALS
    | C_CALL (1, idx)       -> put (Instr.C_CALL1 idx)
    | C_CALL (2, idx)       -> put (Instr.C_CALL2 idx)
    | C_CALL (3, idx)       -> put (Instr.C_CALL3 idx)
    | C_CALL (4, idx)       -> put (Instr.C_CALL4 idx)
    | C_CALL (5, idx)       -> put (Instr.C_CALL5 idx)
    | C_CALL (narg, idx)    -> put (Instr.C_CALLN (narg, idx))
    | CONSTINT 0            -> put Instr.CONST0
    | CONSTINT 1            -> put Instr.CONST1
    | CONSTINT 2            -> put Instr.CONST2
    | CONSTINT 3            -> put Instr.CONST3
    | CONSTINT n            -> put (Instr.CONSTINT n)
    | UNAPP NEG             -> put Instr.NEGINT
    | BINAPP ADD            -> put Instr.ADDINT
    | BINAPP SUB            -> put Instr.SUBINT
    | BINAPP MUL            -> put Instr.MULINT
    | BINAPP DIV            -> put Instr.DIVINT
    | BINAPP MOD            -> put Instr.MODINT
    | BINAPP AND            -> put Instr.ANDINT
    | BINAPP OR             -> put Instr.ORINT
    | BINAPP XOR            -> put Instr.XORINT
    | BINAPP LSL            -> put Instr.LSLINT
    | BINAPP LSR            -> put Instr.LSRINT
    | BINAPP ASR            -> put Instr.ASRINT
    | COMPARE EQ            -> put Instr.EQ
    | COMPARE NEQ           -> put Instr.NEQ
    | COMPARE LT            -> put Instr.LTINT
    | COMPARE LE            -> put Instr.LEINT
    | COMPARE GT            -> put Instr.GTINT
    | COMPARE GE            -> put Instr.GEINT
    | UNAPP (OFFSET n)      -> put (Instr.OFFSETINT n)
    | OFFSETREF n           -> put (Instr.OFFSETREF n)
    | UNAPP ISINT           -> put Instr.ISINT
    | GETMETHOD             -> put Instr.GETMETHOD
    | COMPBRANCH (EQ,n,ptr) -> put (Instr.BEQ (n, ptr))
    | COMPBRANCH (NEQ,n,ptr)-> put (Instr.BNEQ (n, ptr))
    | COMPBRANCH (LT,n,ptr) -> put (Instr.BLTINT (n, ptr))
    | COMPBRANCH (LE,n,ptr) -> put (Instr.BLEINT (n, ptr))
    | COMPBRANCH (GT,n,ptr) -> put (Instr.BGTINT (n, ptr))
    | COMPBRANCH (GE,n,ptr) -> put (Instr.BGEINT (n, ptr))
    | COMPARE ULT           -> put Instr.ULTINT
    | COMPARE UGE           -> put Instr.UGEINT
    | COMPBRANCH (ULT,n,ptr)-> put (Instr.BULTINT (n, ptr))
    | COMPBRANCH (UGE,n,ptr)-> put (Instr.BUGEINT (n, ptr))
    | GETPUBMET tag         -> put (Instr.GETPUBMET (tag, 0))
    | GETDYNMET             -> put Instr.GETDYNMET
    | STOP                  -> put Instr.STOP
  and after_push () =
    if has_unpointed_next () then
      match get () with
      | ACC 0              -> put Instr.PUSHACC0
      | ACC 1              -> put Instr.PUSHACC1
      | ACC 2              -> put Instr.PUSHACC2
      | ACC 3              -> put Instr.PUSHACC3
      | ACC 4              -> put Instr.PUSHACC4
      | ACC 5              -> put Instr.PUSHACC5
      | ACC 6              -> put Instr.PUSHACC6
      | ACC 7              -> put Instr.PUSHACC7
      | ACC n              -> put (Instr.PUSHACC n)
      | ENVACC 1           -> put Instr.PUSHENVACC1
      | ENVACC 2           -> put Instr.PUSHENVACC2
      | ENVACC 3           -> put Instr.PUSHENVACC3
      | ENVACC 4           -> put Instr.PUSHENVACC4
      | ENVACC n           -> put (Instr.PUSHENVACC n)
      | OFFSETCLOSURE (-1) -> put Instr.PUSHOFFSETCLOSUREM2
      | OFFSETCLOSURE 0    -> put Instr.PUSHOFFSETCLOSURE0
      | OFFSETCLOSURE 1    -> put Instr.PUSHOFFSETCLOSURE2
      | OFFSETCLOSURE n    -> put (Instr.PUSHOFFSETCLOSURE (2 * n))
      | ATOM 0             -> put Instr.PUSHATOM0
      | ATOM n             -> put (Instr.PUSHATOM n)
      | CONSTINT 0         -> put Instr.PUSHCONST0
      | CONSTINT 1         -> put Instr.PUSHCONST1
      | CONSTINT 2         -> put Instr.PUSHCONST2
      | CONSTINT 3         -> put Instr.PUSHCONST3
      | CONSTINT n         -> put (Instr.PUSHCONSTINT n)
      | GETGLOBAL n        -> after_push_getglobal n
      | PUSH               -> put Instr.PUSH; after_push ()
      | normed_instr       -> put Instr.PUSH; default normed_instr
    else put Instr.PUSH
  and after_getglobal n =
    if has_unpointed_next () then
      match get () with
      | GETFIELD p   -> put (Instr.GETGLOBALFIELD (n, p))
      | normed_instr -> put (Instr.GETGLOBAL n); default normed_instr
    else put (Instr.GETGLOBAL n)
  and after_push_getglobal n =
    if has_unpointed_next () then
      match get () with
      | GETFIELD p   -> put (Instr.PUSHGETGLOBALFIELD (n, p))
      | normed_instr -> put (Instr.PUSHGETGLOBAL n); default normed_instr
    else put (Instr.PUSHGETGLOBAL n) in
  let remap_ptrs ptrs = Array.map (fun ptr -> ptr_map.(ptr)) ptrs in
  let remap_instr instr = match instr with
    | Instr.PUSH_RETADDR ptr        -> Instr.PUSH_RETADDR ptr_map.(ptr)
    | Instr.CLOSURE (n, ptr)        -> Instr.CLOSURE (n, ptr_map.(ptr))
    | Instr.CLOSUREREC (f, v, o, t) -> Instr.CLOSUREREC (f, v, ptr_map.(o), remap_ptrs t)
    | Instr.BRANCH ptr              -> Instr.BRANCH ptr_map.(ptr)
    | Instr.BRANCHIF ptr            -> Instr.BRANCHIF ptr_map.(ptr)
    | Instr.BRANCHIFNOT ptr         -> Instr.BRANCHIFNOT ptr_map.(ptr)
    | Instr.SWITCH (n, ptrs)        -> Instr.SWITCH (n, remap_ptrs ptrs)
    | Instr.PUSHTRAP ptr            -> Instr.PUSHTRAP ptr_map.(ptr)
    | Instr.BEQ (n, ptr)            -> Instr.BEQ (n, ptr_map.(ptr))
    | Instr.BNEQ (n, ptr)           -> Instr.BNEQ (n, ptr_map.(ptr))
    | Instr.BLTINT (n, ptr)         -> Instr.BLTINT (n, ptr_map.(ptr))
    | Instr.BLEINT (n, ptr)         -> Instr.BLEINT (n, ptr_map.(ptr))
    | Instr.BGTINT (n, ptr)         -> Instr.BGTINT (n, ptr_map.(ptr))
    | Instr.BGEINT (n, ptr)         -> Instr.BGEINT (n, ptr_map.(ptr))
    | Instr.BULTINT (n, ptr)        -> Instr.BULTINT (n, ptr_map.(ptr))
    | Instr.BUGEINT (n, ptr)        -> Instr.BUGEINT (n, ptr_map.(ptr))
    | _ -> instr in
  while !normed_instr_ind < normed_instr_nb do default (get ()) done;
  Array.map remap_instr (Array.of_list (List.rev !instrs))
