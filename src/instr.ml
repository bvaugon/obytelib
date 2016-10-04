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

type t =
  | ACC0
  | ACC1
  | ACC2
  | ACC3
  | ACC4
  | ACC5
  | ACC6
  | ACC7
  | ACC                of int
  | PUSH
  | PUSHACC0
  | PUSHACC1
  | PUSHACC2
  | PUSHACC3
  | PUSHACC4
  | PUSHACC5
  | PUSHACC6
  | PUSHACC7
  | PUSHACC            of int
  | POP                of int
  | ASSIGN             of int
  | ENVACC1
  | ENVACC2
  | ENVACC3
  | ENVACC4
  | ENVACC             of int
  | PUSHENVACC1
  | PUSHENVACC2
  | PUSHENVACC3
  | PUSHENVACC4
  | PUSHENVACC         of int
  | PUSH_RETADDR       of int
  | APPLY              of int
  | APPLY1
  | APPLY2
  | APPLY3
  | APPTERM            of int * int
  | APPTERM1           of int
  | APPTERM2           of int
  | APPTERM3           of int
  | RETURN             of int
  | RESTART
  | GRAB               of int
  | CLOSURE            of int * int
  | CLOSUREREC         of int * int * int * int array
  | OFFSETCLOSUREM2
  | OFFSETCLOSURE0
  | OFFSETCLOSURE2
  | OFFSETCLOSURE      of int
  | PUSHOFFSETCLOSUREM2
  | PUSHOFFSETCLOSURE0
  | PUSHOFFSETCLOSURE2
  | PUSHOFFSETCLOSURE  of int
  | GETGLOBAL          of int
  | PUSHGETGLOBAL      of int
  | GETGLOBALFIELD     of int * int
  | PUSHGETGLOBALFIELD of int * int
  | SETGLOBAL          of int
  | ATOM0
  | ATOM               of int
  | PUSHATOM0
  | PUSHATOM           of int
  | MAKEBLOCK          of int * int
  | MAKEBLOCK1         of int
  | MAKEBLOCK2         of int
  | MAKEBLOCK3         of int
  | MAKEFLOATBLOCK     of int
  | GETFIELD0
  | GETFIELD1
  | GETFIELD2
  | GETFIELD3
  | GETFIELD           of int
  | GETFLOATFIELD      of int
  | SETFIELD0
  | SETFIELD1
  | SETFIELD2
  | SETFIELD3
  | SETFIELD           of int
  | SETFLOATFIELD      of int
  | VECTLENGTH
  | GETVECTITEM
  | SETVECTITEM
  | GETSTRINGCHAR
  | SETSTRINGCHAR
  | BRANCH             of int
  | BRANCHIF           of int
  | BRANCHIFNOT        of int
  | SWITCH             of int * int array
  | BOOLNOT
  | PUSHTRAP           of int
  | POPTRAP
  | RAISE
  | RERAISE
  | RAISE_NOTRACE
  | CHECK_SIGNALS
  | C_CALL1            of int
  | C_CALL2            of int
  | C_CALL3            of int
  | C_CALL4            of int
  | C_CALL5            of int
  | C_CALLN            of int * int
  | CONST0
  | CONST1
  | CONST2
  | CONST3
  | CONSTINT           of int
  | PUSHCONST0
  | PUSHCONST1
  | PUSHCONST2
  | PUSHCONST3
  | PUSHCONSTINT       of int
  | NEGINT
  | ADDINT
  | SUBINT
  | MULINT
  | DIVINT
  | MODINT
  | ANDINT
  | ORINT
  | XORINT
  | LSLINT
  | LSRINT
  | ASRINT
  | EQ
  | NEQ
  | LTINT
  | LEINT
  | GTINT
  | GEINT
  | OFFSETINT          of int
  | OFFSETREF          of int
  | ISINT
  | GETMETHOD
  | BEQ                of int * int
  | BNEQ               of int * int
  | BLTINT             of int * int
  | BLEINT             of int * int
  | BGTINT             of int * int
  | BGEINT             of int * int
  | ULTINT
  | UGEINT
  | BULTINT            of int * int
  | BUGEINT            of int * int
  | GETPUBMET          of int * int
  | GETDYNMET
  | STOP
  | EVENT
  | BREAK

(***)

let bprint pp_ptr pp_cfun pp_data buf instr =
  let open Printf in
  let open Tools in
  match instr with
  | ACC0                      -> bprintf buf "ACC0"
  | ACC1                      -> bprintf buf "ACC1"
  | ACC2                      -> bprintf buf "ACC2"
  | ACC3                      -> bprintf buf "ACC3"
  | ACC4                      -> bprintf buf "ACC4"
  | ACC5                      -> bprintf buf "ACC5"
  | ACC6                      -> bprintf buf "ACC6"
  | ACC7                      -> bprintf buf "ACC7"
  | ACC n                     -> bprintf buf "ACC %d" n
  | PUSH                      -> bprintf buf "PUSH"
  | PUSHACC0                  -> bprintf buf "PUSHACC0"
  | PUSHACC1                  -> bprintf buf "PUSHACC1"
  | PUSHACC2                  -> bprintf buf "PUSHACC2"
  | PUSHACC3                  -> bprintf buf "PUSHACC3"
  | PUSHACC4                  -> bprintf buf "PUSHACC4"
  | PUSHACC5                  -> bprintf buf "PUSHACC5"
  | PUSHACC6                  -> bprintf buf "PUSHACC6"
  | PUSHACC7                  -> bprintf buf "PUSHACC7"
  | PUSHACC n                 -> bprintf buf "PUSHACC %d" n
  | POP n                     -> bprintf buf "POP %d" n
  | ASSIGN n                  -> bprintf buf "ASSIGN %d" n
  | ENVACC1                   -> bprintf buf "ENVACC1"
  | ENVACC2                   -> bprintf buf "ENVACC2"
  | ENVACC3                   -> bprintf buf "ENVACC3"
  | ENVACC4                   -> bprintf buf "ENVACC4"
  | ENVACC n                  -> bprintf buf "ENVACC %d" n
  | PUSHENVACC1               -> bprintf buf "PUSHENVACC1"
  | PUSHENVACC2               -> bprintf buf "PUSHENVACC2"
  | PUSHENVACC3               -> bprintf buf "PUSHENVACC3"
  | PUSHENVACC4               -> bprintf buf "PUSHENVACC4"
  | PUSHENVACC n              -> bprintf buf "PUSHENVACC %d" n
  | PUSH_RETADDR ptr          -> bprintf buf "PUSH_RETADDR %a" pp_ptr ptr
  | APPLY n                   -> bprintf buf "APPLY %d" n
  | APPLY1                    -> bprintf buf "APPLY1"
  | APPLY2                    -> bprintf buf "APPLY2"
  | APPLY3                    -> bprintf buf "APPLY3"
  | APPTERM (n, s)            -> bprintf buf "APPTERM %d %d" n s
  | APPTERM1 s                -> bprintf buf "APPTERM1 %d" s
  | APPTERM2 s                -> bprintf buf "APPTERM2 %d" s
  | APPTERM3 s                -> bprintf buf "APPTERM3 %d" s
  | RETURN n                  -> bprintf buf "RETURN %d" n
  | RESTART                   -> bprintf buf "RESTART"
  | GRAB n                    -> bprintf buf "GRAB %d" n
  | CLOSURE (n, ptr)          -> bprintf buf "CLOSURE %d %a" n pp_ptr ptr
  | CLOSUREREC (f, v, o, t)   -> bprintf buf "CLOSUREREC %d %d %a %a" f v pp_ptr o (bprint_mlarray pp_ptr) t
  | OFFSETCLOSUREM2           -> bprintf buf "OFFSETCLOSUREM2"
  | OFFSETCLOSURE0            -> bprintf buf "OFFSETCLOSURE0"
  | OFFSETCLOSURE2            -> bprintf buf "OFFSETCLOSURE2"
  | OFFSETCLOSURE n           -> bprintf buf "OFFSETCLOSURE %d" n
  | PUSHOFFSETCLOSUREM2       -> bprintf buf "PUSHOFFSETCLOSUREM2"
  | PUSHOFFSETCLOSURE0        -> bprintf buf "PUSHOFFSETCLOSURE0"
  | PUSHOFFSETCLOSURE2        -> bprintf buf "PUSHOFFSETCLOSURE2"
  | PUSHOFFSETCLOSURE n       -> bprintf buf "PUSHOFFSETCLOSURE %d" n
  | GETGLOBAL n               -> bprintf buf "GETGLOBAL %a" pp_data n
  | PUSHGETGLOBAL n           -> bprintf buf "PUSHGETGLOBAL %a" pp_data n
  | GETGLOBALFIELD (n, p)     -> bprintf buf "GETGLOBALFIELD %a %d" pp_data n p
  | PUSHGETGLOBALFIELD (n, p) -> bprintf buf "PUSHGETGLOBALFIELD %a %d" pp_data n p
  | SETGLOBAL n               -> bprintf buf "SETGLOBAL %a" pp_data n
  | ATOM0                     -> bprintf buf "ATOM0"
  | ATOM tag                  -> bprintf buf "ATOM %d" tag
  | PUSHATOM0                 -> bprintf buf "PUSHATOM0"
  | PUSHATOM tag              -> bprintf buf "PUSHATOM %d" tag
  | MAKEBLOCK (tag, sz)       -> bprintf buf "MAKEBLOCK %d %d" tag sz
  | MAKEBLOCK1 tag            -> bprintf buf "MAKEBLOCK1 %d" tag
  | MAKEBLOCK2 tag            -> bprintf buf "MAKEBLOCK2 %d" tag
  | MAKEBLOCK3 tag            -> bprintf buf "MAKEBLOCK3 %d" tag
  | MAKEFLOATBLOCK sz         -> bprintf buf "MAKEFLOATBLOCK %d" sz
  | GETFIELD0                 -> bprintf buf "GETFIELD0"
  | GETFIELD1                 -> bprintf buf "GETFIELD1"
  | GETFIELD2                 -> bprintf buf "GETFIELD2"
  | GETFIELD3                 -> bprintf buf "GETFIELD3"
  | GETFIELD n                -> bprintf buf "GETFIELD %d" n
  | GETFLOATFIELD n           -> bprintf buf "GETFLOATFIELD %d" n
  | SETFIELD0                 -> bprintf buf "SETFIELD0"
  | SETFIELD1                 -> bprintf buf "SETFIELD1"
  | SETFIELD2                 -> bprintf buf "SETFIELD2"
  | SETFIELD3                 -> bprintf buf "SETFIELD3"
  | SETFIELD n                -> bprintf buf "SETFIELD %d" n
  | SETFLOATFIELD n           -> bprintf buf "SETFLOATFIELD %d" n
  | VECTLENGTH                -> bprintf buf "VECTLENGTH"
  | GETVECTITEM               -> bprintf buf "GETVECTITEM"
  | SETVECTITEM               -> bprintf buf "SETVECTITEM"
  | GETSTRINGCHAR             -> bprintf buf "GETSTRINGCHAR"
  | SETSTRINGCHAR             -> bprintf buf "SETSTRINGCHAR"
  | BRANCH ptr                -> bprintf buf "BRANCH %a" pp_ptr ptr
  | BRANCHIF ptr              -> bprintf buf "BRANCHIF %a" pp_ptr ptr
  | BRANCHIFNOT ptr           -> bprintf buf "BRANCHIFNOT %a" pp_ptr ptr
  | SWITCH (n, ptrs)          -> bprintf buf "SWITCH %d %a" n (bprint_mlarray pp_ptr) ptrs
  | BOOLNOT                   -> bprintf buf "BOOLNOT"
  | PUSHTRAP ptr              -> bprintf buf "PUSHTRAP %a" pp_ptr ptr
  | POPTRAP                   -> bprintf buf "POPTRAP"
  | RAISE                     -> bprintf buf "RAISE"
  | RERAISE                   -> bprintf buf "RERAISE"
  | RAISE_NOTRACE             -> bprintf buf "RAISE_NOTRACE"
  | CHECK_SIGNALS             -> bprintf buf "CHECK_SIGNALS"
  | C_CALL1 idx               -> bprintf buf "C_CALL1 %a" pp_cfun idx
  | C_CALL2 idx               -> bprintf buf "C_CALL2 %a" pp_cfun idx
  | C_CALL3 idx               -> bprintf buf "C_CALL3 %a" pp_cfun idx
  | C_CALL4 idx               -> bprintf buf "C_CALL4 %a" pp_cfun idx
  | C_CALL5 idx               -> bprintf buf "C_CALL5 %a" pp_cfun idx
  | C_CALLN (narg, idx)       -> bprintf buf "C_CALLN %d %a" narg pp_cfun idx
  | CONST0                    -> bprintf buf "CONST0"
  | CONST1                    -> bprintf buf "CONST1"
  | CONST2                    -> bprintf buf "CONST2"
  | CONST3                    -> bprintf buf "CONST3"
  | CONSTINT n                -> bprintf buf "CONSTINT %d" n
  | PUSHCONST0                -> bprintf buf "PUSHCONST0"
  | PUSHCONST1                -> bprintf buf "PUSHCONST1"
  | PUSHCONST2                -> bprintf buf "PUSHCONST2"
  | PUSHCONST3                -> bprintf buf "PUSHCONST3"
  | PUSHCONSTINT n            -> bprintf buf "PUSHCONSTINT %d" n
  | NEGINT                    -> bprintf buf "NEGINT"
  | ADDINT                    -> bprintf buf "ADDINT"
  | SUBINT                    -> bprintf buf "SUBINT"
  | MULINT                    -> bprintf buf "MULINT"
  | DIVINT                    -> bprintf buf "DIVINT"
  | MODINT                    -> bprintf buf "MODINT"
  | ANDINT                    -> bprintf buf "ANDINT"
  | ORINT                     -> bprintf buf "ORINT"
  | XORINT                    -> bprintf buf "XORINT"
  | LSLINT                    -> bprintf buf "LSLINT"
  | LSRINT                    -> bprintf buf "LSRINT"
  | ASRINT                    -> bprintf buf "ASRINT"
  | EQ                        -> bprintf buf "EQ"
  | NEQ                       -> bprintf buf "NEQ"
  | LTINT                     -> bprintf buf "LTINT"
  | LEINT                     -> bprintf buf "LEINT"
  | GTINT                     -> bprintf buf "GTINT"
  | GEINT                     -> bprintf buf "GEINT"
  | OFFSETINT n               -> bprintf buf "OFFSETINT %d" n
  | OFFSETREF n               -> bprintf buf "OFFSETREF %d" n
  | ISINT                     -> bprintf buf "ISINT"
  | GETMETHOD                 -> bprintf buf "GETMETHOD"
  | BEQ (n, ptr)              -> bprintf buf "BEQ %d %a" n pp_ptr ptr
  | BNEQ (n, ptr)             -> bprintf buf "BNEQ %d %a" n pp_ptr ptr
  | BLTINT (n, ptr)           -> bprintf buf "BLTINT %d %a" n pp_ptr ptr
  | BLEINT (n, ptr)           -> bprintf buf "BLEINT %d %a" n pp_ptr ptr
  | BGTINT (n, ptr)           -> bprintf buf "BGTINT %d %a" n pp_ptr ptr
  | BGEINT (n, ptr)           -> bprintf buf "BGEINT %d %a" n pp_ptr ptr
  | ULTINT                    -> bprintf buf "ULTINT"
  | UGEINT                    -> bprintf buf "UGEINT"
  | BULTINT (n, ptr)          -> bprintf buf "BULTINT %d %a" n pp_ptr ptr
  | BUGEINT (n, ptr)          -> bprintf buf "BUGEINT %d %a" n pp_ptr ptr
  | GETPUBMET (tag, cache)    -> bprintf buf "GETPUBMET %d %d" tag cache
  | GETDYNMET                 -> bprintf buf "GETDYNMET"
  | STOP                      -> bprintf buf "STOP"
  | EVENT                     -> bprintf buf "EVENT"
  | BREAK                     -> bprintf buf "BREAK"

let to_string =
  let pp_ptr buf ptr = Printf.bprintf buf "%d" ptr in
  let pp_cfun buf idx = Printf.bprintf buf "%d" idx in
  let pp_data buf ind = Printf.bprintf buf "%d" ind in
  Tools.to_string_of_bprint (bprint pp_ptr pp_cfun pp_data)

(***)

let get_ptrs instr = match instr with
  | PUSH_RETADDR ptr | BRANCH ptr | BRANCHIF ptr | BRANCHIFNOT ptr
  | PUSHTRAP ptr | CLOSURE (_, ptr) | BEQ (_, ptr) | BNEQ (_, ptr)
  | BLTINT (_, ptr) | BLEINT (_, ptr) | BGTINT (_, ptr) | BGEINT (_, ptr)
  | BULTINT (_, ptr) | BUGEINT (_, ptr) -> [ ptr ]
  | CLOSUREREC (_, _, ptr, ptrs) -> ptr :: Array.to_list ptrs
  | SWITCH (_, ptrs) -> Array.to_list ptrs
  | _ -> []

let get_nexts ind instr = match instr with
  | STOP | RETURN _ | APPTERM _ | APPTERM1 _ | APPTERM2 _ | APPTERM3 _
  | RAISE | RERAISE | RAISE_NOTRACE -> []
  | GRAB _ -> [ ind - 1; ind + 1 ]
  | BRANCH ptr -> [ ptr ]
  | BRANCHIF ptr | BRANCHIFNOT ptr | BEQ (_, ptr) | BNEQ (_, ptr)
  | BLTINT (_, ptr) | BLEINT (_, ptr) | BGTINT (_, ptr) | BGEINT (_, ptr)
  | BULTINT (_, ptr) | BUGEINT (_, ptr) | PUSH_RETADDR ptr
  | PUSHTRAP ptr -> [ ind + 1; ptr ]
  | SWITCH (_, ptrs) -> Array.to_list ptrs
  | _ -> [ ind + 1 ]

(***)

let read version next_word =
  let opcode =
    let w = next_word () in
    match version with
    | Version.V008 ->
      if w <= 91 then w else w + 2
    | Version.V010 ->
      w
    | Version.V011 ->
      if w <= 91 then w
      else if w = 146 then 92
      else if w = 147 then 93
      else w + 2 in
  match opcode with
  |   0 -> ACC0
  |   1 -> ACC1
  |   2 -> ACC2
  |   3 -> ACC3
  |   4 -> ACC4
  |   5 -> ACC5
  |   6 -> ACC6
  |   7 -> ACC7
  |   8 -> ACC (next_word ())
  |   9 -> PUSH
  |  10 -> PUSHACC0
  |  11 -> PUSHACC1
  |  12 -> PUSHACC2
  |  13 -> PUSHACC3
  |  14 -> PUSHACC4
  |  15 -> PUSHACC5
  |  16 -> PUSHACC6
  |  17 -> PUSHACC7
  |  18 -> PUSHACC (next_word ())
  |  19 -> POP (next_word ())
  |  20 -> ASSIGN (next_word ())
  |  21 -> ENVACC1
  |  22 -> ENVACC2
  |  23 -> ENVACC3
  |  24 -> ENVACC4
  |  25 -> ENVACC (next_word ())
  |  26 -> PUSHENVACC1
  |  27 -> PUSHENVACC2
  |  28 -> PUSHENVACC3
  |  29 -> PUSHENVACC4
  |  30 -> PUSHENVACC (next_word ())
  |  31 -> PUSH_RETADDR (next_word ())
  |  32 -> APPLY (next_word ())
  |  33 -> APPLY1
  |  34 -> APPLY2
  |  35 -> APPLY3
  |  36 -> let n = next_word () in let s = next_word () in APPTERM (n, s)
  |  37 -> APPTERM1 (next_word ())
  |  38 -> APPTERM2 (next_word ())
  |  39 -> APPTERM3 (next_word ())
  |  40 -> RETURN (next_word ())
  |  41 -> RESTART
  |  42 -> GRAB (next_word ())
  |  43 -> let n = next_word () in let ptr = next_word () in CLOSURE (n, ptr)
  |  44 ->
    let f = next_word () in
    let v = next_word () in
    let o = next_word () in
    let t = Array.make (f - 1) (-1) in
    for i = 0 to f - 2 do t.(i) <- next_word () done;
    CLOSUREREC (f, v, o, t)
  |  45 -> OFFSETCLOSUREM2
  |  46 -> OFFSETCLOSURE0
  |  47 -> OFFSETCLOSURE2
  |  48 -> OFFSETCLOSURE (next_word ())
  |  49 -> PUSHOFFSETCLOSUREM2
  |  50 -> PUSHOFFSETCLOSURE0
  |  51 -> PUSHOFFSETCLOSURE2
  |  52 -> PUSHOFFSETCLOSURE (next_word ())
  |  53 -> GETGLOBAL (next_word ())
  |  54 -> PUSHGETGLOBAL (next_word ())
  |  55 -> let n = next_word () in let p = next_word () in GETGLOBALFIELD (n, p)
  |  56 -> let n = next_word () in let p = next_word () in PUSHGETGLOBALFIELD (n, p)
  |  57 -> SETGLOBAL (next_word ())
  |  58 -> ATOM0
  |  59 -> ATOM (next_word ())
  |  60 -> PUSHATOM0
  |  61 -> PUSHATOM (next_word ())
  |  62 -> let sz = next_word () in let tag = next_word () in MAKEBLOCK (tag, sz)
  |  63 -> MAKEBLOCK1 (next_word ())
  |  64 -> MAKEBLOCK2 (next_word ())
  |  65 -> MAKEBLOCK3 (next_word ())
  |  66 -> MAKEFLOATBLOCK (next_word ())
  |  67 -> GETFIELD0
  |  68 -> GETFIELD1
  |  69 -> GETFIELD2
  |  70 -> GETFIELD3
  |  71 -> GETFIELD (next_word ())
  |  72 -> GETFLOATFIELD (next_word ())
  |  73 -> SETFIELD0
  |  74 -> SETFIELD1
  |  75 -> SETFIELD2
  |  76 -> SETFIELD3
  |  77 -> SETFIELD (next_word ())
  |  78 -> SETFLOATFIELD (next_word ())
  |  79 -> VECTLENGTH
  |  80 -> GETVECTITEM
  |  81 -> SETVECTITEM
  |  82 -> GETSTRINGCHAR
  |  83 -> SETSTRINGCHAR
  |  84 -> BRANCH (next_word ())
  |  85 -> BRANCHIF (next_word ())
  |  86 -> BRANCHIFNOT (next_word ())
  |  87 ->
    let n = next_word () in
    let size_tag = n lsr 16 in
    let size_long = n land 0xFFFF in
    let size = size_tag + size_long in
    let tab = Array.init size (fun _ -> next_word ()) in
    SWITCH (n, tab)
  |  88 -> BOOLNOT
  |  89 -> PUSHTRAP (next_word ())
  |  90 -> POPTRAP
  |  91 -> RAISE
  |  92 -> RERAISE
  |  93 -> RAISE_NOTRACE
  |  94 -> CHECK_SIGNALS
  |  95 -> C_CALL1 (next_word ())
  |  96 -> C_CALL2 (next_word ())
  |  97 -> C_CALL3 (next_word ())
  |  98 -> C_CALL4 (next_word ())
  |  99 -> C_CALL5 (next_word ())
  | 100 -> let narg = next_word () in let idx = next_word () in C_CALLN (narg, idx)
  | 101 -> CONST0
  | 102 -> CONST1
  | 103 -> CONST2
  | 104 -> CONST3
  | 105 -> CONSTINT (next_word ())
  | 106 -> PUSHCONST0
  | 107 -> PUSHCONST1
  | 108 -> PUSHCONST2
  | 109 -> PUSHCONST3
  | 110 -> PUSHCONSTINT (next_word ())
  | 111 -> NEGINT
  | 112 -> ADDINT
  | 113 -> SUBINT
  | 114 -> MULINT
  | 115 -> DIVINT
  | 116 -> MODINT
  | 117 -> ANDINT
  | 118 -> ORINT
  | 119 -> XORINT
  | 120 -> LSLINT
  | 121 -> LSRINT
  | 122 -> ASRINT
  | 123 -> EQ
  | 124 -> NEQ
  | 125 -> LTINT
  | 126 -> LEINT
  | 127 -> GTINT
  | 128 -> GEINT
  | 129 -> OFFSETINT (next_word ())
  | 130 -> OFFSETREF (next_word ())
  | 131 -> ISINT
  | 132 -> GETMETHOD
  | 133 -> let n = next_word () in let ptr = next_word () in BEQ (n, ptr)
  | 134 -> let n = next_word () in let ptr = next_word () in BNEQ (n, ptr)
  | 135 -> let n = next_word () in let ptr = next_word () in BLTINT (n, ptr)
  | 136 -> let n = next_word () in let ptr = next_word () in BLEINT (n, ptr)
  | 137 -> let n = next_word () in let ptr = next_word () in BGTINT (n, ptr)
  | 138 -> let n = next_word () in let ptr = next_word () in BGEINT (n, ptr)
  | 139 -> ULTINT
  | 140 -> UGEINT
  | 141 -> let n = next_word () in let ptr = next_word () in BULTINT (n, ptr)
  | 142 -> let n = next_word () in let ptr = next_word () in BUGEINT (n, ptr)
  | 143 -> let tag = next_word () in let cache = next_word () in GETPUBMET (tag, cache)
  | 144 -> GETDYNMET
  | 145 -> STOP
  | 146 -> EVENT
  | 147 -> BREAK
  | _ -> failwith (Printf.sprintf "invalid opcode: %d" opcode)

let write version write_word write_ptr instr =
  let write_opcode w = match version, w with
    | Version.V008, _ when w <= 91 -> write_word w
    | Version.V008, (92 | 93) -> write_word 91
    | Version.V008, _ -> write_word (w - 2)
    | Version.V010, _ -> write_word w
    | Version.V011, _ when w <= 91 -> write_word w
    | Version.V011, 92 -> write_word 146
    | Version.V011, 93 -> write_word 147
    | Version.V011, _ -> write_word (w - 2) in
  let write_ptrs delta ptrs = Array.iter (write_ptr delta) ptrs in
  match instr with
  | ACC0                      -> write_opcode 0
  | ACC1                      -> write_opcode 1
  | ACC2                      -> write_opcode 2
  | ACC3                      -> write_opcode 3
  | ACC4                      -> write_opcode 4
  | ACC5                      -> write_opcode 5
  | ACC6                      -> write_opcode 6
  | ACC7                      -> write_opcode 7
  | ACC n                     -> write_opcode 8; write_word n
  | PUSH                      -> write_opcode 9 
  | PUSHACC0                  -> write_opcode 10
  | PUSHACC1                  -> write_opcode 11
  | PUSHACC2                  -> write_opcode 12
  | PUSHACC3                  -> write_opcode 13
  | PUSHACC4                  -> write_opcode 14
  | PUSHACC5                  -> write_opcode 15
  | PUSHACC6                  -> write_opcode 16
  | PUSHACC7                  -> write_opcode 17
  | PUSHACC n                 -> write_opcode 18; write_word n
  | POP n                     -> write_opcode 19; write_word n
  | ASSIGN n                  -> write_opcode 20; write_word n
  | ENVACC1                   -> write_opcode 21
  | ENVACC2                   -> write_opcode 22
  | ENVACC3                   -> write_opcode 23
  | ENVACC4                   -> write_opcode 24
  | ENVACC n                  -> write_opcode 25; write_word n
  | PUSHENVACC1               -> write_opcode 26
  | PUSHENVACC2               -> write_opcode 27
  | PUSHENVACC3               -> write_opcode 28
  | PUSHENVACC4               -> write_opcode 29
  | PUSHENVACC n              -> write_opcode 30; write_word n
  | PUSH_RETADDR ptr          -> write_opcode 31; write_ptr 1 ptr
  | APPLY n                   -> write_opcode 32; write_word n
  | APPLY1                    -> write_opcode 33
  | APPLY2                    -> write_opcode 34
  | APPLY3                    -> write_opcode 35
  | APPTERM (n, s)            -> write_opcode 36; write_word n; write_word s
  | APPTERM1 s                -> write_opcode 37; write_word s
  | APPTERM2 s                -> write_opcode 38; write_word s
  | APPTERM3 s                -> write_opcode 39; write_word s
  | RETURN n                  -> write_opcode 40; write_word n
  | RESTART                   -> write_opcode 41
  | GRAB n                    -> write_opcode 42; write_word n
  | CLOSURE (n, ptr)          -> write_opcode 43; write_word n; write_ptr 2 ptr
  | CLOSUREREC (f, v, o, t)   -> write_opcode 44; write_word f; write_word v; write_ptr 3 o; write_ptrs 3 t
  | OFFSETCLOSUREM2           -> write_opcode 45
  | OFFSETCLOSURE0            -> write_opcode 46
  | OFFSETCLOSURE2            -> write_opcode 47
  | OFFSETCLOSURE n           -> write_opcode 48; write_word n
  | PUSHOFFSETCLOSUREM2       -> write_opcode 49
  | PUSHOFFSETCLOSURE0        -> write_opcode 50
  | PUSHOFFSETCLOSURE2        -> write_opcode 51
  | PUSHOFFSETCLOSURE n       -> write_opcode 52; write_word n
  | GETGLOBAL n               -> write_opcode 53; write_word n
  | PUSHGETGLOBAL n           -> write_opcode 54; write_word n
  | GETGLOBALFIELD (n, p)     -> write_opcode 55; write_word n; write_word p
  | PUSHGETGLOBALFIELD (n, p) -> write_opcode 56; write_word n; write_word p
  | SETGLOBAL n               -> write_opcode 57; write_word n
  | ATOM0                     -> write_opcode 58
  | ATOM tag                  -> write_opcode 59; write_word tag
  | PUSHATOM0                 -> write_opcode 60
  | PUSHATOM tag              -> write_opcode 61; write_word tag
  | MAKEBLOCK (tag, sz)       -> write_opcode 62; write_word sz; write_word tag
  | MAKEBLOCK1 tag            -> write_opcode 63; write_word tag
  | MAKEBLOCK2 tag            -> write_opcode 64; write_word tag
  | MAKEBLOCK3 tag            -> write_opcode 65; write_word tag
  | MAKEFLOATBLOCK sz         -> write_opcode 66; write_word sz
  | GETFIELD0                 -> write_opcode 67
  | GETFIELD1                 -> write_opcode 68
  | GETFIELD2                 -> write_opcode 69
  | GETFIELD3                 -> write_opcode 70
  | GETFIELD n                -> write_opcode 71; write_word n
  | GETFLOATFIELD n           -> write_opcode 72; write_word n
  | SETFIELD0                 -> write_opcode 73
  | SETFIELD1                 -> write_opcode 74
  | SETFIELD2                 -> write_opcode 75
  | SETFIELD3                 -> write_opcode 76
  | SETFIELD n                -> write_opcode 77; write_word n
  | SETFLOATFIELD n           -> write_opcode 78; write_word n
  | VECTLENGTH                -> write_opcode 79
  | GETVECTITEM               -> write_opcode 80
  | SETVECTITEM               -> write_opcode 81
  | GETSTRINGCHAR             -> write_opcode 82
  | SETSTRINGCHAR             -> write_opcode 83
  | BRANCH ptr                -> write_opcode 84; write_ptr 1 ptr
  | BRANCHIF ptr              -> write_opcode 85; write_ptr 1 ptr
  | BRANCHIFNOT ptr           -> write_opcode 86; write_ptr 1 ptr
  | SWITCH (n, ptrs)          -> write_opcode 87; write_word n; write_ptrs 2 ptrs
  | BOOLNOT                   -> write_opcode 88
  | PUSHTRAP ptr              -> write_opcode 89; write_ptr 1 ptr
  | POPTRAP                   -> write_opcode 90
  | RAISE                     -> write_opcode 91
  | RERAISE                   -> write_opcode 92
  | RAISE_NOTRACE             -> write_opcode 93
  | CHECK_SIGNALS             -> write_opcode 94
  | C_CALL1 idx               -> write_opcode 95; write_word idx
  | C_CALL2 idx               -> write_opcode 96; write_word idx
  | C_CALL3 idx               -> write_opcode 97; write_word idx
  | C_CALL4 idx               -> write_opcode 98; write_word idx
  | C_CALL5 idx               -> write_opcode 99; write_word idx
  | C_CALLN (narg, idx)       -> write_opcode 100; write_word narg; write_word idx
  | CONST0                    -> write_opcode 101
  | CONST1                    -> write_opcode 102
  | CONST2                    -> write_opcode 103
  | CONST3                    -> write_opcode 104
  | CONSTINT n                -> write_opcode 105; write_word n
  | PUSHCONST0                -> write_opcode 106
  | PUSHCONST1                -> write_opcode 107
  | PUSHCONST2                -> write_opcode 108
  | PUSHCONST3                -> write_opcode 109
  | PUSHCONSTINT n            -> write_opcode 110; write_word n
  | NEGINT                    -> write_opcode 111
  | ADDINT                    -> write_opcode 112
  | SUBINT                    -> write_opcode 113
  | MULINT                    -> write_opcode 114
  | DIVINT                    -> write_opcode 115
  | MODINT                    -> write_opcode 116
  | ANDINT                    -> write_opcode 117
  | ORINT                     -> write_opcode 118
  | XORINT                    -> write_opcode 119
  | LSLINT                    -> write_opcode 120
  | LSRINT                    -> write_opcode 121
  | ASRINT                    -> write_opcode 122
  | EQ                        -> write_opcode 123
  | NEQ                       -> write_opcode 124
  | LTINT                     -> write_opcode 125
  | LEINT                     -> write_opcode 126
  | GTINT                     -> write_opcode 127
  | GEINT                     -> write_opcode 128
  | OFFSETINT n               -> write_opcode 129; write_word n
  | OFFSETREF n               -> write_opcode 130; write_word n
  | ISINT                     -> write_opcode 131
  | GETMETHOD                 -> write_opcode 132
  | BEQ (n, ptr)              -> write_opcode 133; write_word n; write_ptr 2 ptr
  | BNEQ (n, ptr)             -> write_opcode 134; write_word n; write_ptr 2 ptr
  | BLTINT (n, ptr)           -> write_opcode 135; write_word n; write_ptr 2 ptr
  | BLEINT (n, ptr)           -> write_opcode 136; write_word n; write_ptr 2 ptr
  | BGTINT (n, ptr)           -> write_opcode 137; write_word n; write_ptr 2 ptr
  | BGEINT (n, ptr)           -> write_opcode 138; write_word n; write_ptr 2 ptr
  | ULTINT                    -> write_opcode 139
  | UGEINT                    -> write_opcode 140
  | BULTINT (n, ptr)          -> write_opcode 141; write_word n; write_ptr 2 ptr
  | BUGEINT (n, ptr)          -> write_opcode 142; write_word n; write_ptr 2 ptr
  | GETPUBMET (tag, cache)    -> write_opcode 143; write_word tag; write_word cache
  | GETDYNMET                 -> write_opcode 144
  | STOP                      -> write_opcode 145
  | EVENT                     -> write_opcode 146
  | BREAK                     -> write_opcode 147
