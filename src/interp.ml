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
open Bytefile

exception Stop
exception Eval_error of string

type env =
  | No_closure
  | Closure of int * Obj.t array
  | Rec_closure of int * int array * Obj.t array * int

let eval_error fmt =
  Printf.ksprintf (fun s -> raise (Eval_error s)) fmt

let ptr_of_env env = match env with
  | No_closure -> eval_error "closure expected"
  | Closure (ptr, _) -> ptr
  | Rec_closure (o, t, _, i) -> if i = 0 then o else t.(i - 1)

let envacc env n =
  match env with
  | No_closure -> eval_error "closure expected"
  | Closure (_, blk) -> blk.(n - 1)
  | Rec_closure (_o, t, blk, i) -> blk.(n - (2 * Array.length t + 1 - 2 * i))

let offsetclosure env n =
  match env with
  | No_closure | Closure _-> eval_error "recursive closure expected"
  | Rec_closure (o, t, blk, i) -> Rec_closure (o, t, blk, i + n/2)

let env_of_closure env =
  match env with
  | No_closure |Rec_closure _-> eval_error "simple closure expected"
  | Closure (_, blk) -> blk

let eval globals code cfuns =
  Instr.(Astack.(Obj.(
  let unit = repr () in
  let stack = Astack.create (1024 * 1024) 1024 unit in
  let getenv_key = repr "UNIQUE KEY!" in
  let stop_pc = Array.length code - 1 in
  if code.(stop_pc) <> STOP then
    fail "invalid bytecode: should terminate by a STOP instruction";
  let rec box_env env =
    repr (
      fun arg ->
        if arg == getenv_key then repr env else
          let accu = ref (box_env env) in
          push stack (repr 0);          (* extra_args *)
          push stack (repr No_closure); (* env        *)
          push stack (repr stop_pc);    (* pc -> STOP *)
          push stack arg;               (* the arg    *)
          run (ptr_of_env env) env accu;
          !accu
    )
  and unbox_env f =
    (obj f : Obj.t -> env) getenv_key
  and run init_pc init_env accu =
    let pc = ref init_pc in
    let env = ref init_env in
    let extra_args = ref 0 in
    let trap_sp = ref (-1) in
    try while true do
      try while true do
        match code.(!pc) with
        | ACC0  -> accu := acc stack 0; incr pc;
        | ACC1  -> accu := acc stack 1; incr pc;
        | ACC2  -> accu := acc stack 2; incr pc;
        | ACC3  -> accu := acc stack 3; incr pc;
        | ACC4  -> accu := acc stack 4; incr pc;
        | ACC5  -> accu := acc stack 5; incr pc;
        | ACC6  -> accu := acc stack 6; incr pc;
        | ACC7  -> accu := acc stack 7; incr pc;
        | ACC n -> accu := acc stack n; incr pc;
        | PUSH | PUSHACC0 -> push stack !accu; incr pc;
        | PUSHACC1  -> push stack !accu; accu := acc stack 1; incr pc;
        | PUSHACC2  -> push stack !accu; accu := acc stack 2; incr pc;
        | PUSHACC3  -> push stack !accu; accu := acc stack 3; incr pc;
        | PUSHACC4  -> push stack !accu; accu := acc stack 4; incr pc;
        | PUSHACC5  -> push stack !accu; accu := acc stack 5; incr pc;
        | PUSHACC6  -> push stack !accu; accu := acc stack 6; incr pc;
        | PUSHACC7  -> push stack !accu; accu := acc stack 7; incr pc;
        | PUSHACC n -> push stack !accu; accu := acc stack n; incr pc;
        | POP n -> popn stack n; incr pc;
        | ASSIGN n -> assign stack n !accu; accu := unit; incr pc;
        | ENVACC1  -> accu := envacc !env 1; incr pc;
        | ENVACC2  -> accu := envacc !env 2; incr pc;
        | ENVACC3  -> accu := envacc !env 3; incr pc;
        | ENVACC4  -> accu := envacc !env 4; incr pc;
        | ENVACC n -> accu := envacc !env n; incr pc;
        | PUSHENVACC1  -> push stack !accu; accu := envacc !env 1; incr pc;
        | PUSHENVACC2  -> push stack !accu; accu := envacc !env 2; incr pc;
        | PUSHENVACC3  -> push stack !accu; accu := envacc !env 3; incr pc;
        | PUSHENVACC4  -> push stack !accu; accu := envacc !env 4; incr pc;
        | PUSHENVACC n -> push stack !accu; accu := envacc !env n; incr pc;
        | PUSH_RETADDR ptr ->
          push stack (repr !extra_args);
          push stack (repr !env);
          push stack (repr ptr);
          incr pc;
        | APPLY n ->
          extra_args := n - 1;
          env := unbox_env !accu;
          pc := ptr_of_env !env;
        | APPLY1 ->
          let arg = pop stack in
          push stack (repr !extra_args);
          push stack (repr !env);
          push stack (repr (!pc + 1));
          push stack arg;
          extra_args := 0;
          env := unbox_env !accu;
          pc := ptr_of_env !env;
        | APPLY2 ->
          let arg1 = pop stack in
          let arg2 = pop stack in
          push stack (repr !extra_args);
          push stack (repr !env);
          push stack (repr (!pc + 1));
          push stack arg2;
          push stack arg1;
          extra_args := 1;
          env := unbox_env !accu;
          pc := ptr_of_env !env;
        | APPLY3 ->
          let arg1 = pop stack in
          let arg2 = pop stack in
          let arg3 = pop stack in
          push stack (repr !extra_args);
          push stack (repr !env);
          push stack (repr (!pc + 1));
          push stack arg3;
          push stack arg2;
          push stack arg1;
          extra_args := 2;
          env := unbox_env !accu;
          pc := ptr_of_env !env;
        | APPTERM (n, s) ->
          for i = 0 to n - 1 do
            assign stack (s - i - 1) (acc stack (n - i - 1));
          done;
          popn stack (s - n);
          extra_args := !extra_args + n - 1;
          env := unbox_env !accu;
          pc := ptr_of_env !env;
        | APPTERM1 s ->
          assign stack (s - 1) (acc stack 0);
          popn stack (s - 1);
          env := unbox_env !accu;
          pc := ptr_of_env !env;
        | APPTERM2 s ->
          assign stack (s - 1) (acc stack 1);
          assign stack (s - 2) (acc stack 0);
          popn stack (s - 2);
          incr extra_args;
          env := unbox_env !accu;
          pc := ptr_of_env !env;
        | APPTERM3 s ->
          assign stack (s - 1) (acc stack 2);
          assign stack (s - 2) (acc stack 1);
          assign stack (s - 3) (acc stack 0);
          popn stack (s - 3);
          extra_args := !extra_args + 2;
          env := unbox_env !accu;
          pc := ptr_of_env !env;
        | RETURN n ->
          popn stack n;
          if !extra_args = 0 then (
            pc         := (obj (pop stack) : int);
            env        := (obj (pop stack) : env);
            extra_args := (obj (pop stack) : int);
          ) else (
            decr extra_args;
            env := unbox_env !accu;
            pc := ptr_of_env !env;
          )
        | RESTART ->
          let blk = env_of_closure !env in
          let n = Array.length blk - 1 in
          for i = n downto 1 do push stack blk.(i) done;
          env := (obj blk.(0) : env);
          extra_args := !extra_args + n;
          incr pc;
        | GRAB n ->
          if !extra_args >= n then (
            extra_args := !extra_args - n;
            incr pc;
          ) else (
            let blk = Array.make (!extra_args + 2) (repr (unbox_env !accu)) in
            for i = 1 to !extra_args + 1 do blk.(i) <- pop stack done;
            accu       := box_env (Closure (!pc - 1, blk));
            pc         := (obj (pop stack) : int);
            env        := (obj (pop stack) : env);
            extra_args := (obj (pop stack) : int);
          )
        | CLOSURE (n, ptr) ->
          let blk = Array.make n !accu in
          for i = 1 to n - 1 do blk.(i) <- pop stack done;
          accu := box_env (Closure (ptr, blk));
          incr pc;
        | CLOSUREREC (_f, v, o, t) ->
          let blk = Array.make v !accu in
          for i = 1 to v - 1 do blk.(i) <- pop stack done;
          accu := box_env (Rec_closure (o, t, blk, 0));
          push stack !accu;
          for i = 1 to Array.length t do
            push stack (box_env (Rec_closure (o, t, blk, i)));
          done;
          incr pc;
        | OFFSETCLOSUREM2 -> accu := box_env (offsetclosure !env (-2)); incr pc;
        | OFFSETCLOSURE0  -> accu := box_env (offsetclosure !env 0); incr pc;
        | OFFSETCLOSURE2  -> accu := box_env (offsetclosure !env 2); incr pc;
        | OFFSETCLOSURE n -> accu := box_env (offsetclosure !env n); incr pc;
        | PUSHOFFSETCLOSUREM2 ->
          push stack !accu;
          accu := box_env (offsetclosure !env (-2));
          incr pc;
        | PUSHOFFSETCLOSURE0 ->
          push stack !accu;
          accu := box_env (offsetclosure !env 0);
          incr pc;
        | PUSHOFFSETCLOSURE2 ->
          push stack !accu;
          accu := box_env (offsetclosure !env 2);
          incr pc;
        | PUSHOFFSETCLOSURE n ->
          push stack !accu;
          accu := box_env (offsetclosure !env n);
          incr pc;
        | GETGLOBAL n -> accu := globals.(n); incr pc;
        | PUSHGETGLOBAL n -> push stack !accu; accu := globals.(n); incr pc;
        | GETGLOBALFIELD (n, p) -> accu := field globals.(n) p; incr pc;
        | PUSHGETGLOBALFIELD (n, p) ->
          push stack !accu;
          accu := field globals.(n) p;
          incr pc;
        | SETGLOBAL n -> globals.(n) <- !accu; accu := unit; incr pc;
        | ATOM0        -> accu := new_block 0 0; incr pc;
        | ATOM tag     -> accu := new_block tag 0; incr pc;
        | PUSHATOM0    -> push stack !accu; accu := new_block 0 0; incr pc;
        | PUSHATOM tag -> push stack !accu; accu := new_block tag 0; incr pc;
        | MAKEBLOCK (tag, sz) ->
          let blk = new_block tag sz in
          set_field blk 0 !accu;
          for i = 1 to sz - 1 do set_field blk i (pop stack) done;
          accu := blk;
          incr pc;
        | MAKEBLOCK1 tag ->
          let blk = new_block tag 1 in
          set_field blk 0 !accu;
          accu := blk;
          incr pc;
        | MAKEBLOCK2 tag ->
          let blk = new_block tag 2 in
          set_field blk 0 !accu;
          set_field blk 1 (pop stack);
          accu := blk;
          incr pc;
        | MAKEBLOCK3 tag ->
          let blk = new_block tag 3 in
          set_field blk 0 !accu;
          set_field blk 1 (pop stack);
          set_field blk 2 (pop stack);
          accu := blk;
          incr pc;
        | MAKEFLOATBLOCK sz ->
          let blk = new_block double_array_tag sz in
          set_double_field blk 0 (obj !accu : float);
          for i = 1 to sz - 1 do
            set_double_field blk i (obj (pop stack) : float);
          done;
          accu := blk;
          incr pc;
        | GETFIELD0  -> accu := field !accu 0; incr pc;
        | GETFIELD1  -> accu := field !accu 1; incr pc;
        | GETFIELD2  -> accu := field !accu 2; incr pc;
        | GETFIELD3  -> accu := field !accu 3; incr pc;
        | GETFIELD n -> accu := field !accu n; incr pc;
        | GETFLOATFIELD ind -> accu := repr (double_field !accu ind); incr pc;
        | SETFIELD0  -> set_field !accu 0 (pop stack); accu := unit; incr pc;
        | SETFIELD1  -> set_field !accu 1 (pop stack); accu := unit; incr pc;
        | SETFIELD2  -> set_field !accu 2 (pop stack); accu := unit; incr pc;
        | SETFIELD3  -> set_field !accu 3 (pop stack); accu := unit; incr pc;
        | SETFIELD n -> set_field !accu n (pop stack); accu := unit; incr pc;
        | SETFLOATFIELD ind ->
          set_double_field !accu ind (obj (pop stack) : float);
          accu := unit;
          incr pc;
        | VECTLENGTH ->
          accu := repr (size !accu);
          incr pc;
        | GETVECTITEM ->
          accu := field !accu (obj (pop stack) : int);
          incr pc;
        | SETVECTITEM ->
          let i = pop stack in
          let v = pop stack in
          set_field !accu (obj i : int) v;
          accu := unit;
          incr pc;
        | GETSTRINGCHAR ->
          accu := repr (obj !accu : string).[(obj (pop stack) : int)];
          incr pc;
        | SETSTRINGCHAR ->
          let i = pop stack in
          let c = pop stack in
          (obj !accu : string).[(obj i : int)] <- (obj c : char);
          accu := unit;
          incr pc;
        | BRANCH ptr ->
          pc := ptr;
        | BRANCHIF ptr ->
          if !accu != repr false then pc := ptr else incr pc;
        | BRANCHIFNOT ptr ->
          if !accu == repr false then pc := ptr else incr pc;
        | SWITCH (n, ptrs) ->
          if is_int !accu then pc := ptrs.((obj !accu : int))
          else pc := ptrs.(tag !accu + (n land 0xFFFF))
        | BOOLNOT ->
          accu := repr (not (obj !accu : bool));
          incr pc;
        | PUSHTRAP ptr ->
          push stack (repr !extra_args);
          push stack (repr !env);
          push stack (repr !trap_sp);
          push stack (repr ptr);
          trap_sp := Astack.size stack;
          incr pc;
        | POPTRAP ->
          trap_sp := (obj (acc stack 1) : int);
          popn stack 4;
          incr pc;
        | RAISE | RERAISE | RAISE_NOTRACE ->
          raise (obj !accu : exn);
        | CHECK_SIGNALS ->
          incr pc;
        | C_CALL1 idx ->
          accu := (obj cfuns.(idx) : t -> t) !accu;
          incr pc;
        | C_CALL2 idx ->
          accu := (obj cfuns.(idx) : t -> t -> t) !accu (pop stack);
          incr pc;
        | C_CALL3 idx ->
          accu := (obj cfuns.(idx) : t -> t -> t -> t)
            !accu (acc stack 0) (acc stack 1);
          popn stack 2;
          incr pc;
        | C_CALL4 idx ->
          accu := (obj cfuns.(idx) : t -> t -> t -> t -> t)
            !accu (acc stack 0) (acc stack 1) (acc stack 2);
          popn stack 3;
          incr pc;
        | C_CALL5 idx ->
          accu := (obj cfuns.(idx) : t -> t -> t -> t -> t -> t)
            !accu (acc stack 0) (acc stack 1) (acc stack 2) (acc stack 3);
          popn stack 4;
          incr pc;
        | C_CALLN (narg, idx) ->
          accu := Prim.apply narg cfuns.(idx) !accu stack;
          popn stack (narg - 1);
          incr pc;
        | CONST0     -> accu := repr 0; incr pc;
        | CONST1     -> accu := repr 1; incr pc;
        | CONST2     -> accu := repr 2; incr pc;
        | CONST3     -> accu := repr 3; incr pc;
        | CONSTINT n -> accu := repr n; incr pc;
        | PUSHCONST0     -> push stack !accu; accu := repr 0; incr pc;
        | PUSHCONST1     -> push stack !accu; accu := repr 1; incr pc;
        | PUSHCONST2     -> push stack !accu; accu := repr 2; incr pc;
        | PUSHCONST3     -> push stack !accu; accu := repr 3; incr pc;
        | PUSHCONSTINT n -> push stack !accu; accu := repr n; incr pc;
        | NEGINT ->
          accu := repr (-(obj !accu : int));
          incr pc;
        | ADDINT ->
          accu := repr ((obj !accu : int) + (obj (pop stack) : int));
          incr pc;
        | SUBINT ->
          accu := repr ((obj !accu : int) - (obj (pop stack) : int));
          incr pc;
        | MULINT ->
          accu := repr ((obj !accu : int) * (obj (pop stack) : int));
          incr pc;
        | DIVINT ->
          accu := repr ((obj !accu : int) / (obj (pop stack) : int));
          incr pc;
        | MODINT ->
          accu := repr ((obj !accu : int) mod (obj (pop stack) : int));
          incr pc;
        | ANDINT ->
          accu := repr ((obj !accu : int) land (obj (pop stack) : int));
          incr pc;
        | ORINT ->
          accu := repr ((obj !accu : int) lor (obj (pop stack) : int));
          incr pc;
        | XORINT ->
          accu := repr ((obj !accu : int) lxor (obj (pop stack) : int));
          incr pc;
        | LSLINT ->
          accu := repr ((obj !accu : int) lsl (obj (pop stack) : int));
          incr pc;
        | LSRINT ->
          accu := repr ((obj !accu : int) lsr (obj (pop stack) : int));
          incr pc;
        | ASRINT ->
          accu := repr ((obj !accu : int) asr (obj (pop stack) : int));
          incr pc;
        | EQ ->
          accu := repr (!accu == pop stack);
          incr pc;
        | NEQ ->
          accu := repr (!accu != pop stack);
          incr pc;
        | LTINT ->
          accu := repr ((obj !accu : int) < (obj (pop stack) : int));
          incr pc;
        | LEINT ->
          accu := repr ((obj !accu : int) <= (obj (pop stack) : int));
          incr pc;
        | GTINT ->
          accu := repr ((obj !accu : int) > (obj (pop stack) : int));
          incr pc;
        | GEINT ->
          accu := repr ((obj !accu : int) >= (obj (pop stack) : int));
          incr pc;
        | OFFSETINT ofs ->
          accu := repr ((obj !accu : int) + ofs);
          incr pc;
        | OFFSETREF ofs ->
          let r = (obj !accu : int ref) in
          r := !r + ofs;
          accu := unit;
          incr pc;
        | ISINT ->
          accu := repr (is_int !accu);
          incr pc;
        | GETMETHOD ->
          accu := field (field (acc stack 0) 0) (obj !accu : int);
          incr pc;
        | BEQ (n, ptr) ->
          if n == (obj !accu : int) then pc := ptr else incr pc;
        | BNEQ (n, ptr) ->
          if n != (obj !accu : int) then pc := ptr else incr pc;
        | BLTINT (n, ptr) ->
          if n < (obj !accu : int) then pc := ptr else incr pc;
        | BLEINT (n, ptr) ->
          if n <= (obj !accu : int) then pc := ptr else incr pc;
        | BGTINT (n, ptr) ->
          if n > (obj !accu : int) then pc := ptr else incr pc;
        | BGEINT (n, ptr) ->
          if n >= (obj !accu : int) then pc := ptr else incr pc;
        | ULTINT ->
          let n = (obj !accu : int) + min_int
          and p = (obj (pop stack) : int) + min_int in
          accu := repr (n < p);
          incr pc;
        | UGEINT ->
          let n = (obj !accu : int) + min_int
          and p = (obj (pop stack) : int) + min_int in
          accu := repr (n >= p);
          incr pc;
        | BULTINT (n, ptr) ->
          if n + min_int < (obj !accu : int) + min_int
          then pc := ptr else incr pc;
        | BUGEINT (n, ptr) ->
          if n + min_int >= (obj !accu : int) + min_int
          then pc := ptr else incr pc;
        | GETPUBMET (tag, _cache) ->
          push stack !accu;
          accu := find_object_method !accu tag;
          incr pc;
        | GETDYNMET ->
          accu := find_object_method (acc stack 0) (obj !accu : int);
          incr pc;
        | STOP  -> raise Stop
        | EVENT -> eval_error "unexpected instruction EVENT"
        | BREAK -> eval_error "unexpected instruction BREAK"
      done with
      | (Stop | Eval_error _) as exn -> raise exn
      | exn  ->
        if !trap_sp = -1 then raise exn;
        let ofs = Astack.size stack - !trap_sp in
        popn stack ofs;
        pc         := (obj (pop stack) : int);
        trap_sp    := (obj (pop stack) : int);
        env        := (obj (pop stack) : env);
        extra_args := (obj (pop stack) : int);
        accu       := (repr exn);
    done with
      | Stop -> ()
      | Eval_error msg -> fail "%s" msg in
  run 0 No_closure (ref unit))))

let make_cfun_map code prim =
  let prim_nb = Array.length prim in
  let used = Array.make prim_nb false in
  let cfuns = Array.make prim_nb (Obj.repr ()) in
  let manage_c_call instr = match instr with
    | Instr.C_CALL1 idx | Instr.C_CALL2 idx | Instr.C_CALL3 idx | Instr.C_CALL4 idx
    | Instr.C_CALL5 idx | Instr.C_CALLN (_, idx) -> used.(idx) <- true
    | _ -> () in
  Array.iter manage_c_call code;
  for i = 0 to prim_nb - 1 do
    if used.(i) then cfuns.(i) <- Prim.find_prim prim.(i);
  done;
  cfuns

let eval_bytefile { data; prim; code; _ } =
  let globals = Data.to_objs data in
  let cfuns = make_cfun_map code prim in
  Data.fix_std_exceptions globals;
  eval globals code cfuns
