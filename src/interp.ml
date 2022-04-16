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
  | No_closure | Closure _ -> eval_error "recursive closure expected"
  | Rec_closure (o, t, blk, i) -> Rec_closure (o, t, blk, i + n/2)

let env_of_closure env =
  match env with
  | No_closure | Rec_closure _ -> eval_error "simple closure expected"
  | Closure (_, blk) -> blk

let eval globals code cfuns =
  let unit = Obj.repr () in
  let stack = Astack.create (1024 * 1024) 1024 unit in
  let getenv_key = Obj.repr (ref "UNIQUE KEY!") in
  let stop_pc = Array.length code - 1 in
  if code.(stop_pc) <> Instr.STOP then
    fail "invalid bytecode: should terminate by a STOP instruction";
  let rec box_env env =
    Obj.repr (
      fun arg ->
        if arg == getenv_key then Obj.repr env else
          let accu = ref (box_env env) in
          Astack.push stack (Obj.repr 0);          (* extra_args *)
          Astack.push stack (Obj.repr No_closure); (* env        *)
          Astack.push stack (Obj.repr stop_pc);    (* pc -> STOP *)
          Astack.push stack arg;                   (* the arg    *)
          run (ptr_of_env env) env accu;
          !accu
    )
  and unbox_env f =
    (Obj.obj f : Obj.t -> env) getenv_key
  and run init_pc init_env accu =
    let pc = ref init_pc in
    let env = ref init_env in
    let extra_args = ref 0 in
    let trap_sp = ref (-1) in
    try while true do
      try while true do
        match code.(!pc) with
        | Instr.ACC0  -> accu := Astack.acc stack 0; incr pc;
        | Instr.ACC1  -> accu := Astack.acc stack 1; incr pc;
        | Instr.ACC2  -> accu := Astack.acc stack 2; incr pc;
        | Instr.ACC3  -> accu := Astack.acc stack 3; incr pc;
        | Instr.ACC4  -> accu := Astack.acc stack 4; incr pc;
        | Instr.ACC5  -> accu := Astack.acc stack 5; incr pc;
        | Instr.ACC6  -> accu := Astack.acc stack 6; incr pc;
        | Instr.ACC7  -> accu := Astack.acc stack 7; incr pc;
        | Instr.ACC n -> accu := Astack.acc stack n; incr pc;
        | Instr.PUSH | Instr.PUSHACC0 -> Astack.push stack !accu; incr pc;
        | Instr.PUSHACC1  -> Astack.push stack !accu; accu := Astack.acc stack 1; incr pc;
        | Instr.PUSHACC2  -> Astack.push stack !accu; accu := Astack.acc stack 2; incr pc;
        | Instr.PUSHACC3  -> Astack.push stack !accu; accu := Astack.acc stack 3; incr pc;
        | Instr.PUSHACC4  -> Astack.push stack !accu; accu := Astack.acc stack 4; incr pc;
        | Instr.PUSHACC5  -> Astack.push stack !accu; accu := Astack.acc stack 5; incr pc;
        | Instr.PUSHACC6  -> Astack.push stack !accu; accu := Astack.acc stack 6; incr pc;
        | Instr.PUSHACC7  -> Astack.push stack !accu; accu := Astack.acc stack 7; incr pc;
        | Instr.PUSHACC n -> Astack.push stack !accu; accu := Astack.acc stack n; incr pc;
        | Instr.POP n -> Astack.popn stack n; incr pc;
        | Instr.ASSIGN n -> Astack.assign stack n !accu; accu := unit; incr pc;
        | Instr.ENVACC1  -> accu := envacc !env 1; incr pc;
        | Instr.ENVACC2  -> accu := envacc !env 2; incr pc;
        | Instr.ENVACC3  -> accu := envacc !env 3; incr pc;
        | Instr.ENVACC4  -> accu := envacc !env 4; incr pc;
        | Instr.ENVACC n -> accu := envacc !env n; incr pc;
        | Instr.PUSHENVACC1  -> Astack.push stack !accu; accu := envacc !env 1; incr pc;
        | Instr.PUSHENVACC2  -> Astack.push stack !accu; accu := envacc !env 2; incr pc;
        | Instr.PUSHENVACC3  -> Astack.push stack !accu; accu := envacc !env 3; incr pc;
        | Instr.PUSHENVACC4  -> Astack.push stack !accu; accu := envacc !env 4; incr pc;
        | Instr.PUSHENVACC n -> Astack.push stack !accu; accu := envacc !env n; incr pc;
        | Instr.PUSH_RETADDR ptr ->
          Astack.push stack (Obj.repr !extra_args);
          Astack.push stack (Obj.repr !env);
          Astack.push stack (Obj.repr ptr);
          incr pc;
        | Instr.APPLY n ->
          extra_args := n - 1;
          env := unbox_env !accu;
          pc := ptr_of_env !env;
        | Instr.APPLY1 ->
          let arg = Astack.pop stack in
          Astack.push stack (Obj.repr !extra_args);
          Astack.push stack (Obj.repr !env);
          Astack.push stack (Obj.repr (!pc + 1));
          Astack.push stack arg;
          extra_args := 0;
          env := unbox_env !accu;
          pc := ptr_of_env !env;
        | Instr.APPLY2 ->
          let arg1 = Astack.pop stack in
          let arg2 = Astack.pop stack in
          Astack.push stack (Obj.repr !extra_args);
          Astack.push stack (Obj.repr !env);
          Astack.push stack (Obj.repr (!pc + 1));
          Astack.push stack arg2;
          Astack.push stack arg1;
          extra_args := 1;
          env := unbox_env !accu;
          pc := ptr_of_env !env;
        | Instr.APPLY3 ->
          let arg1 = Astack.pop stack in
          let arg2 = Astack.pop stack in
          let arg3 = Astack.pop stack in
          Astack.push stack (Obj.repr !extra_args);
          Astack.push stack (Obj.repr !env);
          Astack.push stack (Obj.repr (!pc + 1));
          Astack.push stack arg3;
          Astack.push stack arg2;
          Astack.push stack arg1;
          extra_args := 2;
          env := unbox_env !accu;
          pc := ptr_of_env !env;
        | Instr.APPTERM (n, s) ->
          for i = 0 to n - 1 do
            Astack.assign stack (s - i - 1) (Astack.acc stack (n - i - 1));
          done;
          Astack.popn stack (s - n);
          extra_args := !extra_args + n - 1;
          env := unbox_env !accu;
          pc := ptr_of_env !env;
        | Instr.APPTERM1 s ->
          Astack.assign stack (s - 1) (Astack.acc stack 0);
          Astack.popn stack (s - 1);
          env := unbox_env !accu;
          pc := ptr_of_env !env;
        | Instr.APPTERM2 s ->
          Astack.assign stack (s - 1) (Astack.acc stack 1);
          Astack.assign stack (s - 2) (Astack.acc stack 0);
          Astack.popn stack (s - 2);
          incr extra_args;
          env := unbox_env !accu;
          pc := ptr_of_env !env;
        | Instr.APPTERM3 s ->
          Astack.assign stack (s - 1) (Astack.acc stack 2);
          Astack.assign stack (s - 2) (Astack.acc stack 1);
          Astack.assign stack (s - 3) (Astack.acc stack 0);
          Astack.popn stack (s - 3);
          extra_args := !extra_args + 2;
          env := unbox_env !accu;
          pc := ptr_of_env !env;
        | Instr.RETURN n ->
          Astack.popn stack n;
          if !extra_args = 0 then (
            pc         := (Obj.obj (Astack.pop stack) : int);
            env        := (Obj.obj (Astack.pop stack) : env);
            extra_args := (Obj.obj (Astack.pop stack) : int);
          ) else (
            decr extra_args;
            env := unbox_env !accu;
            pc := ptr_of_env !env;
          )
        | Instr.RESTART ->
          let blk = env_of_closure !env in
          let n = Array.length blk - 1 in
          for i = n downto 1 do Astack.push stack blk.(i) done;
          env := (Obj.obj blk.(0) : env);
          extra_args := !extra_args + n;
          incr pc;
        | Instr.GRAB n ->
          if !extra_args >= n then (
            extra_args := !extra_args - n;
            incr pc;
          ) else (
            let blk = Array.make (!extra_args + 2) (Obj.repr ()) in
            blk.(0) <- Obj.repr !env;
            for i = 1 to !extra_args + 1 do blk.(i) <- Astack.pop stack done;
            accu       := box_env (Closure (!pc - 1, blk));
            pc         := (Obj.obj (Astack.pop stack) : int);
            env        := (Obj.obj (Astack.pop stack) : env);
            extra_args := (Obj.obj (Astack.pop stack) : int);
          )
        | Instr.CLOSURE (n, ptr) ->
          let blk = Array.make n (Obj.repr ()) in
          if n > 0 then blk.(0) <- !accu;
          for i = 1 to n - 1 do blk.(i) <- Astack.pop stack done;
          accu := box_env (Closure (ptr, blk));
          incr pc;
        | Instr.CLOSUREREC (_f, v, o, t) ->
          let blk = Array.make v (Obj.repr ()) in
          if v > 0 then blk.(0) <- !accu;
          for i = 1 to v - 1 do blk.(i) <- Astack.pop stack done;
          accu := box_env (Rec_closure (o, t, blk, 0));
          Astack.push stack !accu;
          for i = 1 to Array.length t do
            Astack.push stack (box_env (Rec_closure (o, t, blk, i)));
          done;
          incr pc;
        | Instr.OFFSETCLOSUREM2 -> accu := box_env (offsetclosure !env (-2)); incr pc;
        | Instr.OFFSETCLOSURE0  -> accu := box_env (offsetclosure !env 0); incr pc;
        | Instr.OFFSETCLOSURE2  -> accu := box_env (offsetclosure !env 2); incr pc;
        | Instr.OFFSETCLOSURE n -> accu := box_env (offsetclosure !env n); incr pc;
        | Instr.PUSHOFFSETCLOSUREM2 ->
          Astack.push stack !accu;
          accu := box_env (offsetclosure !env (-2));
          incr pc;
        | Instr.PUSHOFFSETCLOSURE0 ->
          Astack.push stack !accu;
          accu := box_env (offsetclosure !env 0);
          incr pc;
        | Instr.PUSHOFFSETCLOSURE2 ->
          Astack.push stack !accu;
          accu := box_env (offsetclosure !env 2);
          incr pc;
        | Instr.PUSHOFFSETCLOSURE n ->
          Astack.push stack !accu;
          accu := box_env (offsetclosure !env n);
          incr pc;
        | Instr.GETGLOBAL n -> accu := globals.(n); incr pc;
        | Instr.PUSHGETGLOBAL n -> Astack.push stack !accu; accu := globals.(n); incr pc;
        | Instr.GETGLOBALFIELD (n, p) -> accu := Obj.field globals.(n) p; incr pc;
        | Instr.PUSHGETGLOBALFIELD (n, p) ->
          Astack.push stack !accu;
          accu := Obj.field globals.(n) p;
          incr pc;
        | Instr.SETGLOBAL n -> globals.(n) <- !accu; accu := unit; incr pc;
        | Instr.ATOM0        -> accu := Obj.new_block 0 0; incr pc;
        | Instr.ATOM tag     -> accu := Obj.new_block tag 0; incr pc;
        | Instr.PUSHATOM0    -> Astack.push stack !accu; accu := Obj.new_block 0 0; incr pc;
        | Instr.PUSHATOM tag -> Astack.push stack !accu; accu := Obj.new_block tag 0; incr pc;
        | Instr.MAKEBLOCK (tag, sz) ->
          let blk = Obj.new_block tag sz in
          Obj.set_field blk 0 !accu;
          for i = 1 to sz - 1 do Obj.set_field blk i (Astack.pop stack) done;
          accu := blk;
          incr pc;
        | Instr.MAKEBLOCK1 tag ->
          let blk = Obj.new_block tag 1 in
          Obj.set_field blk 0 !accu;
          accu := blk;
          incr pc;
        | Instr.MAKEBLOCK2 tag ->
          let blk = Obj.new_block tag 2 in
          Obj.set_field blk 0 !accu;
          Obj.set_field blk 1 (Astack.pop stack);
          accu := blk;
          incr pc;
        | Instr.MAKEBLOCK3 tag ->
          let blk = Obj.new_block tag 3 in
          Obj.set_field blk 0 !accu;
          Obj.set_field blk 1 (Astack.pop stack);
          Obj.set_field blk 2 (Astack.pop stack);
          accu := blk;
          incr pc;
        | Instr.MAKEFLOATBLOCK sz ->
          let blk = Obj.new_block Obj.double_array_tag sz in
          Obj.set_double_field blk 0 (Obj.obj !accu : float);
          for i = 1 to sz - 1 do
            Obj.set_double_field blk i (Obj.obj (Astack.pop stack) : float);
          done;
          accu := blk;
          incr pc;
        | Instr.GETFIELD0  -> accu := Obj.field !accu 0; incr pc;
        | Instr.GETFIELD1  -> accu := Obj.field !accu 1; incr pc;
        | Instr.GETFIELD2  -> accu := Obj.field !accu 2; incr pc;
        | Instr.GETFIELD3  -> accu := Obj.field !accu 3; incr pc;
        | Instr.GETFIELD n -> accu := Obj.field !accu n; incr pc;
        | Instr.GETFLOATFIELD ind -> accu := Obj.repr (Obj.double_field !accu ind); incr pc;
        | Instr.SETFIELD0  -> Obj.set_field !accu 0 (Astack.pop stack); accu := unit; incr pc;
        | Instr.SETFIELD1  -> Obj.set_field !accu 1 (Astack.pop stack); accu := unit; incr pc;
        | Instr.SETFIELD2  -> Obj.set_field !accu 2 (Astack.pop stack); accu := unit; incr pc;
        | Instr.SETFIELD3  -> Obj.set_field !accu 3 (Astack.pop stack); accu := unit; incr pc;
        | Instr.SETFIELD n -> Obj.set_field !accu n (Astack.pop stack); accu := unit; incr pc;
        | Instr.SETFLOATFIELD ind ->
          Obj.set_double_field !accu ind (Obj.obj (Astack.pop stack) : float);
          accu := unit;
          incr pc;
        | Instr.VECTLENGTH ->
          accu := Obj.repr (Obj.size !accu);
          incr pc;
        | Instr.GETVECTITEM ->
          accu := Obj.field !accu (Obj.obj (Astack.pop stack) : int);
          incr pc;
        | Instr.SETVECTITEM ->
          let i = Astack.pop stack in
          let v = Astack.pop stack in
          Obj.set_field !accu (Obj.obj i : int) v;
          accu := unit;
          incr pc;
        | Instr.GETBYTESCHAR | Instr.GETSTRINGCHAR ->
          accu := Obj.repr (Bytes.get (Obj.obj !accu : bytes) (Obj.obj (Astack.pop stack) : int));
          incr pc;
        | Instr.SETBYTESCHAR ->
          let i = Astack.pop stack in
          let c = Astack.pop stack in
          Bytes.set (Obj.obj !accu : bytes) (Obj.obj i : int) (Obj.obj c : char);
          accu := unit;
          incr pc;
        | Instr.BRANCH ptr ->
          pc := ptr;
        | Instr.BRANCHIF ptr ->
          if !accu != Obj.repr false then pc := ptr else incr pc;
        | Instr.BRANCHIFNOT ptr ->
          if !accu == Obj.repr false then pc := ptr else incr pc;
        | Instr.SWITCH (n, ptrs) ->
          if Obj.is_int !accu then pc := ptrs.((Obj.obj !accu : int))
          else pc := ptrs.(Obj.tag !accu + (n land 0xFFFF))
        | Instr.BOOLNOT ->
          accu := Obj.repr (not (Obj.obj !accu : bool));
          incr pc;
        | Instr.PUSHTRAP ptr ->
          Astack.push stack (Obj.repr !extra_args);
          Astack.push stack (Obj.repr !env);
          Astack.push stack (Obj.repr !trap_sp);
          Astack.push stack (Obj.repr ptr);
          trap_sp := Astack.size stack;
          incr pc;
        | Instr.POPTRAP ->
          trap_sp := (Obj.obj (Astack.acc stack 1) : int);
          Astack.popn stack 4;
          incr pc;
        | Instr.RAISE | Instr.RERAISE | Instr.RAISE_NOTRACE ->
          raise (Obj.obj !accu : exn);
        | Instr.CHECK_SIGNALS ->
          incr pc;
        | Instr.C_CALL1 idx ->
          accu := (Obj.obj cfuns.(idx) : Obj.t -> Obj.t) !accu;
          incr pc;
        | Instr.C_CALL2 idx ->
          accu := (Obj.obj cfuns.(idx) : Obj.t -> Obj.t -> Obj.t) !accu (Astack.pop stack);
          incr pc;
        | Instr.C_CALL3 idx ->
          accu := (Obj.obj cfuns.(idx) : Obj.t -> Obj.t -> Obj.t -> Obj.t)
            !accu (Astack.acc stack 0) (Astack.acc stack 1);
          Astack.popn stack 2;
          incr pc;
        | Instr.C_CALL4 idx ->
          accu := (Obj.obj cfuns.(idx) : Obj.t -> Obj.t -> Obj.t -> Obj.t -> Obj.t)
            !accu (Astack.acc stack 0) (Astack.acc stack 1) (Astack.acc stack 2);
          Astack.popn stack 3;
          incr pc;
        | Instr.C_CALL5 idx ->
          accu := (Obj.obj cfuns.(idx) : Obj.t -> Obj.t -> Obj.t -> Obj.t -> Obj.t -> Obj.t)
            !accu (Astack.acc stack 0) (Astack.acc stack 1) (Astack.acc stack 2) (Astack.acc stack 3);
          Astack.popn stack 4;
          incr pc;
        | Instr.C_CALLN (narg, idx) ->
          accu := Prim.apply narg cfuns.(idx) !accu stack;
          Astack.popn stack (narg - 1);
          incr pc;
        | Instr.CONST0     -> accu := Obj.repr 0; incr pc;
        | Instr.CONST1     -> accu := Obj.repr 1; incr pc;
        | Instr.CONST2     -> accu := Obj.repr 2; incr pc;
        | Instr.CONST3     -> accu := Obj.repr 3; incr pc;
        | Instr.CONSTINT n -> accu := Obj.repr n; incr pc;
        | Instr.PUSHCONST0     -> Astack.push stack !accu; accu := Obj.repr 0; incr pc;
        | Instr.PUSHCONST1     -> Astack.push stack !accu; accu := Obj.repr 1; incr pc;
        | Instr.PUSHCONST2     -> Astack.push stack !accu; accu := Obj.repr 2; incr pc;
        | Instr.PUSHCONST3     -> Astack.push stack !accu; accu := Obj.repr 3; incr pc;
        | Instr.PUSHCONSTINT n -> Astack.push stack !accu; accu := Obj.repr n; incr pc;
        | Instr.NEGINT ->
          accu := Obj.repr (-(Obj.obj !accu : int));
          incr pc;
        | Instr.ADDINT ->
          accu := Obj.repr ((Obj.obj !accu : int) + (Obj.obj (Astack.pop stack) : int));
          incr pc;
        | Instr.SUBINT ->
          accu := Obj.repr ((Obj.obj !accu : int) - (Obj.obj (Astack.pop stack) : int));
          incr pc;
        | Instr.MULINT ->
          accu := Obj.repr ((Obj.obj !accu : int) * (Obj.obj (Astack.pop stack) : int));
          incr pc;
        | Instr.DIVINT ->
          accu := Obj.repr ((Obj.obj !accu : int) / (Obj.obj (Astack.pop stack) : int));
          incr pc;
        | Instr.MODINT ->
          accu := Obj.repr ((Obj.obj !accu : int) mod (Obj.obj (Astack.pop stack) : int));
          incr pc;
        | Instr.ANDINT ->
          accu := Obj.repr ((Obj.obj !accu : int) land (Obj.obj (Astack.pop stack) : int));
          incr pc;
        | Instr.ORINT ->
          accu := Obj.repr ((Obj.obj !accu : int) lor (Obj.obj (Astack.pop stack) : int));
          incr pc;
        | Instr.XORINT ->
          accu := Obj.repr ((Obj.obj !accu : int) lxor (Obj.obj (Astack.pop stack) : int));
          incr pc;
        | Instr.LSLINT ->
          accu := Obj.repr ((Obj.obj !accu : int) lsl (Obj.obj (Astack.pop stack) : int));
          incr pc;
        | Instr.LSRINT ->
          accu := Obj.repr ((Obj.obj !accu : int) lsr (Obj.obj (Astack.pop stack) : int));
          incr pc;
        | Instr.ASRINT ->
          accu := Obj.repr ((Obj.obj !accu : int) asr (Obj.obj (Astack.pop stack) : int));
          incr pc;
        | Instr.EQ ->
          accu := Obj.repr (!accu == Astack.pop stack);
          incr pc;
        | Instr.NEQ ->
          accu := Obj.repr (!accu != Astack.pop stack);
          incr pc;
        | Instr.LTINT ->
          accu := Obj.repr ((Obj.obj !accu : int) < (Obj.obj (Astack.pop stack) : int));
          incr pc;
        | Instr.LEINT ->
          accu := Obj.repr ((Obj.obj !accu : int) <= (Obj.obj (Astack.pop stack) : int));
          incr pc;
        | Instr.GTINT ->
          accu := Obj.repr ((Obj.obj !accu : int) > (Obj.obj (Astack.pop stack) : int));
          incr pc;
        | Instr.GEINT ->
          accu := Obj.repr ((Obj.obj !accu : int) >= (Obj.obj (Astack.pop stack) : int));
          incr pc;
        | Instr.OFFSETINT ofs ->
          accu := Obj.repr ((Obj.obj !accu : int) + ofs);
          incr pc;
        | Instr.OFFSETREF ofs ->
          let r = (Obj.obj !accu : int ref) in
          r := !r + ofs;
          accu := unit;
          incr pc;
        | Instr.ISINT ->
          accu := Obj.repr (Obj.is_int !accu);
          incr pc;
        | Instr.GETMETHOD ->
          accu := Obj.field (Obj.field (Astack.acc stack 0) 0) (Obj.obj !accu : int);
          incr pc;
        | Instr.BEQ (n, ptr) ->
          if n == (Obj.obj !accu : int) then pc := ptr else incr pc;
        | Instr.BNEQ (n, ptr) ->
          if n != (Obj.obj !accu : int) then pc := ptr else incr pc;
        | Instr.BLTINT (n, ptr) ->
          if n < (Obj.obj !accu : int) then pc := ptr else incr pc;
        | Instr.BLEINT (n, ptr) ->
          if n <= (Obj.obj !accu : int) then pc := ptr else incr pc;
        | Instr.BGTINT (n, ptr) ->
          if n > (Obj.obj !accu : int) then pc := ptr else incr pc;
        | Instr.BGEINT (n, ptr) ->
          if n >= (Obj.obj !accu : int) then pc := ptr else incr pc;
        | Instr.ULTINT ->
          let n = (Obj.obj !accu : int) + min_int
          and p = (Obj.obj (Astack.pop stack) : int) + min_int in
          accu := Obj.repr (n < p);
          incr pc;
        | Instr.UGEINT ->
          let n = (Obj.obj !accu : int) + min_int
          and p = (Obj.obj (Astack.pop stack) : int) + min_int in
          accu := Obj.repr (n >= p);
          incr pc;
        | Instr.BULTINT (n, ptr) ->
          if n + min_int < (Obj.obj !accu : int) + min_int
          then pc := ptr else incr pc;
        | Instr.BUGEINT (n, ptr) ->
          if n + min_int >= (Obj.obj !accu : int) + min_int
          then pc := ptr else incr pc;
        | Instr.GETPUBMET (tag, _cache) ->
          Astack.push stack !accu;
          accu := find_object_method !accu tag;
          incr pc;
        | Instr.GETDYNMET ->
          accu := find_object_method (Astack.acc stack 0) (Obj.obj !accu : int);
          incr pc;
        | Instr.STOP  -> raise Stop
        | Instr.EVENT -> eval_error "unexpected instruction EVENT"
        | Instr.BREAK -> eval_error "unexpected instruction BREAK"
      done with
      | (Stop | Eval_error _) as exn -> raise exn
      | exn  ->
        if !trap_sp = -1 then raise exn;
        let ofs = Astack.size stack - !trap_sp in
        Astack.popn stack ofs;
        pc         := (Obj.obj (Astack.pop stack) : int);
        trap_sp    := (Obj.obj (Astack.pop stack) : int);
        env        := (Obj.obj (Astack.pop stack) : env);
        extra_args := (Obj.obj (Astack.pop stack) : int);
        accu       := (Obj.repr exn);
    done with
      | Stop -> ()
      | Eval_error msg -> fail "%s" msg in
  run 0 No_closure (ref unit)

let make_cfun_map code prim =
  let prim_nb = Array.length prim in
  let arities = Array.make prim_nb None in
  let cfuns = Array.make prim_nb (Obj.repr (fun _ -> assert false)) in
  let compute_arities instr =
    match instr with
    | Instr.C_CALL1 idx      -> arities.(idx) <- Some 1
    | Instr.C_CALL2 idx      -> arities.(idx) <- Some 2
    | Instr.C_CALL3 idx      -> arities.(idx) <- Some 3
    | Instr.C_CALL4 idx      -> arities.(idx) <- Some 4
    | Instr.C_CALL5 idx      -> arities.(idx) <- Some 5
    | Instr.C_CALLN (n, idx) -> arities.(idx) <- Some n
    | _ -> () in
  Array.iter compute_arities code;
  for i = 0 to prim_nb - 1 do
    match arities.(i) with
    | None -> ()
    | Some arity -> cfuns.(i) <- Prim.find_prim arity prim.(i)
  done;
  cfuns

let eval_bytefile { data; prim; code; _ } =
  let globals = Data.to_objs data in
  let cfuns = make_cfun_map code prim in
  Data.fix_std_exceptions globals;
  eval globals code cfuns
