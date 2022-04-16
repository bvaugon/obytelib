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
  | Rec_closure of int array * Obj.t array * int

let eval_error fmt =
  Printf.ksprintf (fun s -> raise (Eval_error s)) fmt

let ptr_of_env env = match env with
  | No_closure -> eval_error "closure expected"
  | Closure (ptr, _) -> ptr
  | Rec_closure (ptrs, _, i) -> ptrs.(i)

let envacc env n =
  match env with
  | No_closure -> eval_error "closure expected"
  | Closure (_, blk) -> blk.(n - 1)
  | Rec_closure (ptrs, blk, i) -> blk.(n - (2 * Array.length ptrs - 1 - 2 * i))

let offsetclosure env n =
  match env with
  | No_closure | Closure _-> eval_error "recursive closure expected"
  | Rec_closure (p, blk, i) -> Rec_closure (p, blk, i + n)

let env_of_closure env =
  match env with
  | No_closure |Rec_closure _-> eval_error "simple closure expected"
  | Closure (_, blk) -> blk

let eval globals normed_code cfuns =
  let unit = Obj.repr () in
  let stack = Astack.create (1024 * 1024) 1024 unit in
  let getenv_key = Obj.repr (ref "UNIQUE KEY!") in
  let stop_pc = Array.length normed_code - 1 in
  if normed_code.(stop_pc) <> Normalised_instr.STOP then
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
        match normed_code.(!pc) with
        | Normalised_instr.ACC n ->
          accu := Astack.acc stack n;
          incr pc;
        | Normalised_instr.PUSH ->
          Astack.push stack !accu;
          incr pc;
        | Normalised_instr.POP n ->
          Astack.popn stack n;
          incr pc;
        | Normalised_instr.ASSIGN n ->
          Astack.assign stack n !accu;
          accu := unit;
          incr pc;
        | Normalised_instr.ENVACC n ->
          accu := envacc !env n;
          incr pc;
        | Normalised_instr.PUSH_RETADDR ptr ->
          Astack.push stack (Obj.repr !extra_args);
          Astack.push stack (Obj.repr !env);
          Astack.push stack (Obj.repr ptr);
          incr pc;
        | Normalised_instr.APPLY n ->
          if n < 4 then (
            let args = Array.init n (fun i -> Astack.acc stack (n - i - 1)) in
            Astack.popn stack n;
            Astack.push stack (Obj.repr !extra_args);
            Astack.push stack (Obj.repr !env);
            Astack.push stack (Obj.repr (!pc + 1));
            Array.iter (fun arg -> Astack.push stack arg) args;
          );
          extra_args := n - 1;
          env := unbox_env !accu;
          pc := ptr_of_env !env;
        | Normalised_instr.APPTERM (n, s) ->
          for i = 0 to n - 1 do
            Astack.assign stack (s - i - 1) (Astack.acc stack (n - i - 1));
          done;
          Astack.popn stack (s - n);
          extra_args := !extra_args + n - 1;
          env := unbox_env !accu;
          pc := ptr_of_env !env;
        | Normalised_instr.RETURN n ->
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
        | Normalised_instr.RESTART ->
          let blk = env_of_closure !env in
          let n = Array.length blk - 1 in
          for i = n downto 1 do Astack.push stack blk.(i) done;
          env := (Obj.obj blk.(0) : env);
          extra_args := !extra_args + n;
          incr pc;
        | Normalised_instr.GRAB n ->
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
        | Normalised_instr.CLOSURE (n, ptr) ->
          let blk = Array.make n (Obj.repr ()) in
          if n > 0 then blk.(0) <- !accu;
          for i = 1 to n - 1 do blk.(i) <- Astack.pop stack done;
          accu := box_env (Closure (ptr, blk));
          incr pc;
        | Normalised_instr.CLOSUREREC (n, p) ->
          let blk = Array.make n (Obj.repr ()) in
          if n > 0 then blk.(0) <- !accu;
          for i = 1 to n - 1 do blk.(i) <- Astack.pop stack done;
          accu := box_env (Rec_closure (p, blk, 0));
          Astack.push stack !accu;
          for i = 1 to Array.length p - 1 do
            Astack.push stack (box_env (Rec_closure (p, blk, i)));
          done;
          incr pc;
        | Normalised_instr.OFFSETCLOSURE n ->
          accu := box_env (offsetclosure !env n);
          incr pc;
        | Normalised_instr.GETGLOBAL n ->
          accu := globals.(n);
          incr pc;
        | Normalised_instr.SETGLOBAL n ->
          globals.(n) <- !accu;
          accu := unit;
          incr pc;
        | Normalised_instr.ATOM tag ->
          accu := Obj.new_block tag 0;
          incr pc;
        | Normalised_instr.MAKEBLOCK (tag, sz) ->
          let blk = Obj.new_block tag sz in
          Obj.set_field blk 0 !accu;
          for i = 1 to sz - 1 do Obj.set_field blk i (Astack.pop stack) done;
          accu := blk;
          incr pc;
        | Normalised_instr.MAKEFLOATBLOCK sz ->
          let blk = Obj.new_block Obj.double_array_tag sz in
          Obj.set_double_field blk 0 (Obj.obj !accu : float);
          for i = 1 to sz - 1 do
            Obj.set_double_field blk i (Obj.obj (Astack.pop stack) : float);
          done;
          accu := blk;
          incr pc;
        | Normalised_instr.GETFIELD ind ->
          accu := Obj.field !accu ind;
          incr pc;
        | Normalised_instr.GETFLOATFIELD ind ->
          accu := Obj.repr (Obj.double_field !accu ind);
          incr pc;
        | Normalised_instr.SETFIELD ind ->
          Obj.set_field !accu ind (Astack.pop stack);
          accu := unit;
          incr pc;
        | Normalised_instr.SETFLOATFIELD ind ->
          Obj.set_double_field !accu ind (Obj.obj (Astack.pop stack) : float);
          accu := unit;
          incr pc;
        | Normalised_instr.UNAPP Normalised_instr.VECTLENGTH ->
          accu := Obj.repr (Obj.size !accu);
          incr pc;
        | Normalised_instr.GETVECTITEM ->
          accu := Obj.field !accu (Obj.obj (Astack.pop stack) : int);
          incr pc;
        | Normalised_instr.SETVECTITEM ->
          let i = Astack.pop stack in
          let v = Astack.pop stack in
          Obj.set_field !accu (Obj.obj i : int) v;
          accu := unit;
          incr pc;
        | Normalised_instr.GETBYTESCHAR | Normalised_instr.GETSTRINGCHAR ->
          accu := Obj.repr (Bytes.get (Obj.obj !accu : bytes) (Obj.obj (Astack.pop stack) : int));
          incr pc;
        | Normalised_instr.SETBYTESCHAR ->
          let i = Astack.pop stack in
          let c = Astack.pop stack in
          Bytes.set (Obj.obj !accu : bytes) (Obj.obj i : int) (Obj.obj c : char);
          accu := unit;
          incr pc;
        | Normalised_instr.BRANCH ptr ->
          pc := ptr;
        | Normalised_instr.BRANCHIF ptr ->
          if !accu != Obj.repr false then pc := ptr else incr pc;
        | Normalised_instr.BRANCHIFNOT ptr ->
          if !accu == Obj.repr false then pc := ptr else incr pc;
        | Normalised_instr.SWITCH (iptrs, pptrs) ->
          if Obj.is_int !accu then pc := iptrs.((Obj.obj !accu : int))
          else pc := pptrs.(Obj.tag !accu)
        | Normalised_instr.UNAPP Normalised_instr.NOT ->
          accu := Obj.repr (not (Obj.obj !accu : bool));
          incr pc;
        | Normalised_instr.PUSHTRAP ptr ->
          Astack.push stack (Obj.repr !extra_args);
          Astack.push stack (Obj.repr !env);
          Astack.push stack (Obj.repr !trap_sp);
          Astack.push stack (Obj.repr ptr);
          trap_sp := Astack.size stack;
          incr pc;
        | Normalised_instr.POPTRAP ->
          trap_sp := (Obj.obj (Astack.acc stack 1) : int);
          Astack.popn stack 4;
          incr pc;
        | Normalised_instr.RAISE | Normalised_instr.RERAISE | Normalised_instr.RAISE_NOTRACE ->
          raise (Obj.obj !accu : exn);
        | Normalised_instr.CHECK_SIGNALS ->
          incr pc;
        | Normalised_instr.C_CALL (narg, idx) ->
          accu := Prim.apply narg cfuns.(idx) !accu stack;
          Astack.popn stack (narg - 1);
          incr pc;
        | Normalised_instr.CONSTINT n ->
          accu := Obj.repr n;
          incr pc;
        | Normalised_instr.UNAPP Normalised_instr.NEG ->
          accu := Obj.repr (-(Obj.obj !accu : int));
          incr pc;
        | Normalised_instr.BINAPP Normalised_instr.ADD ->
          accu := Obj.repr ((Obj.obj !accu : int) + (Obj.obj (Astack.pop stack) : int));
          incr pc;
        | Normalised_instr.BINAPP Normalised_instr.SUB ->
          accu := Obj.repr ((Obj.obj !accu : int) - (Obj.obj (Astack.pop stack) : int));
          incr pc;
        | Normalised_instr.BINAPP Normalised_instr.MUL ->
          accu := Obj.repr ((Obj.obj !accu : int) * (Obj.obj (Astack.pop stack) : int));
          incr pc;
        | Normalised_instr.BINAPP Normalised_instr.DIV ->
          accu := Obj.repr ((Obj.obj !accu : int) / (Obj.obj (Astack.pop stack) : int));
          incr pc;
        | Normalised_instr.BINAPP Normalised_instr.MOD ->
          accu := Obj.repr ((Obj.obj !accu : int) mod (Obj.obj (Astack.pop stack) : int));
          incr pc;
        | Normalised_instr.BINAPP Normalised_instr.AND ->
          accu := Obj.repr ((Obj.obj !accu : int) land (Obj.obj (Astack.pop stack) : int));
          incr pc;
        | Normalised_instr.BINAPP Normalised_instr.OR ->
          accu := Obj.repr ((Obj.obj !accu : int) lor (Obj.obj (Astack.pop stack) : int));
          incr pc;
        | Normalised_instr.BINAPP Normalised_instr.XOR ->
          accu := Obj.repr ((Obj.obj !accu : int) lxor (Obj.obj (Astack.pop stack) : int));
          incr pc;
        | Normalised_instr.BINAPP Normalised_instr.LSL ->
          accu := Obj.repr ((Obj.obj !accu : int) lsl (Obj.obj (Astack.pop stack) : int));
          incr pc;
        | Normalised_instr.BINAPP Normalised_instr.LSR ->
          accu := Obj.repr ((Obj.obj !accu : int) lsr (Obj.obj (Astack.pop stack) : int));
          incr pc;
        | Normalised_instr.BINAPP Normalised_instr.ASR ->
          accu := Obj.repr ((Obj.obj !accu : int) asr (Obj.obj (Astack.pop stack) : int));
          incr pc;
        | Normalised_instr.COMPARE Normalised_instr.EQ ->
          accu := Obj.repr (!accu == Astack.pop stack);
          incr pc;
        | Normalised_instr.COMPARE Normalised_instr.NEQ ->
          accu := Obj.repr (!accu != Astack.pop stack);
          incr pc;
        | Normalised_instr.COMPARE Normalised_instr.LT ->
          accu := Obj.repr ((Obj.obj !accu : int) < (Obj.obj (Astack.pop stack) : int));
          incr pc;
        | Normalised_instr.COMPARE Normalised_instr.LE ->
          accu := Obj.repr ((Obj.obj !accu : int) <= (Obj.obj (Astack.pop stack) : int));
          incr pc;
        | Normalised_instr.COMPARE Normalised_instr.GT ->
          accu := Obj.repr ((Obj.obj !accu : int) > (Obj.obj (Astack.pop stack) : int));
          incr pc;
        | Normalised_instr.COMPARE Normalised_instr.GE ->
          accu := Obj.repr ((Obj.obj !accu : int) >= (Obj.obj (Astack.pop stack) : int));
          incr pc;
        | Normalised_instr.UNAPP (Normalised_instr.OFFSET ofs) ->
          accu := Obj.repr ((Obj.obj !accu : int) + ofs);
          incr pc;
        | Normalised_instr.OFFSETREF ofs ->
          let r = (Obj.obj !accu : int ref) in
          r := !r + ofs;
          accu := unit;
          incr pc;
        | Normalised_instr.UNAPP Normalised_instr.ISINT ->
          accu := Obj.repr (Obj.is_int !accu);
          incr pc;
        | Normalised_instr.GETMETHOD ->
          accu := Obj.field (Obj.field (Astack.acc stack 0) 0) (Obj.obj !accu : int);
          incr pc;
        | Normalised_instr.COMPBRANCH (Normalised_instr.EQ, n, ptr) ->
          if n == (Obj.obj !accu : int) then pc := ptr else incr pc;
        | Normalised_instr.COMPBRANCH (Normalised_instr.NEQ, n, ptr) ->
          if n != (Obj.obj !accu : int) then pc := ptr else incr pc;
        | Normalised_instr.COMPBRANCH (Normalised_instr.LT, n, ptr) ->
          if n < (Obj.obj !accu : int) then pc := ptr else incr pc;
        | Normalised_instr.COMPBRANCH (Normalised_instr.LE, n, ptr) ->
          if n <= (Obj.obj !accu : int) then pc := ptr else incr pc;
        | Normalised_instr.COMPBRANCH (Normalised_instr.GT, n, ptr) ->
          if n > (Obj.obj !accu : int) then pc := ptr else incr pc;
        | Normalised_instr.COMPBRANCH (Normalised_instr.GE, n, ptr) ->
          if n >= (Obj.obj !accu : int) then pc := ptr else incr pc;
        | Normalised_instr.COMPARE Normalised_instr.ULT ->
          let n = (Obj.obj !accu : int) + min_int
          and p = (Obj.obj (Astack.pop stack) : int) + min_int in
          accu := Obj.repr (n < p);
          incr pc;
        | Normalised_instr.COMPARE Normalised_instr.UGE ->
          let n = (Obj.obj !accu : int) + min_int
          and p = (Obj.obj (Astack.pop stack) : int) + min_int in
          accu := Obj.repr (n >= p);
          incr pc;
        | Normalised_instr.COMPBRANCH (Normalised_instr.ULT, n, ptr) ->
          if n + min_int < (Obj.obj !accu : int) + min_int
          then pc := ptr else incr pc;
        | Normalised_instr.COMPBRANCH (Normalised_instr.UGE, n, ptr) ->
          if n + min_int >= (Obj.obj !accu : int) + min_int
          then pc := ptr else incr pc;
        | Normalised_instr.GETPUBMET tag ->
          Astack.push stack !accu;
          accu := find_object_method !accu tag;
          incr pc;
        | Normalised_instr.GETDYNMET ->
          accu := find_object_method (Astack.acc stack 0) (Obj.obj !accu : int);
          incr pc;
        | Normalised_instr.STOP ->
          raise Stop
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

let make_cfun_map normed_code prim =
  let prim_nb = Array.length prim in
  let arities = Array.make prim_nb None in
  let cfuns = Array.make prim_nb (Obj.repr (fun _ -> assert false)) in
  let compute_arities instr =
    match instr with
    | Normalised_instr.C_CALL (arity, idx) -> arities.(idx) <- Some arity
    | _ -> () in
  Array.iter compute_arities normed_code;
  for i = 0 to prim_nb - 1 do
    match arities.(i) with
    | None -> ()
    | Some arity -> cfuns.(i) <- Prim.find_prim arity prim.(i)
  done;
  cfuns

let eval_bytefile { data; prim; code; _ } =
  let globals = Data.to_objs data in
  let normed_code = Normalised_code.of_code code in
  let cfuns = make_cfun_map normed_code prim in
  Data.fix_std_exceptions globals;
  eval globals normed_code cfuns
