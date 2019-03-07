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
  Astack.(Normalised_instr.(Obj.(
  let unit = repr () in
  let stack = Astack.create (1024 * 1024) 1024 unit in
  let getenv_key = repr "UNIQUE KEY!" in
  let stop_pc = Array.length normed_code - 1 in
  if normed_code.(stop_pc) <> STOP then
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
        match normed_code.(!pc) with
        | ACC n ->
          accu := acc stack n;
          incr pc;
        | PUSH ->
          push stack !accu;
          incr pc;
        | POP n ->
          popn stack n;
          incr pc;
        | ASSIGN n ->
          assign stack n !accu;
          accu := unit;
          incr pc;
        | ENVACC n ->
          accu := envacc !env n;
          incr pc;
        | PUSH_RETADDR ptr ->
          push stack (repr !extra_args);
          push stack (repr !env);
          push stack (repr ptr);
          incr pc;
        | APPLY n ->
          if n < 4 then (
            let args = Array.init n (fun i -> acc stack (n - i - 1)) in
            popn stack n;
            push stack (repr !extra_args);
            push stack (repr !env);
            push stack (repr (!pc + 1));
            Array.iter (fun arg -> push stack arg) args;
          );
          extra_args := n - 1;
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
        | CLOSUREREC (n, p) ->
          let blk = Array.make n !accu in
          for i = 1 to n - 1 do blk.(i) <- pop stack done;
          accu := box_env (Rec_closure (p, blk, 0));
          push stack !accu;
          for i = 1 to Array.length p - 1 do
            push stack (box_env (Rec_closure (p, blk, i)));
          done;
          incr pc;
        | OFFSETCLOSURE n ->
          accu := box_env (offsetclosure !env n);
          incr pc;
        | GETGLOBAL n ->
          accu := globals.(n);
          incr pc;
        | SETGLOBAL n ->
          globals.(n) <- !accu;
          accu := unit;
          incr pc;
        | ATOM tag ->
          accu := new_block tag 0;
          incr pc;
        | MAKEBLOCK (tag, sz) ->
          let blk = new_block tag sz in
          set_field blk 0 !accu;
          for i = 1 to sz - 1 do set_field blk i (pop stack) done;
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
        | GETFIELD ind ->
          accu := field !accu ind;
          incr pc;
        | GETFLOATFIELD ind ->
          accu := repr (double_field !accu ind);
          incr pc;
        | SETFIELD ind ->
          set_field !accu ind (pop stack);
          accu := unit;
          incr pc;
        | SETFLOATFIELD ind ->
          set_double_field !accu ind (obj (pop stack) : float);
          accu := unit;
          incr pc;
        | UNAPP VECTLENGTH ->
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
        | GETBYTESCHAR | GETSTRINGCHAR ->
          accu := repr (Bytes.get (obj !accu : bytes) (obj (pop stack) : int));
          incr pc;
        | SETBYTESCHAR ->
          let i = pop stack in
          let c = pop stack in
          Bytes.set (obj !accu : bytes) (obj i : int) (obj c : char);
          accu := unit;
          incr pc;
        | BRANCH ptr ->
          pc := ptr;
        | BRANCHIF ptr ->
          if !accu != repr false then pc := ptr else incr pc;
        | BRANCHIFNOT ptr ->
          if !accu == repr false then pc := ptr else incr pc;
        | SWITCH (iptrs, pptrs) ->
          if is_int !accu then pc := iptrs.((obj !accu : int))
          else pc := pptrs.(tag !accu)
        | UNAPP NOT ->
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
        | C_CALL (narg, idx) ->
          accu := Prim.apply narg cfuns.(idx) !accu stack;
          popn stack (narg - 1);
          incr pc;
        | CONSTINT n ->
          accu := repr n;
          incr pc;
        | UNAPP NEG ->
          accu := repr (-(obj !accu : int));
          incr pc;
        | BINAPP ADD ->
          accu := repr ((obj !accu : int) + (obj (pop stack) : int));
          incr pc;
        | BINAPP SUB ->
          accu := repr ((obj !accu : int) - (obj (pop stack) : int));
          incr pc;
        | BINAPP MUL ->
          accu := repr ((obj !accu : int) * (obj (pop stack) : int));
          incr pc;
        | BINAPP DIV ->
          accu := repr ((obj !accu : int) / (obj (pop stack) : int));
          incr pc;
        | BINAPP MOD ->
          accu := repr ((obj !accu : int) mod (obj (pop stack) : int));
          incr pc;
        | BINAPP AND ->
          accu := repr ((obj !accu : int) land (obj (pop stack) : int));
          incr pc;
        | BINAPP OR ->
          accu := repr ((obj !accu : int) lor (obj (pop stack) : int));
          incr pc;
        | BINAPP XOR ->
          accu := repr ((obj !accu : int) lxor (obj (pop stack) : int));
          incr pc;
        | BINAPP LSL ->
          accu := repr ((obj !accu : int) lsl (obj (pop stack) : int));
          incr pc;
        | BINAPP LSR ->
          accu := repr ((obj !accu : int) lsr (obj (pop stack) : int));
          incr pc;
        | BINAPP ASR ->
          accu := repr ((obj !accu : int) asr (obj (pop stack) : int));
          incr pc;
        | COMPARE EQ ->
          accu := repr (!accu == pop stack);
          incr pc;
        | COMPARE NEQ ->
          accu := repr (!accu != pop stack);
          incr pc;
        | COMPARE LT ->
          accu := repr ((obj !accu : int) < (obj (pop stack) : int));
          incr pc;
        | COMPARE LE ->
          accu := repr ((obj !accu : int) <= (obj (pop stack) : int));
          incr pc;
        | COMPARE GT ->
          accu := repr ((obj !accu : int) > (obj (pop stack) : int));
          incr pc;
        | COMPARE GE ->
          accu := repr ((obj !accu : int) >= (obj (pop stack) : int));
          incr pc;
        | UNAPP (OFFSET ofs) ->
          accu := repr ((obj !accu : int) + ofs);
          incr pc;
        | OFFSETREF ofs ->
          let r = (obj !accu : int ref) in
          r := !r + ofs;
          accu := unit;
          incr pc;
        | UNAPP ISINT ->
          accu := repr (is_int !accu);
          incr pc;
        | GETMETHOD ->
          accu := field (field (acc stack 0) 0) (obj !accu : int);
          incr pc;
        | COMPBRANCH (EQ, n, ptr) ->
          if n == (obj !accu : int) then pc := ptr else incr pc;
        | COMPBRANCH (NEQ, n, ptr) ->
          if n != (obj !accu : int) then pc := ptr else incr pc;
        | COMPBRANCH (LT, n, ptr) ->
          if n < (obj !accu : int) then pc := ptr else incr pc;
        | COMPBRANCH (LE, n, ptr) ->
          if n <= (obj !accu : int) then pc := ptr else incr pc;
        | COMPBRANCH (GT, n, ptr) ->
          if n > (obj !accu : int) then pc := ptr else incr pc;
        | COMPBRANCH (GE, n, ptr) ->
          if n >= (obj !accu : int) then pc := ptr else incr pc;
        | COMPARE ULT ->
          let n = (obj !accu : int) + min_int
          and p = (obj (pop stack) : int) + min_int in
          accu := repr (n < p);
          incr pc;
        | COMPARE UGE ->
          let n = (obj !accu : int) + min_int
          and p = (obj (pop stack) : int) + min_int in
          accu := repr (n >= p);
          incr pc;
        | COMPBRANCH (ULT, n, ptr) ->
          if n + min_int < (obj !accu : int) + min_int
          then pc := ptr else incr pc;
        | COMPBRANCH (UGE, n, ptr) ->
          if n + min_int >= (obj !accu : int) + min_int
          then pc := ptr else incr pc;
        | GETPUBMET tag ->
          push stack !accu;
          accu := find_object_method !accu tag;
          incr pc;
        | GETDYNMET ->
          accu := find_object_method (acc stack 0) (obj !accu : int);
          incr pc;
        | STOP ->
          raise Stop
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
