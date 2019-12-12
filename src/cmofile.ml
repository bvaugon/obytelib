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

(*************************************************************************)

type constant =
| Const_int       of int
| Const_char      of char
| Const_string    of string * string option
| Const_float     of string
| Const_int32     of int32
| Const_int64     of int64
| Const_nativeint of nativeint

type structured_constant =
| Const_base        of constant
| Const_pointer     of int
| Const_block       of int * structured_constant list
| Const_float_array of string list
| Const_immstring   of string

type reloc_info =
| Reloc_literal   of structured_constant  (* structured constant    *)
| Reloc_getglobal of Symb.ident           (* reference to a global  *)
| Reloc_setglobal of Symb.ident           (* definition of a global *)
| Reloc_primitive of string               (* C primitive number     *)
    
type compilation_unit = {
  cu_name               : string;                          (* Name of compilation unit                                                        *)
  mutable cu_pos        : int;                             (* Absolute position in file                                                       *)
  cu_codesize           : int;                             (* Size of code block                                                              *)
  cu_reloc              : (reloc_info * int) list;         (* Relocation information                                                          *)
  cu_imports            : (string * Digest.t option) list; (* Names and CRC of intfs imported                                                 *)
  cu_required_globals   : Symb.ident list;                 (* Compilation units whose initialization side effects must occur before this one. *)
  cu_primitives         : string list;                     (* Primitives declared inside                                                      *)
  mutable cu_force_link : bool;                            (* Must be linked even if unref'ed                                                 *)
  mutable cu_debug      : int;                             (* Position of debugging info, or 0                                                *)
  cu_debugsize          : int;                             (* Length of debugging info                                                        *)
}

type t = {
  version : Version.t;
  unit    : compilation_unit;
  code    : Code.t;
}

(*************************************************************************)

module Legacy_V008 : sig
  type compilation_unit_v008
  val import : compilation_unit -> compilation_unit_v008
  val export : compilation_unit_v008 -> compilation_unit
end = struct
  module T = struct
    type constant =
    | Const_int       of int
    | Const_char      of char
    | Const_string    of string
    | Const_float     of string
    | Const_int32     of int32
    | Const_int64     of int64
    | Const_nativeint of nativeint

    type structured_constant =
    | Const_base        of constant
    | Const_pointer     of int
    | Const_block       of int * structured_constant list
    | Const_float_array of string list
    | Const_immstring   of string

    type reloc_info =
    | Reloc_literal   of structured_constant  (* structured constant    *)
    | Reloc_getglobal of Symb.ident           (* reference to a global  *)
    | Reloc_setglobal of Symb.ident           (* definition of a global *)
    | Reloc_primitive of string               (* C primitive number     *)
    
    type compilation_unit = {
      cu_name               : string;                          (* Name of compilation unit         *)
      mutable cu_pos        : int;                             (* Absolute position in file        *)
      cu_codesize           : int;                             (* Size of code block               *)
      cu_reloc              : (reloc_info * int) list;         (* Relocation information           *)
      cu_imports            : (string * Digest.t option) list; (* Names and CRC of intfs imported  *)
      cu_primitives         : string list;                     (* Primitives declared inside       *)
      mutable cu_force_link : bool;                            (* Must be linked even if unref'ed  *)
      mutable cu_debug      : int;                             (* Position of debugging info, or 0 *)
      cu_debugsize          : int;                             (* Length of debugging info         *)
    }
  end

  type compilation_unit_v008 = T.compilation_unit

  let import =
    let import_constant cst =
      match cst with
      | Const_int n         -> T.Const_int n
      | Const_char c        -> T.Const_char c
      | Const_string (s, _) -> T.Const_string s
      | Const_float f       -> T.Const_float f
      | Const_int32 i       -> T.Const_int32 i
      | Const_int64 i       -> T.Const_int64 i
      | Const_nativeint n   -> T.Const_nativeint n in

    let rec import_structured_constant sc =
      match sc with
      | Const_base c             -> T.Const_base (import_constant c)
      | Const_pointer ptr        -> T.Const_pointer ptr
      | Const_block (tag, scs)   -> T.Const_block (tag, List.map import_structured_constant scs)
      | Const_float_array floats -> T.Const_float_array floats
      | Const_immstring str      -> T.Const_immstring str in

    let import_reloc_info reloc_info =
      match reloc_info with
      | Reloc_literal sc     -> T.Reloc_literal (import_structured_constant sc)
      | Reloc_getglobal id   -> T.Reloc_getglobal id
      | Reloc_setglobal id   -> T.Reloc_setglobal id
      | Reloc_primitive prim -> T.Reloc_primitive prim in

    let import_cu_reloc cu_reloc =
      List.map (fun (reloc_info, n) -> (import_reloc_info reloc_info, n)) cu_reloc in

    fun {
      cu_name; cu_pos; cu_codesize; cu_reloc; cu_imports;
      cu_required_globals = _;
      cu_primitives; cu_force_link; cu_debug; cu_debugsize;
    } -> {
      T.cu_name; cu_pos; cu_codesize;
      cu_reloc = import_cu_reloc cu_reloc;
      cu_imports; cu_primitives; cu_force_link;
      cu_debug; cu_debugsize;
    }

  let export =
    let export_constant cst =
      match cst with
      | T.Const_int n       -> Const_int n
      | T.Const_char c      -> Const_char c
      | T.Const_string s    -> Const_string (s, None)
      | T.Const_float f     -> Const_float f
      | T.Const_int32 i     -> Const_int32 i
      | T.Const_int64 i     -> Const_int64 i
      | T.Const_nativeint n -> Const_nativeint n in

    let rec export_structured_constant sc =
      match sc with
      | T.Const_base c             -> Const_base (export_constant c)
      | T.Const_pointer ptr        -> Const_pointer ptr
      | T.Const_block (tag, scs)   -> Const_block (tag, List.map export_structured_constant scs)
      | T.Const_float_array floats -> Const_float_array floats
      | T.Const_immstring str      -> Const_immstring str in

    let export_reloc_info reloc_info =
      match reloc_info with
      | T.Reloc_literal sc     -> Reloc_literal (export_structured_constant sc)
      | T.Reloc_getglobal id   -> Reloc_getglobal id
      | T.Reloc_setglobal id   -> Reloc_setglobal id
      | T.Reloc_primitive prim -> Reloc_primitive prim in

    let export_cu_reloc cu_reloc =
      List.map (fun (reloc_info, n) -> (export_reloc_info reloc_info, n)) cu_reloc in

    fun {
      T.cu_name; cu_pos; cu_codesize; cu_reloc; cu_imports;
      cu_primitives; cu_force_link; cu_debug; cu_debugsize;
    } -> {
      cu_name; cu_pos; cu_codesize;
      cu_reloc = export_cu_reloc cu_reloc;
      cu_imports; cu_primitives;
      cu_required_globals = [];
      cu_force_link; cu_debug; cu_debugsize;
    }
end

(*************************************************************************)

module Legacy_V010 : sig
  type compilation_unit_v010
  val import : compilation_unit -> compilation_unit_v010
  val export : compilation_unit_v010 -> compilation_unit
end = struct
  module T = struct
    type compilation_unit = {
      cu_name               : string;                          (* Name of compilation unit         *)
      mutable cu_pos        : int;                             (* Absolute position in file        *)
      cu_codesize           : int;                             (* Size of code block               *)
      cu_reloc              : (reloc_info * int) list;         (* Relocation information           *)
      cu_imports            : (string * Digest.t option) list; (* Names and CRC of intfs imported  *)
      cu_primitives         : string list;                     (* Primitives declared inside       *)
      mutable cu_force_link : bool;                            (* Must be linked even if unref'ed  *)
      mutable cu_debug      : int;                             (* Position of debugging info, or 0 *)
      cu_debugsize          : int;                             (* Length of debugging info         *)
    }
  end

  type compilation_unit_v010 = T.compilation_unit

  let import {
    cu_name; cu_pos; cu_codesize; cu_reloc; cu_imports;
    cu_required_globals = _;
    cu_primitives; cu_force_link; cu_debug; cu_debugsize;
  } = {
    T.cu_name; cu_pos; cu_codesize; cu_reloc; cu_imports;
    cu_primitives; cu_force_link; cu_debug; cu_debugsize;
  }

  let export {
    T.cu_name; cu_pos; cu_codesize; cu_reloc; cu_imports;
    cu_primitives; cu_force_link; cu_debug; cu_debugsize;
  } = {
    cu_name; cu_pos; cu_codesize; cu_reloc; cu_imports;
    cu_required_globals = [];
    cu_primitives; cu_force_link; cu_debug; cu_debugsize;
  }
end

(*************************************************************************)

let version_of_magic str =
  match str with
  | "Caml1999O008" -> Some Version.V008
  | "Caml1999O010" -> Some Version.V010
  | "Caml1999O011" -> Some Version.V011
  | "Caml1999O023" -> Some Version.V023
  | "Caml1999O025" -> Some Version.V025
  | "Caml1999O026" -> Some Version.V026
  | "Caml1999O027" -> Some Version.V027
  | _ -> None

let magic_of_version v =
  match v with
  | Version.V008 -> "Caml1999O008"
  | Version.V010 -> "Caml1999O010"
  | Version.V011 -> "Caml1999O011"
  | Version.V023 -> "Caml1999O023"
  | Version.V025 -> "Caml1999O025"
  | Version.V026 -> "Caml1999O026"
  | Version.V027 -> "Caml1999O027"

let magic_len = String.length (magic_of_version Version.V008)

(*************************************************************************)

let reloc cmo =
  let value_of_constant c =
    match c with
    | Const_int n -> Value.Int n
    | Const_char c -> Value.Int (int_of_char c)
    | Const_string (s, _) -> Value.String s
    | Const_float s -> Value.Float (float_of_string s)
    | Const_int32 i -> Value.Int32 i
    | Const_int64 i -> Value.Int64 i
    | Const_nativeint n -> Value.Nativeint n in

  let rec value_of_structured_constant sc =
    match sc with
    | Const_base c ->
      value_of_constant c
    | Const_pointer ptr ->
      Value.Int ptr
    | Const_block (tag, fields) ->
      let fields = Array.of_list (List.map value_of_structured_constant fields) in
      if tag = Obj.object_tag then Value.Object fields
      else Value.Block (tag, fields)
    | Const_float_array floats ->
      Value.Float_array (Array.of_list (List.map float_of_string floats))
    | Const_immstring str ->
      Value.String str in

  let data_ind = ref 0 in
  let prim_ind = ref 0 in

  let literals   = Hashtbl.create 16 in
  let globals    = Hashtbl.create 16 in
  let primitives = Hashtbl.create 16 in

  let idents     = Hashtbl.create 16 in

  List.iter (fun (reloc_info, pos) ->
    match reloc_info with
    | Reloc_literal sc ->
      Hashtbl.add literals (pos / 4) (value_of_structured_constant sc, !data_ind);
      incr data_ind;
    | Reloc_getglobal id | Reloc_setglobal id ->
      let ind =
        try Hashtbl.find idents id.Symb.name
        with Not_found ->
          let ind = !data_ind in
          Hashtbl.add idents id.Symb.name ind;
          incr data_ind;
          ind in
      Hashtbl.add globals (pos / 4) (id, ind);
    | Reloc_primitive prim ->
      Hashtbl.add primitives (pos / 4) (prim, !prim_ind);
      incr prim_ind;
  ) cmo.unit.cu_reloc;

  let data_nb = !data_ind in
  let prim_nb = !prim_ind in

  let data = Array.make data_nb (Value.Int 0) in
  let symb = Array.make data_nb None in
  let prim = Array.make prim_nb "" in

  Hashtbl.iter (fun _pos (value, ind) -> data.(ind) <- value) literals;
  Hashtbl.iter (fun _pos (ident, ind) -> symb.(ind) <- Some ident) globals;
  Hashtbl.iter (fun _pos (pname, ind) -> prim.(ind) <- pname) primitives;

  let code_pos = ref 0 in
  let code = Array.make (Array.length cmo.code) Instr.STOP in
  let find_global pos =
    try snd (Hashtbl.find literals pos)
    with Not_found -> snd (Hashtbl.find globals pos) in
  let find_prim pos =
    snd (Hashtbl.find primitives pos) in
  Array.iteri (fun instr_ind instr ->
    let word_count =
      let cnt = ref 0 in
      Instr.write cmo.version (fun _ -> incr cnt) (fun _ _ -> incr cnt) instr;
      !cnt in
    let new_instr =
      try
        match instr with
        | Instr.GETGLOBAL _               -> Instr.GETGLOBAL (find_global (!code_pos + 1))
        | Instr.SETGLOBAL _               -> Instr.SETGLOBAL (find_global (!code_pos + 1))
        | Instr.PUSHGETGLOBAL _           -> Instr.PUSHGETGLOBAL (find_global (!code_pos + 1))
        | Instr.GETGLOBALFIELD (_, n)     -> Instr.GETGLOBALFIELD (find_global (!code_pos + 1), n)
        | Instr.PUSHGETGLOBALFIELD (_, n) -> Instr.PUSHGETGLOBALFIELD (find_global (!code_pos + 1), n)
        | Instr.C_CALL1 _                 -> Instr.C_CALL1 (find_prim (!code_pos + 1))
        | Instr.C_CALL2 _                 -> Instr.C_CALL2 (find_prim (!code_pos + 1))
        | Instr.C_CALL3 _                 -> Instr.C_CALL3 (find_prim (!code_pos + 1))
        | Instr.C_CALL4 _                 -> Instr.C_CALL4 (find_prim (!code_pos + 1))
        | Instr.C_CALL5 _                 -> Instr.C_CALL5 (find_prim (!code_pos + 1))
        | Instr.C_CALLN (n, _)            -> Instr.C_CALLN (n, find_prim (!code_pos + 1))
        | _                               -> instr
      with Not_found ->
        instr in
    code.(instr_ind) <- new_instr;
    code_pos := !code_pos + word_count;
  ) cmo.code;

  (data, symb, prim, code)

(*************************************************************************)

let print =
  let open Printf in

  let print_list p oc l =
    let rec f l =
      match l with
      | [] -> ()
      | [ last ] -> p oc last
      | x :: rest -> fprintf oc "%a ; " p x; f rest
    in
    fprintf oc "[ "; f l; fprintf oc "]" in

  let print_vlist i p oc l =
    fprintf oc "[\n";
    List.iter (fprintf oc "%s%a;\n" (String.make (i+2) ' ') p) l;
    fprintf oc "%s]" (String.make i ' ') in
  
  let print_string_option oc sopt =
    match sopt with
    | None   -> fprintf oc "None"
    | Some s -> fprintf oc "Some %S" s in

  let print_constant oc c =
    match c with
    | Const_int n         -> fprintf oc "Const_int %d" n
    | Const_char c        -> fprintf oc "Const_char %C" c
    | Const_string (s, o) -> fprintf oc "Const_string (%S, %a)" s print_string_option o
    | Const_float f       -> fprintf oc "Const_float %S" f
    | Const_int32 n       -> fprintf oc "Const_int32 %ldl" n
    | Const_int64 n       -> fprintf oc "Const_int64 %LdL" n
    | Const_nativeint n   -> fprintf oc "Const_nativeint %ndn" n in

  let rec print_structured_constant oc sc =
    match sc with
    | Const_base c         -> fprintf oc "Const_base (%a)" print_constant c
    | Const_pointer n      -> fprintf oc "Const_pointer %d" n
    | Const_block (n, scs) -> fprintf oc "Const_block (%d, %a)" n (print_list print_structured_constant) scs;
    | Const_float_array fs -> fprintf oc "Const_float_array %a" (print_list (fun oc s -> fprintf oc "%S" s)) fs;
    | Const_immstring s    -> fprintf oc "Const_immstring %S" s in

  let print_reloc_info oc ri =
    match ri with
    | Reloc_literal sc   -> fprintf oc "Reloc_literal (%a)" print_structured_constant sc
    | Reloc_getglobal id -> fprintf oc "Reloc_getglobal %a" Symb.print_ident id
    | Reloc_setglobal id -> fprintf oc "Reloc_setglobal %a" Symb.print_ident id
    | Reloc_primitive s  -> fprintf oc "Reloc_primitive %S" s in

  let print_compilation_unit oc {
    cu_name; cu_pos; cu_codesize; cu_reloc; cu_imports; cu_required_globals;
    cu_primitives; cu_force_link; cu_debug; cu_debugsize
  } =
    fprintf oc "\
{\n\
\  cu_name = %S;\n\
\  cu_pos = %d;\n\
\  cu_codesize = %d;\n\
\  cu_reloc = %a;\n\
\  cu_imports = %a;\n\
\  cu_required_globals = %a;\n\
\  cu_primitives = %a;\n\
\  cu_force_link = %b;\n\
\  cu_debug = %d;\n\
\  cu_debugsize = %d;\n\
}\n\
" cu_name
  cu_pos
  cu_codesize
  (print_vlist 2 (fun oc (ri, n) -> fprintf oc "(%a, %d)" print_reloc_info ri n)) cu_reloc
  (print_vlist 2 (fun oc (s, d) -> fprintf oc "(%S, %s)" s
    (match d with
    | None -> "None"
    | Some d -> "Some \"" ^ Digest.to_hex d ^ "\""))
  ) cu_imports
  (print_vlist 2 Symb.print_ident) cu_required_globals
  (print_vlist 2 (fun oc s -> fprintf oc "%S" s)) cu_primitives
  cu_force_link
  cu_debug
  cu_debugsize in

  fun oc cmo ->
    let data, symb, prim, code = reloc cmo in
    Printf.fprintf oc "Version = %s\n" (Version.to_string cmo.version);
    Printf.fprintf oc "\n########> COMPILATION UNIT\n\n";
    print_compilation_unit oc cmo.unit;
    Printf.fprintf oc "\n########> SYMB\n\n";
    Symb.print oc symb;
    Printf.fprintf oc "\n########> DATA\n\n";
    Data.print symb oc data;
    Printf.fprintf oc "\n########> PRIM\n\n";
    Prim.print oc prim;
    Printf.fprintf oc "\n########> CODE\n\n";
    Normalised_code.print data symb prim oc (Normalised_code.of_code code)

(*************************************************************************)

let read file_name =
  let ic =
    try open_in_bin file_name
    with _ -> fail "fail to open file %S for reading" file_name in
  try
    let version =
      let buffer = Bytes.create magic_len in
      really_input ic buffer 0 magic_len;
      let str = Bytes.to_string buffer in
      match version_of_magic str with
      | Some v -> v
      | None -> failwith (Printf.sprintf "unexpected magic string for a .cmo file: %S" str) in
    let compunit_pos = input_binary_int ic in
    seek_in ic compunit_pos;
    let unit =
      match version with
      | Version.V008 -> Legacy_V008.export (input_value ic : Legacy_V008.compilation_unit_v008)
      | Version.V010 -> Legacy_V010.export (input_value ic : Legacy_V010.compilation_unit_v010)
      | Version.V011 -> (input_value ic : compilation_unit)
      | Version.V023 -> (input_value ic : compilation_unit)
      | Version.V025 -> (input_value ic : compilation_unit)
      | Version.V026 -> (input_value ic : compilation_unit)
      | Version.V027 -> (input_value ic : compilation_unit)
    in
    let index = [ { Index.section = Section.CODE; offset = unit.cu_pos; length = unit.cu_codesize } ] in
    let code = Code.read version index ic in
    close_in ic;
    { version; unit; code }
  with
  | End_of_file ->
    close_in ic;
    fail "file %S is not a valid bytecode file" file_name
  | Failure msg | Sys_error msg ->
    close_in ic;
    fail "file %S is not a valid bytecode file (%s)" file_name msg
  | exn ->
    close_in ic;
    fail "fail to read bytecode file %S (internal error: %s)" file_name (Printexc.to_string exn)

(*************************************************************************)

let write file_name { version; unit; code } =
  let code_size = Code.size version code in
  let unit = {
    unit with
      cu_pos = magic_len + 4;
      cu_codesize = code_size;
      cu_debug = 0;
      cu_debugsize = 0;
  } in
  let marshaled_unit =
    match version with
    | Version.V008 -> Marshal.to_string (Legacy_V008.import unit) []
    | Version.V010 -> Marshal.to_string (Legacy_V010.import unit) []
    | Version.V011 -> Marshal.to_string unit []
    | Version.V023 -> Marshal.to_string unit []
    | Version.V025 -> Marshal.to_string unit []
    | Version.V026 -> Marshal.to_string unit []
    | Version.V027 -> Marshal.to_string unit []
  in
  let oc =
    try open_out file_name
    with _ -> fail "fail to open file %S for writting" file_name in
  try
    output_string oc (magic_of_version version);
    output_binary_int oc (magic_len + 4 + code_size);
    Code.write version oc code;
    output_string oc marshaled_unit;
    close_out oc;
  with
  | Failure msg | Sys_error msg ->
    close_out oc;
    fail "fail to write cmo file %S (%s)" file_name msg;
  | exn ->
    close_out oc;
    fail "fail to write cmo file %S (internal error: %s)" file_name (Printexc.to_string exn)

(*************************************************************************)
