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

(** OCaml bytecode tools

    OByteLib: the entry point module of the library *)

(** Tool to mangage bytecode file versions *)
module Version : sig
  (** The two bytecode versions corresponding to magic strings:
      "Caml1999X008", "Caml1999X010" and "Caml1999X011" *)
  type t = V008 | V010 | V011

  (** Conversion from version to pretty string *)
  val to_string : t -> string

  (** Conversion from version to magic string contained in bytecode file *)
  val to_magic : t -> string

  (** Conversion of magic string to version. Raise a Failure if the parameter
      is an unknwon magic string *)
  val of_magic : string -> t

  (** Read version from a bytecode file. Automatically move to the end
      of the file. Raise a Failure if the file is too short or the
      magic string invalid *)
  val read : in_channel -> t

  (** Write the magic string associated to the given version in a channel *)
  val write : out_channel -> t -> unit
end

(** Tool to manipulate section names *)
module Section : sig
  (** Bytecode files are divided into sections represented by a 4-letter name *)
  type t = CODE | DLPT | DLLS | PRIM | DATA | SYMB | CRCS | DBUG

  (** Convert a 4-letter string to it associated constructor. Raise a
      Failure if no section match *)
  val of_string : string -> t

  (** Convert a construtor to its 4-letter associated name *)
  val to_string : t -> string
end

(** Tool to manipulate indexes comming from the end of bytecode files *)
module Index : sig
  (** Index entry containning the section name, the offset from the
      beginning of the file (in bytes), and the length of the section
      (in bytes) *)
  type entry = { section: Section.t; offset: int; length: int; }

  (** The index main type: a list of entries *)
  type t = entry list

  (** Read the index from the bytecode file *)
  val read : in_channel -> t

  (** Write an index in a bytecode file *)
  val write : out_channel -> t -> unit

  (** Find a section in an index. Raise a Failure if the requested
      section is not found. Return a tuple (offset, length) where
      offset is the offset of the section from the beginning of the
      file in bytes, and length is the length of the section in
      bytes *)
  val find_section : t -> Section.t -> int * int

  (** Pretty-print an index *)
  val print : out_channel -> t -> unit
end

(** Sections *)

(** Manipulation of the binary data stored between the VM command and
    the beginning of sections *)
module Extra : sig
  (** Extra data type: a sequence of bytes *)
  type t = string

  (** An empty extra section *)
  val empty : t

  (** Read the extra section from a bytecode file *)
  val read : Index.t -> in_channel -> t

  (** Write an extra section to a bytecode file *)
  val write : out_channel -> t -> unit
end

(** Management of the DLPT section containing dynamic library paths *)
module Dlpt : sig
  (** Type of an imported DLPT section: a array of paths *)
  type t = string array

  (** An empty DLPT section *)
  val empty : t

  (** Pretty-print path list *)
  val print : out_channel -> t -> unit

  (** Read the DLPT section from a bytecode file. If it does not appear,
      return an empty array *)
  val read : Index.t -> in_channel -> t

  (** Write a DLPT section to a bytecode file *)
  val write : out_channel -> t -> unit
end

(** Management of the DLLS section containing the names of needed
    dynamic libraries *)
module Dlls : sig
  (** Type of an imported DLL section: a array of dl names *)
  type t = string array

  (** An empty DLLS section *)
  val empty : t

  (** Pretty-print dll names *)
  val print : out_channel -> t -> unit

  (** Read the DLLS section from a bytecode file. If it does not
      appear, return an empty array *)
  val read : Index.t -> in_channel -> t

  (** Write a DLLS section to a bytecode file *)
  val write : out_channel -> t -> unit
end

(** Management of the CRCS section containing module names and MD5
    hash of the corresponding interfaces *)
module Crcs : sig
  (** Type of an entry of the CRCS section *)
  type entry = string * Digest.t option

  (** Type of an imported CRCS section: a list of entries *)
  type t = entry list

  (** An empty CRCS section *)
  val empty : t

  (** Pretty-print an imported CRCS section *)
  val print : out_channel -> t -> unit

  (** Read the CRCS section from a bytecode file. Return an empty array if
      the CRCS section does not appear in the bytecode file *)
  val read : Index.t -> in_channel -> t

  (** Write a CRCS section in a bytecode file *)
  val write : out_channel -> t -> unit
end

(** Management of the SYMB section containing meta-data about global
    table (see the DATA section) *)
module Symb : sig
  (** Type of an identifier *)
  type ident = { stamp: int; name: string; mutable flags: int }

  (** Type of the symbol map *)
  type t = ident option array

  (** An empty symbol map *)
  val empty : t

  (** Pretty-print the SYMB section *)
  val print : out_channel -> t -> unit

  (** Read the SYMB section from a bytecode file. Return empty if no
      SYMB section found *)
  val read : Index.t -> in_channel -> t

  (** Write the SYMB section in a bytecode file *)
  val write : out_channel -> t -> unit
end

(** Tools to manipulate a part of the DBUG section from a bytecode file *)
module Dbug : sig
  type 'a ident_data =
    { ident: Symb.ident;
      data: 'a;
      previous: 'a ident_data option }

  type 'a ident_tbl =
    Empty
  | Node of 'a ident_tbl * 'a ident_data * 'a ident_tbl * int

  type compilation_env =
    { ce_stack: int ident_tbl;
      ce_heap: int ident_tbl;
      ce_rec: int ident_tbl }

  (** Type representing expr positions in source files *)
  type location_t = {
    loc_start: Lexing.position;
    loc_end: Lexing.position;
    loc_ghost: bool;
  }

  type env_summary
  type subst_t
  type types_type_expr
  
  (** A debug event, with some field masked with abstract types *)
  type debug_event =
    { mutable ev_pos: int;                (* Position in bytecode *)
      ev_module: string;                  (* Name of defining module *)
      ev_loc: location_t;                 (* Location in source file *)
      ev_kind: debug_event_kind;          (* Before/after event *)
      ev_info: debug_event_info;          (* Extra information *)
      ev_typenv: env_summary;             (* Typing environment *)
      ev_typsubst: subst_t;               (* Substitution over types *)
      ev_compenv: compilation_env;        (* Compilation environment *)
      ev_stacksize: int;                  (* Size of stack frame *)
      ev_repr: debug_event_repr }         (* Position of the representative *)
      
  and debug_event_kind =
    Event_before
  | Event_after of types_type_expr
  | Event_pseudo
      
  and debug_event_info =
    Event_function
  | Event_return of int
  | Event_other
      
  and debug_event_repr =
    Event_none
  | Event_parent of int ref
  | Event_child of int ref

  (** Type representing an imported DBUG section: a list of debug events *)
  type t = (int * debug_event list * string list) array

  (** An empty DBUG section *)
  val empty : t

  (** Pretty-print a DBUG section *)
  val print : out_channel -> t -> unit

  (** Read the DBUG section from an bytecode file. Return an empty
      list if no DBUG section found *)
  val read : Index.t -> in_channel -> t

  (** Write the DBUG section to a bytecode file *)
  val write : out_channel -> t -> unit
end

(** OCaml pretty-representation of values stored in the DATA section *)
module Value : sig
  (** The main type representing the different kinds of value *)
  type t =
    | Int         of int
    | Int32       of Int32.t
    | Int64       of Int64.t
    | Nativeint   of Nativeint.t
    | Float       of float
    | Float_array of float array
    | String      of string
    | Object      of t array
    | Block       of int * t array

  (** Convert a value to a pretty-string *)
  val to_string : t -> string

  (** Convert an Obj.t (usually comming from a cell of the DATA
      section) to a structured OCaml tree *)
  val of_obj : Obj.t -> t

  (** [make_to_obj ()] creates a converter of values to Obj.t that
      factorize int32, in64, nativeints and floats. Usefull for
      convert a table of Obj.t that share immutable blocks *)
  val make_to_obj : unit -> t -> Obj.t
end

(** Reading, writing, pretty-printing and management of the DATA
    section containing the array of global data *)
module Data : sig
  (** Type of an imported DATA section: a table of structured values *)
  type t = Value.t array

  (** Pretty-print a DATA section *)
  val print : Symb.t -> out_channel -> t -> unit

  (** Read the DATA section from a bytecode file. Raise a Failure if
      the DATA seciton does not exists *)
  val read : Index.t -> in_channel -> t

  (** Export and write a DATA section into a bytecode file *)
  val write : out_channel -> t -> unit

  (** Convert a table of structured values in a table of Obj.t *)
  val to_objs : t -> Obj.t array

  (** Replace the first cells of the DATA array containning pointers
      to standard exceptions (Failure, Stack_overflow, etc.) to share them
      with the current process exceptions *)
  val fix_std_exceptions : Obj.t array -> unit
end

(** Reading, writing, pretty-printing and management of the PRIM
    section containing the mapping of C primitive names to C_CALL
    ids *)
module Prim : sig
  (** Type of table of primitive names. Indexes in the table
      correspond to C_CALL ids and contained strings are C function
      names *)
  type t = string array

  (** Read the table of primitives from a bytecode file *)
  val read : Index.t -> in_channel -> t

  (** Write a table of primitives in a bytecode file *)
  val write : out_channel -> t -> unit
end

(** Low-level bytecode instruction management. Each constructor
    correspond directly to a VM instruction. To use a normalised
    representation of bytecode instructions, please see the
    Normalised_* modules *)
module Instr : sig
  (** Type representing the 148 bytecode instructions as they are
      defined for the V010 bytecode version *)
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
    | RERAISE
    | RAISE_NOTRACE

  (** Convert an instruction to a pretty-string *)
  val to_string : t -> string

  (** Get the code pointers from an instruction. Code pointers are
      indexes in the table of instructions (see the Code module) *)
  val get_ptrs : t -> int list
end

(** Reading, writting and pretty-printing of the CODE section *)
module Code : sig
  (** Type representing the imported CODE section: an array of
      instructions. Code pointers from instructions are indexes in the
      table of instructions *)
  type t = Instr.t array

  (** Group instructions into connected components, separating
      function bodies and top-level code. Return an integer array of
      the same size of the given code. Two instructions are in the
      same connected component iff they correspond to the same integer
      in the returned array *)
  val kosaraju : t -> int array

  (** Pretty-print code *)
  val print : Data.t -> Symb.t -> Prim.t -> out_channel -> t -> unit

  (** Read the CODE section from a bytecode file. If it does not
      appear, raise a Failure *)
  val read : Version.t -> Index.t -> in_channel -> t

  (** Write a CODE section to a bytecode file *)
  val write : Version.t -> out_channel -> t -> unit
end

(** Bytecode file *)

(** Reading, writting and pretty-printing tools for complete bytecode file *)
module Bytefile : sig
  (** Type of an imported bytecode file *)
  type t = {
    version : Version.t;
    vmpath  : string option;
    vmarg   : string option;
    index   : Index.t;
    extra   : Extra.t;
    data    : Data.t;
    prim    : Prim.t;
    code    : Code.t;
    dlpt    : Dlpt.t;
    dlls    : Dlls.t;
    crcs    : Crcs.t;
    dbug    : Dbug.t;
    symb    : Symb.t;
  }

  (** Pretty-print the contents of a bytecode file *)
  val print : out_channel -> t -> unit

  (** Read and import a bytecode file *)
  val read : string -> t

  (** Export and write a bytecode file *)
  val write : string -> Version.t -> ?vmpath:string -> ?vmarg:string ->
    ?extra:string -> ?dlpt:Dlpt.t -> ?dlls:Dlls.t -> ?crcs:Crcs.t ->
    ?dbug:Dbug.t -> ?symb:Symb.t -> Data.t -> Prim.t -> Code.t -> unit
end

(** Cmo file *)

module Cmofile : sig
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

  (** Pretty-print the contents of a .cmo file *)
  val print : out_channel -> t -> unit

  (** Read and import a .cmo file *)
  val read : string -> t

  (** Export and write a .cmo file *)
  val write : string -> t -> unit

  (** Resolve pointers to global data, symbols and primitives *)
  val reloc : t -> Data.t * Symb.t * Prim.t * Code.t
end

(** Normalised code *)

(** Normalised version of the bytecode instruction set. Remove
    redondancies like ACC0/ACC 0 or PUSHACC/PUSH+ACC, group operators,
    and simplify/prepare some instruction parameters (see CLOSUREREC
    and SWITCH) *)
module Normalised_instr : sig
  (** Type for unary operators transforming the accumulator *)
  type unop = NOT | NEG | OFFSET of int | VECTLENGTH | ISINT

  (** Type for binary operators (except comparisons, see compop)
      giving accu and stack top as parameters, poping the top of the
      stack and storing the result in the accu without side effect *)
  type binop = ADD | SUB | MUL | DIV | MOD | AND | OR | XOR | LSL | LSR | ASR

  (** Comparison operators used for interger comparison instructions
      and conditionnal branchs based on a comparison *)
  type compop = EQ | NEQ | LT | LE | GT | GE | ULT | UGE

  (** Type for normalised bytecode instructions *)
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

  (** Convert a unary operator to a pretty-string *)
  val string_of_unop : unop -> string

  (** Convert a binary operator to a pretty-string *)
  val string_of_binop : binop -> string

  (** Convert a comparison operator to a pretty-string *)
  val string_of_compop : compop -> string

  (** Convert a normalised bytecode instruction to a pretty string *)
  val to_string : t -> string

  (** Return code pointers referenced by the given instructions. Code
      pointers ar indexes in the code represented by an array of
      instructions *)
  val get_ptrs : t -> int list
end

(** Normalised version of Code (see Normalised_instr) *)
module Normalised_code : sig
  (** Type of a normalised code segment: an array of normalised
      instructions. Code pointers contained in instructions are
      indexes in the embedding code array *)
  type t = Normalised_instr.t array

  (** Group instructions into connected components, separating
      function bodies and top-level code. See the similar function
      Code.kosaraju for more details *)
  val kosaraju : t -> int array

  (** Pretty-print normalised code *)
  val print : Data.t -> Symb.t -> Prim.t -> out_channel -> t -> unit

  (** Convert standard bytecode into normalised code *)
  val of_code : Code.t -> t

  (** Convert normalised code into standard bytecode *)
  val to_code : t -> Code.t
end

(** Interpreters *)

(** Bytecode interpreter written in OCaml supporting callbacks,
    closure exchange and cross exceptions raise/catch *)
module Interp : sig
  (** [eval globals code prim] interpretes the given bytecode using
      the global array [globals] and the primitive map [prim]. If an
      exception escapes from the bytecode, it is raised through
      eval *)
  val eval : Obj.t array -> Code.t -> Obj.t array -> unit

  (** Interprete the given bytecode file in the current environment.
      Support callbacks, closure exchange and cross exceptions
      raise/catch. If an exception escapes from the bytecode, it is
      raised through eval. Be aware of side effects provides by the C
      runtime which are shared between the current program and the
      bytecode program. For example: the bytecode at_exit function
      overload the current at_exit *)
  val eval_bytefile : Bytefile.t -> unit
end

(** Normalised version of Interp: interprete normalised code *)
module Normalised_interp : sig
  (** Same as Interp.eval but work on normalised code *)
  val eval : Obj.t array -> Normalised_code.t -> Obj.t array -> unit

  (** Same as Interp.eval_bytecode but start to convert standard
      bytecode into normalised code before interpreting it *)
  val eval_bytefile : Bytefile.t -> unit
end
