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

type t

type legacy =
  { stamp: int; name: string; mutable flags: int }
              
type recent =
  | Local  of string * int       (* { name: string; stamp: int } *)
  | Scoped of string * int * int (* { name: string; stamp: int; scope: int } *)
  | Global of string
  | Predef of string * int       (* { name: string; stamp: int } *)
[@@warning "-37"]

type generic =
  | Legacy of legacy
  | Recent of recent

(******************************************************************************)

module Check = struct
  let error () =
    failwith "unrecognized symbol identifier"
  
  let block obj tag size =
    if not (Obj.is_block obj) then error ();
    if not (Obj.tag obj = tag) then error ();
    if not (Obj.size obj = size) then error ()

  let int obj =
    if not (Obj.is_int obj) then error ()

  let string obj =
    if not (Obj.is_block obj) then error ();
    if not (Obj.tag obj = Obj.string_tag) then error ()
end

(******************************************************************************)

let coerce (ident : t) : generic =
  let obj = Obj.repr ident in
  if not (Obj.is_block obj) then Check.error ();
  match Obj.tag obj, Obj.size obj with
  | 0, 3 ->
    Check.int (Obj.field obj 0);
    Check.string (Obj.field obj 1);
    Check.int (Obj.field obj 2);
    Legacy (Obj.obj obj)
  | 0, 2 ->
    Check.string (Obj.field obj 0);
    Check.int (Obj.field obj 1);
    Recent (Obj.obj obj)
  | 1, 3 ->
    Check.string (Obj.field obj 0);
    Check.int (Obj.field obj 1);
    Check.int (Obj.field obj 2);
    Recent (Obj.obj obj)
  | 2, 1 ->
    Check.string (Obj.field obj 0);
    Recent (Obj.obj obj)
  | 3, 2 ->
    Check.string (Obj.field obj 0);
    Check.int (Obj.field obj 1);
    Recent (Obj.obj obj)
  | _ ->
    Check.error ()

(******************************************************************************)

let print oc ident =
  match coerce ident with
  | Legacy { stamp; name; flags }        -> Printf.fprintf oc "{ stamp = %d; name = %S; flags = %d }" stamp name flags
  | Recent (Local  (name, stamp))        -> Printf.fprintf oc "Local { name = %S; stmp = %d }" name stamp
  | Recent (Scoped (name, stamp, scope)) -> Printf.fprintf oc "Scoped { name = %S; stamp = %d; scope = %d }" name stamp scope
  | Recent (Global name)                 -> Printf.fprintf oc "Global %S" name
  | Recent (Predef (name, stamp))        -> Printf.fprintf oc "Predef { name = %S; stamp = %d }" name stamp

(***)

let name ident =
  match coerce ident with
  | Legacy { name; _ }           -> name
  | Recent (Local (name, _))     -> name
  | Recent (Scoped (name, _, _)) -> name
  | Recent (Global name)         -> name
  | Recent (Predef (name, _))    -> name

(***)

let check version ident =
  let expected =
    match version with
    | Version.V008 | Version.V010 | Version.V011 | Version.V022
    | Version.V023 ->
      `Legacy
    | Version.V025 | Version.V026 | Version.V027 | Version.V028
    | Version.V029 | Version.V030 | Version.V031 ->
      `Recent in
  match expected, coerce ident with
  | `Legacy, Legacy _ | `Recent, Recent _ -> ()
  | `Recent, Legacy _ | `Legacy, Recent _ -> failwith "bytecode file version is incompatible with symbol identifiers"

let check_opt version ident_opt =
  match ident_opt with
  | None -> ()
  | Some ident -> check version ident

(******************************************************************************)
