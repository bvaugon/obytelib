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

type t = CODE | DLPT | DLLS | PRIM | DATA | SYMB | CRCS | DBUG

let of_string s = match s with
  | "CODE" -> CODE | "DLPT" -> DLPT | "DLLS" -> DLLS | "PRIM" -> PRIM
  | "DATA" -> DATA | "SYMB" -> SYMB | "CRCS" -> CRCS | "DBUG" -> DBUG
  | _      -> Tools.fail "unknown section %S" s

let to_string ty = match ty with
  | CODE -> "CODE" | DLPT -> "DLPT" | DLLS -> "DLLS" | PRIM -> "PRIM"
  | DATA -> "DATA" | SYMB -> "SYMB" | CRCS -> "CRCS" | DBUG -> "DBUG"
