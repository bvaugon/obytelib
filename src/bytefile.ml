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

(***)

let read_vmcmd ic =
  seek_in ic 0;
  let line = input_line ic in
  let len = String.length line in
  let error () = fail "invalid first line, virtual machine path not found" in
  if len < 3 || line.[0] <> '#' || line.[1] <> '!' then (None, None) else
    let ind = ref 2 in
    while !ind < len && line.[!ind] = ' ' do incr ind done;
    if !ind = len then error ();
    let path_start = !ind in
    while !ind < len && line.[!ind] <> ' ' do incr ind done;
    let path_stop = !ind in
    let path = String.sub line path_start (path_stop - path_start) in
    while !ind < len && line.[!ind] = ' ' do incr ind done;
    let arg_start = !ind in
    if arg_start = len then (Some path, None) else (
      ind := len - 1;
      while line.[!ind] = ' ' do decr ind done;
      let arg_stop = !ind + 1 in
      let arg = String.sub line arg_start (arg_stop - arg_start) in
      (Some path, Some arg)
    )

let write_vmcmd oc vmpath vmarg =
  match vmpath with None -> () | Some path ->
    Printf.fprintf oc "#! %s" path;
    match vmarg with None -> () | Some arg -> Printf.fprintf oc " %s" arg
  
let read file_name =
  let ic =
    try open_in_bin file_name
    with _ -> fail "fail to open file %S for reading" file_name in
  try
    let (vmpath, vmarg) = read_vmcmd ic in
    let version = Version.read ic in
    let index = Index.read ic in
    let extra = Extra.read index ic in
    let prim = Prim.read index ic in
    let data = Data.read index ic in
    let code = Code.read version index ic in
    let dlpt = Dlpt.read index ic in
    let dlls = Dlls.read index ic in
    let crcs = Crcs.read index ic in
    let dbug = Dbug.read index ic in
    let symb = Symb.read index ic in
    close_in ic;
    { version; vmpath; vmarg; index; extra; data;
      prim; code; dlpt; dlls; crcs; dbug; symb }
  with
  | Failure msg ->
    close_in ic;
    fail "file %S is not a valid bytecode file (%s)" file_name msg
  | exn ->
    close_in ic;
    fail "fail to read bytecode file %S (internal error: %s)"
      file_name (Printexc.to_string exn)

(***)

let write file_name version ?vmpath ?vmarg
    ?(extra=Extra.empty) ?(dlpt=Dlpt.empty) ?(dlls=Dlls.empty)
    ?(crcs=Crcs.empty) ?(dbug=Dbug.empty) ?(symb=Symb.empty) data prim code =
  let oflags = [ Open_wronly; Open_creat; Open_trunc; Open_binary ] in
  let oc =
    try open_out_gen oflags 0o751 file_name
    with _ -> fail "fail to open file %S for writting" file_name in
  let write_section section writer xxx =
    let offset = pos_out oc in
    let () = writer oc xxx in
    let length = pos_out oc - offset in
    Index.({ section; offset; length }) in
  try
    write_vmcmd oc vmpath vmarg;
    Extra.write oc extra;
    let i0 = write_section Section.CODE (Code.write version) code in
    let i1 = write_section Section.PRIM Prim.write prim in
    let i2 = write_section Section.DATA Data.write data in
    let i3 = write_section Section.DLPT Dlpt.write dlpt in
    let i4 = write_section Section.DLLS Dlls.write dlls in
    let i5 = write_section Section.CRCS Crcs.write crcs in
    let i6 = write_section Section.DBUG Dbug.write dbug in
    let i7 = write_section Section.SYMB Symb.write symb in
    let index = [ i0; i1; i2; i3; i4; i5; i6; i7 ] in
    Index.write oc index;
    Version.write oc version;
    close_out oc;
  with
  | Failure msg ->
    close_out oc;
    fail "fail to write bytecode file %S (%s)" file_name msg
  | exn ->
    close_out oc;
    fail "fail to write bytecode file %S (internal error: %s)" file_name (Printexc.to_string exn)
    

(***)

let print oc { version; vmpath; vmarg; index; extra; data;
               prim; code; dlpt; dlls; crcs; dbug; symb } =
  (match vmpath with
    | None -> Printf.fprintf oc "VM path    = \n";
    | Some path -> Printf.fprintf oc "VM path    = %S\n" path);
  (match vmarg with
    | None -> Printf.fprintf oc "VM arg     = \n"
    | Some arg -> Printf.fprintf oc "VM arg     = %S\n" arg);
  Printf.fprintf oc "Version    = %s\n" (Version.to_string version);
  Printf.fprintf oc "Extra size = %d\n" (Extra.size extra);
  Printf.fprintf oc "\n########> INDEX\n\n";
  Index.print oc index;
  if dlpt <> Dlpt.empty then (
    Printf.fprintf oc "\n########> DLPT\n\n";
    Dlpt.print oc dlpt;
  );
  if dlls <> Dlls.empty then (
    Printf.fprintf oc "\n########> DLLS\n\n";
    Dlls.print oc dlls;
  );
  if crcs <> Crcs.empty then (
    Printf.fprintf oc "\n########> CRCS\n\n";
    Crcs.print oc crcs;
  );
  if dbug <> Dbug.empty then (
    Printf.fprintf oc "\n########> DBUG\n\n";
    Dbug.print oc dbug;
  );
  if symb <> Symb.empty then (
    Printf.fprintf oc "\n########> SYMB\n\n";
    Symb.print oc symb;
  );
  Printf.fprintf oc "\n########> DATA\n\n";
  Data.print symb oc data;
  Printf.fprintf oc "\n########> PRIM\n\n";
  Prim.print oc prim;
  Printf.fprintf oc "\n########> CODE\n\n";
  Normalised_code.print data symb prim oc (Normalised_code.of_code code)

(***)
