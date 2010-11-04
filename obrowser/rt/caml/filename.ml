(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*          Xavier Leroy and Damien Doligez, INRIA Rocquencourt        *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* $Id: filename.ml,v 1.41.12.1 2008/11/20 18:36:52 doligez Exp $ *)

let generic_quote quotequote s =
  let l = String.length s in
  let b = Buffer.create (l + 20) in
  Buffer.add_char b '\'';
  for i = 0 to l - 1 do
    if s.[i] = '\''
    then Buffer.add_string b quotequote
    else Buffer.add_char b  s.[i]
  done;
  Buffer.add_char b '\'';
  Buffer.contents b

let generic_basename rindex_dir_sep current_dir_name name =
  let raw_name =
    try
      let p = rindex_dir_sep name + 1 in
      String.sub name p (String.length name - p)
    with Not_found ->
      name
  in
  if raw_name = "" then current_dir_name else raw_name

let generic_dirname rindex_dir_sep current_dir_name dir_sep name =
  try
    match rindex_dir_sep name with
      0 -> dir_sep
    | n -> String.sub name 0 n
  with Not_found ->
    current_dir_name

module Unix = struct
  let current_dir_name = "."
  let parent_dir_name = ".."
  let dir_sep = "/"
  let is_dir_sep s i = s.[i] = '/'
  let rindex_dir_sep s = String.rindex s '/'
  let is_relative n = String.length n < 1 || n.[0] <> '/';;
  let is_implicit n =
    is_relative n
    && (String.length n < 2 || String.sub n 0 2 <> "./")
    && (String.length n < 3 || String.sub n 0 3 <> "../")
  let check_suffix name suff =
    String.length name >= String.length suff &&
    String.sub name (String.length name - String.length suff)
                    (String.length suff) = suff
  let temp_dir_name =
    try Sys.getenv "TMPDIR" with Not_found -> "/tmp"
  let quote = generic_quote "'\\''"
  let basename = generic_basename rindex_dir_sep current_dir_name
  let dirname = generic_dirname rindex_dir_sep current_dir_name dir_sep
end

module Win32 = struct
  let current_dir_name = "."
  let parent_dir_name = ".."
  let dir_sep = "\\"
  let is_dir_sep s i = let c = s.[i] in c = '/' || c = '\\' || c = ':'
  let rindex_dir_sep s =
    let rec pos i =
      if i < 0 then raise Not_found
      else if is_dir_sep s i then i
      else pos (i - 1)
    in pos (String.length s - 1)
  let is_relative n =
    (String.length n < 1 || n.[0] <> '/')
    && (String.length n < 1 || n.[0] <> '\\')
    && (String.length n < 2 || n.[1] <> ':')
  let is_implicit n =
    is_relative n
    && (String.length n < 2 || String.sub n 0 2 <> "./")
    && (String.length n < 2 || String.sub n 0 2 <> ".\\")
    && (String.length n < 3 || String.sub n 0 3 <> "../")
    && (String.length n < 3 || String.sub n 0 3 <> "..\\")
  let check_suffix name suff =
   String.length name >= String.length suff &&
   (let s = String.sub name (String.length name - String.length suff)
                            (String.length suff) in
    String.lowercase s = String.lowercase suff)
  let temp_dir_name =
    try Sys.getenv "TEMP" with Not_found -> "."
  let quote s =
    let l = String.length s in
    let b = Buffer.create (l + 20) in
    Buffer.add_char b '\"';
    let rec loop i =
      if i = l then Buffer.add_char b '\"' else
      match s.[i] with
      | '\"' -> loop_bs 0 i;
      | '\\' -> loop_bs 0 i;
      | c    -> Buffer.add_char b c; loop (i+1);
    and loop_bs n i =
      if i = l then begin
        Buffer.add_char b '\"';
        add_bs n;
      end else begin
        match s.[i] with
        | '\"' -> add_bs (2*n+1); Buffer.add_char b '\"'; loop (i+1);
        | '\\' -> loop_bs (n+1) (i+1);
        | c    -> add_bs n; loop i
      end
    and add_bs n = for j = 1 to n do Buffer.add_char b '\\'; done
    in
    loop 0;
    Buffer.contents b
  let has_drive s =
    let is_letter = function
      | 'A' .. 'Z' | 'a' .. 'z' -> true
      | _ -> false
    in
    String.length s >= 2 && is_letter s.[0] && s.[1] = ':'
  let drive_and_path s =
    if has_drive s
    then (String.sub s 0 2, String.sub s 2 (String.length s - 2))
    else ("", s)
  let dirname s =
    let (drive, path) = drive_and_path s in
    let dir = generic_dirname rindex_dir_sep current_dir_name dir_sep path in
    drive ^ dir
  let basename s =
    let (drive, path) = drive_and_path s in
    generic_basename rindex_dir_sep current_dir_name path
end

module Cygwin = struct
  let current_dir_name = "."
  let parent_dir_name = ".."
  let dir_sep = "/"
  let is_dir_sep = Win32.is_dir_sep
  let rindex_dir_sep = Win32.rindex_dir_sep
  let is_relative = Win32.is_relative
  let is_implicit = Win32.is_implicit
  let check_suffix = Win32.check_suffix
  let temp_dir_name = Unix.temp_dir_name
  let quote = Unix.quote
  let basename = generic_basename rindex_dir_sep current_dir_name
  let dirname = generic_dirname rindex_dir_sep current_dir_name dir_sep
end

let (current_dir_name, parent_dir_name, dir_sep, is_dir_sep, rindex_dir_sep,
     is_relative, is_implicit, check_suffix, temp_dir_name, quote, basename,
     dirname) =
  match Sys.os_type with
    "Unix" ->
      (Unix.current_dir_name, Unix.parent_dir_name, Unix.dir_sep,
       Unix.is_dir_sep, Unix.rindex_dir_sep,
       Unix.is_relative, Unix.is_implicit, Unix.check_suffix,
       Unix.temp_dir_name, Unix.quote, Unix.basename, Unix.dirname)
  | "Win32" ->
      (Win32.current_dir_name, Win32.parent_dir_name, Win32.dir_sep,
       Win32.is_dir_sep, Win32.rindex_dir_sep,
       Win32.is_relative, Win32.is_implicit, Win32.check_suffix,
       Win32.temp_dir_name, Win32.quote, Win32.basename, Win32.dirname)
  | "Cygwin" ->
      (Cygwin.current_dir_name, Cygwin.parent_dir_name, Cygwin.dir_sep,
       Cygwin.is_dir_sep, Cygwin.rindex_dir_sep,
       Cygwin.is_relative, Cygwin.is_implicit, Cygwin.check_suffix,
       Cygwin.temp_dir_name, Cygwin.quote, Cygwin.basename, Cygwin.dirname)
  | _ -> assert false

let concat dirname filename =
  let l = String.length dirname in
  if l = 0 || is_dir_sep dirname (l-1)
  then dirname ^ filename
  else dirname ^ dir_sep ^ filename

let chop_suffix name suff =
  let n = String.length name - String.length suff in
  if n < 0 then invalid_arg "Filename.chop_suffix" else String.sub name 0 n

let chop_extension name =
  let rec search_dot i =
    if i < 0 || is_dir_sep name i then invalid_arg "Filename.chop_extension"
    else if name.[i] = '.' then String.sub name 0 i
    else search_dot (i - 1) in
  search_dot (String.length name - 1)

external open_desc: string -> open_flag list -> int -> int = "caml_sys_open"
external close_desc: int -> unit = "caml_sys_close"

let prng = Random.State.make_self_init ();;

let temp_file_name prefix suffix =
  let rnd = (Random.State.bits prng) land 0xFFFFFF in
  concat temp_dir_name (Printf.sprintf "%s%06x%s" prefix rnd suffix)
;;

let temp_file prefix suffix =
  let rec try_name counter =
    let name = temp_file_name prefix suffix in
    try
      close_desc(open_desc name [Open_wronly; Open_creat; Open_excl] 0o600);
      name
    with Sys_error _ as e ->
      if counter >= 1000 then raise e else try_name (counter + 1)
  in try_name 0

let open_temp_file ?(mode = [Open_text]) prefix suffix =
  let rec try_name counter =
    let name = temp_file_name prefix suffix in
    try
      (name,
       open_out_gen (Open_wronly::Open_creat::Open_excl::mode) 0o600 name)
    with Sys_error _ as e ->
      if counter >= 1000 then raise e else try_name (counter + 1)
  in try_name 0
