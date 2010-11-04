(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* $Id: pervasives.ml,v 1.81 2006/11/17 08:34:01 weis Exp $ *)

(* type 'a option = None | Some of 'a *)

(* Exceptions *)

external raise : exn -> 'a = "%raise"

let failwith s = raise(Failure s)
let invalid_arg s = raise(Invalid_argument s)

exception Exit

(* Comparisons *)

external (=) : 'a -> 'a -> bool = "%equal"
external (<>) : 'a -> 'a -> bool = "%notequal"
external (<) : 'a -> 'a -> bool = "%lessthan"
external (>) : 'a -> 'a -> bool = "%greaterthan"
external (<=) : 'a -> 'a -> bool = "%lessequal"
external (>=) : 'a -> 'a -> bool = "%greaterequal"
external compare: 'a -> 'a -> int = "%compare"

let min x y = if x <= y then x else y
let max x y = if x >= y then x else y

external (==) : 'a -> 'a -> bool = "%eq"
external (!=) : 'a -> 'a -> bool = "%noteq"

(* Boolean operations *)

external not : bool -> bool = "%boolnot"
external (&) : bool -> bool -> bool = "%sequand"
external (&&) : bool -> bool -> bool = "%sequand"
external (or) : bool -> bool -> bool = "%sequor"
external (||) : bool -> bool -> bool = "%sequor"

(* Integer operations *)

external (~-) : int -> int = "%negint"
external succ : int -> int = "%succint"
external pred : int -> int = "%predint"
external (+) : int -> int -> int = "%addint"
external (-) : int -> int -> int = "%subint"
external ( * ) : int -> int -> int = "%mulint"
external (/) : int -> int -> int = "%divint"
external (mod) : int -> int -> int = "%modint"

let abs x = if x >= 0 then x else -x

external (land) : int -> int -> int = "%andint"
external (lor) : int -> int -> int = "%orint"
external (lxor) : int -> int -> int = "%xorint"

let lnot x = x lxor (-1)

external (lsl) : int -> int -> int = "%lslint"
external (lsr) : int -> int -> int = "%lsrint"
external (asr) : int -> int -> int = "%asrint"

let min_int = 1 lsl (if 1 lsl 31 = 0 then 30 else 62)
let max_int = min_int - 1

(* Floating-point operations *)

external (~-.) : float -> float = "%negfloat"
external (+.) : float -> float -> float = "%addfloat"
external (-.) : float -> float -> float = "%subfloat"
external ( *. ) : float -> float -> float = "%mulfloat"
external (/.) : float -> float -> float = "%divfloat"
external ( ** ) : float -> float -> float = "caml_power_float" "pow" "float"
external exp : float -> float = "caml_exp_float" "exp" "float"
external acos : float -> float = "caml_acos_float" "acos" "float"
external asin : float -> float = "caml_asin_float" "asin" "float"
external atan : float -> float = "caml_atan_float" "atan" "float"
external atan2 : float -> float -> float = "caml_atan2_float" "atan2" "float"
external cos : float -> float = "caml_cos_float" "cos" "float"
external cosh : float -> float = "caml_cosh_float" "cosh" "float"
external log : float -> float = "caml_log_float" "log" "float"
external log10 : float -> float = "caml_log10_float" "log10" "float"
external sin : float -> float = "caml_sin_float" "sin" "float"
external sinh : float -> float = "caml_sinh_float" "sinh" "float"
external sqrt : float -> float = "caml_sqrt_float" "sqrt" "float"
external tan : float -> float = "caml_tan_float" "tan" "float"
external tanh : float -> float = "caml_tanh_float" "tanh" "float"
external ceil : float -> float = "caml_ceil_float" "ceil" "float"
external floor : float -> float = "caml_floor_float" "floor" "float"
external abs_float : float -> float = "%absfloat"
external mod_float : float -> float -> float = "caml_fmod_float" "fmod" "float"
external frexp : float -> float * int = "caml_frexp_float"
external ldexp : float -> int -> float = "caml_ldexp_float"
external modf : float -> float * float = "caml_modf_float"
external float : int -> float = "%floatofint"
external float_of_int : int -> float = "%floatofint"
external truncate : float -> int = "%intoffloat"
external int_of_float : float -> int = "%intoffloat"
external float_of_bits : int64 -> float = "caml_int64_float_of_bits"
let infinity =
  float_of_bits 0x7F_F0_00_00_00_00_00_00L
let neg_infinity =
  float_of_bits 0xFF_F0_00_00_00_00_00_00L
let nan =
  float_of_bits 0x7F_F0_00_00_00_00_00_01L
let max_float =
  float_of_bits 0x7F_EF_FF_FF_FF_FF_FF_FFL
let min_float =
  float_of_bits 0x00_10_00_00_00_00_00_00L
let epsilon_float =
  float_of_bits 0x3C_B0_00_00_00_00_00_00L

type fpclass =
    FP_normal
  | FP_subnormal
  | FP_zero
  | FP_infinite
  | FP_nan
external classify_float: float -> fpclass = "caml_classify_float"

(* String operations -- more in module String *)

external string_length : string -> int = "%string_length"
external string_create: int -> string = "caml_create_string"
external string_blit : string -> int -> string -> int -> int -> unit
                     = "caml_blit_string" "noalloc"

let (^) s1 s2 =
  let l1 = string_length s1 and l2 = string_length s2 in
  let s = string_create (l1 + l2) in
  string_blit s1 0 s 0 l1;
  string_blit s2 0 s l1 l2;
  s

(* Character operations -- more in module Char *)

external int_of_char : char -> int = "%identity"
external unsafe_char_of_int : int -> char = "%identity"
let char_of_int n =
  if n < 0 || n > 255 then invalid_arg "char_of_int" else unsafe_char_of_int n

(* Unit operations *)

external ignore : 'a -> unit = "%ignore"

(* Pair operations *)

external fst : 'a * 'b -> 'a = "%field0"
external snd : 'a * 'b -> 'b = "%field1"

(* String conversion functions *)

external format_int: string -> int -> string = "caml_format_int"
external format_float: string -> float -> string = "caml_format_float"

let string_of_bool b =
  if b then "true" else "false"
let bool_of_string = function
  | "true" -> true
  | "false" -> false
  | _ -> invalid_arg "bool_of_string"

let string_of_int n =
  format_int "%d" n

external int_of_string : string -> int = "caml_int_of_string"

module String = struct
  external get : string -> int -> char = "%string_safe_get"
  external set : string -> int -> char -> unit = "%string_safe_set"
end

let valid_float_lexem s =
  let l = string_length s in
  let rec loop i =
    if i >= l then s ^ "." else
    match s.[i] with
    | '0' .. '9' | '-' -> loop (i+1)
    | _ -> s
  in
  loop 0
;;

let string_of_float f = valid_float_lexem (format_float "%.12g" f);;

external float_of_string : string -> float = "caml_float_of_string"

(* List operations -- more in module List *)

let rec (@) l1 l2 =
  match l1 with
    [] -> l2
  | hd :: tl -> hd :: (tl @ l2)

(* I/O operations *)

type in_channel
type out_channel

let open_descriptor_out _ = failwith "not implemented in obrowser"
let open_descriptor_in _ = failwith "not implemented in obrowser"

let stdin = Obj.magic 0
let stdout = Obj.magic 0
let stderr = Obj.magic 0

(* General output functions *)

type open_flag =
    Open_rdonly | Open_wronly | Open_append
  | Open_creat | Open_trunc | Open_excl
  | Open_binary | Open_text | Open_nonblock

let open_desc _ _ _ = failwith "not implemented in obrowser"
let open_out_gen mode perm name = failwith "not implemented in obrowser"
let open_out name = failwith "not implemented in obrowser"
let open_out_bin name = failwith "not implemented in obrowser"
let flush _ =  failwith "not implemented in obrowser"
let out_channels_list _ = failwith "not implemented in obrowser"
let flush_all () = failwith "not implemented in obrowser"
let unsafe_output _ _ _ _ = failwith "not implemented in obrowser"
let output_char _ _ = failwith "not implemented in obrowser"
let output_string oc s = failwith "not implemented in obrowser"
let output oc s ofs len = failwith "not implemented in obrowser"
let output_byte _ _ = failwith "not implemented in obrowser"
let  output_binary_int _ _ = failwith "not implemented in obrowser"
let marshal_to_channel _ _ _ = failwith "not implemented in obrowser"
let output_value _ _ = failwith "not implemented in obrowser"
let seek_out _ _ = failwith "not implemented in obrowser"
let pos_out _ = failwith "not implemented in obrowser"
let out_channel_length _ = failwith "not implemented in obrowser"
let close_out_channel _ = failwith "not implemented in obrowser"
let close_out _ = failwith "not implemented in obrowser"
let close_out_noerr _ = failwith "not implemented in obrowser"
let set_binary_mode_out _ _  = failwith "not implemented in obrowser"

(* General input functions *)

let open_in_gen _ _ _ =  failwith "not implemented in obrowser"
let open_in _ =  failwith "not implemented in obrowser"
let open_in_bin _ =  failwith "not implemented in obrowser"
let input_char _ =  failwith "not implemented in obrowser"
let unsafe_input _ _ _ _ = failwith "not implemented in obrowser"
let input _ _ _ _ = failwith "not implemented in obrowser"
let rec unsafe_really_input _ _ _ _ = failwith "not implemented in obrowser"
let really_input _ _ _ _ = failwith "not implemented in obrowser"
let input_scan_line _ = failwith "not implemented in obrowser"
let input_line _ = failwith "not implemented in obrowser"

let input_byte _ = failwith "not implemented in obrowser"
let input_binary_int _ = failwith "not implemented in obrowser"
let input_value _ = failwith "not implemented in obrowser"
let seek_in _ _ = failwith "not implemented in obrowser"
let pos_in _ = failwith "not implemented in obrowser"
let in_channel_length _ = failwith "not implemented in obrowser"
let close_in _ = failwith "not implemented in obrowser"
let close_in_noerr _ = failwith "not implemented in obrowser"
let set_binary_mode_in _ _ = failwith "not implemented in obrowser"

(* Output functions on standard output *)

let print_char c = JSOO_basic_io.write (let s = string_create 1 in s.[0] <- c ; s)
let print_string s = JSOO_basic_io.write s
let print_int i = JSOO_basic_io.write (string_of_int i)
let print_float f = JSOO_basic_io.write (string_of_float f)
let print_endline s =
  print_string s; print_char '\n'
let print_newline () = print_char '\n'

(* Output functions on standard error *)

let prerr_char c = JSOO_basic_io.write (let s = string_create 1 in s.[0] <- c ; s)
let prerr_string s = JSOO_basic_io.write s
let prerr_int i = JSOO_basic_io.write (string_of_int i)
let prerr_float f = JSOO_basic_io.write (string_of_float f)
let prerr_endline s =
  prerr_string s; prerr_char '\n'
let prerr_newline () = prerr_char '\n'

(* Input functions on standard input *)

let read_line () = failwith "not implemented in obrowser"
let read_int () = failwith "not implemented in obrowser"
let read_float () = failwith "not implemented in obrowser"

(* Operations on large files *)

module LargeFile =
  struct
    let seek_out _ _ = failwith "not implemented in obrowser"
    let pos_out _ = failwith "not implemented in obrowser"
    let out_channel_length _ = failwith "not implemented in obrowser"
    let seek_in _ _ = failwith "not implemented in obrowser"
    let pos_in _ = failwith "not implemented in obrowser"
    let in_channel_length _ = failwith "not implemented in obrowser"
  end

(* References *)

type 'a ref = { mutable contents: 'a }
external ref: 'a -> 'a ref = "%makemutable"
external (!): 'a ref -> 'a = "%field0"
external (:=): 'a ref -> 'a -> unit = "%setfield0"
external incr: int ref -> unit = "%incr"
external decr: int ref -> unit = "%decr"

(* Formats *)
type ('a, 'b, 'c, 'd) format4 = ('a, 'b, 'c, 'c, 'c, 'd) format6 

type ('a, 'b, 'c) format = ('a, 'b, 'c, 'c) format4

external format_of_string :
 ('a, 'b, 'c, 'd, 'e, 'f) format6 ->
 ('a, 'b, 'c, 'd, 'e, 'f) format6 = "%identity"

external format_to_string :
 ('a, 'b, 'c, 'd, 'e, 'f) format6 -> string = "%identity"
external string_to_format :
 string -> ('a, 'b, 'c, 'd, 'e, 'f) format6 = "%identity"

let (( ^^ ) :
      ('a, 'b, 'c, 'd, 'e, 'f) format6 ->
      ('f, 'b, 'c, 'e, 'g, 'h) format6 ->
      ('a, 'b, 'c, 'd, 'g, 'h) format6) =
  fun fmt1 fmt2 ->
    string_to_format (format_to_string fmt1 ^ format_to_string fmt2);;

let string_of_format fmt =
  let s = format_to_string fmt in
  let l = string_length s in
  let r = string_create l in
  string_blit s 0 r 0 l;
  r

(* Miscellaneous *)

external sys_exit : int -> 'a = "caml_sys_exit"

let exit_function = ref (fun () -> ())

let at_exit f =
  let g = !exit_function in
  exit_function := (fun () -> f(); g())

let do_at_exit () = (!exit_function) ()

let exit retcode =
  do_at_exit ();
  sys_exit retcode

external register_named_value : string -> 'a -> unit
                              = "caml_register_named_value"

let _ = register_named_value "Pervasives.do_at_exit" do_at_exit
