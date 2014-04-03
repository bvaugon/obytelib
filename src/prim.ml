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

include Strsec.Make(struct let section = Section.PRIM end)

(***)

(*type raw_backtrace*)
type backtrace
type dll_address
type dll_handle
type dll_mode
type lex_tables
type parse_tables
type parser_env
type parser_input
type parser_output
type terminfo_status

external caml_abs_float : float -> float = "caml_abs_float"
external caml_acos_float : float -> float = "caml_acos_float"
external caml_add_float : float -> float -> float = "caml_add_float"
external caml_alloc_dummy : int -> Obj.t = "caml_alloc_dummy"
external caml_alloc_dummy_float : int -> Obj.t = "caml_alloc_dummy_float"
external caml_array_append : 'a array -> 'a array -> 'a array = "caml_array_append"
external caml_array_blit : 'a array -> int -> 'a array -> int -> int -> unit = "caml_array_blit"
external caml_array_concat : 'a array list -> 'a array = "caml_array_concat"
external caml_array_get : 'a array -> int -> 'a = "caml_array_get"
external caml_array_get_addr : 'a array -> int -> 'a = "caml_array_get_addr"
external caml_array_get_float : float array -> int -> float = "caml_array_get_float"
external caml_array_set : 'a array -> int -> 'a -> unit = "caml_array_set"
external caml_array_set_addr : 'a array -> int -> 'a -> unit = "caml_array_set_addr"
external caml_array_set_float : float array -> int -> float -> unit = "caml_array_set_float"
external caml_array_sub : 'a array -> int -> int -> 'a array = "caml_array_sub"
external caml_array_unsafe_get : 'a array -> int -> 'a = "caml_array_unsafe_get"
external caml_array_unsafe_get_float : float array -> int -> float = "caml_array_unsafe_get_float"
external caml_array_unsafe_set : 'a array -> int -> 'a -> unit = "caml_array_unsafe_set"
external caml_array_unsafe_set_addr : 'a array -> int -> 'a -> unit = "caml_array_unsafe_set_addr"
external caml_array_unsafe_set_float : float array -> int -> float -> unit = "caml_array_unsafe_set_float"
external caml_asin_float : float -> float = "caml_asin_float"
external caml_atan2_float : float -> float -> float = "caml_atan2_float"
external caml_atan_float : float -> float = "caml_atan_float"
external caml_backtrace_status : unit -> bool = "caml_backtrace_status"
external caml_bitvect_test : string -> int -> int = "caml_bitvect_test"
external caml_blit_string : string -> int -> string -> int -> int -> unit = "caml_blit_string"
(*external caml_bswap16 : int -> int = "caml_bswap16"*)
external caml_ceil_float : float -> float = "caml_ceil_float"
external caml_channel_descriptor : out_channel -> int = "caml_channel_descriptor"
external caml_classify_float : float -> fpclass = "caml_classify_float"
external caml_compare : 'a -> 'a -> int = "caml_compare"
(*external caml_convert_raw_backtrace : raw_backtrace -> backtrace option = "caml_convert_raw_backtrace"*)
external caml_copysign_float : float -> float -> float = "caml_copysign_float"
external caml_cos_float : float -> float = "caml_cos_float"
external caml_cosh_float : float -> float = "caml_cosh_float"
external caml_create_string : int -> string = "caml_create_string"
external caml_div_float : float -> float -> float = "caml_div_float"
external caml_dynlink_add_primitive : dll_address -> int = "caml_dynlink_add_primitive"
external caml_dynlink_close_lib : dll_handle -> unit = "caml_dynlink_close_lib"
external caml_dynlink_get_current_libs : unit -> dll_handle array = "caml_dynlink_get_current_libs"
external caml_dynlink_lookup_symbol : dll_handle -> string -> dll_address = "caml_dynlink_lookup_symbol"
external caml_dynlink_open_lib : dll_mode -> string -> dll_handle = "caml_dynlink_open_lib"
(*external caml_ensure_stack_capacity : int -> unit = "caml_ensure_stack_capacity"*) let caml_ensure_stack_capacity _ = ()
external caml_eq_float : float -> float -> bool = "caml_eq_float"
external caml_equal : 'a -> 'a -> bool = "caml_equal"
external caml_exp_float : float -> float = "caml_exp_float"
external caml_expm1_float : float -> float = "caml_expm1_float"
external caml_fill_string : string -> int -> int -> char -> unit = "caml_fill_string"
external caml_final_register : ('a -> unit) -> 'a -> unit = "caml_final_register"
external caml_final_release : unit -> unit = "caml_final_release"
external caml_float_compare : float -> float -> int = "caml_float_compare"
external caml_float_of_int : int -> float = "caml_float_of_int"
external caml_float_of_string : string -> float = "caml_float_of_string"
external caml_floor_float : float -> float = "caml_floor_float"
external caml_fmod_float : float -> float -> float = "caml_fmod_float"
external caml_format_float : string -> float -> string = "caml_format_float"
external caml_format_int : string -> int -> string = "caml_format_int"
external caml_frexp_float : float -> float * int = "caml_frexp_float"
external caml_gc_compaction : unit -> unit = "caml_gc_compaction"
external caml_gc_counters : unit -> (float * float * float) = "caml_gc_counters"
external caml_gc_full_major : unit -> unit = "caml_gc_full_major"
external caml_gc_get : unit -> Gc.control = "caml_gc_get"
external caml_gc_major : unit -> unit = "caml_gc_major"
external caml_gc_major_slice : int -> int = "caml_gc_major_slice"
external caml_gc_minor : unit -> unit = "caml_gc_minor"
external caml_gc_quick_stat : unit -> float = "caml_gc_quick_stat"
external caml_gc_set : Gc.control -> unit = "caml_gc_set"
external caml_gc_stat : unit -> Gc.stat = "caml_gc_stat"
external caml_ge_float : float -> float -> bool = "caml_ge_float"
(*external caml_get_current_callstack : int -> raw_backtrace = "caml_get_current_callstack"*)
(*external caml_get_current_environment : unit -> Obj.t = "caml_get_current_environment"*)
external caml_get_exception_backtrace : unit -> backtrace = "caml_get_exception_backtrace"
(*external caml_get_exception_raw_backtrace : unit -> raw_backtrace = "caml_get_exception_raw_backtrace"*)
external caml_get_global_data : unit -> Obj.t array = "caml_get_global_data"
external caml_get_public_method : CamlinternalOO.obj -> CamlinternalOO.tag -> CamlinternalOO.closure = "caml_get_public_method"
external caml_get_section_table : unit -> (string * Obj.t) list = "caml_get_section_table"
external caml_greaterequal : 'a -> 'a -> bool = "caml_greaterequal"
external caml_greaterthan : 'a -> 'a -> bool = "caml_greaterthan"
external caml_gt_float : float -> float -> bool = "caml_gt_float"
external caml_hash : int -> int -> int -> 'a -> int = "caml_hash"
external caml_hash_univ_param : int -> int -> 'a -> int = "caml_hash_univ_param"
external caml_hypot_float : float -> float -> float = "caml_hypot_float"
external caml_input_value : in_channel -> 'a = "caml_input_value"
external caml_input_value_from_string : string -> int -> 'a = "caml_input_value_from_string"
external caml_install_signal_handler : int -> Sys.signal_behavior -> Sys.signal_behavior = "caml_install_signal_handler"
external caml_int32_add : int32 -> int32 -> int32 = "caml_int32_add"
external caml_int32_and : int32 -> int32 -> int32 = "caml_int32_and"
external caml_int32_bits_of_float : float -> int32 = "caml_int32_bits_of_float"
(*external caml_int32_bswap : int32 -> int32 = "caml_int32_bswap"*)
external caml_int32_compare : int32 -> int32 -> int = "caml_int32_compare"
external caml_int32_div : int32 -> int32 -> int32 = "caml_int32_div"
external caml_int32_float_of_bits : int32 -> float = "caml_int32_float_of_bits"
external caml_int32_format : string -> int32 -> string = "caml_int32_format"
external caml_int32_mod : int32 -> int32 -> int32 = "caml_int32_mod"
external caml_int32_mul : int32 -> int32 -> int32 = "caml_int32_mul"
external caml_int32_neg : int32 -> int32 = "caml_int32_neg"
external caml_int32_of_float : float -> int32 = "caml_int32_of_float"
external caml_int32_of_int : int -> int32 = "caml_int32_of_int"
external caml_int32_of_string : string -> int32 = "caml_int32_of_string"
external caml_int32_or : int32 -> int32 -> int32 = "caml_int32_or"
external caml_int32_shift_left : int32 -> int -> int32 = "caml_int32_shift_left"
external caml_int32_shift_right : int32 -> int -> int32 = "caml_int32_shift_right"
external caml_int32_shift_right_unsigned : int32 -> int -> int32 = "caml_int32_shift_right_unsigned"
external caml_int32_sub : int32 -> int32 -> int32 = "caml_int32_sub"
external caml_int32_to_float : int32 -> float = "caml_int32_to_float"
external caml_int32_to_int : int32 -> int = "caml_int32_to_int"
external caml_int32_xor : int32 -> int32 -> int32 = "caml_int32_xor"
external caml_int64_add : int64 -> int64 -> int64 = "caml_int64_add"
external caml_int64_and : int64 -> int64 -> int64 = "caml_int64_and"
external caml_int64_bits_of_float : float -> int64 = "caml_int64_bits_of_float"
(*external caml_int64_bswap : int64 -> int64 = "caml_int64_bswap"*)
external caml_int64_compare : int64 -> int64 -> int = "caml_int64_compare"
external caml_int64_div : int64 -> int64 -> int64 = "caml_int64_div"
external caml_int64_float_of_bits : int64 -> float = "caml_int64_float_of_bits"
external caml_int64_format : string -> int64 -> string = "caml_int64_format"
external caml_int64_mod : int64 -> int64 -> int64 = "caml_int64_mod"
external caml_int64_mul : int64 -> int64 -> int64 = "caml_int64_mul"
external caml_int64_neg : int64 -> int64 = "caml_int64_neg"
external caml_int64_of_float : float -> int64 = "caml_int64_of_float"
external caml_int64_of_int : int -> int64 = "caml_int64_of_int"
external caml_int64_of_int32 : int32 -> int64 = "caml_int64_of_int32"
external caml_int64_of_nativeint : nativeint -> int64 = "caml_int64_of_nativeint"
external caml_int64_of_string : string -> int64 = "caml_int64_of_string"
external caml_int64_or : int64 -> int64 -> int64 = "caml_int64_or"
external caml_int64_shift_left : int64 -> int -> int64 = "caml_int64_shift_left"
external caml_int64_shift_right : int64 -> int -> int64 = "caml_int64_shift_right"
external caml_int64_shift_right_unsigned : int64 -> int -> int64 = "caml_int64_shift_right_unsigned"
external caml_int64_sub : int64 -> int64 -> int64 = "caml_int64_sub"
external caml_int64_to_float : int64 -> float = "caml_int64_to_float"
external caml_int64_to_int : int64 -> int = "caml_int64_to_int"
external caml_int64_to_int32 : int64 -> int32 = "caml_int64_to_int32"
external caml_int64_to_nativeint : int64 -> nativeint = "caml_int64_to_nativeint"
external caml_int64_xor : int64 -> int64 -> int64 = "caml_int64_xor"
external caml_int_compare : int -> int -> int = "caml_int_compare"
external caml_int_of_float : float -> int = "caml_int_of_float"
external caml_int_of_string : string -> int = "caml_int_of_string"
external caml_invoke_traced_function : Obj.t -> Obj.t -> Obj.t -> Obj.t = "caml_invoke_traced_function"
external caml_is_printable : char -> bool = "caml_is_printable"
external caml_lazy_follow_forward : 'a lazy_t -> 'a = "caml_lazy_follow_forward"
external caml_lazy_make_forward : 'a -> 'a lazy_t = "caml_lazy_make_forward"
external caml_ldexp_float : float -> int -> float = "caml_ldexp_float"
external caml_le_float : float -> float -> bool = "caml_le_float"
external caml_lessequal : 'a -> 'a -> bool = "caml_lessequal"
external caml_lessthan : 'a -> 'a -> bool = "caml_lessthan"
external caml_lex_engine : lex_tables -> int -> Lexing.lexbuf -> int = "caml_lex_engine"
external caml_log10_float : float -> float = "caml_log10_float"
external caml_log1p_float : float -> float = "caml_log1p_float"
external caml_log_float : float -> float = "caml_log_float"
external caml_lt_float : float -> float -> bool = "caml_lt_float"
external caml_make_array : 'a array -> 'a array = "caml_make_array"
external caml_make_vect : int -> 'a -> 'a array = "caml_make_vect"
external caml_marshal_data_size : string -> int -> int = "caml_marshal_data_size"
external caml_md5_chan : in_channel -> int -> Digest.t = "caml_md5_chan"
external caml_md5_string : string -> int -> int -> Digest.t = "caml_md5_string"
external caml_ml_channel_size : in_channel -> int = "caml_ml_channel_size"
external caml_ml_channel_size_64 : in_channel -> int64 = "caml_ml_channel_size_64"
external caml_ml_close_channel : in_channel -> unit = "caml_ml_close_channel"
external caml_ml_flush : out_channel -> unit = "caml_ml_flush"
external caml_ml_flush_partial : out_channel -> int = "caml_ml_flush_partial"
external caml_ml_input : in_channel -> string -> int -> int -> int = "caml_ml_input"
external caml_ml_input_char : in_channel -> char = "caml_ml_input_char"
external caml_ml_input_int : in_channel -> int = "caml_ml_input_int"
external caml_ml_input_scan_line : in_channel -> int = "caml_ml_input_scan_line"
external caml_ml_open_descriptor_in : int -> in_channel = "caml_ml_open_descriptor_in"
external caml_ml_open_descriptor_out : int -> out_channel = "caml_ml_open_descriptor_out"
external caml_ml_out_channels_list : unit -> out_channel list = "caml_ml_out_channels_list"
external caml_ml_output : out_channel -> string -> int -> int -> unit = "caml_ml_output"
external caml_ml_output_char : out_channel -> char -> unit = "caml_ml_output_char"
external caml_ml_output_int : out_channel -> int -> unit = "caml_ml_output_int"
(*external caml_ml_output_partial : out_channel -> string -> int -> int -> int = "caml_ml_output_partial"*)
external caml_ml_pos_in : in_channel -> int = "caml_ml_pos_in"
external caml_ml_pos_in_64 : in_channel -> int64 = "caml_ml_pos_in_64"
external caml_ml_pos_out : out_channel -> int = "caml_ml_pos_out"
external caml_ml_pos_out_64 : out_channel -> int64 = "caml_ml_pos_out_64"
external caml_ml_seek_in : in_channel -> int -> unit = "caml_ml_seek_in"
external caml_ml_seek_in_64 : in_channel -> int64 -> unit = "caml_ml_seek_in_64"
external caml_ml_seek_out : out_channel -> int -> unit = "caml_ml_seek_out"
external caml_ml_seek_out_64 : out_channel -> int64 -> unit = "caml_ml_seek_out_64"
external caml_ml_set_binary_mode : in_channel -> bool -> unit = "caml_ml_set_binary_mode"
external caml_ml_string_length : string -> int = "caml_ml_string_length"
external caml_modf_float : float -> float * float = "caml_modf_float"
external caml_mul_float : float -> float -> float = "caml_mul_float"
external caml_nativeint_add : nativeint -> nativeint -> nativeint = "caml_nativeint_add"
external caml_nativeint_and : nativeint -> nativeint -> nativeint = "caml_nativeint_and"
(*external caml_nativeint_bswap : nativeint -> nativeint = "caml_nativeint_bswap"*)
external caml_nativeint_compare : nativeint -> nativeint -> int = "caml_nativeint_compare"
external caml_nativeint_div : nativeint -> nativeint -> nativeint = "caml_nativeint_div"
external caml_nativeint_format : string -> nativeint -> string = "caml_nativeint_format"
external caml_nativeint_mod : nativeint -> nativeint -> nativeint = "caml_nativeint_mod"
external caml_nativeint_mul : nativeint -> nativeint -> nativeint = "caml_nativeint_mul"
external caml_nativeint_neg : nativeint -> nativeint = "caml_nativeint_neg"
external caml_nativeint_of_float : float -> nativeint = "caml_nativeint_of_float"
external caml_nativeint_of_int : int -> nativeint = "caml_nativeint_of_int"
external caml_nativeint_of_int32 : int32 -> nativeint = "caml_nativeint_of_int32"
external caml_nativeint_of_string : string -> nativeint = "caml_nativeint_of_string"
external caml_nativeint_or : nativeint -> nativeint -> nativeint = "caml_nativeint_or"
external caml_nativeint_shift_left : nativeint -> int -> nativeint = "caml_nativeint_shift_left"
external caml_nativeint_shift_right : nativeint -> int -> nativeint = "caml_nativeint_shift_right"
external caml_nativeint_shift_right_unsigned : nativeint -> int -> nativeint = "caml_nativeint_shift_right_unsigned"
external caml_nativeint_sub : nativeint -> nativeint -> nativeint = "caml_nativeint_sub"
external caml_nativeint_to_float : nativeint -> float = "caml_nativeint_to_float"
external caml_nativeint_to_int : nativeint -> int = "caml_nativeint_to_int"
external caml_nativeint_to_int32 : nativeint -> int32 = "caml_nativeint_to_int32"
external caml_nativeint_xor : nativeint -> nativeint -> nativeint = "caml_nativeint_xor"
external caml_neg_float : float -> float = "caml_neg_float"
external caml_neq_float : float -> float -> bool = "caml_neq_float"
external caml_new_lex_engine : lex_tables -> int -> Lexing.lexbuf -> int = "caml_new_lex_engine"
external caml_notequal : 'a -> 'a -> bool = "caml_notequal"
external caml_obj_add_offset : Obj.t -> int32 -> Obj.t = "caml_obj_add_offset"
external caml_obj_block : int -> int -> Obj.t = "caml_obj_block"
external caml_obj_dup : Obj.t -> Obj.t = "caml_obj_dup"
external caml_obj_is_block : Obj.t -> bool = "caml_obj_is_block"
external caml_obj_set_tag : Obj.t -> int -> unit = "caml_obj_set_tag"
external caml_obj_tag : Obj.t -> int = "caml_obj_tag"
external caml_obj_truncate : Obj.t -> int -> unit = "caml_obj_truncate"
external caml_output_value : out_channel -> 'a -> unit list -> unit = "caml_output_value"
external caml_output_value_to_buffer : string -> int -> int -> 'a -> Marshal.extern_flags list -> int = "caml_output_value_to_buffer"
external caml_output_value_to_string : 'a -> Marshal.extern_flags list -> string = "caml_output_value_to_string"
external caml_parse_engine : parse_tables -> parser_env -> parser_input -> Obj.t -> parser_output = "caml_parse_engine"
external caml_power_float : float -> float -> float = "caml_power_float"
external caml_realloc_global : int -> unit = "caml_realloc_global"
external caml_record_backtrace : bool -> unit = "caml_record_backtrace"
(*external caml_register_code_fragment : string -> int -> string -> unit = "caml_register_code_fragment"*)
external caml_register_named_value : string -> 'a -> unit = "caml_register_named_value"
(*external caml_reify_bytecode : string -> int -> closure = "caml_reify_bytecode"*)
external caml_set_parser_trace : bool -> bool = "caml_set_parser_trace"
external caml_sin_float : float -> float = "caml_sin_float"
external caml_sinh_float : float -> float = "caml_sinh_float"
external caml_sqrt_float : float -> float = "caml_sqrt_float"
external caml_static_alloc : int -> string = "caml_static_alloc"
external caml_static_free : string -> unit = "caml_static_free"
(*external caml_static_release_bytecode : unit -> unit = "caml_static_release_bytecode"*)
external caml_static_resize : string -> int -> string = "caml_static_resize"
external caml_string_compare : string -> string -> int = "caml_string_compare"
external caml_string_equal : string -> string -> bool = "caml_string_equal"
external caml_string_get : string -> int -> char = "caml_string_get"
(*external caml_string_get16 : string -> int -> int = "caml_string_get16"*)
(*external caml_string_get32 : string -> int -> int32 = "caml_string_get32"*)
(*external caml_string_get64 : string -> int -> int64 = "caml_string_get64"*)
external caml_string_greaterequal : string -> string -> bool = "caml_string_greaterequal"
external caml_string_greaterthan : string -> string -> bool = "caml_string_greaterthan"
external caml_string_lessequal : string -> string -> bool = "caml_string_lessequal"
external caml_string_lessthan : string -> string -> bool = "caml_string_lessthan"
external caml_string_notequal : string -> string -> bool = "caml_string_notequal"
external caml_string_set : string -> int -> char -> unit = "caml_string_set"
(*external caml_string_set16 : string -> int -> int -> unit = "caml_string_set16"*)
(*external caml_string_set32 : string -> int -> int32 -> unit = "caml_string_set32"*)
(*external caml_string_set64 : string -> int -> int64 -> unit = "caml_string_set64"*)
external caml_sub_float : float -> float -> float = "caml_sub_float"
external caml_sys_chdir : string -> unit = "caml_sys_chdir"
external caml_sys_close : int -> unit = "caml_sys_close"
(*external caml_sys_const_big_endian : unit -> bool = "caml_sys_const_big_endian"*) let caml_sys_const_big_endian () = Sys.big_endian
(*external caml_sys_const_ostype_cygwin : unit -> bool = "caml_sys_const_ostype_cygwin"*) let caml_sys_const_ostype_cygwin () = Sys.os_type = "Cygwin"
(*external caml_sys_const_ostype_unix : unit -> bool = "caml_sys_const_ostype_unix"*) let caml_sys_const_ostype_unix () = Sys.os_type = "Unix"
(*external caml_sys_const_ostype_win32 : unit -> bool = "caml_sys_const_ostype_win32"*) let caml_sys_const_ostype_win32 () = Sys.os_type = "Win32"
(*external caml_sys_const_word_size : unit -> int = "caml_sys_const_word_size"*) let caml_sys_const_word_size () = Sys.word_size
external caml_sys_exit : int -> 'a = "caml_sys_exit"
external caml_sys_file_exists : string -> bool = "caml_sys_file_exists"
external caml_sys_get_argv : unit -> string * string array = "caml_sys_get_argv"
external caml_sys_get_config : unit -> string * int * bool = "caml_sys_get_config"
external caml_sys_getcwd : unit -> string = "caml_sys_getcwd"
external caml_sys_getenv : string -> string  = "caml_sys_getenv"
external caml_sys_is_directory : string -> bool = "caml_sys_is_directory"
external caml_sys_open : string -> open_flag list -> int -> int = "caml_sys_open"
external caml_sys_random_seed : unit -> int array = "caml_sys_random_seed"
external caml_sys_read_directory : string -> string array = "caml_sys_read_directory"
external caml_sys_remove : string -> unit = "caml_sys_remove"
external caml_sys_rename : string -> string -> unit = "caml_sys_rename"
external caml_sys_system_command : string -> int = "caml_sys_system_command"
external caml_sys_time : unit -> float = "caml_sys_time"
external caml_tan_float : float -> float = "caml_tan_float"
external caml_tanh_float : float -> float = "caml_tanh_float"
external caml_terminfo_backup : int -> unit = "caml_terminfo_backup"
external caml_terminfo_resume : int -> unit = "caml_terminfo_resume"
external caml_terminfo_setup : out_channel -> terminfo_status = "caml_terminfo_setup"
external caml_terminfo_standout : bool -> unit = "caml_terminfo_standout"
external caml_update_dummy : Obj.t -> Obj.t -> Obj.t = "caml_update_dummy"
external caml_weak_blit : 'a Weak.t -> int -> 'a Weak.t -> int -> int -> unit = "caml_weak_blit"
external caml_weak_check : 'a Weak.t -> int -> bool = "caml_weak_check"
external caml_weak_create : int -> 'a Weak.t = "caml_weak_create"
external caml_weak_get : 'a Weak.t -> int -> 'a option = "caml_weak_get"
external caml_weak_get_copy : 'a Weak.t -> int -> 'a option = "caml_weak_get_copy"
external caml_weak_set : 'a Weak.t -> int -> 'a option -> unit = "caml_weak_set"

let find_prim name = match name with
  | "caml_abs_float"                      -> Obj.repr caml_abs_float
  | "caml_acos_float"                     -> Obj.repr caml_acos_float
  | "caml_add_float"                      -> Obj.repr caml_add_float
  | "caml_alloc_dummy"                    -> Obj.repr caml_alloc_dummy
  | "caml_alloc_dummy_float"              -> Obj.repr caml_alloc_dummy_float
  | "caml_array_append"                   -> Obj.repr caml_array_append
  | "caml_array_blit"                     -> Obj.repr caml_array_blit
  | "caml_array_concat"                   -> Obj.repr caml_array_concat
  | "caml_array_get"                      -> Obj.repr caml_array_get
  | "caml_array_get_addr"                 -> Obj.repr caml_array_get_addr
  | "caml_array_get_float"                -> Obj.repr caml_array_get_float
  | "caml_array_set"                      -> Obj.repr caml_array_set
  | "caml_array_set_addr"                 -> Obj.repr caml_array_set_addr
  | "caml_array_set_float"                -> Obj.repr caml_array_set_float
  | "caml_array_sub"                      -> Obj.repr caml_array_sub
  | "caml_array_unsafe_get"               -> Obj.repr caml_array_unsafe_get
  | "caml_array_unsafe_get_float"         -> Obj.repr caml_array_unsafe_get_float
  | "caml_array_unsafe_set"               -> Obj.repr caml_array_unsafe_set
  | "caml_array_unsafe_set_addr"          -> Obj.repr caml_array_unsafe_set_addr
  | "caml_array_unsafe_set_float"         -> Obj.repr caml_array_unsafe_set_float
  | "caml_asin_float"                     -> Obj.repr caml_asin_float
  | "caml_atan2_float"                    -> Obj.repr caml_atan2_float
  | "caml_atan_float"                     -> Obj.repr caml_atan_float
  | "caml_backtrace_status"               -> Obj.repr caml_backtrace_status
  | "caml_bitvect_test"                   -> Obj.repr caml_bitvect_test
  | "caml_blit_string"                    -> Obj.repr caml_blit_string
(*  | "caml_bswap16"                        -> Obj.repr caml_bswap16*)
  | "caml_ceil_float"                     -> Obj.repr caml_ceil_float
  | "caml_channel_descriptor"             -> Obj.repr caml_channel_descriptor
  | "caml_classify_float"                 -> Obj.repr caml_classify_float
  | "caml_compare"                        -> Obj.repr caml_compare
(*  | "caml_convert_raw_backtrace"          -> Obj.repr caml_convert_raw_backtrace*)
  | "caml_copysign_float"                 -> Obj.repr caml_copysign_float
  | "caml_cos_float"                      -> Obj.repr caml_cos_float
  | "caml_cosh_float"                     -> Obj.repr caml_cosh_float
  | "caml_create_string"                  -> Obj.repr caml_create_string
  | "caml_div_float"                      -> Obj.repr caml_div_float
  | "caml_dynlink_add_primitive"          -> Obj.repr caml_dynlink_add_primitive
  | "caml_dynlink_close_lib"              -> Obj.repr caml_dynlink_close_lib
  | "caml_dynlink_get_current_libs"       -> Obj.repr caml_dynlink_get_current_libs
  | "caml_dynlink_lookup_symbol"          -> Obj.repr caml_dynlink_lookup_symbol
  | "caml_dynlink_open_lib"               -> Obj.repr caml_dynlink_open_lib
  | "caml_ensure_stack_capacity"          -> Obj.repr caml_ensure_stack_capacity
  | "caml_eq_float"                       -> Obj.repr caml_eq_float
  | "caml_equal"                          -> Obj.repr caml_equal
  | "caml_exp_float"                      -> Obj.repr caml_exp_float
  | "caml_expm1_float"                    -> Obj.repr caml_expm1_float
  | "caml_fill_string"                    -> Obj.repr caml_fill_string
  | "caml_final_register"                 -> Obj.repr caml_final_register
  | "caml_final_release"                  -> Obj.repr caml_final_release
  | "caml_float_compare"                  -> Obj.repr caml_float_compare
  | "caml_float_of_int"                   -> Obj.repr caml_float_of_int
  | "caml_float_of_string"                -> Obj.repr caml_float_of_string
  | "caml_floor_float"                    -> Obj.repr caml_floor_float
  | "caml_fmod_float"                     -> Obj.repr caml_fmod_float
  | "caml_format_float"                   -> Obj.repr caml_format_float
  | "caml_format_int"                     -> Obj.repr caml_format_int
  | "caml_frexp_float"                    -> Obj.repr caml_frexp_float
  | "caml_gc_compaction"                  -> Obj.repr caml_gc_compaction
  | "caml_gc_counters"                    -> Obj.repr caml_gc_counters
  | "caml_gc_full_major"                  -> Obj.repr caml_gc_full_major
  | "caml_gc_get"                         -> Obj.repr caml_gc_get
  | "caml_gc_major"                       -> Obj.repr caml_gc_major
  | "caml_gc_major_slice"                 -> Obj.repr caml_gc_major_slice
  | "caml_gc_minor"                       -> Obj.repr caml_gc_minor
  | "caml_gc_quick_stat"                  -> Obj.repr caml_gc_quick_stat
  | "caml_gc_set"                         -> Obj.repr caml_gc_set
  | "caml_gc_stat"                        -> Obj.repr caml_gc_stat
  | "caml_ge_float"                       -> Obj.repr caml_ge_float
(*  | "caml_get_current_callstack"          -> Obj.repr caml_get_current_callstack*)
(*  | "caml_get_current_environment"        -> Obj.repr caml_get_current_environment*)
  | "caml_get_exception_backtrace"        -> Obj.repr caml_get_exception_backtrace
(*  | "caml_get_exception_raw_backtrace"    -> Obj.repr caml_get_exception_raw_backtrace*)
  | "caml_get_global_data"                -> Obj.repr caml_get_global_data
  | "caml_get_public_method"              -> Obj.repr caml_get_public_method
  | "caml_get_section_table"              -> Obj.repr caml_get_section_table
  | "caml_greaterequal"                   -> Obj.repr caml_greaterequal
  | "caml_greaterthan"                    -> Obj.repr caml_greaterthan
  | "caml_gt_float"                       -> Obj.repr caml_gt_float
  | "caml_hash"                           -> Obj.repr caml_hash
  | "caml_hash_univ_param"                -> Obj.repr caml_hash_univ_param
  | "caml_hypot_float"                    -> Obj.repr caml_hypot_float
  | "caml_input_value"                    -> Obj.repr caml_input_value
  | "caml_input_value_from_string"        -> Obj.repr caml_input_value_from_string
  | "caml_install_signal_handler"         -> Obj.repr caml_install_signal_handler
  | "caml_int32_add"                      -> Obj.repr caml_int32_add
  | "caml_int32_and"                      -> Obj.repr caml_int32_and
  | "caml_int32_bits_of_float"            -> Obj.repr caml_int32_bits_of_float
(*  | "caml_int32_bswap"                    -> Obj.repr caml_int32_bswap*)
  | "caml_int32_compare"                  -> Obj.repr caml_int32_compare
  | "caml_int32_div"                      -> Obj.repr caml_int32_div
  | "caml_int32_float_of_bits"            -> Obj.repr caml_int32_float_of_bits
  | "caml_int32_format"                   -> Obj.repr caml_int32_format
  | "caml_int32_mod"                      -> Obj.repr caml_int32_mod
  | "caml_int32_mul"                      -> Obj.repr caml_int32_mul
  | "caml_int32_neg"                      -> Obj.repr caml_int32_neg
  | "caml_int32_of_float"                 -> Obj.repr caml_int32_of_float
  | "caml_int32_of_int"                   -> Obj.repr caml_int32_of_int
  | "caml_int32_of_string"                -> Obj.repr caml_int32_of_string
  | "caml_int32_or"                       -> Obj.repr caml_int32_or
  | "caml_int32_shift_left"               -> Obj.repr caml_int32_shift_left
  | "caml_int32_shift_right"              -> Obj.repr caml_int32_shift_right
  | "caml_int32_shift_right_unsigned"     -> Obj.repr caml_int32_shift_right_unsigned
  | "caml_int32_sub"                      -> Obj.repr caml_int32_sub
  | "caml_int32_to_float"                 -> Obj.repr caml_int32_to_float
  | "caml_int32_to_int"                   -> Obj.repr caml_int32_to_int
  | "caml_int32_xor"                      -> Obj.repr caml_int32_xor
  | "caml_int64_add"                      -> Obj.repr caml_int64_add
  | "caml_int64_and"                      -> Obj.repr caml_int64_and
  | "caml_int64_bits_of_float"            -> Obj.repr caml_int64_bits_of_float
(*  | "caml_int64_bswap"                    -> Obj.repr caml_int64_bswap*)
  | "caml_int64_compare"                  -> Obj.repr caml_int64_compare
  | "caml_int64_div"                      -> Obj.repr caml_int64_div
  | "caml_int64_float_of_bits"            -> Obj.repr caml_int64_float_of_bits
  | "caml_int64_format"                   -> Obj.repr caml_int64_format
  | "caml_int64_mod"                      -> Obj.repr caml_int64_mod
  | "caml_int64_mul"                      -> Obj.repr caml_int64_mul
  | "caml_int64_neg"                      -> Obj.repr caml_int64_neg
  | "caml_int64_of_float"                 -> Obj.repr caml_int64_of_float
  | "caml_int64_of_int"                   -> Obj.repr caml_int64_of_int
  | "caml_int64_of_int32"                 -> Obj.repr caml_int64_of_int32
  | "caml_int64_of_nativeint"             -> Obj.repr caml_int64_of_nativeint
  | "caml_int64_of_string"                -> Obj.repr caml_int64_of_string
  | "caml_int64_or"                       -> Obj.repr caml_int64_or
  | "caml_int64_shift_left"               -> Obj.repr caml_int64_shift_left
  | "caml_int64_shift_right"              -> Obj.repr caml_int64_shift_right
  | "caml_int64_shift_right_unsigned"     -> Obj.repr caml_int64_shift_right_unsigned
  | "caml_int64_sub"                      -> Obj.repr caml_int64_sub
  | "caml_int64_to_float"                 -> Obj.repr caml_int64_to_float
  | "caml_int64_to_int"                   -> Obj.repr caml_int64_to_int
  | "caml_int64_to_int32"                 -> Obj.repr caml_int64_to_int32
  | "caml_int64_to_nativeint"             -> Obj.repr caml_int64_to_nativeint
  | "caml_int64_xor"                      -> Obj.repr caml_int64_xor
  | "caml_int_compare"                    -> Obj.repr caml_int_compare
  | "caml_int_of_float"                   -> Obj.repr caml_int_of_float
  | "caml_int_of_string"                  -> Obj.repr caml_int_of_string
  | "caml_invoke_traced_function"         -> Obj.repr caml_invoke_traced_function
  | "caml_is_printable"                   -> Obj.repr caml_is_printable
  | "caml_lazy_follow_forward"            -> Obj.repr caml_lazy_follow_forward
  | "caml_lazy_make_forward"              -> Obj.repr caml_lazy_make_forward
  | "caml_ldexp_float"                    -> Obj.repr caml_ldexp_float
  | "caml_le_float"                       -> Obj.repr caml_le_float
  | "caml_lessequal"                      -> Obj.repr caml_lessequal
  | "caml_lessthan"                       -> Obj.repr caml_lessthan
  | "caml_lex_engine"                     -> Obj.repr caml_lex_engine
  | "caml_log10_float"                    -> Obj.repr caml_log10_float
  | "caml_log1p_float"                    -> Obj.repr caml_log1p_float
  | "caml_log_float"                      -> Obj.repr caml_log_float
  | "caml_lt_float"                       -> Obj.repr caml_lt_float
  | "caml_make_array"                     -> Obj.repr caml_make_array
  | "caml_make_vect"                      -> Obj.repr caml_make_vect
  | "caml_marshal_data_size"              -> Obj.repr caml_marshal_data_size
  | "caml_md5_chan"                       -> Obj.repr caml_md5_chan
  | "caml_md5_string"                     -> Obj.repr caml_md5_string
  | "caml_ml_channel_size"                -> Obj.repr caml_ml_channel_size
  | "caml_ml_channel_size_64"             -> Obj.repr caml_ml_channel_size_64
  | "caml_ml_close_channel"               -> Obj.repr caml_ml_close_channel
  | "caml_ml_flush"                       -> Obj.repr caml_ml_flush
  | "caml_ml_flush_partial"               -> Obj.repr caml_ml_flush_partial
  | "caml_ml_input"                       -> Obj.repr caml_ml_input
  | "caml_ml_input_char"                  -> Obj.repr caml_ml_input_char
  | "caml_ml_input_int"                   -> Obj.repr caml_ml_input_int
  | "caml_ml_input_scan_line"             -> Obj.repr caml_ml_input_scan_line
  | "caml_ml_open_descriptor_in"          -> Obj.repr caml_ml_open_descriptor_in
  | "caml_ml_open_descriptor_out"         -> Obj.repr caml_ml_open_descriptor_out
  | "caml_ml_out_channels_list"           -> Obj.repr caml_ml_out_channels_list
  | "caml_ml_output"                      -> Obj.repr caml_ml_output
  | "caml_ml_output_char"                 -> Obj.repr caml_ml_output_char
  | "caml_ml_output_int"                  -> Obj.repr caml_ml_output_int
(*  | "caml_ml_output_partial"              -> Obj.repr caml_ml_output_partial*)
  | "caml_ml_pos_in"                      -> Obj.repr caml_ml_pos_in
  | "caml_ml_pos_in_64"                   -> Obj.repr caml_ml_pos_in_64
  | "caml_ml_pos_out"                     -> Obj.repr caml_ml_pos_out
  | "caml_ml_pos_out_64"                  -> Obj.repr caml_ml_pos_out_64
  | "caml_ml_seek_in"                     -> Obj.repr caml_ml_seek_in
  | "caml_ml_seek_in_64"                  -> Obj.repr caml_ml_seek_in_64
  | "caml_ml_seek_out"                    -> Obj.repr caml_ml_seek_out
  | "caml_ml_seek_out_64"                 -> Obj.repr caml_ml_seek_out_64
  | "caml_ml_set_binary_mode"             -> Obj.repr caml_ml_set_binary_mode
  | "caml_ml_string_length"               -> Obj.repr caml_ml_string_length
  | "caml_modf_float"                     -> Obj.repr caml_modf_float
  | "caml_mul_float"                      -> Obj.repr caml_mul_float
  | "caml_nativeint_add"                  -> Obj.repr caml_nativeint_add
  | "caml_nativeint_and"                  -> Obj.repr caml_nativeint_and
(*  | "caml_nativeint_bswap"                -> Obj.repr caml_nativeint_bswap*)
  | "caml_nativeint_compare"              -> Obj.repr caml_nativeint_compare
  | "caml_nativeint_div"                  -> Obj.repr caml_nativeint_div
  | "caml_nativeint_format"               -> Obj.repr caml_nativeint_format
  | "caml_nativeint_mod"                  -> Obj.repr caml_nativeint_mod
  | "caml_nativeint_mul"                  -> Obj.repr caml_nativeint_mul
  | "caml_nativeint_neg"                  -> Obj.repr caml_nativeint_neg
  | "caml_nativeint_of_float"             -> Obj.repr caml_nativeint_of_float
  | "caml_nativeint_of_int"               -> Obj.repr caml_nativeint_of_int
  | "caml_nativeint_of_int32"             -> Obj.repr caml_nativeint_of_int32
  | "caml_nativeint_of_string"            -> Obj.repr caml_nativeint_of_string
  | "caml_nativeint_or"                   -> Obj.repr caml_nativeint_or
  | "caml_nativeint_shift_left"           -> Obj.repr caml_nativeint_shift_left
  | "caml_nativeint_shift_right"          -> Obj.repr caml_nativeint_shift_right
  | "caml_nativeint_shift_right_unsigned" -> Obj.repr caml_nativeint_shift_right_unsigned
  | "caml_nativeint_sub"                  -> Obj.repr caml_nativeint_sub
  | "caml_nativeint_to_float"             -> Obj.repr caml_nativeint_to_float
  | "caml_nativeint_to_int"               -> Obj.repr caml_nativeint_to_int
  | "caml_nativeint_to_int32"             -> Obj.repr caml_nativeint_to_int32
  | "caml_nativeint_xor"                  -> Obj.repr caml_nativeint_xor
  | "caml_neg_float"                      -> Obj.repr caml_neg_float
  | "caml_neq_float"                      -> Obj.repr caml_neq_float
  | "caml_new_lex_engine"                 -> Obj.repr caml_new_lex_engine
  | "caml_notequal"                       -> Obj.repr caml_notequal
  | "caml_obj_add_offset"                 -> Obj.repr caml_obj_add_offset
  | "caml_obj_block"                      -> Obj.repr caml_obj_block
  | "caml_obj_dup"                        -> Obj.repr caml_obj_dup
  | "caml_obj_is_block"                   -> Obj.repr caml_obj_is_block
  | "caml_obj_set_tag"                    -> Obj.repr caml_obj_set_tag
  | "caml_obj_tag"                        -> Obj.repr caml_obj_tag
  | "caml_obj_truncate"                   -> Obj.repr caml_obj_truncate
  | "caml_output_value"                   -> Obj.repr caml_output_value
  | "caml_output_value_to_buffer"         -> Obj.repr caml_output_value_to_buffer
  | "caml_output_value_to_string"         -> Obj.repr caml_output_value_to_string
  | "caml_parse_engine"                   -> Obj.repr caml_parse_engine
  | "caml_power_float"                    -> Obj.repr caml_power_float
  | "caml_realloc_global"                 -> Obj.repr caml_realloc_global
  | "caml_record_backtrace"               -> Obj.repr caml_record_backtrace
(*  | "caml_register_code_fragment"         -> Obj.repr caml_register_code_fragment*)
  | "caml_register_named_value"           -> Obj.repr caml_register_named_value
(*  | "caml_reify_bytecode"                 -> Obj.repr caml_reify_bytecode*)
  | "caml_set_parser_trace"               -> Obj.repr caml_set_parser_trace
  | "caml_sin_float"                      -> Obj.repr caml_sin_float
  | "caml_sinh_float"                     -> Obj.repr caml_sinh_float
  | "caml_sqrt_float"                     -> Obj.repr caml_sqrt_float
  | "caml_static_alloc"                   -> Obj.repr caml_static_alloc
  | "caml_static_free"                    -> Obj.repr caml_static_free
(*  | "caml_static_release_bytecode"        -> Obj.repr caml_static_release_bytecode*)
  | "caml_static_resize"                  -> Obj.repr caml_static_resize
  | "caml_string_compare"                 -> Obj.repr caml_string_compare
  | "caml_string_equal"                   -> Obj.repr caml_string_equal
  | "caml_string_get"                     -> Obj.repr caml_string_get
(*  | "caml_string_get16"                   -> Obj.repr caml_string_get16*)
(*  | "caml_string_get32"                   -> Obj.repr caml_string_get32*)
(*  | "caml_string_get64"                   -> Obj.repr caml_string_get64*)
  | "caml_string_greaterequal"            -> Obj.repr caml_string_greaterequal
  | "caml_string_greaterthan"             -> Obj.repr caml_string_greaterthan
  | "caml_string_lessequal"               -> Obj.repr caml_string_lessequal
  | "caml_string_lessthan"                -> Obj.repr caml_string_lessthan
  | "caml_string_notequal"                -> Obj.repr caml_string_notequal
  | "caml_string_set"                     -> Obj.repr caml_string_set
(*  | "caml_string_set16"                   -> Obj.repr caml_string_set16*)
(*  | "caml_string_set32"                   -> Obj.repr caml_string_set32*)
(*  | "caml_string_set64"                   -> Obj.repr caml_string_set64*)
  | "caml_sub_float"                      -> Obj.repr caml_sub_float
  | "caml_sys_chdir"                      -> Obj.repr caml_sys_chdir
  | "caml_sys_close"                      -> Obj.repr caml_sys_close
  | "caml_sys_const_big_endian"           -> Obj.repr caml_sys_const_big_endian
  | "caml_sys_const_ostype_cygwin"        -> Obj.repr caml_sys_const_ostype_cygwin
  | "caml_sys_const_ostype_unix"          -> Obj.repr caml_sys_const_ostype_unix
  | "caml_sys_const_ostype_win32"         -> Obj.repr caml_sys_const_ostype_win32
  | "caml_sys_const_word_size"            -> Obj.repr caml_sys_const_word_size
  | "caml_sys_exit"                       -> Obj.repr caml_sys_exit
  | "caml_sys_file_exists"                -> Obj.repr caml_sys_file_exists
  | "caml_sys_get_argv"                   -> Obj.repr caml_sys_get_argv
  | "caml_sys_get_config"                 -> Obj.repr caml_sys_get_config
  | "caml_sys_getcwd"                     -> Obj.repr caml_sys_getcwd
  | "caml_sys_getenv"                     -> Obj.repr caml_sys_getenv
  | "caml_sys_is_directory"               -> Obj.repr caml_sys_is_directory
  | "caml_sys_open"                       -> Obj.repr caml_sys_open
  | "caml_sys_random_seed"                -> Obj.repr caml_sys_random_seed
  | "caml_sys_read_directory"             -> Obj.repr caml_sys_read_directory
  | "caml_sys_remove"                     -> Obj.repr caml_sys_remove
  | "caml_sys_rename"                     -> Obj.repr caml_sys_rename
  | "caml_sys_system_command"             -> Obj.repr caml_sys_system_command
  | "caml_sys_time"                       -> Obj.repr caml_sys_time
  | "caml_tan_float"                      -> Obj.repr caml_tan_float
  | "caml_tanh_float"                     -> Obj.repr caml_tanh_float
  | "caml_terminfo_backup"                -> Obj.repr caml_terminfo_backup
  | "caml_terminfo_resume"                -> Obj.repr caml_terminfo_resume
  | "caml_terminfo_setup"                 -> Obj.repr caml_terminfo_setup
  | "caml_terminfo_standout"              -> Obj.repr caml_terminfo_standout
  | "caml_update_dummy"                   -> Obj.repr caml_update_dummy
  | "caml_weak_blit"                      -> Obj.repr caml_weak_blit
  | "caml_weak_check"                     -> Obj.repr caml_weak_check
  | "caml_weak_create"                    -> Obj.repr caml_weak_create
  | "caml_weak_get"                       -> Obj.repr caml_weak_get
  | "caml_weak_get_copy"                  -> Obj.repr caml_weak_get_copy
  | "caml_weak_set"                       -> Obj.repr caml_weak_set
  | _                                     -> Tools.fail "external function %S not found" name

(***)

let apply narg cfun arg0 stack =
  let open Astack in
  let rec f narg cfun arg0 stack ofs =
    match narg with
    | 1 ->
      (Obj.obj cfun : Obj.t -> Obj.t) arg0
    | 2 ->
      (Obj.obj cfun : Obj.t -> Obj.t -> Obj.t) arg0 (acc stack ofs)
    | 3 ->
      (Obj.obj cfun : Obj.t -> Obj.t -> Obj.t -> Obj.t) arg0 (acc stack ofs)
        (acc stack (ofs + 1))
    | 4 ->
      (Obj.obj cfun : Obj.t -> Obj.t -> Obj.t -> Obj.t -> Obj.t) arg0
        (acc stack ofs) (acc stack (ofs + 1)) (acc stack (ofs + 2))
    | 5 ->
      (Obj.obj cfun : Obj.t -> Obj.t -> Obj.t -> Obj.t -> Obj.t -> Obj.t)
        arg0 (acc stack ofs) (acc stack (ofs + 1)) (acc stack (ofs + 2))
        (acc stack (ofs + 3))
    | _ ->
      f (narg - 5) (f 5 cfun arg0 stack ofs) (acc stack (ofs + 4)) stack
        (ofs + 5) in
  f narg cfun arg0 stack 0
