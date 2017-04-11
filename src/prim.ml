include Strsec.Make(struct let section = Section.PRIM end)

external caml_abs_float : 'a -> 'a = "caml_abs_float"
external caml_acos_float : 'a -> 'a = "caml_acos_float"
external caml_add_debug_info : 'a -> 'a = "caml_add_debug_info"
external caml_add_float : 'a -> 'a = "caml_add_float"
external caml_alloc_dummy : 'a -> 'a = "caml_alloc_dummy"
external caml_alloc_dummy_float : 'a -> 'a = "caml_alloc_dummy_float"
external caml_alloc_dummy_function : 'a -> 'a = "caml_alloc_dummy_function"
external caml_alloc_float_array : 'a -> 'a = "caml_alloc_float_array"
external caml_array_append : 'a -> 'a = "caml_array_append"
external caml_array_blit : 'a -> 'a = "caml_array_blit"
external caml_array_concat : 'a -> 'a = "caml_array_concat"
external caml_array_get : 'a -> 'a = "caml_array_get"
external caml_array_get_addr : 'a -> 'a = "caml_array_get_addr"
external caml_array_get_float : 'a -> 'a = "caml_array_get_float"
external caml_array_set : 'a -> 'a = "caml_array_set"
external caml_array_set_addr : 'a -> 'a = "caml_array_set_addr"
external caml_array_set_float : 'a -> 'a = "caml_array_set_float"
external caml_array_sub : 'a -> 'a = "caml_array_sub"
external caml_array_unsafe_get : 'a -> 'a = "caml_array_unsafe_get"
external caml_array_unsafe_get_float : 'a -> 'a = "caml_array_unsafe_get_float"
external caml_array_unsafe_set : 'a -> 'a = "caml_array_unsafe_set"
external caml_array_unsafe_set_addr : 'a -> 'a = "caml_array_unsafe_set_addr"
external caml_array_unsafe_set_float : 'a -> 'a = "caml_array_unsafe_set_float"
external caml_asin_float : 'a -> 'a = "caml_asin_float"
external caml_atan2_float : 'a -> 'a = "caml_atan2_float"
external caml_atan_float : 'a -> 'a = "caml_atan_float"
external caml_backtrace_status : 'a -> 'a = "caml_backtrace_status"
external caml_bitvect_test : 'a -> 'a = "caml_bitvect_test"
external caml_blit_bytes : 'a -> 'a = "caml_blit_bytes"
external caml_blit_string : 'a -> 'a = "caml_blit_string"
external caml_bswap16 : 'a -> 'a = "caml_bswap16"
external caml_bytes_compare : 'a -> 'a = "caml_bytes_compare"
external caml_bytes_equal : 'a -> 'a = "caml_bytes_equal"
external caml_bytes_get : 'a -> 'a = "caml_bytes_get"
external caml_bytes_greaterequal : 'a -> 'a = "caml_bytes_greaterequal"
external caml_bytes_greaterthan : 'a -> 'a = "caml_bytes_greaterthan"
external caml_bytes_lessequal : 'a -> 'a = "caml_bytes_lessequal"
external caml_bytes_lessthan : 'a -> 'a = "caml_bytes_lessthan"
external caml_bytes_notequal : 'a -> 'a = "caml_bytes_notequal"
external caml_bytes_set : 'a -> 'a = "caml_bytes_set"
external caml_ceil_float : 'a -> 'a = "caml_ceil_float"
external caml_channel_descriptor : 'a -> 'a = "caml_channel_descriptor"
external caml_classify_float : 'a -> 'a = "caml_classify_float"
external caml_compare : 'a -> 'a = "caml_compare"
external caml_convert_raw_backtrace : 'a -> 'a = "caml_convert_raw_backtrace"
external caml_convert_raw_backtrace_slot : 'a -> 'a = "caml_convert_raw_backtrace_slot"
external caml_copysign_float : 'a -> 'a = "caml_copysign_float"
external caml_cos_float : 'a -> 'a = "caml_cos_float"
external caml_cosh_float : 'a -> 'a = "caml_cosh_float"
external caml_create_bytes : 'a -> 'a = "caml_create_bytes"
external caml_create_string : 'a -> 'a = "caml_create_string"
external caml_div_float : 'a -> 'a = "caml_div_float"
external caml_dynlink_add_primitive : 'a -> 'a = "caml_dynlink_add_primitive"
external caml_dynlink_close_lib : 'a -> 'a = "caml_dynlink_close_lib"
external caml_dynlink_get_current_libs : 'a -> 'a = "caml_dynlink_get_current_libs"
external caml_dynlink_lookup_symbol : 'a -> 'a = "caml_dynlink_lookup_symbol"
external caml_dynlink_open_lib : 'a -> 'a = "caml_dynlink_open_lib"
external caml_ephe_blit_data : 'a -> 'a = "caml_ephe_blit_data"
external caml_ephe_blit_key : 'a -> 'a = "caml_ephe_blit_key"
external caml_ephe_check_data : 'a -> 'a = "caml_ephe_check_data"
external caml_ephe_check_key : 'a -> 'a = "caml_ephe_check_key"
external caml_ephe_create : 'a -> 'a = "caml_ephe_create"
external caml_ephe_get_data : 'a -> 'a = "caml_ephe_get_data"
external caml_ephe_get_data_copy : 'a -> 'a = "caml_ephe_get_data_copy"
external caml_ephe_get_key : 'a -> 'a = "caml_ephe_get_key"
external caml_ephe_get_key_copy : 'a -> 'a = "caml_ephe_get_key_copy"
external caml_ephe_set_data : 'a -> 'a = "caml_ephe_set_data"
external caml_ephe_set_key : 'a -> 'a = "caml_ephe_set_key"
external caml_ephe_unset_data : 'a -> 'a = "caml_ephe_unset_data"
external caml_ephe_unset_key : 'a -> 'a = "caml_ephe_unset_key"
external caml_eq_float : 'a -> 'a = "caml_eq_float"
external caml_equal : 'a -> 'a = "caml_equal"
external caml_exp_float : 'a -> 'a = "caml_exp_float"
external caml_expm1_float : 'a -> 'a = "caml_expm1_float"
external caml_fill_bytes : 'a -> 'a = "caml_fill_bytes"
external caml_fill_string : 'a -> 'a = "caml_fill_string"
external caml_final_register : 'a -> 'a = "caml_final_register"
external caml_final_register_called_without_value : 'a -> 'a = "caml_final_register_called_without_value"
external caml_final_release : 'a -> 'a = "caml_final_release"
external caml_float_compare : 'a -> 'a = "caml_float_compare"
external caml_float_of_int : 'a -> 'a = "caml_float_of_int"
external caml_float_of_string : 'a -> 'a = "caml_float_of_string"
external caml_floor_float : 'a -> 'a = "caml_floor_float"
external caml_fmod_float : 'a -> 'a = "caml_fmod_float"
external caml_format_float : 'a -> 'a = "caml_format_float"
external caml_format_int : 'a -> 'a = "caml_format_int"
external caml_fresh_oo_id : 'a -> 'a = "caml_fresh_oo_id"
external caml_frexp_float : 'a -> 'a = "caml_frexp_float"
external caml_gc_compaction : 'a -> 'a = "caml_gc_compaction"
external caml_gc_counters : 'a -> 'a = "caml_gc_counters"
external caml_gc_full_major : 'a -> 'a = "caml_gc_full_major"
external caml_gc_get : 'a -> 'a = "caml_gc_get"
external caml_gc_huge_fallback_count : 'a -> 'a = "caml_gc_huge_fallback_count"
external caml_gc_major : 'a -> 'a = "caml_gc_major"
external caml_gc_major_slice : 'a -> 'a = "caml_gc_major_slice"
external caml_gc_minor : 'a -> 'a = "caml_gc_minor"
external caml_gc_minor_words : 'a -> 'a = "caml_gc_minor_words"
external caml_gc_quick_stat : 'a -> 'a = "caml_gc_quick_stat"
external caml_gc_set : 'a -> 'a = "caml_gc_set"
external caml_gc_stat : 'a -> 'a = "caml_gc_stat"
external caml_ge_float : 'a -> 'a = "caml_ge_float"
external caml_get_current_callstack : 'a -> 'a = "caml_get_current_callstack"
external caml_get_exception_backtrace : 'a -> 'a = "caml_get_exception_backtrace"
external caml_get_exception_raw_backtrace : 'a -> 'a = "caml_get_exception_raw_backtrace"
external caml_get_global_data : 'a -> 'a = "caml_get_global_data"
external caml_get_major_bucket : 'a -> 'a = "caml_get_major_bucket"
external caml_get_major_credit : 'a -> 'a = "caml_get_major_credit"
external caml_get_minor_free : 'a -> 'a = "caml_get_minor_free"
external caml_get_public_method : 'a -> 'a = "caml_get_public_method"
external caml_get_section_table : 'a -> 'a = "caml_get_section_table"
external caml_greaterequal : 'a -> 'a = "caml_greaterequal"
external caml_greaterthan : 'a -> 'a = "caml_greaterthan"
external caml_gt_float : 'a -> 'a = "caml_gt_float"
external caml_hash : 'a -> 'a = "caml_hash"
external caml_hash_univ_param : 'a -> 'a = "caml_hash_univ_param"
external caml_hexstring_of_float : 'a -> 'a = "caml_hexstring_of_float"
external caml_hypot_float : 'a -> 'a = "caml_hypot_float"
external caml_input_value : 'a -> 'a = "caml_input_value"
external caml_input_value_from_string : 'a -> 'a = "caml_input_value_from_string"
external caml_input_value_to_outside_heap : 'a -> 'a = "caml_input_value_to_outside_heap"
external caml_install_signal_handler : 'a -> 'a = "caml_install_signal_handler"
external caml_int32_add : 'a -> 'a = "caml_int32_add"
external caml_int32_and : 'a -> 'a = "caml_int32_and"
external caml_int32_bits_of_float : 'a -> 'a = "caml_int32_bits_of_float"
external caml_int32_bswap : 'a -> 'a = "caml_int32_bswap"
external caml_int32_compare : 'a -> 'a = "caml_int32_compare"
external caml_int32_div : 'a -> 'a = "caml_int32_div"
external caml_int32_float_of_bits : 'a -> 'a = "caml_int32_float_of_bits"
external caml_int32_format : 'a -> 'a = "caml_int32_format"
external caml_int32_mod : 'a -> 'a = "caml_int32_mod"
external caml_int32_mul : 'a -> 'a = "caml_int32_mul"
external caml_int32_neg : 'a -> 'a = "caml_int32_neg"
external caml_int32_of_float : 'a -> 'a = "caml_int32_of_float"
external caml_int32_of_int : 'a -> 'a = "caml_int32_of_int"
external caml_int32_of_string : 'a -> 'a = "caml_int32_of_string"
external caml_int32_or : 'a -> 'a = "caml_int32_or"
external caml_int32_shift_left : 'a -> 'a = "caml_int32_shift_left"
external caml_int32_shift_right : 'a -> 'a = "caml_int32_shift_right"
external caml_int32_shift_right_unsigned : 'a -> 'a = "caml_int32_shift_right_unsigned"
external caml_int32_sub : 'a -> 'a = "caml_int32_sub"
external caml_int32_to_float : 'a -> 'a = "caml_int32_to_float"
external caml_int32_to_int : 'a -> 'a = "caml_int32_to_int"
external caml_int32_xor : 'a -> 'a = "caml_int32_xor"
external caml_int64_add : 'a -> 'a = "caml_int64_add"
external caml_int64_and : 'a -> 'a = "caml_int64_and"
external caml_int64_bits_of_float : 'a -> 'a = "caml_int64_bits_of_float"
external caml_int64_bswap : 'a -> 'a = "caml_int64_bswap"
external caml_int64_compare : 'a -> 'a = "caml_int64_compare"
external caml_int64_div : 'a -> 'a = "caml_int64_div"
external caml_int64_float_of_bits : 'a -> 'a = "caml_int64_float_of_bits"
external caml_int64_format : 'a -> 'a = "caml_int64_format"
external caml_int64_mod : 'a -> 'a = "caml_int64_mod"
external caml_int64_mul : 'a -> 'a = "caml_int64_mul"
external caml_int64_neg : 'a -> 'a = "caml_int64_neg"
external caml_int64_of_float : 'a -> 'a = "caml_int64_of_float"
external caml_int64_of_int : 'a -> 'a = "caml_int64_of_int"
external caml_int64_of_int32 : 'a -> 'a = "caml_int64_of_int32"
external caml_int64_of_nativeint : 'a -> 'a = "caml_int64_of_nativeint"
external caml_int64_of_string : 'a -> 'a = "caml_int64_of_string"
external caml_int64_or : 'a -> 'a = "caml_int64_or"
external caml_int64_shift_left : 'a -> 'a = "caml_int64_shift_left"
external caml_int64_shift_right : 'a -> 'a = "caml_int64_shift_right"
external caml_int64_shift_right_unsigned : 'a -> 'a = "caml_int64_shift_right_unsigned"
external caml_int64_sub : 'a -> 'a = "caml_int64_sub"
external caml_int64_to_float : 'a -> 'a = "caml_int64_to_float"
external caml_int64_to_int : 'a -> 'a = "caml_int64_to_int"
external caml_int64_to_int32 : 'a -> 'a = "caml_int64_to_int32"
external caml_int64_to_nativeint : 'a -> 'a = "caml_int64_to_nativeint"
external caml_int64_xor : 'a -> 'a = "caml_int64_xor"
external caml_int_as_pointer : 'a -> 'a = "caml_int_as_pointer"
external caml_int_compare : 'a -> 'a = "caml_int_compare"
external caml_int_of_float : 'a -> 'a = "caml_int_of_float"
external caml_int_of_string : 'a -> 'a = "caml_int_of_string"
external caml_invoke_traced_function : 'a -> 'a = "caml_invoke_traced_function"
external caml_lazy_follow_forward : 'a -> 'a = "caml_lazy_follow_forward"
external caml_lazy_make_forward : 'a -> 'a = "caml_lazy_make_forward"
external caml_ldexp_float : 'a -> 'a = "caml_ldexp_float"
external caml_le_float : 'a -> 'a = "caml_le_float"
external caml_lessequal : 'a -> 'a = "caml_lessequal"
external caml_lessthan : 'a -> 'a = "caml_lessthan"
external caml_lex_engine : 'a -> 'a = "caml_lex_engine"
external caml_log10_float : 'a -> 'a = "caml_log10_float"
external caml_log1p_float : 'a -> 'a = "caml_log1p_float"
external caml_log_float : 'a -> 'a = "caml_log_float"
external caml_lt_float : 'a -> 'a = "caml_lt_float"
external caml_make_array : 'a -> 'a = "caml_make_array"
external caml_make_float_vect : 'a -> 'a = "caml_make_float_vect"
external caml_make_vect : 'a -> 'a = "caml_make_vect"
external caml_marshal_data_size : 'a -> 'a = "caml_marshal_data_size"
external caml_md5_chan : 'a -> 'a = "caml_md5_chan"
external caml_md5_string : 'a -> 'a = "caml_md5_string"
external caml_ml_bytes_length : 'a -> 'a = "caml_ml_bytes_length"
external caml_ml_channel_size : 'a -> 'a = "caml_ml_channel_size"
external caml_ml_channel_size_64 : 'a -> 'a = "caml_ml_channel_size_64"
external caml_ml_close_channel : 'a -> 'a = "caml_ml_close_channel"
external caml_ml_enable_runtime_warnings : 'a -> 'a = "caml_ml_enable_runtime_warnings"
external caml_ml_flush : 'a -> 'a = "caml_ml_flush"
external caml_ml_flush_partial : 'a -> 'a = "caml_ml_flush_partial"
external caml_ml_input : 'a -> 'a = "caml_ml_input"
external caml_ml_input_char : 'a -> 'a = "caml_ml_input_char"
external caml_ml_input_int : 'a -> 'a = "caml_ml_input_int"
external caml_ml_input_scan_line : 'a -> 'a = "caml_ml_input_scan_line"
external caml_ml_open_descriptor_in : 'a -> 'a = "caml_ml_open_descriptor_in"
external caml_ml_open_descriptor_out : 'a -> 'a = "caml_ml_open_descriptor_out"
external caml_ml_out_channels_list : 'a -> 'a = "caml_ml_out_channels_list"
external caml_ml_output : 'a -> 'a = "caml_ml_output"
external caml_ml_output_bytes : 'a -> 'a = "caml_ml_output_bytes"
external caml_ml_output_char : 'a -> 'a = "caml_ml_output_char"
external caml_ml_output_int : 'a -> 'a = "caml_ml_output_int"
external caml_ml_output_partial : 'a -> 'a = "caml_ml_output_partial"
external caml_ml_pos_in : 'a -> 'a = "caml_ml_pos_in"
external caml_ml_pos_in_64 : 'a -> 'a = "caml_ml_pos_in_64"
external caml_ml_pos_out : 'a -> 'a = "caml_ml_pos_out"
external caml_ml_pos_out_64 : 'a -> 'a = "caml_ml_pos_out_64"
external caml_ml_runtime_warnings_enabled : 'a -> 'a = "caml_ml_runtime_warnings_enabled"
external caml_ml_seek_in : 'a -> 'a = "caml_ml_seek_in"
external caml_ml_seek_in_64 : 'a -> 'a = "caml_ml_seek_in_64"
external caml_ml_seek_out : 'a -> 'a = "caml_ml_seek_out"
external caml_ml_seek_out_64 : 'a -> 'a = "caml_ml_seek_out_64"
external caml_ml_set_binary_mode : 'a -> 'a = "caml_ml_set_binary_mode"
external caml_ml_set_channel_name : 'a -> 'a = "caml_ml_set_channel_name"
external caml_ml_string_length : 'a -> 'a = "caml_ml_string_length"
external caml_modf_float : 'a -> 'a = "caml_modf_float"
external caml_mul_float : 'a -> 'a = "caml_mul_float"
external caml_nativeint_add : 'a -> 'a = "caml_nativeint_add"
external caml_nativeint_and : 'a -> 'a = "caml_nativeint_and"
external caml_nativeint_bswap : 'a -> 'a = "caml_nativeint_bswap"
external caml_nativeint_compare : 'a -> 'a = "caml_nativeint_compare"
external caml_nativeint_div : 'a -> 'a = "caml_nativeint_div"
external caml_nativeint_format : 'a -> 'a = "caml_nativeint_format"
external caml_nativeint_mod : 'a -> 'a = "caml_nativeint_mod"
external caml_nativeint_mul : 'a -> 'a = "caml_nativeint_mul"
external caml_nativeint_neg : 'a -> 'a = "caml_nativeint_neg"
external caml_nativeint_of_float : 'a -> 'a = "caml_nativeint_of_float"
external caml_nativeint_of_int : 'a -> 'a = "caml_nativeint_of_int"
external caml_nativeint_of_int32 : 'a -> 'a = "caml_nativeint_of_int32"
external caml_nativeint_of_string : 'a -> 'a = "caml_nativeint_of_string"
external caml_nativeint_or : 'a -> 'a = "caml_nativeint_or"
external caml_nativeint_shift_left : 'a -> 'a = "caml_nativeint_shift_left"
external caml_nativeint_shift_right : 'a -> 'a = "caml_nativeint_shift_right"
external caml_nativeint_shift_right_unsigned : 'a -> 'a = "caml_nativeint_shift_right_unsigned"
external caml_nativeint_sub : 'a -> 'a = "caml_nativeint_sub"
external caml_nativeint_to_float : 'a -> 'a = "caml_nativeint_to_float"
external caml_nativeint_to_int : 'a -> 'a = "caml_nativeint_to_int"
external caml_nativeint_to_int32 : 'a -> 'a = "caml_nativeint_to_int32"
external caml_nativeint_xor : 'a -> 'a = "caml_nativeint_xor"
external caml_neg_float : 'a -> 'a = "caml_neg_float"
external caml_neq_float : 'a -> 'a = "caml_neq_float"
external caml_new_lex_engine : 'a -> 'a = "caml_new_lex_engine"
external caml_notequal : 'a -> 'a = "caml_notequal"
external caml_obj_add_offset : 'a -> 'a = "caml_obj_add_offset"
external caml_obj_block : 'a -> 'a = "caml_obj_block"
external caml_obj_dup : 'a -> 'a = "caml_obj_dup"
external caml_obj_is_block : 'a -> 'a = "caml_obj_is_block"
external caml_obj_reachable_words : 'a -> 'a = "caml_obj_reachable_words"
external caml_obj_set_tag : 'a -> 'a = "caml_obj_set_tag"
external caml_obj_tag : 'a -> 'a = "caml_obj_tag"
external caml_obj_truncate : 'a -> 'a = "caml_obj_truncate"
external caml_output_value : 'a -> 'a = "caml_output_value"
external caml_output_value_to_buffer : 'a -> 'a = "caml_output_value_to_buffer"
external caml_output_value_to_string : 'a -> 'a = "caml_output_value_to_string"
external caml_parse_engine : 'a -> 'a = "caml_parse_engine"
external caml_power_float : 'a -> 'a = "caml_power_float"
external caml_raw_backtrace_length : 'a -> 'a = "caml_raw_backtrace_length"
external caml_raw_backtrace_next_slot : 'a -> 'a = "caml_raw_backtrace_next_slot"
external caml_raw_backtrace_slot : 'a -> 'a = "caml_raw_backtrace_slot"
external caml_realloc_global : 'a -> 'a = "caml_realloc_global"
external caml_record_backtrace : 'a -> 'a = "caml_record_backtrace"
external caml_register_channel_for_spacetime : 'a -> 'a = "caml_register_channel_for_spacetime"
external caml_register_named_value : 'a -> 'a = "caml_register_named_value"
external caml_reify_bytecode : 'a -> 'a = "caml_reify_bytecode"
external caml_remove_debug_info : 'a -> 'a = "caml_remove_debug_info"
external caml_runtime_parameters : 'a -> 'a = "caml_runtime_parameters"
external caml_runtime_variant : 'a -> 'a = "caml_runtime_variant"
external caml_set_oo_id : 'a -> 'a = "caml_set_oo_id"
external caml_set_parser_trace : 'a -> 'a = "caml_set_parser_trace"
external caml_sin_float : 'a -> 'a = "caml_sin_float"
external caml_sinh_float : 'a -> 'a = "caml_sinh_float"
external caml_spacetime_enabled : 'a -> 'a = "caml_spacetime_enabled"
external caml_sqrt_float : 'a -> 'a = "caml_sqrt_float"
external caml_static_alloc : 'a -> 'a = "caml_static_alloc"
external caml_static_free : 'a -> 'a = "caml_static_free"
external caml_static_release_bytecode : 'a -> 'a = "caml_static_release_bytecode"
external caml_static_resize : 'a -> 'a = "caml_static_resize"
external caml_string_compare : 'a -> 'a = "caml_string_compare"
external caml_string_equal : 'a -> 'a = "caml_string_equal"
external caml_string_get : 'a -> 'a = "caml_string_get"
external caml_string_get16 : 'a -> 'a = "caml_string_get16"
external caml_string_get32 : 'a -> 'a = "caml_string_get32"
external caml_string_get64 : 'a -> 'a = "caml_string_get64"
external caml_string_greaterequal : 'a -> 'a = "caml_string_greaterequal"
external caml_string_greaterthan : 'a -> 'a = "caml_string_greaterthan"
external caml_string_lessequal : 'a -> 'a = "caml_string_lessequal"
external caml_string_lessthan : 'a -> 'a = "caml_string_lessthan"
external caml_string_notequal : 'a -> 'a = "caml_string_notequal"
external caml_string_set : 'a -> 'a = "caml_string_set"
external caml_string_set16 : 'a -> 'a = "caml_string_set16"
external caml_string_set32 : 'a -> 'a = "caml_string_set32"
external caml_string_set64 : 'a -> 'a = "caml_string_set64"
external caml_sub_float : 'a -> 'a = "caml_sub_float"
external caml_sys_chdir : 'a -> 'a = "caml_sys_chdir"
external caml_sys_close : 'a -> 'a = "caml_sys_close"
external caml_sys_const_backend_type : 'a -> 'a = "caml_sys_const_backend_type"
external caml_sys_const_big_endian : 'a -> 'a = "caml_sys_const_big_endian"
external caml_sys_const_int_size : 'a -> 'a = "caml_sys_const_int_size"
external caml_sys_const_max_wosize : 'a -> 'a = "caml_sys_const_max_wosize"
external caml_sys_const_ostype_cygwin : 'a -> 'a = "caml_sys_const_ostype_cygwin"
external caml_sys_const_ostype_unix : 'a -> 'a = "caml_sys_const_ostype_unix"
external caml_sys_const_ostype_win32 : 'a -> 'a = "caml_sys_const_ostype_win32"
external caml_sys_const_word_size : 'a -> 'a = "caml_sys_const_word_size"
external caml_sys_exit : 'a -> 'a = "caml_sys_exit"
external caml_sys_file_exists : 'a -> 'a = "caml_sys_file_exists"
external caml_sys_get_argv : 'a -> 'a = "caml_sys_get_argv"
external caml_sys_get_config : 'a -> 'a = "caml_sys_get_config"
external caml_sys_getcwd : 'a -> 'a = "caml_sys_getcwd"
external caml_sys_getenv : 'a -> 'a = "caml_sys_getenv"
external caml_sys_isatty : 'a -> 'a = "caml_sys_isatty"
external caml_sys_is_directory : 'a -> 'a = "caml_sys_is_directory"
external caml_sys_open : 'a -> 'a = "caml_sys_open"
external caml_sys_random_seed : 'a -> 'a = "caml_sys_random_seed"
external caml_sys_read_directory : 'a -> 'a = "caml_sys_read_directory"
external caml_sys_remove : 'a -> 'a = "caml_sys_remove"
external caml_sys_rename : 'a -> 'a = "caml_sys_rename"
external caml_sys_system_command : 'a -> 'a = "caml_sys_system_command"
external caml_sys_time : 'a -> 'a = "caml_sys_time"
external caml_tan_float : 'a -> 'a = "caml_tan_float"
external caml_tanh_float : 'a -> 'a = "caml_tanh_float"
external caml_terminfo_backup : 'a -> 'a = "caml_terminfo_backup"
external caml_terminfo_resume : 'a -> 'a = "caml_terminfo_resume"
external caml_terminfo_setup : 'a -> 'a = "caml_terminfo_setup"
external caml_terminfo_standout : 'a -> 'a = "caml_terminfo_standout"
external caml_update_dummy : 'a -> 'a = "caml_update_dummy"
external caml_weak_blit : 'a -> 'a = "caml_weak_blit"
external caml_weak_check : 'a -> 'a = "caml_weak_check"
external caml_weak_create : 'a -> 'a = "caml_weak_create"
external caml_weak_get : 'a -> 'a = "caml_weak_get"
external caml_weak_get_copy : 'a -> 'a = "caml_weak_get_copy"
external caml_weak_set : 'a -> 'a = "caml_weak_set"

let find_prim name =
  match name with
  | "caml_abs_float" -> Obj.repr caml_abs_float
  | "caml_acos_float" -> Obj.repr caml_acos_float
  | "caml_add_debug_info" -> Obj.repr caml_add_debug_info
  | "caml_add_float" -> Obj.repr caml_add_float
  | "caml_alloc_dummy" -> Obj.repr caml_alloc_dummy
  | "caml_alloc_dummy_float" -> Obj.repr caml_alloc_dummy_float
  | "caml_alloc_dummy_function" -> Obj.repr caml_alloc_dummy_function
  | "caml_alloc_float_array" -> Obj.repr caml_alloc_float_array
  | "caml_array_append" -> Obj.repr caml_array_append
  | "caml_array_blit" -> Obj.repr caml_array_blit
  | "caml_array_concat" -> Obj.repr caml_array_concat
  | "caml_array_get" -> Obj.repr caml_array_get
  | "caml_array_get_addr" -> Obj.repr caml_array_get_addr
  | "caml_array_get_float" -> Obj.repr caml_array_get_float
  | "caml_array_set" -> Obj.repr caml_array_set
  | "caml_array_set_addr" -> Obj.repr caml_array_set_addr
  | "caml_array_set_float" -> Obj.repr caml_array_set_float
  | "caml_array_sub" -> Obj.repr caml_array_sub
  | "caml_array_unsafe_get" -> Obj.repr caml_array_unsafe_get
  | "caml_array_unsafe_get_float" -> Obj.repr caml_array_unsafe_get_float
  | "caml_array_unsafe_set" -> Obj.repr caml_array_unsafe_set
  | "caml_array_unsafe_set_addr" -> Obj.repr caml_array_unsafe_set_addr
  | "caml_array_unsafe_set_float" -> Obj.repr caml_array_unsafe_set_float
  | "caml_asin_float" -> Obj.repr caml_asin_float
  | "caml_atan2_float" -> Obj.repr caml_atan2_float
  | "caml_atan_float" -> Obj.repr caml_atan_float
  | "caml_backtrace_status" -> Obj.repr caml_backtrace_status
  | "caml_bitvect_test" -> Obj.repr caml_bitvect_test
  | "caml_blit_bytes" -> Obj.repr caml_blit_bytes
  | "caml_blit_string" -> Obj.repr caml_blit_string
  | "caml_bswap16" -> Obj.repr caml_bswap16
  | "caml_bytes_compare" -> Obj.repr caml_bytes_compare
  | "caml_bytes_equal" -> Obj.repr caml_bytes_equal
  | "caml_bytes_get" -> Obj.repr caml_bytes_get
  | "caml_bytes_greaterequal" -> Obj.repr caml_bytes_greaterequal
  | "caml_bytes_greaterthan" -> Obj.repr caml_bytes_greaterthan
  | "caml_bytes_lessequal" -> Obj.repr caml_bytes_lessequal
  | "caml_bytes_lessthan" -> Obj.repr caml_bytes_lessthan
  | "caml_bytes_notequal" -> Obj.repr caml_bytes_notequal
  | "caml_bytes_set" -> Obj.repr caml_bytes_set
  | "caml_ceil_float" -> Obj.repr caml_ceil_float
  | "caml_channel_descriptor" -> Obj.repr caml_channel_descriptor
  | "caml_classify_float" -> Obj.repr caml_classify_float
  | "caml_compare" -> Obj.repr caml_compare
  | "caml_convert_raw_backtrace" -> Obj.repr caml_convert_raw_backtrace
  | "caml_convert_raw_backtrace_slot" -> Obj.repr caml_convert_raw_backtrace_slot
  | "caml_copysign_float" -> Obj.repr caml_copysign_float
  | "caml_cos_float" -> Obj.repr caml_cos_float
  | "caml_cosh_float" -> Obj.repr caml_cosh_float
  | "caml_create_bytes" -> Obj.repr caml_create_bytes
  | "caml_create_string" -> Obj.repr caml_create_string
  | "caml_div_float" -> Obj.repr caml_div_float
  | "caml_dynlink_add_primitive" -> Obj.repr caml_dynlink_add_primitive
  | "caml_dynlink_close_lib" -> Obj.repr caml_dynlink_close_lib
  | "caml_dynlink_get_current_libs" -> Obj.repr caml_dynlink_get_current_libs
  | "caml_dynlink_lookup_symbol" -> Obj.repr caml_dynlink_lookup_symbol
  | "caml_dynlink_open_lib" -> Obj.repr caml_dynlink_open_lib
  | "caml_ephe_blit_data" -> Obj.repr caml_ephe_blit_data
  | "caml_ephe_blit_key" -> Obj.repr caml_ephe_blit_key
  | "caml_ephe_check_data" -> Obj.repr caml_ephe_check_data
  | "caml_ephe_check_key" -> Obj.repr caml_ephe_check_key
  | "caml_ephe_create" -> Obj.repr caml_ephe_create
  | "caml_ephe_get_data" -> Obj.repr caml_ephe_get_data
  | "caml_ephe_get_data_copy" -> Obj.repr caml_ephe_get_data_copy
  | "caml_ephe_get_key" -> Obj.repr caml_ephe_get_key
  | "caml_ephe_get_key_copy" -> Obj.repr caml_ephe_get_key_copy
  | "caml_ephe_set_data" -> Obj.repr caml_ephe_set_data
  | "caml_ephe_set_key" -> Obj.repr caml_ephe_set_key
  | "caml_ephe_unset_data" -> Obj.repr caml_ephe_unset_data
  | "caml_ephe_unset_key" -> Obj.repr caml_ephe_unset_key
  | "caml_eq_float" -> Obj.repr caml_eq_float
  | "caml_equal" -> Obj.repr caml_equal
  | "caml_exp_float" -> Obj.repr caml_exp_float
  | "caml_expm1_float" -> Obj.repr caml_expm1_float
  | "caml_fill_bytes" -> Obj.repr caml_fill_bytes
  | "caml_fill_string" -> Obj.repr caml_fill_string
  | "caml_final_register" -> Obj.repr caml_final_register
  | "caml_final_register_called_without_value" -> Obj.repr caml_final_register_called_without_value
  | "caml_final_release" -> Obj.repr caml_final_release
  | "caml_float_compare" -> Obj.repr caml_float_compare
  | "caml_float_of_int" -> Obj.repr caml_float_of_int
  | "caml_float_of_string" -> Obj.repr caml_float_of_string
  | "caml_floor_float" -> Obj.repr caml_floor_float
  | "caml_fmod_float" -> Obj.repr caml_fmod_float
  | "caml_format_float" -> Obj.repr caml_format_float
  | "caml_format_int" -> Obj.repr caml_format_int
  | "caml_fresh_oo_id" -> Obj.repr caml_fresh_oo_id
  | "caml_frexp_float" -> Obj.repr caml_frexp_float
  | "caml_gc_compaction" -> Obj.repr caml_gc_compaction
  | "caml_gc_counters" -> Obj.repr caml_gc_counters
  | "caml_gc_full_major" -> Obj.repr caml_gc_full_major
  | "caml_gc_get" -> Obj.repr caml_gc_get
  | "caml_gc_huge_fallback_count" -> Obj.repr caml_gc_huge_fallback_count
  | "caml_gc_major" -> Obj.repr caml_gc_major
  | "caml_gc_major_slice" -> Obj.repr caml_gc_major_slice
  | "caml_gc_minor" -> Obj.repr caml_gc_minor
  | "caml_gc_minor_words" -> Obj.repr caml_gc_minor_words
  | "caml_gc_quick_stat" -> Obj.repr caml_gc_quick_stat
  | "caml_gc_set" -> Obj.repr caml_gc_set
  | "caml_gc_stat" -> Obj.repr caml_gc_stat
  | "caml_ge_float" -> Obj.repr caml_ge_float
  | "caml_get_current_callstack" -> Obj.repr caml_get_current_callstack
  | "caml_get_exception_backtrace" -> Obj.repr caml_get_exception_backtrace
  | "caml_get_exception_raw_backtrace" -> Obj.repr caml_get_exception_raw_backtrace
  | "caml_get_global_data" -> Obj.repr caml_get_global_data
  | "caml_get_major_bucket" -> Obj.repr caml_get_major_bucket
  | "caml_get_major_credit" -> Obj.repr caml_get_major_credit
  | "caml_get_minor_free" -> Obj.repr caml_get_minor_free
  | "caml_get_public_method" -> Obj.repr caml_get_public_method
  | "caml_get_section_table" -> Obj.repr caml_get_section_table
  | "caml_greaterequal" -> Obj.repr caml_greaterequal
  | "caml_greaterthan" -> Obj.repr caml_greaterthan
  | "caml_gt_float" -> Obj.repr caml_gt_float
  | "caml_hash" -> Obj.repr caml_hash
  | "caml_hash_univ_param" -> Obj.repr caml_hash_univ_param
  | "caml_hexstring_of_float" -> Obj.repr caml_hexstring_of_float
  | "caml_hypot_float" -> Obj.repr caml_hypot_float
  | "caml_input_value" -> Obj.repr caml_input_value
  | "caml_input_value_from_string" -> Obj.repr caml_input_value_from_string
  | "caml_input_value_to_outside_heap" -> Obj.repr caml_input_value_to_outside_heap
  | "caml_install_signal_handler" -> Obj.repr caml_install_signal_handler
  | "caml_int32_add" -> Obj.repr caml_int32_add
  | "caml_int32_and" -> Obj.repr caml_int32_and
  | "caml_int32_bits_of_float" -> Obj.repr caml_int32_bits_of_float
  | "caml_int32_bswap" -> Obj.repr caml_int32_bswap
  | "caml_int32_compare" -> Obj.repr caml_int32_compare
  | "caml_int32_div" -> Obj.repr caml_int32_div
  | "caml_int32_float_of_bits" -> Obj.repr caml_int32_float_of_bits
  | "caml_int32_format" -> Obj.repr caml_int32_format
  | "caml_int32_mod" -> Obj.repr caml_int32_mod
  | "caml_int32_mul" -> Obj.repr caml_int32_mul
  | "caml_int32_neg" -> Obj.repr caml_int32_neg
  | "caml_int32_of_float" -> Obj.repr caml_int32_of_float
  | "caml_int32_of_int" -> Obj.repr caml_int32_of_int
  | "caml_int32_of_string" -> Obj.repr caml_int32_of_string
  | "caml_int32_or" -> Obj.repr caml_int32_or
  | "caml_int32_shift_left" -> Obj.repr caml_int32_shift_left
  | "caml_int32_shift_right" -> Obj.repr caml_int32_shift_right
  | "caml_int32_shift_right_unsigned" -> Obj.repr caml_int32_shift_right_unsigned
  | "caml_int32_sub" -> Obj.repr caml_int32_sub
  | "caml_int32_to_float" -> Obj.repr caml_int32_to_float
  | "caml_int32_to_int" -> Obj.repr caml_int32_to_int
  | "caml_int32_xor" -> Obj.repr caml_int32_xor
  | "caml_int64_add" -> Obj.repr caml_int64_add
  | "caml_int64_and" -> Obj.repr caml_int64_and
  | "caml_int64_bits_of_float" -> Obj.repr caml_int64_bits_of_float
  | "caml_int64_bswap" -> Obj.repr caml_int64_bswap
  | "caml_int64_compare" -> Obj.repr caml_int64_compare
  | "caml_int64_div" -> Obj.repr caml_int64_div
  | "caml_int64_float_of_bits" -> Obj.repr caml_int64_float_of_bits
  | "caml_int64_format" -> Obj.repr caml_int64_format
  | "caml_int64_mod" -> Obj.repr caml_int64_mod
  | "caml_int64_mul" -> Obj.repr caml_int64_mul
  | "caml_int64_neg" -> Obj.repr caml_int64_neg
  | "caml_int64_of_float" -> Obj.repr caml_int64_of_float
  | "caml_int64_of_int" -> Obj.repr caml_int64_of_int
  | "caml_int64_of_int32" -> Obj.repr caml_int64_of_int32
  | "caml_int64_of_nativeint" -> Obj.repr caml_int64_of_nativeint
  | "caml_int64_of_string" -> Obj.repr caml_int64_of_string
  | "caml_int64_or" -> Obj.repr caml_int64_or
  | "caml_int64_shift_left" -> Obj.repr caml_int64_shift_left
  | "caml_int64_shift_right" -> Obj.repr caml_int64_shift_right
  | "caml_int64_shift_right_unsigned" -> Obj.repr caml_int64_shift_right_unsigned
  | "caml_int64_sub" -> Obj.repr caml_int64_sub
  | "caml_int64_to_float" -> Obj.repr caml_int64_to_float
  | "caml_int64_to_int" -> Obj.repr caml_int64_to_int
  | "caml_int64_to_int32" -> Obj.repr caml_int64_to_int32
  | "caml_int64_to_nativeint" -> Obj.repr caml_int64_to_nativeint
  | "caml_int64_xor" -> Obj.repr caml_int64_xor
  | "caml_int_as_pointer" -> Obj.repr caml_int_as_pointer
  | "caml_int_compare" -> Obj.repr caml_int_compare
  | "caml_int_of_float" -> Obj.repr caml_int_of_float
  | "caml_int_of_string" -> Obj.repr caml_int_of_string
  | "caml_invoke_traced_function" -> Obj.repr caml_invoke_traced_function
  | "caml_lazy_follow_forward" -> Obj.repr caml_lazy_follow_forward
  | "caml_lazy_make_forward" -> Obj.repr caml_lazy_make_forward
  | "caml_ldexp_float" -> Obj.repr caml_ldexp_float
  | "caml_le_float" -> Obj.repr caml_le_float
  | "caml_lessequal" -> Obj.repr caml_lessequal
  | "caml_lessthan" -> Obj.repr caml_lessthan
  | "caml_lex_engine" -> Obj.repr caml_lex_engine
  | "caml_log10_float" -> Obj.repr caml_log10_float
  | "caml_log1p_float" -> Obj.repr caml_log1p_float
  | "caml_log_float" -> Obj.repr caml_log_float
  | "caml_lt_float" -> Obj.repr caml_lt_float
  | "caml_make_array" -> Obj.repr caml_make_array
  | "caml_make_float_vect" -> Obj.repr caml_make_float_vect
  | "caml_make_vect" -> Obj.repr caml_make_vect
  | "caml_marshal_data_size" -> Obj.repr caml_marshal_data_size
  | "caml_md5_chan" -> Obj.repr caml_md5_chan
  | "caml_md5_string" -> Obj.repr caml_md5_string
  | "caml_ml_bytes_length" -> Obj.repr caml_ml_bytes_length
  | "caml_ml_channel_size" -> Obj.repr caml_ml_channel_size
  | "caml_ml_channel_size_64" -> Obj.repr caml_ml_channel_size_64
  | "caml_ml_close_channel" -> Obj.repr caml_ml_close_channel
  | "caml_ml_enable_runtime_warnings" -> Obj.repr caml_ml_enable_runtime_warnings
  | "caml_ml_flush" -> Obj.repr caml_ml_flush
  | "caml_ml_flush_partial" -> Obj.repr caml_ml_flush_partial
  | "caml_ml_input" -> Obj.repr caml_ml_input
  | "caml_ml_input_char" -> Obj.repr caml_ml_input_char
  | "caml_ml_input_int" -> Obj.repr caml_ml_input_int
  | "caml_ml_input_scan_line" -> Obj.repr caml_ml_input_scan_line
  | "caml_ml_open_descriptor_in" -> Obj.repr caml_ml_open_descriptor_in
  | "caml_ml_open_descriptor_out" -> Obj.repr caml_ml_open_descriptor_out
  | "caml_ml_out_channels_list" -> Obj.repr caml_ml_out_channels_list
  | "caml_ml_output" -> Obj.repr caml_ml_output
  | "caml_ml_output_bytes" -> Obj.repr caml_ml_output_bytes
  | "caml_ml_output_char" -> Obj.repr caml_ml_output_char
  | "caml_ml_output_int" -> Obj.repr caml_ml_output_int
  | "caml_ml_output_partial" -> Obj.repr caml_ml_output_partial
  | "caml_ml_pos_in" -> Obj.repr caml_ml_pos_in
  | "caml_ml_pos_in_64" -> Obj.repr caml_ml_pos_in_64
  | "caml_ml_pos_out" -> Obj.repr caml_ml_pos_out
  | "caml_ml_pos_out_64" -> Obj.repr caml_ml_pos_out_64
  | "caml_ml_runtime_warnings_enabled" -> Obj.repr caml_ml_runtime_warnings_enabled
  | "caml_ml_seek_in" -> Obj.repr caml_ml_seek_in
  | "caml_ml_seek_in_64" -> Obj.repr caml_ml_seek_in_64
  | "caml_ml_seek_out" -> Obj.repr caml_ml_seek_out
  | "caml_ml_seek_out_64" -> Obj.repr caml_ml_seek_out_64
  | "caml_ml_set_binary_mode" -> Obj.repr caml_ml_set_binary_mode
  | "caml_ml_set_channel_name" -> Obj.repr caml_ml_set_channel_name
  | "caml_ml_string_length" -> Obj.repr caml_ml_string_length
  | "caml_modf_float" -> Obj.repr caml_modf_float
  | "caml_mul_float" -> Obj.repr caml_mul_float
  | "caml_nativeint_add" -> Obj.repr caml_nativeint_add
  | "caml_nativeint_and" -> Obj.repr caml_nativeint_and
  | "caml_nativeint_bswap" -> Obj.repr caml_nativeint_bswap
  | "caml_nativeint_compare" -> Obj.repr caml_nativeint_compare
  | "caml_nativeint_div" -> Obj.repr caml_nativeint_div
  | "caml_nativeint_format" -> Obj.repr caml_nativeint_format
  | "caml_nativeint_mod" -> Obj.repr caml_nativeint_mod
  | "caml_nativeint_mul" -> Obj.repr caml_nativeint_mul
  | "caml_nativeint_neg" -> Obj.repr caml_nativeint_neg
  | "caml_nativeint_of_float" -> Obj.repr caml_nativeint_of_float
  | "caml_nativeint_of_int" -> Obj.repr caml_nativeint_of_int
  | "caml_nativeint_of_int32" -> Obj.repr caml_nativeint_of_int32
  | "caml_nativeint_of_string" -> Obj.repr caml_nativeint_of_string
  | "caml_nativeint_or" -> Obj.repr caml_nativeint_or
  | "caml_nativeint_shift_left" -> Obj.repr caml_nativeint_shift_left
  | "caml_nativeint_shift_right" -> Obj.repr caml_nativeint_shift_right
  | "caml_nativeint_shift_right_unsigned" -> Obj.repr caml_nativeint_shift_right_unsigned
  | "caml_nativeint_sub" -> Obj.repr caml_nativeint_sub
  | "caml_nativeint_to_float" -> Obj.repr caml_nativeint_to_float
  | "caml_nativeint_to_int" -> Obj.repr caml_nativeint_to_int
  | "caml_nativeint_to_int32" -> Obj.repr caml_nativeint_to_int32
  | "caml_nativeint_xor" -> Obj.repr caml_nativeint_xor
  | "caml_neg_float" -> Obj.repr caml_neg_float
  | "caml_neq_float" -> Obj.repr caml_neq_float
  | "caml_new_lex_engine" -> Obj.repr caml_new_lex_engine
  | "caml_notequal" -> Obj.repr caml_notequal
  | "caml_obj_add_offset" -> Obj.repr caml_obj_add_offset
  | "caml_obj_block" -> Obj.repr caml_obj_block
  | "caml_obj_dup" -> Obj.repr caml_obj_dup
  | "caml_obj_is_block" -> Obj.repr caml_obj_is_block
  | "caml_obj_reachable_words" -> Obj.repr caml_obj_reachable_words
  | "caml_obj_set_tag" -> Obj.repr caml_obj_set_tag
  | "caml_obj_tag" -> Obj.repr caml_obj_tag
  | "caml_obj_truncate" -> Obj.repr caml_obj_truncate
  | "caml_output_value" -> Obj.repr caml_output_value
  | "caml_output_value_to_buffer" -> Obj.repr caml_output_value_to_buffer
  | "caml_output_value_to_string" -> Obj.repr caml_output_value_to_string
  | "caml_parse_engine" -> Obj.repr caml_parse_engine
  | "caml_power_float" -> Obj.repr caml_power_float
  | "caml_raw_backtrace_length" -> Obj.repr caml_raw_backtrace_length
  | "caml_raw_backtrace_next_slot" -> Obj.repr caml_raw_backtrace_next_slot
  | "caml_raw_backtrace_slot" -> Obj.repr caml_raw_backtrace_slot
  | "caml_realloc_global" -> Obj.repr caml_realloc_global
  | "caml_record_backtrace" -> Obj.repr caml_record_backtrace
  | "caml_register_channel_for_spacetime" -> Obj.repr caml_register_channel_for_spacetime
  | "caml_register_named_value" -> Obj.repr caml_register_named_value
  | "caml_reify_bytecode" -> Obj.repr caml_reify_bytecode
  | "caml_remove_debug_info" -> Obj.repr caml_remove_debug_info
  | "caml_runtime_parameters" -> Obj.repr caml_runtime_parameters
  | "caml_runtime_variant" -> Obj.repr caml_runtime_variant
  | "caml_set_oo_id" -> Obj.repr caml_set_oo_id
  | "caml_set_parser_trace" -> Obj.repr caml_set_parser_trace
  | "caml_sin_float" -> Obj.repr caml_sin_float
  | "caml_sinh_float" -> Obj.repr caml_sinh_float
  | "caml_spacetime_enabled" -> Obj.repr caml_spacetime_enabled
  | "caml_sqrt_float" -> Obj.repr caml_sqrt_float
  | "caml_static_alloc" -> Obj.repr caml_static_alloc
  | "caml_static_free" -> Obj.repr caml_static_free
  | "caml_static_release_bytecode" -> Obj.repr caml_static_release_bytecode
  | "caml_static_resize" -> Obj.repr caml_static_resize
  | "caml_string_compare" -> Obj.repr caml_string_compare
  | "caml_string_equal" -> Obj.repr caml_string_equal
  | "caml_string_get" -> Obj.repr caml_string_get
  | "caml_string_get16" -> Obj.repr caml_string_get16
  | "caml_string_get32" -> Obj.repr caml_string_get32
  | "caml_string_get64" -> Obj.repr caml_string_get64
  | "caml_string_greaterequal" -> Obj.repr caml_string_greaterequal
  | "caml_string_greaterthan" -> Obj.repr caml_string_greaterthan
  | "caml_string_lessequal" -> Obj.repr caml_string_lessequal
  | "caml_string_lessthan" -> Obj.repr caml_string_lessthan
  | "caml_string_notequal" -> Obj.repr caml_string_notequal
  | "caml_string_set" -> Obj.repr caml_string_set
  | "caml_string_set16" -> Obj.repr caml_string_set16
  | "caml_string_set32" -> Obj.repr caml_string_set32
  | "caml_string_set64" -> Obj.repr caml_string_set64
  | "caml_sub_float" -> Obj.repr caml_sub_float
  | "caml_sys_chdir" -> Obj.repr caml_sys_chdir
  | "caml_sys_close" -> Obj.repr caml_sys_close
  | "caml_sys_const_backend_type" -> Obj.repr caml_sys_const_backend_type
  | "caml_sys_const_big_endian" -> Obj.repr caml_sys_const_big_endian
  | "caml_sys_const_int_size" -> Obj.repr caml_sys_const_int_size
  | "caml_sys_const_max_wosize" -> Obj.repr caml_sys_const_max_wosize
  | "caml_sys_const_ostype_cygwin" -> Obj.repr caml_sys_const_ostype_cygwin
  | "caml_sys_const_ostype_unix" -> Obj.repr caml_sys_const_ostype_unix
  | "caml_sys_const_ostype_win32" -> Obj.repr caml_sys_const_ostype_win32
  | "caml_sys_const_word_size" -> Obj.repr caml_sys_const_word_size
  | "caml_sys_exit" -> Obj.repr caml_sys_exit
  | "caml_sys_file_exists" -> Obj.repr caml_sys_file_exists
  | "caml_sys_get_argv" -> Obj.repr caml_sys_get_argv
  | "caml_sys_get_config" -> Obj.repr caml_sys_get_config
  | "caml_sys_getcwd" -> Obj.repr caml_sys_getcwd
  | "caml_sys_getenv" -> Obj.repr caml_sys_getenv
  | "caml_sys_isatty" -> Obj.repr caml_sys_isatty
  | "caml_sys_is_directory" -> Obj.repr caml_sys_is_directory
  | "caml_sys_open" -> Obj.repr caml_sys_open
  | "caml_sys_random_seed" -> Obj.repr caml_sys_random_seed
  | "caml_sys_read_directory" -> Obj.repr caml_sys_read_directory
  | "caml_sys_remove" -> Obj.repr caml_sys_remove
  | "caml_sys_rename" -> Obj.repr caml_sys_rename
  | "caml_sys_system_command" -> Obj.repr caml_sys_system_command
  | "caml_sys_time" -> Obj.repr caml_sys_time
  | "caml_tan_float" -> Obj.repr caml_tan_float
  | "caml_tanh_float" -> Obj.repr caml_tanh_float
  | "caml_terminfo_backup" -> Obj.repr caml_terminfo_backup
  | "caml_terminfo_resume" -> Obj.repr caml_terminfo_resume
  | "caml_terminfo_setup" -> Obj.repr caml_terminfo_setup
  | "caml_terminfo_standout" -> Obj.repr caml_terminfo_standout
  | "caml_update_dummy" -> Obj.repr caml_update_dummy
  | "caml_weak_blit" -> Obj.repr caml_weak_blit
  | "caml_weak_check" -> Obj.repr caml_weak_check
  | "caml_weak_create" -> Obj.repr caml_weak_create
  | "caml_weak_get" -> Obj.repr caml_weak_get
  | "caml_weak_get_copy" -> Obj.repr caml_weak_get_copy
  | "caml_weak_set" -> Obj.repr caml_weak_set
  | _ -> Tools.fail "external function %S not found" name

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
