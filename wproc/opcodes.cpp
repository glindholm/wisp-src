//
//	Copyright (c) 1996-1998 NeoMedia Technologies Inc. All rights reserved.
//
//	Project:	WPROC
//	Id:		$Id:$
//	RCS:		$Source:$
//	
//// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : opcodes.cpp
// Author : George Soules
// Date   : 28 February 1991

// Specification
#include "opcodes.hpp"

// Classes
// (none)

// Definitions and subprograms
// (none)

#if DEBUG


char *opcode_name(opcode an_opcode) {
   switch (an_opcode) {
      case add_op                      : return "add_op";
      case and_op                      : return "and_op";
      case assign_op                   : return "assign_op";
      case branch_always_op            : return "branch_always_op";
      case branch_false_op             : return "branch_false_op";
      case branch_true_op              : return "branch_true_op";
      case builtin_op                  : return "builtin_op";
      case call_op                     : return "call_op";
      case call_return_op              : return "call_return_op";
      case catenate_op                 : return "catenate_op";
      case close_op                    : return "close_op";
      case copy_and_push_top_op        : return "copy_and_push_top_op";
      case declare_op                  : return "declare_op";
      case destroy_op                  : return "destroy_op";
      case divide_op                   : return "divide_op";
      case eoc_op                      : return "eoc_op";
      case for_loop_exit_op            : return "for_loop_exit_op";
      case for_loop_incr_op            : return "for_loop_incr_op";
      case for_loop_init_op            : return "for_loop_init_op";
      case for_loop_test_op            : return "for_loop_test_op";
      case halt_op                     : return "halt_op";
      case interpret_op                : return "interpret_op";
      case label_assign_op             : return "label_assign_op";
      case label_decl_op               : return "label_decl_op";
      case label_ref_op                : return "label_ref_op";
      case message_op                  : return "message_op";
      case multiply_op                 : return "multiply_op";
      case negate_op                   : return "negate_op";
      case not_op                      : return "not_op";
      case open_op                     : return "open_op";
      case or_op                       : return "or_op";
      case pop_top_op                  : return "pop_top_op";
      case print_op                    : return "print_op";
      case print_subop                 : return "print_subop";
      case prompt_op                   : return "prompt_op";
      case protect_op                  : return "protect_op";
      case protect_subop               : return "protect_subop";
      case push_int_8_op               : return "push_int_8_op";
      case push_int_16_op              : return "push_int_16_op";
      case push_int_32_op              : return "push_int_32_op";
      case push_string_op              : return "push_string_op";
      case push_int_8_and_range_op     : return "push_int_8_and_range_op";
      case push_int_16_and_range_op    : return "push_int_16_and_range_op";
      case push_int_32_and_range_op    : return "push_int_32_and_range_op";
      case push_string_and_range_op    : return "push_string_and_range_op";
      case push_variable_and_range_op  : return "push_variable_and_range_op";
      case read_op                     : return "read_op";
      case rel_case_eq_op              : return "rel_case_eg_op";
      case rel_eq_op                   : return "rel_eg_op";
      case rel_ge_op                   : return "rel_ge_op";
      case rel_gt_op                   : return "rel_gt_op";
      case rel_le_op                   : return "rel_le_op";
      case rel_lt_op                   : return "rel_lt_op";
      case rel_ne_op                   : return "rel_ne_op";
      case rename_op                   : return "rename_op";
      case return_op                   : return "return_op";
      case run_op                      : return "run_op";
      case run_subop                   : return "run_subop";
      case scratch_op                  : return "scratch_op";
      case screen_op                   : return "screen_op";
      case scrnio_subop                : return "scrnio_subop";
      case set_op                      : return "set_op";
      case source_name_op              : return "source_name_op";
      case statement_op                : return "statement_op";
      case statement_end_op            : return "statement_end_op";
      case sub_exp_range_op            : return "sub_exp_range_op";
      case system_op                   : return "system_op";
      case subtract_op                 : return "subtract_op";
      case trace_op                    : return "trace_op";
      case trace_subop                 : return "trace_subop";
      case using_op                    : return "using_op";
      case write_op                    : return "write_op";
      case call_exp_op                 : return "call_exp_op";
      case goto_exp_op                 : return "goto_exp_op";

#if WANG
      case backref_full_op             : return "backref_full_op";
      case backref_partial_op          : return "backref_partial_op";
      case exists_op                   : return "exists_op";
      case extract_op                  : return "extract_op";
      case lib_name_op                 : return "lib_name_op";
      case logoff_op                   : return "logoff_op";
      case submit_op                   : return "submit_op";
      case submit_subop                : return "submit_subop";
      case using_end_op                : return "using_end_op";
      case vol_name_op                 : return "vol_name_op";
      case extract_records_op          : return "extract_records_op";
#endif
      default                          : return "UNKNOWN_op";
   }
}

#endif


//
//	History:
//	$Log: opcodes.cpp,v $
//	Revision 1.6  1998/08/31 19:13:59  gsl
//	drcs update
//	
//

//	
//	RCS file: /disk1/neomedia/RCS/wisp/wproc/opcodes.cpp,v
//	Working file: opcodes.cpp
//	head: 1.5
//	branch:
//	locks: strict
//	access list:
//		gsl
//		scass
//		ljn
//		jockc
//		jlima
//	symbolic names:
//	keyword substitution: kv
//	total revisions: 5;	selected revisions: 5
//	description:
//	----------------------------
//	revision 1.5
//	date: 1996-07-25 14:15:33-04;  author: gsl;  state: V4_3_00;  lines: +0 -0
//	Renamed from opcodes.cc to opcodes.cpp
//	----------------------------
//	revision 1.4
//	date: 1995-04-25 06:00:07-04;  author: gsl;  state: V3_3_19;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:52:24-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:33:04-05;  author: gsl;  state: V3_3x12;  lines: +1 -0
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:15-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
