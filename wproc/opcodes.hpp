//
//	Copyright (c) Shell Stream Software LLC, All Rights Reserved.
//
//	Project:	WPROC
//	
//// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : opcodes.hpp
// Author : George Soules
// Date   : 28 February 1991

#ifndef OPCODES__HPP
#define OPCODES__HPP

// Classes
// (none)

// Definitions and subprograms
// (none)


// WARNING: New opcodes must be added to the end of the list to avoid code
//          incompatibilites with earlier versions.

enum opcode {
   add_op,
   and_op,
   assign_op,
   branch_always_op,
   branch_false_op,
   branch_true_op,
   builtin_op,
   call_op,
   call_return_op,
   catenate_op,
   close_op,
   copy_and_push_top_op,
   declare_op,
   destroy_op,
   divide_op,
   eoc_op,
   for_loop_exit_op,
   for_loop_incr_op,
   for_loop_init_op,
   for_loop_test_op,
   halt_op,
   interpret_op,
   label_assign_op,
   label_decl_op,
   label_ref_op,
   message_op,
   multiply_op,
   negate_op,
   not_op,
   open_op,
   or_op,
   pop_top_op,
   print_op,
   print_subop,
   prompt_op,
   protect_op,
   protect_subop,
   push_int_8_op,
   push_int_16_op,
   push_int_32_op,
   push_string_op,
   push_int_8_and_range_op,
   push_int_16_and_range_op,
   push_int_32_and_range_op,
   push_string_and_range_op,
   push_variable_and_range_op,
   read_op,
   rel_case_eq_op,
   rel_eq_op,
   rel_ge_op,
   rel_gt_op,
   rel_le_op,
   rel_lt_op,
   rel_ne_op,
   rename_op,
   return_op,
   run_op,
   run_subop,
   scratch_op,
   screen_op,
   scrnio_subop,
   set_op,
   source_name_op,
   statement_op,
   statement_end_op,
   sub_exp_range_op,
   system_op,
   subtract_op,
   trace_op,
   trace_subop,
   using_op,
   write_op,
   call_exp_op,
   goto_exp_op,

// Added for Wang version
   backref_full_op,
   backref_partial_op,
   exists_op,
   extract_op,
   lib_name_op,
   logoff_op,
   submit_op,
   submit_subop,
   using_end_op,
   vol_name_op,
   extract_records_op
};


char *opcode_name(opcode an_opcode);

#endif


//
//	History:
//	$Log: opcodes.hpp,v $
//	Revision 1.5  1998/08/31 19:13:59  gsl
//	drcs update
//	
//

//	
//
//	Working file: opcodes.hpp
//	head: 1.4
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
//	total revisions: 4;	selected revisions: 4
//	description:
//	----------------------------
//	revision 1.4
//	date: 1995-04-25 06:00:08-04;  author: gsl;  state: V4_3_00;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:52:25-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:33:05-05;  author: gsl;  state: V3_3x12;  lines: +2 -1
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:15-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
