//
//	Copyright (c) 1996-1998 NeoMedia Technologies Inc. All rights reserved.
//
//	Project:	WPROC
//	
//// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : subops.hpp
// Author : George Soules
// Date   : 2 July 1991

#ifndef SUBOPS__HPP
#define SUBOPS__HPP

// Classes
// (none)

// Definitions and subprograms
// (none)


// WARNING: New kinds must be added to the end of a list to avoid code
//          incompatibilites with earlier versions.

enum print_opcode {
#if WANG
   print_class_op,
   print_status_op,
   print_form_op,
   print_copies_op,
   print_disp_op
#else
   print_copies_op,
   print_eject_op,
   print_spool_op
#endif
};


enum protect_opcode {
   protect_archive_op,
   protect_hidden_op,
   protect_readonly_op,
   protect_system_op
};


enum run_opcode {   // The tags are intermixed here to preserve
#if ! WANG          // the original (! WANG & DOS) order of sub ops.
   run_append_op,
#endif
   run_cexit_op,
   run_eexit_op,
#if ! WANG
   run_input_op,
   run_output_op,
#if DOS
   run_swap_op,
   run_type_op
#endif
#endif
#if WANG
   run_display_op,
   run_enter_op,
   run_trans_op
#endif
};


enum screen_opcode {
   // Option opcodes
   screen_alarm_op,
   screen_border_op,
   screen_bordercolor_op,
   screen_corner_op,
   screen_curcol_op,
   screen_currow_op,
   screen_cursor_op,
   screen_enable_op,
   screen_erase_op,
   screen_escape_op,
   screen_facs_op,
   screen_fkey_op,
   screen_modchar_op,
   screen_restore_op,
   screen_restrict_op,
   screen_row_op,
   screen_screencolor_op,
   screen_titlecolor_op,
   screen_title_op,
   screen_options_end_op,

   // Other opcodes
   screen_attr_op,
   screen_attr_cond_op,
   screen_center_op,
   screen_field_op,
   screen_newline_op,
   screen_center_cond_op, // Added in version 1.1
   screen_line_cond_op    // Added in version 1.1
};


#if WANG
enum submit_opcode {
   submit_globals_op,
   submit_environ_op,
   submit_class_op,
   submit_status_op,
   submit_dump_op,
   submit_cpu_op,
   submit_action_op,
   submit_disp_op
};
#endif

#endif


//
//	History:
//	$Log: subops.hpp,v $
//	Revision 1.5  1998/08/31 19:14:18  gsl
//	drcs update
//	
//

//	
//	RCS file: /disk1/neomedia/RCS/wisp/wproc/subops.hpp,v
//	Working file: subops.hpp
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
//	date: 1995-04-25 06:00:26-04;  author: gsl;  state: V4_3_00;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:52:42-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:33:27-05;  author: gsl;  state: V3_3x12;  lines: +0 -0
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:29-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
