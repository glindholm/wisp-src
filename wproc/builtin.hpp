//
//	Copyright (c) 1996-1998 NeoMedia Technologies Inc. All rights reserved.
//
//	Project:	WPROC
//	
//// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : builtin.hpp
// Author : George Soules
// Date   : 15 May 1991

#ifndef BUILTIN__HPP
#define BUILTIN__HPP

// Classes
// (none)

// Definitions and subprograms
// (none)


// WARNING: New kinds must be added to the end of the list to avoid code
//          incompatibilites with earlier versions.

enum builtin_kind {
   bi_abs,
   bi_alarm,
   bi_arg,
   bi_args,
   bi_byte,
   bi_center,
   bi_change_dir,
#if DOS
   bi_change_drive,
#endif
   bi_color,
   bi_copy,
   bi_current_dir,
#if DOS
   bi_current_drive,
#endif
   bi_date,
   bi_delay,
   bi_dir_entries,
   bi_dir_exists,
   bi_disk_space,
#if DOS
   bi_dos_version,
#endif
   bi_extract,
   bi_false,
   bi_file_dir,
#if DOS
   bi_file_drive,
#endif
   bi_file_exists,
   bi_file_ext,
   bi_file_lines,
   bi_file_name,
   bi_file_search,
   bi_get_char,
   bi_get_key,
   bi_index,
   bi_integer,
   bi_is_alpha,
   bi_is_alphanum,
#if DOS
   bi_is_archive,
#endif
   bi_is_color,
   bi_is_hex,
   bi_is_digits,
#if DOS
   bi_is_dir,
   bi_is_hidden,
#endif
   bi_is_lower,
   bi_is_integer,
   bi_is_printable,
#if DOS
   bi_is_readonly,
   bi_is_system,
#endif
   bi_is_upper,
   bi_is_yesno,
   bi_key_ready,
   bi_left,
   bi_length,
   bi_lower,
   bi_make_dir,
   bi_match,
   bi_max,
#if DOS
   bi_memory,
#endif
   bi_min,
   bi_mod,
   bi_monochrome,
   bi_parse,
   bi_proc_name,
   bi_random,
   bi_rank,
   bi_read_dir,
   bi_remainder,
   bi_remove_dir,
   bi_right,
   bi_sort,
   bi_strip,
   bi_time,
   bi_translate,
   bi_trim,
   bi_true,
   bi_unique,
   bi_upper,
   bi_verify,
   bi_yesno,

// Added in version 1.1
   bi_curcol,
   bi_currow,
   bi_cursor,
   bi_file_date,
   bi_file_size,
   bi_file_time,

#if WANG
   bi_label,
#endif

// Added in version 1.2
   bi_drive_ready,
   bi_set_file_date,
   bi_set_file_time,
   bi_verify_date,
   bi_verify_time,

#if NETWORK
   bi_logged_in,
   bi_network_address,
   bi_network_available,
   bi_user_name,
#endif

#if NETWORK || WANG
   bi_login_name,
#endif

   bi_last_one
};

#endif


//
//	History:
//	$Log: builtin.hpp,v $
//	Revision 1.6  1998/08/31 19:13:34  gsl
//	drcs update
//	
//

//	
//	RCS file: /disk1/neomedia/RCS/wisp/wproc/builtin.hpp,v
//	Working file: builtin.hpp
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
//	date: 1996-07-25 19:45:03-04;  author: gsl;  state: V4_3_00;  lines: +1 -4
//	Fix for NT
//	----------------------------
//	revision 1.4
//	date: 1995-04-25 05:59:40-04;  author: gsl;  state: V3_3_19;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:51:57-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:32:36-05;  author: gsl;  state: V3_3x12;  lines: +0 -0
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:50:58-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
