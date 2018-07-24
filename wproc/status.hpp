//
//	Copyright (c) Shell Stream Software LLC, All Rights Reserved.
//
//	Project:	WPROC
//	
//// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : status.hpp
// Author : George Soules
// Date   : 17 July 1991

#ifndef STATUS__HPP
#define STATUS__HPP

// Classes
// (none)

// Definitions and subprograms
// (none)


enum status_kind {
   status_none,
   status_ok,
   status_cancel,
   status_destroy,
   status_file_not_found,
   status_memory_error,
   status_need_code,
   status_open_error,
   status_runtime_error,
#if RUNTIME
   status_source_input,
#endif
#if WANG
   status_logoff,
#endif
   status_swap_error,
   status_link_failed
};




#endif


//
//	History:
//	$Log: status.hpp,v $
//	Revision 1.6  1998/08/31 19:14:18  gsl
//	drcs update
//	
//

//	
//
//	Working file: status.hpp
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
//	date: 1996-07-25 19:48:04-04;  author: gsl;  state: V4_3_00;  lines: +1 -1
//	NT
//	----------------------------
//	revision 1.4
//	date: 1995-04-25 06:00:26-04;  author: gsl;  state: V3_3_19;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:52:41-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:33:26-05;  author: gsl;  state: V3_3x12;  lines: +2 -1
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:28-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
