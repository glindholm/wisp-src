//
//	Copyright (c) Shell Stream Software LLC, All Rights Reserved.
//
//	Project:	WPROC
//	
//// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : debugaid.hpp
// Author : George Soules
// Date   : 24 February 1991

#ifndef DEBUGAID__HPP
#define DEBUGAID__HPP

// Classes
#include "options.hpp"

// Definitions and subprograms
#include "assert.hpp"
#include "environ.hpp"



Boolean valid_ptr(void *p);
void    debug_trace_begin(Boolean option, const char *msg, const char *kind);
void    debug_trace_end(Boolean option);
void    debug_trace(Boolean option, const char *msg, const char *kind);
void    debug_trace_i(Boolean option, int_32 number, const char *kind);
void    debug_trace_si(Boolean option, const char *msg, int_32 number, const char *kind);
void    debug_trace_sc(Boolean option, const char *msg, char the_char, const char *kind);
void    debug_trace_ss(Boolean option, const char *msg1, const char *msg2, const char *kind);
void    debug_trace_sq(Boolean option, const char *msg1, const char *msg2, const char *kind);
void    print_trace_id(const char *trace_kind);
void    debug_trace_level_save();
void    debug_trace_level_restore();

#define trace_begin(kind, arg)\
debug_trace_begin((Boolean) user_options.debug_trace_##kind(), arg, #kind)

#define trace_end(kind)\
debug_trace_end((Boolean) user_options.debug_trace_##kind())

#define trace(kind, arg)\
debug_trace((Boolean) user_options.debug_trace_##kind(), arg, #kind)

#define trace_i(kind, arg)\
debug_trace_i((Boolean) user_options.debug_trace_##kind(), arg, #kind)

#define trace_si(kind, arg1, arg2)\
debug_trace_si((Boolean) user_options.debug_trace_##kind(), arg1, arg2, #kind)

#define trace_sc(kind, arg1, arg2)\
debug_trace_sc((Boolean) user_options.debug_trace_##kind(), arg1, arg2, #kind)

#define trace_ss(kind, arg1, arg2)\
debug_trace_ss((Boolean) user_options.debug_trace_##kind(), arg1, arg2, #kind)

#define trace_sq(kind, arg1, arg2)\
debug_trace_sq((Boolean) user_options.debug_trace_##kind(), arg1, arg2, #kind)

#define trace_level_save()\
debug_trace_level_save()

#define trace_level_restore()\
debug_trace_level_restore()

#ifdef NOT_USED

#define trace_begin(kind, arg)
#define trace_end(kind)
#define trace(kind, arg)
#define trace_i(kind, arg)
#define trace_si(kind, arg1, arg2)
#define trace_sc(kind, arg1, arg2)
#define trace_ss(kind, arg1, arg2)
#define trace_sq(kind, arg1, arg2)
#define trace_level_save()
#define trace_level_restore()

#endif
#endif


//
//	History:
//	$Log: debugaid.hpp,v $
//	Revision 1.8  2003/02/11 19:05:26  gsl
//	Remove unneeded #ifdef's for DEBUG
//	
//	Revision 1.7  1998/08/31 19:13:40  gsl
//	drcs update
//	
//

//	
//
//	Working file: debugaid.hpp
//	head: 1.6
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
//	total revisions: 6;	selected revisions: 6
//	description:
//	----------------------------
//	revision 1.6
//	date: 1997-06-09 17:35:06-04;  author: scass;  state: V4_3_00;  lines: +2 -2
//	int4 -> int_32
//	----------------------------
//	revision 1.5
//	date: 1997-06-09 16:55:20-04;  author: scass;  state: Exp;  lines: +2 -2
//	Changed long to int4 for portability.
//	----------------------------
//	revision 1.4
//	date: 1995-04-25 05:59:47-04;  author: gsl;  state: V3_3_93;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:52:03-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:32:43-05;  author: gsl;  state: V3_3x12;  lines: +20 -15
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:02-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
