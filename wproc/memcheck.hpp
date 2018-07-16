//
//	Copyright (c) 1996-1998 NeoMedia Technologies Inc. All rights reserved.
//
//	Project:	WPROC
//	Id:		$Id:$
//	RCS:		$Source:$
//	
//// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : memcheck.hpp
// Author : George Soules
// Date   : 9 February 1991

#ifndef MEMCHECK__HPP
#define MEMCHECK__HPP

// Classes
// (none)

// Definitions and subprograms
// (none)

#if DEBUG
void check_and_verify_heap(int caller_id);
void dump_heap();

void check_memory_usage(
   unsigned long initial_memory,
   unsigned long current_memory);
#endif


#if DEBUG && (DOS || DOS_HOST)
unsigned long im; // initial memory
unsigned long cm; // current memory
#define save_memory_state()  im = farcoreleft()
#define check_memory_state() cm = farcoreleft(); check_memory_usage(im, cm)
#define check_heap(id)       check_and_verify_heap(id)

#else

#define save_memory_state()
#define check_memory_state()
#define check_heap()

#endif
#endif

//
//	History:
//	$Log: memcheck.hpp,v $
//	Revision 1.8  1998-08-31 15:13:55-04  gsl
//	drcs update
//
//

//	
//	RCS file: /disk1/neomedia/RCS/wisp/wproc/memcheck.hpp,v
//	Working file: memcheck.hpp
//	head: 1.7
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
//	total revisions: 7;	selected revisions: 7
//	description:
//	----------------------------
//	revision 1.7
//	date: 1997-06-10 00:39:32-04;  author: scass;  state: V4_3_00;  lines: +4 -4
//	int_32 not defined and not needed so put back to long
//	----------------------------
//	revision 1.6
//	date: 1997-06-09 17:39:05-04;  author: scass;  state: Exp;  lines: +4 -4
//	int4 -> int_32
//	----------------------------
//	revision 1.5
//	date: 1997-06-09 16:57:31-04;  author: scass;  state: Exp;  lines: +4 -4
//	Changed long to int4
//	----------------------------
//	revision 1.4
//	date: 1995-04-25 06:00:04-04;  author: gsl;  state: V3_3_93;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:52:21-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:33:01-05;  author: gsl;  state: V3_3x12;  lines: +0 -0
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:13-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
