//
//	Copyright (c) 1996-1998 NeoMedia Technologies Inc. All rights reserved.
//
//	Project:	WPROC
//	
//// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : assert.hpp
// Author : George Soules
// Date   : 8 October 1991

#ifndef ASSERT__HPP
#define ASSERT__HPP
// Classes
// (none)

// Definitions and subprograms
// (none)

#ifdef NDEBUG
#define assert(p)
#else
void assert_error(char *source_file, int line_number);
#define assert(p) {if (!(p)) assert_error(__FILE__, __LINE__);}
#endif

#endif

//
//	History:
//	$Log: assert.hpp,v $
//	Revision 1.6  1998/08/31 19:13:32  gsl
//	drcs update
//	
//

//	
//	RCS file: /disk1/neomedia/RCS/wisp/wproc/assert.hpp,v
//	Working file: assert.hpp
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
//	date: 1995-06-02 09:07:07-04;  author: gsl;  state: V4_3_00;  lines: +2 -2
//	Change the assert() to an {if (x)...} construct
//	----------------------------
//	revision 1.4
//	date: 1995-04-25 05:59:39-04;  author: gsl;  state: V3_3_16;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:51:56-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:32:34-05;  author: gsl;  state: V3_3x12;  lines: +0 -0
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:50:56-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
