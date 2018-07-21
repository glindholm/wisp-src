//
//	Copyright (c) 1996-1998 NeoMedia Technologies Inc. All rights reserved.
//
//	Project:	WPROC
//	
//// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : memory.hpp
// Author : George Soules
// Date   : 8 April 1991

#ifndef MEMORY__HPP
#define MEMORY__HPP

// Classes
// (none)

// Definitions and subprograms
#include <stdlib.h>


char *new_string(size_t size);

char *dup_string(const char *a_string);

void delete_string(char *&a_string);

void show_memory(char *kind, void* ptr, size_t size);

#endif


//
//	History:
//	$Log: memory.hpp,v $
//	Revision 1.6  2003/02/11 19:05:26  gsl
//	Remove unneeded #ifdef's for DEBUG
//	
//	Revision 1.5  1998/08/31 19:13:56  gsl
//	drcs update
//	
//

//	
//	RCS file: /disk1/neomedia/RCS/wisp/wproc/memory.hpp,v
//	Working file: memory.hpp
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
//	date: 1995-04-25 06:00:05-04;  author: gsl;  state: V4_3_00;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:52:22-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:33:02-05;  author: gsl;  state: V3_3x12;  lines: +0 -0
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:14-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
