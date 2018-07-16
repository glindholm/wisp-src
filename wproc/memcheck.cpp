//
//	Copyright (c) 1996-1998 NeoMedia Technologies Inc. All rights reserved.
//
//	Project:	WPROC
//	Id:		$Id:$
//	RCS:		$Source:$
//	
//// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : memcheck.cpp
// Author : George Soules
// Date   : 9 February 1991

#if DEBUG

// Specification
// "memcheck.hpp" (not included to avoid redeclaration of state variables)

// Classes
#if DOS || DOS_HOST
#include <iostream.h>
#endif

// Definitions and subprograms
#if DOS || DOS_HOST
#include <alloc.h>
#endif
#include <ctype.h>
#include "environ.hpp"


void check_and_verify_heap(int caller_id) {
#if DOS || DOS_HOST
   if (heapcheck() == _HEAPCORRUPT)
      cout << "\nWARNING: HEAP IS CORRUPT (" << caller_id << ")\n";
#endif
}


void dump_heap() {
#if DOS || DOS_HOST
   void         *last_in_use_ptr = NULL;
   unsigned int  last_in_use_size = 0;
   char          c;
   struct        heapinfo hi;
   hi.ptr = NULL;
   while (heapwalk(&hi) == _HEAPOK)
      if (hi.in_use) {
         last_in_use_ptr = hi.ptr;
         last_in_use_size = hi.size;
         cout << hi.size << " bytes at " << hi.ptr << "\n";
      }
   cout << "Last heap node in use (" << last_in_use_size
        << " bytes at " << last_in_use_ptr << "):\n";
   for (int i = 0; i < last_in_use_size; i++) {
      c = (((char*) last_in_use_ptr)+i)[0];
      if (! isprint(c))
         c = '.';
      cout << c;
   }
#endif
}


void check_memory_usage(
   unsigned long initial_memory,
   unsigned long current_memory)
{
#if DOS || DOS_HOST
   Boolean leakage = false;
   if (current_memory != initial_memory) {
      cout << "\n";
      cout << "MEMORY USAGE WARNING:\n";
      cout << "Initial = " << initial_memory << "\n";
      cout << "Current = " << current_memory << "\n";
      leakage = true;
   }
   check_and_verify_heap(0);

   if (leakage)
      dump_heap();
#endif
}

#endif


//
//	History:
//	$Log: memcheck.cpp,v $
//	Revision 1.9  1998-08-31 15:13:55-04  gsl
//	drcs update
//
//

//	
//	RCS file: /disk1/neomedia/RCS/wisp/wproc/memcheck.cpp,v
//	Working file: memcheck.cpp
//	head: 1.8
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
//	total revisions: 8;	selected revisions: 8
//	description:
//	----------------------------
//	revision 1.8
//	date: 1997-06-10 00:34:10-04;  author: scass;  state: V4_3_00;  lines: +2 -2
//	Alpha doe snot have int_32 defined so change back to long
//	because not "really" needed.
//	----------------------------
//	revision 1.7
//	date: 1997-06-09 17:38:44-04;  author: scass;  state: Exp;  lines: +2 -2
//	int4 -> int_32
//	----------------------------
//	revision 1.6
//	date: 1997-06-09 16:47:55-04;  author: scass;  state: Exp;  lines: +2 -2
//	Changed long to int4 for portability.
//	----------------------------
//	revision 1.5
//	date: 1996-07-25 14:15:25-04;  author: gsl;  state: V3_3_93;  lines: +0 -0
//	Renamed from memcheck.cc to memcheck.cpp
//	----------------------------
//	revision 1.4
//	date: 1995-04-25 06:00:04-04;  author: gsl;  state: V3_3_19;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:52:21-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:33:00-05;  author: gsl;  state: V3_3x12;  lines: +1 -1
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:13-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
