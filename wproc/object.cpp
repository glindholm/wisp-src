//
//	Copyright (c) 1996-1998 NeoMedia Technologies Inc. All rights reserved.
//
//	Project:	WPROC
//	
//// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : object.cpp
// Author : George Soules
// Date   : 22 February 1991

// Specification
#include "object.hpp"

// Classes
#include "options.hpp"

// Definitions and subprograms
#include "crt_io.hpp"
#include "debugaid.hpp"
#include "environ.hpp"
#include "memory.hpp"
#include "report.hpp"


#if DOS
void* object::operator new(size_t size) {
   void *allocated = ::operator new(size);
   if (allocated == NULL) {
      write_stdout("\nMemory exhausted allocating an object\n");
      restore_video_state();
      exit(0);
   }
   if (user_options.debug_trace_memory())
      show_memory("alloc object ", allocated, size);
   return allocated;
}


void object::operator delete(void* p, size_t size) {
   assert(valid_ptr(p));
   if (user_options.debug_trace_memory())
      show_memory("free object  ", p, size);
   delete (void*) p;
}
#endif


//
//	History:
//	$Log: object.cpp,v $
//	Revision 1.7  2003/02/11 19:05:26  gsl
//	Remove unneeded #ifdef's for DEBUG
//	
//	Revision 1.6  1998/08/31 19:13:58  gsl
//	drcs update
//	
//

//	
//	RCS file: /disk1/neomedia/RCS/wisp/wproc/object.cpp,v
//	Working file: object.cpp
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
//	date: 1996-07-25 14:15:31-04;  author: gsl;  state: V4_3_00;  lines: +0 -0
//	Renamed from object.cc to object.cpp
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
//	date: 1995-01-27 18:33:04-05;  author: gsl;  state: V3_3x12;  lines: +0 -0
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:15-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
