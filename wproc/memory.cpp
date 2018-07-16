//
//	Copyright (c) 1996-1998 NeoMedia Technologies Inc. All rights reserved.
//
//	Project:	WPROC
//	Id:		$Id:$
//	RCS:		$Source:$
//	
//// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : memory.cpp
// Author : George Soules
// Date   : 8 April 1991

// Specification
#include "memory.hpp"

// Classes
#include "options.hpp"

// Definitions and subprograms
#if DOS || DOS_HOST
#include <alloc.h>
#endif
#include <stdio.h>
#include <string.h>
#include "crt_io.hpp"
#include "debugaid.hpp"


char *new_string(size_t size) {
   char *the_string = (char *) malloc(size);
   if (user_options.debug_trace_memory())
      show_memory("alloc string ", the_string, size);
   if (size && the_string == NULL) {
      puts("\nMemory exhausted allocating a string\n");
      restore_video_state();
      exit(0);
   }
   return the_string;
}


char *dup_string(const char *a_string) {
   char   *the_copy;
   size_t  size;
   size = strlen(a_string) + 1;
   the_copy = new_string(size);
   memcpy(the_copy, a_string, size);
   return the_copy;
}


void delete_string(char *&a_string) {
   if (a_string) {
      assert(valid_ptr(a_string));
      show_memory("free string  ", a_string, strlen(a_string) + 1);
      trace(memory, a_string);
      free(a_string);
      a_string = NULL;
   }
}


void show_memory(char *kind, void* ptr, size_t size) {
   char mess[80];
      
   sprintf(mess, "%s %5ld bytes at 0x%lX", kind, (long)size, (long)ptr);
   debug_trace((Boolean) user_options.debug_trace_memory(), mess, kind);

}




//
//	History:
//	$Log: memory.cpp,v $
//	Revision 1.9.2.1  2003/02/11 18:52:00  gsl
//	Removed unneeded #ifdef code for AIX and DEBUG
//	
//	Revision 1.9  1998/09/02 21:27:35  gsl
//	Changed to use debug_trace instead of printf()
//	
//	Revision 1.8  1998-08-31 15:13:56-04  gsl
//	drcs update
//
//

//	
//	RCS file: /disk1/neomedia/RCS/wisp/wproc/memory.cpp,v
//	Working file: memory.cpp
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
//	date: 1996-07-25 14:15:27-04;  author: gsl;  state: V4_3_00;  lines: +0 -0
//	Renamed from memory.cc to memory.cpp
//	----------------------------
//	revision 1.6
//	date: 1995-08-09 12:36:55-04;  author: gsl;  state: V3_3_19;  lines: +1 -1
//	fix trace of memory alloc/dealloc
//	----------------------------
//	revision 1.5
//	date: 1995-06-02 11:36:46-04;  author: gsl;  state: V3_3_18;  lines: +1 -1
//	fix printf() argument conversions
//	----------------------------
//	revision 1.4
//	date: 1995-04-25 06:00:04-04;  author: gsl;  state: V3_3_16;  lines: +0 -0
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
//	date: 1995-01-27 16:51:14-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
