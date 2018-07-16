//
//	Copyright (c) 1996-1998 NeoMedia Technologies Inc. All rights reserved.
//
//	Project:	WPROC
//	Id:		$Id:$
//	RCS:		$Source:$
//	
//// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : debugaid.cpp
// Author : George Soules
// Date   : 24 February 1991


// Specification
#include "debugaid.hpp"

// Classes
#include "process.hpp"

// Definitions and subprograms
#include "crt_io.hpp"
#include <ctype.h>

#include <stdarg.h>


int dprintf(const char *format, ...)
{
	va_list ap;
	char	buff[1024];
	static	FILE *fDebug = NULL;
	int	rc;

	va_start(ap, format);
			
	vsprintf(buff, format, ap);

	va_end(ap);

	if (NULL==fDebug)
	{
		char *ptr;
		ptr = getenv("WPROCDEBUGLOG");
		if (NULL==ptr || '\0' == *ptr)
		{
			ptr = "wprocdebug.log";
		}
		fDebug = fopen(ptr,"a+");
		if (NULL==fDebug)
		{
			fDebug = stderr;
		}
	}

	rc = fprintf(fDebug, "%s", buff);
	fflush(fDebug);

	return rc;
}

Boolean valid_ptr(void *p) {
#if WANG  // JUST DURING DEVELOPMENT
   if (user_options.debug_trace_heap())
      return true;
#else
   if (! user_options.debug_trace_heap())
      return true;
#endif

#if DOS || DOS_HOST
   struct heapinfo hi;

   if (heapcheck() == _HEAPCORRUPT) {
      cerr << "\nHEAP IS CORRUPT : " << p << "\n";
      return false;
   }

   if (p == NULL) {
      cerr << "\nATTEMPT TO USE NULL POINTER\n";
      return false;
   }

   hi.ptr = NULL;
   while (heapwalk(&hi) != _HEAPEND)
      if (p == hi.ptr && hi.in_use)
         return true;

   cerr << "\nATTEMPT TO USE BAD POINTER : " << p << " -> ";
   for (int i = 0; i < 16; i++) {
      char c = ((char*) p+i)[0];
      if (! isprint(c))
         c = '.';
      dprintf("%c",c);
   }
   cerr << '\n';
   return false;
#else
   return true;
#endif
}


static int trace_level = 0;
static int saved_trace_level = 0;

void print_trace_id(const char *trace_kind) {
   dprintf("%d %8s: ",the_process->nesting_level, trace_kind);
   for (int i = 1; i <= trace_level * 3; i++)
	dprintf(" ");
}


void debug_trace_begin(Boolean option, const char *msg, const char *kind) {
   if (option) {
      print_trace_id(kind);
      dprintf("%s\n",msg);
      trace_level += 1;
   }
}


void debug_trace_end(Boolean option) {
   if (option) {
      trace_level -= 1;
   }
}


void debug_trace(Boolean option, const char *msg, const char *kind) {
   if (option) {
      print_trace_id(kind);
      dprintf("%s\n",msg);
   }
}


void debug_trace_i(Boolean option, int_32 number, const char *kind) {
   if (option) {
	print_trace_id(kind);
	dprintf("%ld\n",number);
   }
}


void debug_trace_si(Boolean option, const char *msg, int_32 number, const char *kind) {
   if (option) {
      print_trace_id(kind);
      dprintf("%s%ld\n",msg, number);
   }
}


void debug_trace_sc(Boolean option, const char *msg, char the_char, const char *kind) {
   if (option) {
      print_trace_id(kind);
      dprintf("%s\"%c\"\n",msg, the_char);
   }
}


void debug_trace_ss(Boolean option, const char *msg1, const char *msg2, const char *kind) {
   if (option) {
      print_trace_id(kind);
      dprintf("%s%s\n", msg1, msg2);
   }
}


void debug_trace_sq(Boolean option, const char *msg1, const char *msg2, const char *kind) {
   if (option) {
      print_trace_id(kind);
      dprintf("%s\"%s\"\n", msg1, msg2);
   }
}


void debug_trace_level_save() {
   saved_trace_level = trace_level;
}


void debug_trace_level_restore() {
   trace_level = saved_trace_level;
}






//
//	History:
//	$Log: debugaid.cpp,v $
//	Revision 1.12  2003/02/11 19:05:26  gsl
//	Remove unneeded #ifdef's for DEBUG
//	
//	Revision 1.11  2001/08/22 20:17:50  gsl
//	fix missing type
//	
//	Revision 1.10  1999-09-13 15:46:39-04  gsl
//	remove odd debug if statement
//
//	Revision 1.9  1998-09-03 09:10:35-04  gsl
//	Changed to write debug trace info to file $WPROCDEBUGLOG (wprocdebug.log)
//	instead of to stdout which interfered with crt io.
//
//	Revision 1.8  1998-08-31 15:13:40-04  gsl
//	drcs update
//
//

//	
//	RCS file: /disk1/neomedia/RCS/wisp/wproc/debugaid.cpp,v
//	Working file: debugaid.cpp
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
//	date: 1997-06-09 17:35:33-04;  author: scass;  state: V4_3_00;  lines: +2 -2
//	int4 -> int_32
//	----------------------------
//	revision 1.6
//	date: 1997-06-09 16:44:52-04;  author: scass;  state: Exp;  lines: +2 -2
//	Changed long to int4 for compatibility.
//	----------------------------
//	revision 1.5
//	date: 1996-07-25 14:14:35-04;  author: gsl;  state: V3_3_93;  lines: +0 -0
//	Renamed from debugaid.cc to debugaid.cpp
//	----------------------------
//	revision 1.4
//	date: 1995-04-25 05:59:46-04;  author: gsl;  state: V3_3_19;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:52:03-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:32:43-05;  author: gsl;  state: V3_3x12;  lines: +24 -63
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:02-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
