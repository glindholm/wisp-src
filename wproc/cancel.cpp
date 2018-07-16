//
//	Copyright (c) 1996-1998 NeoMedia Technologies Inc. All rights reserved.
//
//	Project:	WPROC
//	Id:		$Id:$
//	RCS:		$Source:$
//	
//// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : cancel.cpp
// Author : George Soules
// Date   : 5 July 1991


// Specification
#include "cancel.hpp"

// Classes
// (none)

// Definitions and subprograms
#if DOS || DOS_HOST
#include <dos.h>
#include <process.h>
#else
#if UNIX || WIN32
#include <signal.h>
#endif
#endif

int cancel_requests = 0;


#if UNIX || WIN32
void signal_handler(int sig) {
   set_cancel_handler();
   cancel_handler();
}
#endif


void set_cancel_handler() {
#if DOS || DOS_HOST
   ctrlbrk(cancel_handler);
#else
#if UNIX || WIN32
   signal(SIGINT, signal_handler);
#endif
#endif
}


#if DOS || DOS_HOST
// This code is reached via DOS interrupt 28 (ctrl-c), not via a C++ call.
// Therefore it must not do a stack check and must only return.  Any attempt
// to exit() from this code will cause the C++ runtime to think something
// is wrong and issue a fatal runtime error.
#pragma option -N-
#endif

int cancel_handler() {
   cancel_requests += 1;
   return cancel_requests;
}


Boolean cancel_requested() {
   if (cancel_requests) {
      cancel_requests = 0;
      return true;
   }
   else
      return false;
}


//
//	History:
//	$Log: cancel.cpp,v $
//	Revision 1.7  1998-08-31 15:13:34-04  gsl
//	drcs update
//
//

//	
//	RCS file: /disk1/neomedia/RCS/wisp/wproc/cancel.cpp,v
//	Working file: cancel.cpp
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
//	date: 1996-07-25 19:45:16-04;  author: gsl;  state: V4_3_00;  lines: +3 -3
//	Fix for NT
//	----------------------------
//	revision 1.5
//	date: 1996-07-25 14:14:23-04;  author: gsl;  state: Exp;  lines: +0 -0
//	Renamed from cancel.cc to cancel.cpp
//	----------------------------
//	revision 1.4
//	date: 1995-04-25 05:59:41-04;  author: gsl;  state: V3_3_19;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:51:57-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:32:36-05;  author: gsl;  state: V3_3x12;  lines: +1 -1
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:50:58-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
