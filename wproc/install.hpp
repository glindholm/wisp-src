//
//	Copyright (c) Shell Stream Software LLC, All Rights Reserved.
//
//	Project:	WPROC
//	
//
// Module : install.hpp
// Author : George Soules
// Date   : 7 February 1991

#ifndef INSTALL__HPP
#define INSTALL__HPP

// Classes
// (none)

// Definitions and subprograms
#include "environ.hpp"

#if DOS || DOS_HOST
#define MESSAGES_NAME "RUN"
#define MESSAGES_EXT  ".MSG"
#else
#define MESSAGES_NAME "wproc"
#define MESSAGES_EXT  ".msg"
#endif

char *installation_pathname(char *file, char *ext);

Boolean installation_ok();

#if RUNTIME
#if DEMO
Boolean runtime_demo_ok(const char *source_name);
#endif
#endif

#endif


//
//	History:
//	$Log: install.hpp,v $
//	Revision 1.6  1998/08/31 19:13:53  gsl
//	drcs update
//	
//

//	
//
//	Working file: install.hpp
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
//	date: 1996-07-25 19:46:59-04;  author: gsl;  state: V4_3_00;  lines: +0 -2
//	NT
//	----------------------------
//	revision 1.4
//	date: 1995-04-25 06:00:02-04;  author: gsl;  state: V3_3_19;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:52:19-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:32:58-05;  author: gsl;  state: V3_3x12;  lines: +2 -2
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:11-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
