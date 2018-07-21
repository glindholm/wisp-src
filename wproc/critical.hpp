//
//	Copyright (c) 1996-1998 NeoMedia Technologies Inc. All rights reserved.
//
//	Project:	WPROC
//	
//// Copyright (c) Lexical Software, 1992.  All rights reserved.
//
// Module : critical.hpp
// Author : George Soules
// Date   : 3 July 1992

#ifndef CRITICAL__HPP
#define CRITICAL__HPP

// Classes
// (none)

// Definitions and subprograms
#include "environ.hpp"


#if DOS

Boolean drive_ready(int drive);

#endif
#endif


//
//	History:
//	$Log: critical.hpp,v $
//	Revision 1.5  1998/08/31 19:13:37  gsl
//	drcs update
//	
//

//	
//	RCS file: /disk1/neomedia/RCS/wisp/wproc/critical.hpp,v
//	Working file: critical.hpp
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
//	date: 1995-04-25 05:59:44-04;  author: gsl;  state: V4_3_00;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:52:00-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:32:40-05;  author: gsl;  state: V3_3x12;  lines: +1 -1
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:00-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
