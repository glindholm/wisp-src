//
//	Copyright (c) 1996-1998 NeoMedia Technologies Inc. All rights reserved.
//
//	Project:	WPROC
//	Id:		$Id:$
//	RCS:		$Source:$
//	
//// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : cancel.hpp
// Author : George Soules
// Date   : 5 July 1991

#ifndef CANCEL__HPP
#define CANCEL__HPP

// Classes
// (none)

// Definitions and subprograms
#include "environ.hpp"


void set_cancel_handler();

int cancel_handler();

Boolean cancel_requested();

#endif


//
//	History:
//	$Log: cancel.hpp,v $
//	Revision 1.5  1998/08/31 19:13:34  gsl
//	drcs update
//	
//

//	
//	RCS file: /disk1/neomedia/RCS/wisp/wproc/cancel.hpp,v
//	Working file: cancel.hpp
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
//	date: 1995-04-25 05:59:41-04;  author: gsl;  state: V4_3_00;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:51:58-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:32:37-05;  author: gsl;  state: V3_3x12;  lines: +1 -1
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:50:58-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
