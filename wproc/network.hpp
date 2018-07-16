//
//	Copyright (c) 1996-1998 NeoMedia Technologies Inc. All rights reserved.
//
//	Project:	WPROC
//	Id:		$Id:$
//	RCS:		$Source:$
//	
//// Copyright (c) Lexical Software, 1992.  All rights reserved.
//
// Module : network.hpp
// Author : George Soules
// Date   : 26 July 1992

#if NETWORK

#ifndef NETWORK__HPP
#define NETWORK__HPP

// Classes
#include "machine.hpp"

// Definitions and subprograms
#include "environ.hpp"


char    *nw_network_address();
Boolean  nw_network_available();
char    *nw_user_full_name();
Boolean  nw_user_logged_in();
char    *nw_user_login_name();

#endif
#endif


//
//	History:
//	$Log: network.hpp,v $
//	Revision 1.5  1998/08/31 19:13:58  gsl
//	drcs update
//	
//

//	
//	RCS file: /disk1/neomedia/RCS/wisp/wproc/network.hpp,v
//	Working file: network.hpp
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
//	date: 1995-04-25 06:00:06-04;  author: gsl;  state: V4_3_00;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:52:23-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:33:03-05;  author: gsl;  state: V3_3x12;  lines: +2 -2
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:14-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
