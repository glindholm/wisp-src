//
//	Copyright (c) 1996-1998 NeoMedia Technologies Inc. All rights reserved.
//
//	Project:	WPROC
//	Id:		$Id:$
//	RCS:		$Source:$
//	
//
#if AIX
#include <ctype.h>
int isspace(char c) {
   return isspace((int) c);
}
#endif

//
//	History:
//	$Log: isspace.cpp,v $
//	Revision 1.7  1998-08-31 15:50:34-04  gsl
//	drcs update
//
//	Revision 1.6  1998-08-31 15:13:53-04  gsl
//	drcs update
//
//

//	
//	RCS file: /disk1/neomedia/RCS/wisp/wproc/isspace.cpp,v
//	Working file: isspace.cpp
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
//	date: 1996-07-25 14:15:21-04;  author: gsl;  state: V4_3_00;  lines: +0 -0
//	Renamed from isspace.cc to isspace.cpp
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
//	date: 1995-01-27 18:32:58-05;  author: gsl;  state: V3_3x12;  lines: +0 -0
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:12-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
