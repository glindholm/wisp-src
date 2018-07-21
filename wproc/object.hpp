//
//	Copyright (c) Shell Stream Software LLC, All Rights Reserved.
//
//	Project:	WPROC
//	
//// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : object.hpp
// Author : George Soules
// Date   : 22 February 1991

#ifndef OBJECT__HPP
#define OBJECT__HPP

// Classes
// (none)

// Definitions and subprograms
#include <stdlib.h>


class object {
   public:
#if DOS
      void *operator new(size_t size);
      void  operator delete(void* p, size_t size);
#endif
};

#endif


//
//	History:
//	$Log: object.hpp,v $
//	Revision 1.6  2003/02/11 19:05:26  gsl
//	Remove unneeded #ifdef's for DEBUG
//	
//	Revision 1.5  1998/08/31 19:13:58  gsl
//	drcs update
//	
//

//	
//	RCS file: /disk1/neomedia/RCS/wisp/wproc/object.hpp,v
//	Working file: object.hpp
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
//	date: 1995-04-25 06:00:07-04;  author: gsl;  state: V4_3_00;  lines: +0 -0
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
