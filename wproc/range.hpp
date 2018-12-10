//
//	Copyright (c) Shell Stream Software LLC, All Rights Reserved.
//
//	Project:	WPROC
//	
//// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : range.hpp
// Author : George Soules
// Date   : 1 April 1991

#ifndef RANGE__HPP
#define RANGE__HPP

// Classes
#include "object.hpp"
#include "token.hpp"

// Definitions and subprograms
#include "environ.hpp"


class range : public object {
   public:
      range();
      range(int_32 a_packed_range);
      range(int_32 a_packed_range_1, int_32 a_packed_range_2);
      range(token *a_token);
      int_32 pack();
      struct {
         usign_16 first;
         usign_16 last;
      } row;
      struct {
         usign_8 first;
         usign_8 last;
      } col;
};

#endif


//
//	History:
//	$Log: range.hpp,v $
//	Revision 1.7  1998/08/31 19:14:10  gsl
//	drcs update
//	
//

//	
//
//	Working file: range.hpp
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
//	date: 1997-10-01 18:36:52-04;  author: gsl;  state: V4_3_00;  lines: +2 -2
//	Changed my mine
//	----------------------------
//	revision 1.5
//	date: 1997-10-01 18:31:34-04;  author: gsl;  state: Exp;  lines: +2 -2
//	fix warnings
//	----------------------------
//	revision 1.4
//	date: 1995-04-25 06:00:18-04;  author: gsl;  state: V4_1_02;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:52:34-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:33:17-05;  author: gsl;  state: V3_3x12;  lines: +0 -0
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:22-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
