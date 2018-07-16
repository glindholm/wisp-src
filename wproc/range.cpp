//
//	Copyright (c) 1996-1998 NeoMedia Technologies Inc. All rights reserved.
//
//	Project:	WPROC
//	Id:		$Id:$
//	RCS:		$Source:$
//	
//// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : range.cpp
// Author : George Soules
// Date   : 1 April 1991

// Specification
#include "range.hpp"

// Classes
// (none)

// Definitions and subprograms
#include "debugaid.hpp"


// Note that a packed range does not contain row.last.
// Packed ranges are used for lexical elements that
// cannot cross line boundaries (namely tokens).

union sub_range {
   struct {
      usign_16 row;
      struct {
         usign_8 first;
         usign_8 last;
      } col;
   } unpacked;
   int_32 packed;
};


range::range() {
// trace(object, "range");
   row.first = 0;
   row.last  = 0;
   col.first = 0;
   col.last  = 0;
}


range::range(int_32 a_packed_range) {
// trace(object, "range from packed range");
   sub_range sr;
   sr.packed = a_packed_range;
   row.first = sr.unpacked.row;
   row.last  = sr.unpacked.row;
   col.first = sr.unpacked.col.first;
   col.last  = sr.unpacked.col.last;
}


range::range(int_32 a_packed_range_1, int_32 a_packed_range_2) {
   range range_1(a_packed_range_1);
   range range_2(a_packed_range_2);
   row.first = range_1.row.first;
   col.first = range_1.col.first;
   row.last  = range_2.row.last;
   col.last  = range_2.col.last;
}


range::range(token *a_token) {
// trace(object, "range from token");
   row.first = a_token->row();
   row.last  = row.first;
   col.first = (usign_8)a_token->column();
   col.last  = (usign_8)a_token->last_column();
}


int_32 range::pack() {
   sub_range sr;
   sr.unpacked.row       = row.first;
   sr.unpacked.col.first = col.first;
   sr.unpacked.col.last  = col.last;
   return sr.packed;
}



//
//	History:
//	$Log: range.cpp,v $
//	Revision 1.7  1998-08-31 15:14:10-04  gsl
//	drcs update
//
//

//	
//	RCS file: /disk1/neomedia/RCS/wisp/wproc/range.cpp,v
//	Working file: range.cpp
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
//	date: 1997-10-02 08:39:54-04;  author: gsl;  state: V4_3_00;  lines: +2 -2
//	fix warnings
//	----------------------------
//	revision 1.5
//	date: 1996-07-25 14:16:05-04;  author: gsl;  state: V4_1_02;  lines: +0 -0
//	Renamed from range.cc to range.cpp
//	----------------------------
//	revision 1.4
//	date: 1995-04-25 06:00:17-04;  author: gsl;  state: V3_3_19;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:52:33-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:33:16-05;  author: gsl;  state: V3_3x12;  lines: +0 -0
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:22-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
