//
//	Copyright (c) 1996-1998 NeoMedia Technologies Inc. All rights reserved.
//
//	Project:	WPROC
//	
//// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : e_substr.cpp
// Author : George Soules
// Date   : 11 April 1991

// Specification
#include "machine.hpp"

// Classes
// (none)

// Definitions and subprograms
#include "txt.hpp"


Boolean machine::valid_substring(
   int          args,
   int_32       &start,
   int_32       &length,
   string_index a_size)
{
   assert(args >= 0 && args <= 2);
   if (args) {
      int index = exp.last_index_referenced() + 1;
      assert(exp.exists(index));
      if (a_size == 0) {
         exp[index].fatal_error(48, (char *) NULL, NULL);
         return false;
      }
      start = exp[index].integer(1, a_size, 19, txt_start_value);
      if (bad_exp(index)) return false;
      string_index max_length = a_size - start + 1;
      length = max_length;
      if (args > 1) {
         index += 1;
         if (exp[index].kind() == expression::integer_kind) {
            if (exp[index].integer() == -1)
               length = -1;
         }
         if (length != -1) {
            length = exp[index].integer(0, max_length, 19, txt_length_value);
            if (bad_exp(index)) return false;
         }
      }
   }
   return true;
}


//
//	History:
//	$Log: e_substr.cpp,v $
//	Revision 1.6  1998/08/31 19:13:44  gsl
//	drcs update
//	
//

//	
//	RCS file: /disk1/neomedia/RCS/wisp/wproc/e_substr.cpp,v
//	Working file: e_substr.cpp
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
//	date: 1996-07-25 14:14:54-04;  author: gsl;  state: V4_3_00;  lines: +0 -0
//	Renamed from e_substr.cc to e_substr.cpp
//	----------------------------
//	revision 1.4
//	date: 1995-04-25 05:59:52-04;  author: gsl;  state: V3_3_19;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:52:10-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:32:48-05;  author: gsl;  state: V3_3x12;  lines: +1 -1
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:05-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
