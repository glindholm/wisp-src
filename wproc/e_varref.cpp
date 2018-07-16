//
//	Copyright (c) 1996-1998 NeoMedia Technologies Inc. All rights reserved.
//
//	Project:	WPROC
//	Id:		$Id:$
//	RCS:		$Source:$
//	
//// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : e_varref.cpp
// Author : George Soules
// Date   : 22 April 1991

// Specification
#include "machine.hpp"

// Classes
// (none)

// Definitions and subprograms
#include "txt.hpp"


Boolean machine::variable_reference() {
   symbol *the_symbol   = symbol_from_pcode();
   symbol_attributes is = the_symbol->is;
   int_8   args         = the_pcode_reader->get_int_8();
   int_32  symbol_range = the_pcode_reader->get_int_32();
   int_32  array_index  = 0;

   if (! valid_ref(args, the_symbol, symbol_range))
      return false;
   args = abs(args);

   pop_expressions(args);

   if (is.array) {
      array_data &the_data = (array_data&) the_symbol->data_ref();
      if (args) {
         array_index =
            exp[1].integer(1, the_data.dimension(), 16, txt_array_index);
         if (bad_exp(1))
            return false;
      }
      else
         array_index = 1;
   }

   if (is.integer) {
      int_32 i;
      if (is.array) {
         integer_array_data &the_data =
            (integer_array_data&) the_symbol->data_ref();
         i = the_data[array_index - 1];
      }
      else if (is.builtin) {
         builtin_data &the_data = (builtin_data&) the_symbol->data_ref();
         if (! builtin_value(i, args, the_data, symbol_range))
            return false;
      }
      else { // Simple integer variable
         integer_data &the_data = (integer_data&) the_symbol->data_ref();
         i = the_data.contents();
      }
      lvalue *the_lvalue =
         new lvalue(the_symbol, symbol_range, args, array_index);
      the_stack->push(new expression(i, symbol_range, the_lvalue));
      return true;
   }

   // Symbol is string

   char   *the_string;
   int_32  start = 0;
   int_32  length = 0;

   if (is.array) {
      string_array_data &the_data = (string_array_data&) the_symbol->data_ref();
      if (args > 1) {
         // Substring
         if (valid_substring(args - 1, start, length,
                             the_data[array_index - 1].size()))
            the_string = the_data[array_index - 1].substring((int_16)start, (int_16)length);
         else
            return false;
      }
      else
         the_string = the_data[array_index - 1].contents();
   }
   else if (is.builtin) {
      builtin_data &the_data = (builtin_data&) the_symbol->data_ref();
      if (! builtin_value(the_string, args, the_data, symbol_range))
         return false;
   }
   else { // Simple string variable
      string_data &the_data = (string_data&) the_symbol->data_ref();
      if (args) {
         // Substring
         if (valid_substring(args, start, length, the_data.size()))
            the_string = the_data.substring((int_16)start, (int_16)length);
         else
            return false;
      }
      else
         the_string = the_data.contents();
   }
   lvalue *the_lvalue =
      new lvalue(the_symbol, symbol_range, args, array_index, start, length);
   the_stack->push(new expression(the_string, symbol_range, the_lvalue));
   return true;
}




//
//	History:
//	$Log: e_varref.cpp,v $
//	Revision 1.8  1998-08-31 15:13:45-04  gsl
//	drcs update
//
//

//	
//	RCS file: /disk1/neomedia/RCS/wisp/wproc/e_varref.cpp,v
//	Working file: e_varref.cpp
//	head: 1.7
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
//	total revisions: 7;	selected revisions: 7
//	description:
//	----------------------------
//	revision 1.7
//	date: 1998-07-10 13:05:53-04;  author: gsl;  state: Exp;  lines: +2 -2
//	iitialize start and length
//	----------------------------
//	revision 1.6
//	date: 1997-10-01 18:48:56-04;  author: gsl;  state: V4_3_00;  lines: +2 -2
//	Fix warningd
//	----------------------------
//	revision 1.5
//	date: 1996-07-25 14:14:59-04;  author: gsl;  state: V4_1_02;  lines: +0 -0
//	Renamed from e_varref.cc to e_varref.cpp
//	----------------------------
//	revision 1.4
//	date: 1995-04-25 05:59:52-04;  author: gsl;  state: V3_3_19;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:52:11-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:32:49-05;  author: gsl;  state: V3_3x12;  lines: +1 -1
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:06-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
