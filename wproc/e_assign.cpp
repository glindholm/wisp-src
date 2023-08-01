//
//	Copyright (c) Shell Stream Software LLC, All Rights Reserved.
//
//	Project:	WPROC
//	
//
// Module : e_assign.cpp
// Author : George Soules
// Date   : 5 April 1991

// Specification
#include "machine.hpp"

// Classes
// (none)

// Definitions and subprograms
#include <string.h>
#include "debugaid.hpp"
#include "memory.hpp"
#include "txt.hpp"
#include "utility.hpp"


void machine::exec_assign() {
   int_8   args;
   int_16  array_index;

   pop_expressions(2);

   lvalue &the_lvalue = exp[1].lvalue_ref();
   symbol &the_symbol = the_lvalue.symbol_ref();

   if (the_symbol.is.builtin) {
      fatal_error(the_lvalue.symbol_range(), 23);
      enter_cancel_state();
      return;
   }

   args = the_lvalue.args();

   data &data_ref = the_symbol.data_ref();

   if (the_symbol.is.array) {
      if (the_lvalue.args() == 0) {
         fatal_error(the_lvalue.symbol_range(), 58);
         enter_cancel_state();
         return;
      }
      array_index = the_lvalue.array_index() - 1;
   }

   if (the_symbol.is.integer) {
      int_32 the_rvalue = exp[2].integer(17, txt_int_value);
      if (bad_exp(2))
         return;
      if (the_symbol.is.array) {
         integer_array_data &the_data = (integer_array_data&) data_ref;
         the_data[array_index] = the_rvalue;
      }
      else {
         integer_data &the_data = (integer_data&) data_ref;
         the_data.set_contents(the_rvalue);
      }
      return;
   }

   assert(the_symbol.is.string);
   int_16 start  = the_lvalue.start();
   int_16 length = the_lvalue.length();

   if (the_symbol.is.array) {
      string_array_data &the_data = (string_array_data&) data_ref;
      if (args > 1)
         the_data[array_index].set_substring(start, length, exp[2].string());
      else
         the_data[array_index].set_contents(exp[2].string());

      if (! the_data.ok()) {
         exp[1].fatal_error(53, (int_32) 0, NULL);
         enter_cancel_state();
         return;
      }
   }
   else {
      string_data &the_data = (string_data&) data_ref;
      if (args)
         the_data.set_substring(start, length, exp[2].string());
      else
         the_data.set_contents(exp[2].string());

      if (! the_data.ok()) {
         exp[1].fatal_error(53, (int_32) 0, NULL);
         enter_cancel_state();
         return;
      }
   }
}


void machine::assign(expression *a_variable, char *text) {
   the_stack->push(a_variable);
   the_stack->push(new expression(dup_string(text)));
   exec_assign();
}


void machine::assign(expression *a_variable, int_32 a_number) {
   the_stack->push(a_variable);
   the_stack->push(new expression(a_number));
   exec_assign();
}


void machine::assign(expression *a_variable, expression *a_value) {
   the_stack->push(a_variable);
   the_stack->push(a_value);
   exec_assign();
}


//
//	History:
//	$Log: e_assign.cpp,v $
//	Revision 1.7  1998/08/31 19:13:41  gsl
//	drcs update
//	
//

//	
//
//	Working file: e_assign.cpp
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
//	date: 1997-10-01 13:49:49-04;  author: gsl;  state: V4_3_00;  lines: +3 -3
//	fix warnings
//	----------------------------
//	revision 1.5
//	date: 1996-07-25 14:14:40-04;  author: gsl;  state: V4_1_02;  lines: +0 -0
//	Renamed from e_assign.cc to e_assign.cpp
//	----------------------------
//	revision 1.4
//	date: 1995-04-25 05:59:49-04;  author: gsl;  state: V3_3_19;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:52:06-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:32:45-05;  author: gsl;  state: V3_3x12;  lines: +0 -0
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:03-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
