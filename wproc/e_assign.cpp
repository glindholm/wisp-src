// Copyright (c) Lexical Software, 1991.  All rights reserved.
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

