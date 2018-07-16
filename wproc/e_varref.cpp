// Copyright (c) Lexical Software, 1991.  All rights reserved.
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
   int_32  start;
   int_32  length;

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



