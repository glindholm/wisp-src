// Copyright (c) Lexical Software, 1991.  All rights reserved.
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

