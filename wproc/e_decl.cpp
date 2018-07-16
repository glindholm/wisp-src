// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : e_decl.cpp
// Author : George Soules
// Date   : 5 April 1991

// Specification
#include "machine.hpp"

// Classes
#include "process.hpp"

// Definitions and subprograms
#include <string.h>
#include "debugaid.hpp"
#include "memory.hpp"
#include "utility.hpp"
#include "txt.hpp"


void machine::exec_declare(Boolean formal_parameter) {
   int               args = 0;
   int               index = 0;
   array_index       array_size;
   string_index      string_size = 0;
   const char       *init_string = NULL;
   int_32            init_integer = 0;
   symbol_attributes is;
   int_16            symbols_to_allocate;
   int_32            symbol_range;
   symbol           *the_symbol;
   data             *the_data;
   expression       *an_exp;

   symbols_to_allocate = the_pcode_reader->get_int_8();

#if WANG
   if (1 == the_process->nesting_level && user_options.fetched_args())
      the_args->increment_formals_count(symbols_to_allocate);
#endif

   // Get first symbol
   the_symbol = symbol_from_pcode();
   is = the_symbol->is;
   symbol_range = the_pcode_reader->get_int_32();

   if (is.array)
      args += 1;
   if (is.string && ! is.dynamic)
      args += 1;
   if (is.initialized)
      args += 1;

   pop_expressions(args);

   // Get array size
   if (is.array) {
      index += 1;
      array_size = (array_index)exp[index].integer(1, INT_MAX, 18, txt_array_size);
      if (bad_exp(index)) return;
   }

   // Get string size
   if (is.string && ! is.dynamic) {
      index += 1;
      string_size = (string_index)exp[index].integer(1, INT_MAX, 18, txt_str_size);
      if (bad_exp(index)) return;
   }

   // Get initial value
   if (is.initialized) {
      index += 1;
      if (is.string)
         init_string = exp[index].string();
      else {
         init_integer = exp[index].integer(18, txt_init_value);
         if (bad_exp(index)) return;
      }
   }

   // Allocate each variable
   while (symbols_to_allocate--) {
      the_data = NULL;
      if (formal_parameter) {
         if (++the_arg_index <= the_args->count()) {
            // Get passed value
            an_exp = the_args->exp(the_arg_index);
            if (an_exp) {
               // Parameter is an lvalue--see if it can be passed by reference
               lvalue &exp_lvalue = an_exp->lvalue_ref();
               symbol &exp_symbol = exp_lvalue.symbol_ref();
               if ((exp_symbol.is.integer && is.integer) ||
                   (exp_symbol.is.string  && is.string  &&
                    exp_symbol.is.dynamic == is.dynamic &&
                    exp_lvalue.args()     == 0))
               {
                  the_data = exp_symbol.data_address();
                  if (is.string && ! is.dynamic)
                     if ((int)strlen(an_exp->string()) != string_size)
                        // Fixed length string sizes don't match
                        the_data = NULL;
               }
            }
            if (! the_data) {
               // Parameter is either not an lvalue or can't be passed by ref
#if WANG
               if (1 == the_process->nesting_level && user_options.fetched_args()) {
                  if (is.integer) {
                     int_32 *l = (int_32 *) (the_args->value(the_arg_index));
                     init_integer = *l;
                  }
                  else {
                     if (string_size > FETCHED_ARG_SIZE) {
                        fatal_error(symbol_range, 77, FETCHED_ARG_SIZE);
                        enter_cancel_state();
                        return;
                     }
                     init_string = the_args->value(the_arg_index);
                  }
                  the_symbol->is.fetched_arg = true;
                  the_symbol->set_fetched_value
                     ((usign_8*) (the_args->value(the_arg_index)));
               }
	       else
	       {
		       init_string = the_args->value(the_arg_index);
		       if (is.integer) {
			       if (! string_to_int_32(init_string, init_integer)) {
				       fatal_error(symbol_range, 52, init_string);
				       enter_cancel_state();
				       return;
			       }
		       }
	       }
#else
               init_string = the_args->value(the_arg_index);
               if (is.integer) {
                  if (! string_to_int_32(init_string, init_integer)) {
                     fatal_error(symbol_range, 52, init_string);
                     enter_cancel_state();
                     return;
                  }
               }
#endif
            }
         }
      }
      if (the_data)
         the_symbol->is.not_local = true;  //Is a formal parameter
      else {
	 symbol		*global_symbol = NULL;
	 symbol_table	*global_table = the_symbol_table->global_symbol_table();

         if (global_table)
		global_symbol = global_table->global_lookup(the_symbol->name());

	 if (global_symbol) {
		// We've encountered a locally declared global symbol with the same
		// name as a global symbol declared in a parent procedure. Make
		// sure their type and size match, otherwise issue an error.

		Boolean type_and_size_match = false;

		if (global_symbol->is.integer && the_symbol->is.integer)
			type_and_size_match = true;
		else if (global_symbol->is.string && the_symbol->is.string) {
			if (global_symbol->is.dynamic == the_symbol->is.dynamic) {
				if (!global_symbol->is.dynamic) {
					string_data &the_data = (string_data&) global_symbol->data_ref();
					type_and_size_match = (string_size == the_data.size()) ? true:false;
				}
			}
		}

		if (type_and_size_match) {
			if (the_symbol->is.initialized) {
				fatal_error(symbol_range, 80); // #480 -- global is re-initialized
				enter_cancel_state();
				return;
			}
			the_symbol->is.not_local = true;
			the_data = global_symbol->data_address();
		}
		else {
			fatal_error(symbol_range, 81); // #481 -- type or size mis-match
			enter_cancel_state();
			return;
		}
	 }
      }
  
      if (!the_symbol->is.not_local) {
	    if (is.integer) {
               if (is.array)
                  the_data = new integer_array_data(array_size, init_integer);
               else
                  the_data = new integer_data(init_integer);
            }
            else { // is.string
               if (is.array)
                  the_data = new string_array_data(array_size, string_size, init_string);
               else
                  if (is.dynamic)
                     the_data = new dynamic_string_data(init_string);
                  else
                     the_data = new fixed_string_data(string_size, init_string);
            }
      }

      if (the_symbol->is.allocated)
         // Declare statement being executed again
         the_symbol->delete_data();
      else
         the_symbol->is.allocated = true;

      the_symbol->set_data(the_data);
      the_symbol_table->activate(the_symbol); // FIX001 - Activate the symbol

      // Check the the_data after assigning to symbol so that
      // the_data will get deleted when the symbol is deleted.
      if (! the_data->ok()) {
         exp[1].fatal_error(3, is.array ? array_size     : string_size,
                               is.array ? txt_array_size : txt_str_size);
         enter_cancel_state();
         return;
      }

      if (symbols_to_allocate) {
         the_symbol = symbol_from_pcode();
         symbol_range = the_pcode_reader->get_int_32();
      }
   }
}
