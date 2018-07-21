//
//	Copyright (c) 1996-1998 NeoMedia Technologies Inc. All rights reserved.
//
//	Project:	WPROC
//	
//// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : p_var.cpp
// Author : George Soules
// Date   : 11 April 1991

#if RUNTIME
#else

// Specification
#include "compiler.hpp"

// Classes
#include "options.hpp"

// Definitions and subprograms
#include "debugaid.hpp"
#include "txt.hpp"


//
//  parameters
//     -> '(' expression (',' expression_list ')' | another_expression_list)
//
//  expression_list
//     -> (expression list ',')
//
//  another_expression_list
//     -> ')' ('(' expression_list ')')?


int compiler::parse_comma_list() {
   int count = 0;
   while (kind_is(tk_comma)) {
      discard(); // ','
      if (kind_is(tk_mult)) {
         the_emitter->emit_opcode(push_int_8_and_range_op);
         the_emitter->emit_int_8(-1);
         emit_range_of(the_token);
         discard(); // '*'
      }
      else {
         expression(54);
         if (in_error_state)
            return -1;
      }
      count += 1;
   }
   if (! kind_is(tk_parenright)) {
      syntax_error(7);
      return -1;
   }
   discard(); // ')'
   return count;
}


int compiler::parse_parameters(symbol *a_symbol) {

   // Returns a parameter count
   // -1  -- syntax or semantic error
   //  0  -- no parameters
   //  1  -- array ref or one parameter
   //  n  -- n parameters
   // -n  -- array ref and n-1 parameters

   symbol_attributes is = a_symbol->is;
   Boolean           array_ref = false;
   int               count     = 0;
   int               list_count;


   if (! (is.string || is.builtin || is.array || is.undeclared))
      return 0;

   if (kind_is(tk_parenleft)) {
      discard(); // '('
      expression(54);
      if (in_error_state)
         return -1;
      count += 1;
      if (kind_is(tk_comma)) {
         if (is.array) {
            syntax_error(13);
            return -1;
         }
         list_count = parse_comma_list();
         if (list_count == -1)
            return -1;
         count += list_count;
      }
      else {
         if (! kind_is(tk_parenright)) {
            syntax_error(7);
            return -1;
         }
         discard(); // ')'
         if (is.undeclared || (is.array && is.string)) {
            if (kind_is(tk_parenleft)) {
               discard(); // '('
               array_ref = true;
               expression(54);
               if (in_error_state)
                  return -1;
               count += 1;
               list_count = parse_comma_list();
               if (list_count == -1)
                  return -1;
               count += list_count;
            }
         }
      }
   }
   if (array_ref)
      count = -count;

   return count;
}


int compiler::parse_variable(symbol *a_symbol, range &a_range) {
   token *var_token = new token(*the_token);
   discard(); // variable
   int args = parse_parameters(a_symbol);

   if (user_options.compile() && a_symbol->is.builtin) {
      builtin_data &the_data = (builtin_data&) a_symbol->data_ref();
      if (args < the_data.min_args() || args > the_data.max_args()) {
         token *temp = the_token;
         the_token = var_token;
         semantic_warning(45);
         the_token = temp;
      }
   }
   delete var_token;

   if (args != -1) {
      the_emitter->emit_opcode(push_variable_and_range_op);
      symbol::emit_id(a_symbol->id(), *the_emitter);
      the_emitter->emit_int_8(args);
      a_range.row.last = previous_token.row;
      a_range.col.last = previous_token.last_column;
      the_emitter->emit_int_32(a_range.pack());
   }
   return args;
}
#endif



//
//	History:
//	$Log: p_var.cpp,v $
//	Revision 1.6  1998/08/31 19:14:05  gsl
//	drcs update
//	
//

//	
//	RCS file: /disk1/neomedia/RCS/wisp/wproc/p_var.cpp,v
//	Working file: p_var.cpp
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
//	date: 1996-07-25 14:15:53-04;  author: gsl;  state: V4_3_00;  lines: +0 -0
//	Renamed from p_var.cc to p_var.cpp
//	----------------------------
//	revision 1.4
//	date: 1995-04-25 06:00:13-04;  author: gsl;  state: V3_3_19;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:52:29-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:33:11-05;  author: gsl;  state: V3_3x12;  lines: +1 -1
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:19-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
