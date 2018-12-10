//
//	Copyright (c) Shell Stream Software LLC, All Rights Reserved.
//
//	Project:	WPROC
//	
//// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : p_assign.cpp
// Author : George Soules
// Date   : 4 April 1991

#if RUNTIME
#else

// Specification
#include "compiler.hpp"

// Classes
// (none)

// Definitions and subprograms
#include <string.h>
#include "debugaid.hpp"


//  assign_statement
//     -> 'assign' (assignment list ',')
//
//  assignment
//     -> variable_name parameters? ('='|'+'|'-'|'*'|'/') expression

void compiler::assign_stmt() {
   trace_begin(parser, "assign_stmt");

   symbol   *the_symbol;
   int       args;

   discard(); // 'assign'

   while (true) {
      require_kind(tk_variable, 9);
      range the_range(the_token);
      the_symbol = lookup_or_create_symbol(the_token->lexeme());
      args = parse_variable(the_symbol, the_range);
      if (args == -1)
         return;

      opcode the_operator;
      if (lexeme_is("="))
         the_operator = assign_op;
      else if (kind_is(tk_plus))
         the_operator = add_op;
      else if (kind_is(tk_minus))
         the_operator = subtract_op;
      else if (kind_is(tk_mult))
         the_operator = multiply_op;
      else if (kind_is(tk_div))
         the_operator = divide_op;
      else
         improper_syntax(args ? 3 : 29);

      if (the_operator != assign_op) {
         discard();
         require_syntax("=", 18);
         the_emitter->emit_opcode(copy_and_push_top_op);
      }

      discard(); // operator
      require_nonterminal(expression(44));

      if (the_operator != assign_op)
         the_emitter->emit_opcode(the_operator);

      the_emitter->emit_opcode(assign_op);

      if (! kind_is(tk_comma))
         break;
      else
         discard(); // ','
   }

   trace_end(parser);
}
#endif





//
//	History:
//	$Log: p_assign.cpp,v $
//	Revision 1.7  1998/08/31 19:14:00  gsl
//	drcs update
//	
//

//	
//
//	Working file: p_assign.cpp
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
//	date: 1996-07-25 14:15:37-04;  author: gsl;  state: V4_3_00;  lines: +0 -0
//	Renamed from p_assign.cc to p_assign.cpp
//	----------------------------
//	revision 1.5
//	date: 1995-06-02 10:25:06-04;  author: gsl;  state: V3_3_19;  lines: +0 -1
//	fix warning
//	----------------------------
//	revision 1.4
//	date: 1995-04-25 06:00:09-04;  author: gsl;  state: V3_3_16;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:52:26-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:33:06-05;  author: gsl;  state: V3_3x12;  lines: +0 -0
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:16-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
