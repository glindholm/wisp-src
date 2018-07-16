// Copyright (c) Lexical Software, 1991.  All rights reserved.
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




