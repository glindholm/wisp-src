// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : p_ttyio.cpp
// Author : George Soules
// Date   : 15 June 1991

#if RUNTIME
#else

// Specification
#include "compiler.hpp"

// Classes
#include "symbols.hpp"

// Definitions and subprograms
#include "debugaid.hpp"


//  close_statement
//     -> 'close' filename-exp

void compiler::close_stmt() {
   trace_begin(parser, "close_stmt");
   discard(); // 'close'
   if (! parse_filename())
      return;
   the_emitter->emit_opcode(close_op);
   the_emitter->emit_opcode(label_assign_op);
   trace_end(parser);
}


//  open_statement
//     -> 'open' filename-exp 'for'? 'global'? ('reading' | 'writing')

void compiler::open_stmt() {
   trace_begin(parser, "open_stmt");
   discard(); // 'open'

   usign_8 fcn = 0;

   if (! parse_filename())
      return;

   allow_syntax("for");
   if (keyword_is("global")) {
      discard(); // 'global'
      fcn = fcn | 0x01;
   }
   if (keyword_is("reading")) {
      discard(); // 'reading'
      fcn = fcn | 0x02;
   }
   else if (keyword_is("writing")) {
      discard(); // 'writing'
      fcn = fcn | 0x04;
   }
   else
      syntax_error(69);

   the_emitter->emit_opcode(open_op);
   the_emitter->emit_usign_8(fcn);
   the_emitter->emit_opcode(label_assign_op);
   trace_end(parser);
}


//  read_statement
//     -> 'read' 'line'? variable ('from' filename-exp)?

void compiler::read_stmt() {
   trace_begin(parser, "read_stmt");
   discard(); // 'read'

   symbol *the_symbol;
   usign_8 fcn = 0;

   if (keyword_is("line")) {
      discard(); // 'line'
      fcn = fcn | 0x01;
   }

   require_kind(tk_variable, 9);
   range the_range(the_token);
   the_symbol = lookup_or_create_symbol(the_token->lexeme());
   if (parse_variable(the_symbol, the_range) == -1)
      return;

   if (keyword_is("from")) {
      discard(); // 'from'
      if (! parse_filename())
         return;
      fcn = fcn | 0x02;
   }
   the_emitter->emit_opcode(read_op);
   the_emitter->emit_usign_8(fcn);
   the_emitter->emit_opcode(label_assign_op);
   trace_end(parser);
}


//  write_statement
//     -> 'write' 'line'? exp ('to' filename-exp)?

void compiler::write_stmt() {
   trace_begin(parser, "write_stmt");
   discard(); // 'write'

   usign_8 fcn = 0;

   if (keyword_is("line")) {
      discard(); // 'line'
      fcn = fcn | 0x01;
   }
   require_nonterminal(expression(68));

   if (keyword_is("to")) {
      discard(); // 'to'
      if (! parse_filename())
         return;
      fcn = fcn | 0x02;
   }
   the_emitter->emit_opcode(write_op);
   the_emitter->emit_usign_8(fcn);
   the_emitter->emit_opcode(label_assign_op);
   trace_end(parser);
}
#endif


