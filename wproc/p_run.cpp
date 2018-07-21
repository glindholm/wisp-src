//
//	Copyright (c) Shell Stream Software LLC, All Rights Reserved.
//
//	Project:	WPROC
//	
//// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : p_run.cpp
// Author : George Soules
// Date   : 4 April 1991

#if ! RUNTIME

// Specification
#include "compiler.hpp"

// Classes
#include "tables.hpp"

// Definitions and subprograms
#include <string.h>
#include "debugaid.hpp"
#include "subops.hpp"


#if (! WANG) & DOS
//  run_statement
//     -> 'run' filename-expression parameters? exit_clause* run_clause*
//
//  parameters
//     -> 'using' (parameter-expression list ',')
//
//  exit_clause
//     -> ('error' | 'cancel') 'exit' 'is'? label-id
//
//  run_clause
//     -> 'with'? 'input' 'from' filename-expression
//     -> 'with'? 'output' 'appended'? 'to' filename-expression
//     -> 'swap' '=' yes_no-expression
//     -> 'type' (any-expression list ',')
#endif

void compiler::run_stmt() {
   trace_begin(parser, "run_stmt");

   token *the_eexit_token = NULL;
   token *the_cexit_token = NULL;

   int args = 0;

   discard(); // 'run'
   if (! parse_filename())
      return;

   // Parse parameters
   if (keyword_is("using")) {
      discard(); // 'using'
      if (kind_is_expression()) {
         while (true) {
            require_nonterminal(expression(54));
            args += 1;
            if (! kind_is(tk_comma))
               break;
            else
               discard(); // ','
         }
      }
      if (args == 0)
         improper_syntax(17);
   }

   the_emitter->emit_opcode(run_op);
   the_emitter->emit_int_8(args);

#if WANG
   Boolean trans_seen  = false;
#else
   Boolean input_seen  = false;
   Boolean output_seen = false;
#if DOS
   Boolean swap_seen   = false;
   Boolean type_seen   = false;
#endif
#endif
   Boolean eexit_seen  = false;
   Boolean cexit_seen  = false;

   // Parse clauses

   while (true) {
      // Error or Cancel Exit clause
      if (keyword_is("error") || keyword_is("cancel")) {
         Boolean eexit = BOOLEAN(keyword_is("error"));
         require_semantics
            (! (eexit ? eexit_seen : cexit_seen), eexit ? 9 : 7);
         discard(); // 'error' or 'cancel'
         require_syntax("exit", 22);
         discard(); // 'exit'
         allow_syntax("is")
         require_kind(tk_identifier, 19);
         if (eexit) {
            eexit_seen = true;
            the_eexit_token = the_token;
         }
         else {
            cexit_seen = true;
            the_cexit_token = the_token;
         }
         keep(); // label
         the_emitter->emit_opcode(run_subop);
         the_emitter->emit_usign_8(eexit ? run_eexit_op : run_cexit_op);
      }
      else
         break;
   }

   while (true) {
#if ! WANG
      if (keyword_is("with")) {
         discard(); // 'with'
         if (! (keyword_is("input") || keyword_is("output")))
            improper_syntax(23);
      }

      // Input clause
      if (keyword_is("input")) {
         require_semantics(! input_seen, 6);
         discard(); // 'input'
         require_syntax("from", 20);
         discard(); // 'from'
         if (! parse_filename())
            return;
         the_emitter->emit_opcode(run_subop);
         the_emitter->emit_usign_8(run_input_op);
         input_seen = true;
      }

      // Output clause
      else if (keyword_is("output")) {
         require_semantics(! output_seen, 8);
         discard(); // 'output'
         Boolean append = BOOLEAN(keyword_is("appended"));
         if (append)
            discard(); // 'appended'
         require_syntax("to", append ? 24 : 21);
         discard(); // 'to'
         if (! parse_filename())
            return;
         the_emitter->emit_opcode(run_subop);
         the_emitter->emit_usign_8(append ? run_append_op : run_output_op);
         output_seen = true;
      }

#if DOS
      // Swap option
      else if (keyword_is("swap")) {
         require_semantics(! swap_seen, 10);
         discard(); // 'swap'
         require_syntax("=", 3);
         discard(); // '='
         if (! parse_yes_no())
            return;
         the_emitter->emit_opcode(run_subop);
         the_emitter->emit_usign_8(run_swap_op);
         swap_seen = true;
      }

      // Type clause
      else if (keyword_is("type")) {
         require_semantics(! type_seen, 11);
         discard(); // 'type'
         require_nonterminal(expression(55));
         args = 1;
         while (kind_is(tk_comma)) {
            discard(); // ','
            require_nonterminal(expression(55));
            args += 1;
         }
         the_emitter->emit_opcode(run_subop);
         the_emitter->emit_usign_8(run_type_op);
         the_emitter->emit_int_8(args);
         type_seen = true;
      }
#endif
#endif

#if WANG
      // Transparent option
      if (keyword_is("transparent")) {
         require_semantics(! trans_seen, 56);
         semantic_warning(55);
         discard(); // 'transparent'
         require_syntax("=", 3);
         discard(); // '='
         if (! parse_yes_no())
            return;
         the_emitter->emit_opcode(run_subop);
         the_emitter->emit_usign_8(run_trans_op);
         trans_seen = true;
      }
#endif

      // Misplaced Error or Cancel Exit clause
      else if (keyword_is("error") || keyword_is("cancel")) {
         semantic_error(40);
         // Flush part of statement to avoid erroneous error when 'exit' seen
         discard(); // 'error' or 'cancel'
         if (keyword_is("exit"))
            discard(); // 'exit'
         return;
      }

      else
         break;
   }

#if WANG
   if (! parse_display_enter())
      return;
#endif

   the_emitter->emit_opcode(statement_end_op);
   the_emitter->emit_opcode(label_assign_op);

   if (eexit_seen)
      resolve_label_offset(the_emitter->fixup_offset_for(branch_true_op),
                           *the_eexit_token, false);

   if (cexit_seen)
      resolve_label_offset(the_emitter->fixup_offset_for(branch_true_op),
                          *the_cexit_token, false);

   trace_end(parser);
}


//  system_statement
//     -> 'system' command-expression

void compiler::system_stmt() {
   trace_begin(parser, "system_stmt");
   discard(); // 'system'
   require_nonterminal(expression(56));
   the_emitter->emit_opcode(system_op);
   the_emitter->emit_opcode(label_assign_op);
   trace_end(parser);
}


//  interpret_statement
//     -> 'interpret' string-expression

void compiler::interpret_stmt() {
   trace_begin(parser, "interpret_stmt");
   discard();  // 'interpret'
   require_nonterminal(expression(57));
   the_emitter->emit_opcode(interpret_op);
   the_emitter->emit_opcode(label_assign_op);
   trace_end(parser);
}


//  using_statement
//     -> 'using' (declaration list ',')
//
//  declaration
//     -> (variable list ',') 'as'? type?
//
//  type
//     -> simple_type initializer?
//     -> initializer
//
//  simple_type
//     -> 'integer'
//     -> 'string' ('(' integer-expression ')')?
//
//  initializer
//     -> ('initial' | '=') expression

void compiler::using_stmt() {
   trace_begin(parser, "using_stmt");

   if (stmt_count > 1) {
      semantic_error(13);
      discard(); // 'using'
      return;
   }
   discard(); // 'using'

   while (true) {
      if (parse_declaration(false)) {
         if (kind_is(tk_comma)) {
            discard(); // ','
            delete_token_list();
         }
         else
            break;
      }
      else
         break;
   }
#if WANG
   the_emitter->emit_opcode(using_end_op);
#endif
   trace_end(parser);
}
#endif



//
//	History:
//	$Log: p_run.cpp,v $
//	Revision 1.6  1998/08/31 19:14:03  gsl
//	drcs update
//	
//

//	
//	RCS file: /disk1/neomedia/RCS/wisp/wproc/p_run.cpp,v
//	Working file: p_run.cpp
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
//	date: 1996-07-25 14:15:47-04;  author: gsl;  state: V4_3_00;  lines: +0 -0
//	Renamed from p_run.cc to p_run.cpp
//	----------------------------
//	revision 1.4
//	date: 1995-04-25 06:00:11-04;  author: gsl;  state: V3_3_19;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:52:28-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:33:09-05;  author: gsl;  state: V3_3x12;  lines: +9 -9
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:18-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
