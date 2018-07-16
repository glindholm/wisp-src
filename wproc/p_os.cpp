//
//	Copyright (c) 1996-1998 NeoMedia Technologies Inc. All rights reserved.
//
//	Project:	WPROC
//	Id:		$Id:$
//	RCS:		$Source:$
//	
//// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : p_os.cpp
// Author : George Soules
// Date   : 24 June 1991

#if RUNTIME
#else

// Specification
#include "compiler.hpp"

// Classes
#include "tables.hpp"

// Definitions and subprograms
#include "debugaid.hpp"
#include "subops.hpp"


#if ! WANG
table *print_options   = NULL;

void init_print_options() {
   assert(! print_options);
   print_options = new table();
//                                     Name     Kind             YesNo
   print_options->add(new option_name("copies", print_copies_op, false));
   print_options->add(new option_name("eject",  print_eject_op,  true));
   print_options->add(new option_name("spool",  print_spool_op,  true));
}


//  print_statement
//     -> 'print' filename-exp (',' option)*
//
//  option
//     -> 'copies' '=' integer-exp
//     -> 'spool' '=' yes_no-exp
//     -> 'eject' '=' yes_no-exp


void compiler::print_stmt() {
   trace_begin(parser, "print_stmt");
   discard();  // 'print'

   if (! parse_filename())
      return;

   the_emitter->emit_opcode(print_op);

   if (! print_options)
      init_print_options();

   option_name *the_option;
   print_options->set_none_seen();

   while (kind_is(tk_comma)) {
      discard(); // ','
      the_option = (option_name *) print_options->lookup(the_token->lexeme());
      if (the_option) {
         require_semantics(the_option->seen() == 1, 22);
         discard(); // option
         require_syntax("=", 3);
         discard(); // '='

         if (the_option->is_yes_no()) {
            if (! parse_yes_no())
               return;
         }
         else
            require_nonterminal(expression(58));

         the_emitter->emit_opcode(print_subop);
         the_emitter->emit_usign_8(the_option->kind());
      }
      else
         syntax_error(65);
   }
   the_emitter->emit_opcode(statement_end_op);
   the_emitter->emit_opcode(label_assign_op);
   trace_end(parser);
}
#endif


#if !WANG
table *protect_options = NULL;

void init_protect_options() {
   assert(! protect_options);
   protect_options = new table();
//                                     Name       Kind
   protect_options->add(new name_kind("archive",  protect_archive_op));
   protect_options->add(new name_kind("hidden",   protect_hidden_op));
   protect_options->add(new name_kind("readonly", protect_readonly_op));
   protect_options->add(new name_kind("system",   protect_system_op));
}


//  protect_statement
//     -> 'protect' filename-exp 'to' (option list ',')
//
//  option
//     -> 'archive' '=' yes_no-exp
//     -> 'hidden' '=' yes_no-exp
//     -> 'readonly' '=' yes_no-exp
//     -> 'system' '=' yes_no-exp

void compiler::protect_stmt() {
   trace_begin(parser, "protect_stmt");

   discard();  // 'protect'
   if (! parse_filename())
      return;

   the_emitter->emit_opcode(protect_op);

   require_syntax("to", 24);
   discard(); // 'to'

   if (! protect_options)
      init_protect_options();

   name_kind *the_option;
   protect_options->set_none_seen();

   while (true) {
      the_option = (name_kind *) protect_options->lookup(the_token->lexeme());
      if (the_option) {
         require_semantics(the_option->seen() == 1, 22);
         discard(); // option
         require_syntax("=", 3);
         discard(); // '='
         if (! parse_yes_no())
            return;
         the_emitter->emit_opcode(protect_subop);
         the_emitter->emit_usign_8(the_option->kind());
      }
      else
         syntax_error(66);

      if (kind_is(tk_comma))
         discard(); // ','
      else
         break;
   }
   the_emitter->emit_opcode(statement_end_op);
   the_emitter->emit_opcode(label_assign_op);
   trace_end(parser);
}
#endif


#if ! WANG
//  rename_statement
//     -> 'rename' filename-exp 'to' filename-exp

void compiler::rename_stmt() {
   trace_begin(parser, "rename_stmt");
   discard(); // 'rename'
   if (! parse_filename())
      return;
   require_syntax("to", 24);
   discard(); // 'to'
   if (! parse_filename())
      return;
   the_emitter->emit_opcode(rename_op);
   the_emitter->emit_opcode(label_assign_op);
   trace_end(parser);
}
#endif


#if ! WANG
//  scratch_statement
//     -> 'scratch' filename-exp

void compiler::scratch_stmt() {
   trace_begin(parser, "scratch_stmt");
   discard(); // 'scratch'
   if (! parse_filename())
      return;
   the_emitter->emit_opcode(scratch_op);
   the_emitter->emit_opcode(label_assign_op);
   trace_end(parser);
}
#endif


#if ! WANG
//  set_statement
//     -> 'set' ((name-exp ('=' | 'to') value-exp) list ',')

void compiler::set_stmt() {
   trace_begin(parser, "set_stmt");
   discard(); // 'set'

   while (true) {
      if (! parse_identifier())
         require_nonterminal(simple_expression(62));
      require_syntax("=", 3);
      discard(); // '='
      if (! parse_identifier())
         require_nonterminal(expression(63));
      the_emitter->emit_opcode(set_op);
      if (kind_is(tk_comma))
         discard(); // ','
      else
         break;
   }
   the_emitter->emit_opcode(label_assign_op);
   trace_end(parser);
}
#endif


void compiler::p_os_cleanup() {
#if ! WANG
   if (print_options) {
      delete print_options;
      print_options = NULL;
   }
   if (protect_options) {
      delete protect_options;
      protect_options = NULL;
   }
#endif
}
#endif



//
//	History:
//	$Log: p_os.cpp,v $
//	Revision 1.6  1998/08/31 19:14:03  gsl
//	drcs update
//	
//

//	
//	RCS file: /disk1/neomedia/RCS/wisp/wproc/p_os.cpp,v
//	Working file: p_os.cpp
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
//	date: 1996-07-25 14:15:45-04;  author: gsl;  state: V4_3_00;  lines: +0 -0
//	Renamed from p_os.cc to p_os.cpp
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
//	date: 1995-01-27 18:33:09-05;  author: gsl;  state: V3_3x12;  lines: +0 -0
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:18-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
