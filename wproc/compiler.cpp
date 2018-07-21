//
//	Copyright (c) 1996-1998 NeoMedia Technologies Inc. All rights reserved.
//
//	Project:	WPROC
//	
//// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : compiler.cpp
// Author : George Soules
// Date   : 18 February 1991

#if RUNTIME
#else

// Specification
#include "compiler.hpp"

// Classes
#include "object.hpp"
#include "process.hpp"
#include "report.hpp"
#include "scanner.hpp"
#include "state.hpp"
#include "tables.hpp"
#include "token.hpp"

// Definitions and subprograms
#include <string.h>
#include "cancel.hpp"
#include "debugaid.hpp"
#include "environ.hpp"
#include "opcodes.hpp"
#include "utility.hpp"


const int max_scopes = 16;


// Local definitions and subprograms

class parser : public table_entry {
   public:
#if WANG
      parser(int abbrev, char *a_name) : table_entry(a_name, abbrev) {}
#else
      parser(char *a_name) : table_entry(a_name) {}
#endif
      void parse(compiler *a_compiler) {
#if WANG
         if      (same_string(name(), "assign"))     a_compiler->assign_stmt();
         else if (same_string(name(), "begin"))      a_compiler->block_stmt();
         else if (same_string(name(), "call"))       a_compiler->call_stmt();
         else if (same_string(name(), "case"))       a_compiler->case_stmt();
         else if (same_string(name(), "close"))      a_compiler->close_stmt();
         else if (same_string(name(), "declare"))    a_compiler->declare_stmt();
         else if (same_string(name(), "delacl"))     a_compiler->delacl_stmt();
         else if (same_string(name(), "dismount"))   a_compiler->dismount_stmt();
         else if (same_string(name(), "destroy"))    a_compiler->destroy_stmt();
         else if (same_string(name(), "end"))        a_compiler->end_stmt();
         else if (same_string(name(), "exit"))       a_compiler->exit_stmt();
         else if (same_string(name(), "extracl"))    a_compiler->extracl_stmt();
         else if (same_string(name(), "extract"))    a_compiler->extract_stmt();
         else if (same_string(name(), "for"))        a_compiler->for_stmt();
         else if (same_string(name(), "goto"))       a_compiler->goto_stmt();
         else if (same_string(name(), "if"))         a_compiler->if_stmt();
         else if (same_string(name(), "interpret"))  a_compiler->interpret_stmt();
         else if (same_string(name(), "leave"))      a_compiler->leave_stmt();
         else if (same_string(name(), "loop"))       a_compiler->loop_stmt();
         else if (same_string(name(), "logoff" ))    a_compiler->logoff_stmt();
         else if (same_string(name(), "message"))    a_compiler->message_stmt();
         else if (same_string(name(), "mount"))      a_compiler->mount_stmt();
         else if (same_string(name(), "open"))       a_compiler->open_stmt();
         else if (same_string(name(), "operator"))   a_compiler->operator_stmt();
         else if (same_string(name(), "options"))    a_compiler->options_stmt();
         else if (same_string(name(), "print"))      a_compiler->print_stmt();
         else if (same_string(name(), "prompt"))     a_compiler->prompt_stmt();
         else if (same_string(name(), "protect"))    a_compiler->protect_stmt();
         else if (same_string(name(), "read"))       a_compiler->read_stmt();
         else if (same_string(name(), "rename"))     a_compiler->rename_stmt();
         else if (same_string(name(), "return"))     a_compiler->return_stmt();
         else if (same_string(name(), "run"))        a_compiler->run_stmt();
         else if (same_string(name(), "scratch"))    a_compiler->scratch_stmt();
         else if (same_string(name(), "screen"))     a_compiler->screen_stmt();
         else if (same_string(name(), "set"))        a_compiler->set_stmt();
         else if (same_string(name(), "setacl"))     a_compiler->setacl_stmt();
         else if (same_string(name(), "submit"))     a_compiler->submit_stmt();
         else if (same_string(name(), "subroutine")) a_compiler->subroutine_stmt();
         else if (same_string(name(), "system"))     a_compiler->system_stmt();
         else if (same_string(name(), "trace"))      a_compiler->trace_stmt();
         else if (same_string(name(), "using"))      a_compiler->using_stmt();
         else if (same_string(name(), "while"))      a_compiler->while_stmt();
         else if (same_string(name(), "write"))      a_compiler->write_stmt();
#else
         if      (same_string(name(), "assign"))     a_compiler->assign_stmt();
         else if (same_string(name(), "begin"))      a_compiler->block_stmt();
         else if (same_string(name(), "call"))       a_compiler->call_stmt();
         else if (same_string(name(), "case"))       a_compiler->case_stmt();
         else if (same_string(name(), "close"))      a_compiler->close_stmt();
         else if (same_string(name(), "declare"))    a_compiler->declare_stmt();
         else if (same_string(name(), "destroy"))    a_compiler->destroy_stmt();
         else if (same_string(name(), "end"))        a_compiler->end_stmt();
         else if (same_string(name(), "exit"))       a_compiler->exit_stmt();
         else if (same_string(name(), "for"))        a_compiler->for_stmt();
         else if (same_string(name(), "goto"))       a_compiler->goto_stmt();
         else if (same_string(name(), "if"))         a_compiler->if_stmt();
         else if (same_string(name(), "interpret"))  a_compiler->interpret_stmt();
         else if (same_string(name(), "leave"))      a_compiler->leave_stmt();
         else if (same_string(name(), "loop"))       a_compiler->loop_stmt();
#if WANG
         else if (same_string(name(), "logout"))     a_compiler->logoff_stmt();
#endif
         else if (same_string(name(), "message"))    a_compiler->message_stmt();
         else if (same_string(name(), "open"))       a_compiler->open_stmt();
         else if (same_string(name(), "print"))      a_compiler->print_stmt();
         else if (same_string(name(), "prompt"))     a_compiler->prompt_stmt();
         else if (same_string(name(), "protect"))    a_compiler->protect_stmt();
         else if (same_string(name(), "read"))       a_compiler->read_stmt();
         else if (same_string(name(), "rename"))     a_compiler->rename_stmt();
         else if (same_string(name(), "return"))     a_compiler->return_stmt();
         else if (same_string(name(), "run"))        a_compiler->run_stmt();
         else if (same_string(name(), "scratch"))    a_compiler->scratch_stmt();
         else if (same_string(name(), "screen"))     a_compiler->screen_stmt();
         else if (same_string(name(), "set"))        a_compiler->set_stmt();
         else if (same_string(name(), "subroutine")) a_compiler->subroutine_stmt();
         else if (same_string(name(), "system"))     a_compiler->system_stmt();
         else if (same_string(name(), "trace"))      a_compiler->trace_stmt();
         else if (same_string(name(), "using"))      a_compiler->using_stmt();
         else if (same_string(name(), "while"))      a_compiler->while_stmt();
         else if (same_string(name(), "write"))      a_compiler->write_stmt();
#endif
      }
};

table *parsers = NULL;


void compiler::init_parsers() {
   assert(! parsers);
   parsers = new table();
#if WANG
   parsers->add(new parser(3, "assign"));
   parsers->add(new parser(3, "begin"));
   parsers->add(new parser(3, "call"));
   parsers->add(new parser(3, "case"));
   parsers->add(new parser(3, "close"));
   parsers->add(new parser(3, "declare"));
   parsers->add(new parser(3, "delacl"));
   parsers->add(new parser(4, "dismount"));
   parsers->add(new parser(3, "destroy"));
   parsers->add(new parser(3, "end"));
   parsers->add(new parser(3, "exit"));
   parsers->add(new parser(7, "extracl"));
   parsers->add(new parser(7, "extract"));
   parsers->add(new parser(3, "for"));
   parsers->add(new parser(3, "goto"));
   parsers->add(new parser(2, "if"));
   parsers->add(new parser(3, "interpret"));
   parsers->add(new parser(3, "leave"));
   parsers->add(new parser(3, "loop"));
   parsers->add(new parser(3, "logoff" ));
   parsers->add(new parser(3, "message"));
   parsers->add(new parser(3, "mount"));
   parsers->add(new parser(4, "open"));
   parsers->add(new parser(4, "operator"));
   parsers->add(new parser(3, "options"));
   parsers->add(new parser(3, "print"));
   parsers->add(new parser(4, "prompt"));
   parsers->add(new parser(4, "protect"));
   parsers->add(new parser(3, "read"));
   parsers->add(new parser(3, "rename"));
   parsers->add(new parser(3, "return"));
   parsers->add(new parser(3, "run"));
   parsers->add(new parser(4, "scratch"));
   parsers->add(new parser(4, "screen"));
   parsers->add(new parser(0, "set"));
   parsers->add(new parser(4, "setacl"));
   parsers->add(new parser(4, "submit"));
   parsers->add(new parser(4, "subroutine"));
   parsers->add(new parser(3, "system"));
   parsers->add(new parser(3, "trace"));
   parsers->add(new parser(3, "using"));
   parsers->add(new parser(3, "while"));
   parsers->add(new parser(3, "write"));
#else
   parsers->add(new parser("assign"));
   parsers->add(new parser("begin"));
   parsers->add(new parser("call"));
   parsers->add(new parser("case"));
   parsers->add(new parser("close"));
   parsers->add(new parser("declare"));
   parsers->add(new parser("destroy"));
   parsers->add(new parser("end"));
   parsers->add(new parser("exit"));
   parsers->add(new parser("for"));
   parsers->add(new parser("goto"));
   parsers->add(new parser("if"));
   parsers->add(new parser("interpret"));
   parsers->add(new parser("leave"));
   parsers->add(new parser("loop"));
#if WANG
   parsers->add(new parser("logout"));
#endif
   parsers->add(new parser("message"));
   parsers->add(new parser("open"));
   parsers->add(new parser("print"));
   parsers->add(new parser("prompt"));
   parsers->add(new parser("protect"));
   parsers->add(new parser("read"));
   parsers->add(new parser("rename"));
   parsers->add(new parser("return"));
   parsers->add(new parser("run"));
   parsers->add(new parser("scratch"));
   parsers->add(new parser("screen"));
   parsers->add(new parser("set"));
   parsers->add(new parser("subroutine"));
   parsers->add(new parser("system"));
   parsers->add(new parser("trace"));
   parsers->add(new parser("using"));
   parsers->add(new parser("while"));
   parsers->add(new parser("write"));
#endif
}


// Bodies of compiler members

compiler::compiler(reader        *a_reader,
                   pcode_emitter *an_emitter,
                   symbol_table  *a_symbol_table,
                   char          *a_source_name)
{
   trace_begin(object, "compiler");

   // Create the compiler components
   the_symbol_table = a_symbol_table;
   the_scanner      = new scanner(a_reader);
   the_emitter      = an_emitter;

   // Initialization
   the_scope          = new scope(max_scopes);
   the_source_name    = a_source_name;
   in_error_state     = false;
   in_block           = 0;
   in_subroutine      = 0;
   exits_allowed      = 0;
   error_count        = 0;
   warning_count      = 0;
   stmt_count         = 0;
   previous_token.row = 0;
   previous_token.last_column = 0;
   token_list_head = NULL;
   token_list_tail = NULL;
   token_list_size = 0;
   the_emitter->emit_opcode(source_name_op);
   the_emitter->emit_string(the_source_name);
   the_emitter->emit_int_32(a_reader->timestamp());
   the_process->the_source_timestamp = a_reader->timestamp();

   if (parsers)
      first_compiler = false;
   else {
      first_compiler = true;
      init_parsers();
   }

   // Parse the "procedure" statement
   get_next_token();
   procedure_stmt();

   trace_end(object);
}


compiler::~compiler() {
   delete the_scanner;
   delete the_token;
   delete the_scope;
   fixup_chain.delete_chain();
   subroutine_fixup_chain.delete_chain();
   if (first_compiler) {
      delete parsers;
      parsers = NULL;
      p_scrnio_cleanup();
      p_os_cleanup();
#if WANG
      p_wang_cleanup();
#endif
   }
   trace(object, "~compiler");
}


void compiler::syntax_error(int error_number) {
   location the_location(the_source_name, new range(the_token));
   char  *the_sub = kind_is(tk_eof) ? (char*)NULL : the_token->lexeme();
   report_syntax_error(error_number, &the_location, the_sub);
   delete the_location.the_range;
   in_error_state = true;
   error_count += 1;
}


void compiler::semantic_error(int error_number) {
   location the_location(the_source_name, new range(the_token));
   report_semantic_error(error_number, &the_location, the_token->lexeme());
   delete the_location.the_range;
   if (error_number > 0) {
      error_count += 1;
      in_error_state = true;
   }
   else
      warning_count += 1;
}


void compiler::semantic_warning(int error_number) {
   if (user_options.compile())
      semantic_error(-error_number);
}


void compiler::recover_from_error() {
   trace(parser, "recovering from syntax error");
   trace_level_restore();
   while (! kind_is(tk_eof) && ! kind_is(tk_label) &&
          ! (parsers->lookup(the_token->lexeme()) && kind_is(tk_identifier)))
   {
      trace_ss(parser, "flushing ", the_token->lexeme());
      discard();
  }
}


void compiler::report_bad_refs() {
   token *name_token;
   while ((name_token = fixup_chain.pending_fixup()) != NULL) {
      delete the_token;
      the_token = name_token;
      semantic_error(32);
      if (! user_options.compile())
         break;
   }
}


void compiler::get_next_token() {
   the_token = the_scanner->next_token();
   if (the_token->error_number() && ! in_error_state)
      semantic_error(the_token->error_number());
}


void compiler::discard() {
   previous_token.row         = the_token->row();
   previous_token.last_column = (usign_8)the_token->last_column();
   delete the_token;
   get_next_token();
}


void compiler::keep() {
   previous_token.row         = the_token->row();
   previous_token.last_column = (usign_8)the_token->last_column();

   if (token_list_head) {
      token_list_tail->set_next_token(the_token);
      token_list_tail = the_token;
   }
   else {
      token_list_head = the_token;
      token_list_tail = the_token;
   }
   token_list_size += 1;
   get_next_token();
}


void compiler::delete_token_list() {
   while (token_list_head) {
      token_list_tail = token_list_head->next_token();
      delete token_list_head;
      token_list_head = token_list_tail;
   }
   token_list_size = 0;
}


symbol* compiler::lookup_or_create_symbol(char *a_name) {
   symbol *the_symbol = the_symbol_table->lookup(a_name);
   if (! the_symbol) {
      symbol_attributes is = initial_attributes;
      is.undeclared = true;
      the_symbol =
         the_symbol_table->insert(a_name, is, the_scope->current_block());
      assert(the_symbol);
      semantic_warning(5);
   }
   return the_symbol;
}


void compiler::procedure_stmt() {
   // Handle optional procedure keyword by throwing away rest of line
   if (lexeme_is("procedure") || lexeme_is("proc")) {
      trace_begin(parser, "procedure header");
//      int row = the_token->row();
//FIX004
	the_scanner->ignore_rest_of_line();
	discard();
//      discard();
//      while (! kind_is(tk_eof) && row == the_token->row())
//         discard(true);
      trace_end(parser);
   }
#if WANG
   else {
      if (strcmp(the_process->the_input_pathname, "(unnamed)") != 0)
         semantic_error(47);
   }
#endif
}


//  destroy_statement
//     -> 'destroy' 'procedure'

void compiler::destroy_stmt() {
   trace_begin(parser, "destroy_stmt");
   discard(); // 'destroy'
   require_syntax("procedure", 64);
   discard(); // 'procedure'
   the_emitter->emit_opcode(destroy_op);
   trace_end(parser);
}


//  trace_statement
//     -> 'trace' ['begin' | 'end' | 'into' | ',' ]

void compiler::trace_stmt() {
   trace_begin(parser, "trace_stmt");
   discard(); // 'trace'

   usign_8 fcn = 0;

   if (keyword_is("begin"))
      fcn = 1;
   else if (keyword_is("end"))
      fcn = 2;
#if WANG
   else // 'into' | ',' | (other)
      wang_trace_stmt();
#else
   else
      syntax_error(16);
#endif

   if (fcn) {
      discard(); // 'begin' or 'end'
      the_emitter->emit_opcode(trace_op);
      the_emitter->emit_usign_8(fcn);
   }
   trace_end(parser);
}


Boolean compiler::stmt() {
   symbol *label_symbol = NULL;

   if (! in_error_state) {
      while (kind_is(tk_label)) {
         // Remove ':' from end of label declaration
         the_token->lexeme()[the_token->lexeme_size() - 1] = '\0';
#if WANG
         if (the_token->lexeme_size() > WANG_LABELNAME_SIZE+1) // lexeme includes the trailing ":" character
            semantic_warning(51);
#endif
         label_symbol = declare_label(the_token->lexeme(), false);
         if (! label_symbol)
            return false;
      }

      the_emitter->emit_opcode(statement_op);
      the_emitter->emit_usign_16(the_token->row());
      if (label_symbol) {
         the_emitter->emit_opcode(label_decl_op);
         the_emitter->emit_usign_16(label_symbol->id());
      }
      stmt_count += 1;

      if (kind_is(tk_identifier)) {
         parser *the_parser =
#if WANG
            (parser *) parsers->lookup(the_token->lexeme(), true);
#else
            (parser *) parsers->lookup(the_token->lexeme());
#endif
         if (the_parser) {
#if WANG
            if (user_options.compile())
               if (! same_string(the_token->lexeme(), the_parser->name()))
                  semantic_warning(46);
#endif
            the_parser->parse(this);
         }
         else
            semantic_error(41);
      }
      else if (! kind_is(tk_eof))
         syntax_error(kind_is(tk_variable) ? 4 : 6);
   }

   delete_token_list();
   if (in_error_state && user_options.compile()) {
      recover_from_error();
      in_error_state = false;
   }
   return BOOLEAN(! in_error_state);
}


void compiler::compile_statements(int_32 how_many) {
   trace(general, "compiling");
   trace_level_save();
   int_32 statements_compiled = 0;

   while (statements_compiled < how_many || fixup_chain.fixups_pending()) {
      if (cancel_requested()) {
         error_count = -1;
         break;
      }

      if (kind_is(tk_eof)) {
         the_emitter->emit_opcode(halt_op);
         resolve_label_fixups();
         if (fixup_chain.fixups_pending())
            report_bad_refs();
         break;
      }
      if (! stmt())
         if (! user_options.compile())
            break;
      statements_compiled += 1;
   }
}
#endif

//
//	History:
//	$Log: compiler.cpp,v $
//	Revision 1.12  1998/08/31 19:13:36  gsl
//	drcs update
//	
//

//	
//	RCS file: /disk1/neomedia/RCS/wisp/wproc/compiler.cpp,v
//	Working file: compiler.cpp
//	head: 1.11
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
//	total revisions: 11;	selected revisions: 11
//	description:
//	----------------------------
//	revision 1.11
//	date: 1997-10-02 09:37:26-04;  author: gsl;  state: V4_3_00;  lines: +1 -1
//	fix warnings
//	----------------------------
//	revision 1.10
//	date: 1997-10-02 08:49:37-04;  author: gsl;  state: Exp;  lines: +2 -2
//	fix warnings
//	----------------------------
//	revision 1.9
//	date: 1997-06-09 17:31:57-04;  author: scass;  state: V4_1_02;  lines: +2 -2
//	int4 -> int_32
//	----------------------------
//	revision 1.8
//	date: 1997-06-09 16:43:44-04;  author: scass;  state: Exp;  lines: +2 -2
//	Changed long to int4 for portability.
//	----------------------------
//	revision 1.7
//	date: 1996-07-25 19:45:26-04;  author: gsl;  state: V3_3_93;  lines: +2 -2
//	Fix for NT
//	----------------------------
//	revision 1.6
//	date: 1996-07-25 14:14:27-04;  author: gsl;  state: Exp;  lines: +0 -0
//	Renamed from compiler.cc to compiler.cpp
//	----------------------------
//	revision 1.5
//	date: 1995-06-02 10:24:04-04;  author: gsl;  state: V3_3_19;  lines: +1 -1
//	fix warning
//	----------------------------
//	revision 1.4
//	date: 1995-04-25 05:59:42-04;  author: gsl;  state: V3_3_16;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:51:59-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:32:38-05;  author: gsl;  state: V3_3x12;  lines: +14 -12
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:50:59-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
