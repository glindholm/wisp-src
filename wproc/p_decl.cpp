//
//	Copyright (c) Shell Stream Software LLC, All Rights Reserved.
//
//	Project:	WPROC
//	
//// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : p_decl.cpp
// Author : George Soules
// Date   : 4 April 1991

#if RUNTIME
#else

// Specification
#include "compiler.hpp"

// Classes
#include "process.hpp"

// Definitions and subprograms
#include <string.h>
#include "debugaid.hpp"
#include "txt.hpp"


Boolean compiler::parse_declaration(Boolean declare_statement) {
   symbol_attributes symbol_is = initial_attributes;
   symbol   *the_symbol;
   Boolean   need_type_name = false;

   while (true) {
      if (! kind_is(tk_variable)) {
         syntax_error(9);
         return false;
      }
      the_symbol = the_symbol_table->lookup(the_token->lexeme());

#ifdef FIX001
      if (the_symbol) {
         symbol_attributes is = the_symbol->is;
         if (! (is.not_local || is.undeclared || is.builtin)) {
            semantic_error(4);
            return false;
         }
      }
#endif

      keep(); // variable
      if (! kind_is(tk_comma))
         break;
      else
         discard(); // ','
   }
   if (keyword_is("as")) {
      need_type_name = true;
      discard(); // 'as'
   }

   if (keyword_is("global")) {
      if (declare_statement) {
         need_type_name = false;
         symbol_is.global = true;
         discard(); // 'global'
      }
      else {
         syntax_error(37);
         return false;
      }
   }

   if (keyword_is("array")) {
      if (declare_statement) {
         need_type_name = false;
         symbol_is.array = true;
         discard(); // 'array'
         if (! kind_is(tk_parenleft)) {
            syntax_error(11);
            return false;
         }
         discard(); // '('

         expression(52);
         if (in_error_state)
            return false;

         if (! kind_is(tk_parenright)) {
            syntax_error(7);
            return false;
         }
         discard(); // ')'
         if (keyword_is("of")) {
            need_type_name = true;
            discard(); // 'of'
         }
      }
      else {
         semantic_error(38);
         return false;
      }
   }

   if (keyword_is("integer")) {
      discard(); // 'integer'
      symbol_is.integer = true;
   }
   else if (keyword_is("string")) {
      discard(); // 'string'
      symbol_is.string = true;
      if (kind_is(tk_parenleft)) {
         // string size
         discard(); // '('
         expression(53);
         if (in_error_state)
            return false;
         if (! kind_is(tk_parenright)) {
            syntax_error(7);
            return false;
         }
         discard(); // ')'
      }
      else
         symbol_is.dynamic = true;
   }
   else {
      // Type is string if not specified
      if (need_type_name)
         syntax_error(10);
      symbol_is.string = true;
      symbol_is.dynamic = true;
   }

   if (lexeme_is("=") || keyword_is("initial")) {
      symbol_is.initialized = true;
      discard(); // '=' or 'initial'
      expression(51);
      if (in_error_state)
         return false;
   }

   the_emitter->emit_opcode(declare_statement ? declare_op : using_op);
   the_emitter->emit_usign_8(token_list_size);

   token *this_variable = token_list_head;
   while (this_variable) {
      the_symbol = the_symbol_table->insert
         (this_variable->lexeme(), symbol_is, the_scope->current_block());
      symbol::emit_id(the_symbol->id(), *the_emitter);
      the_emitter->emit_int_32(range(this_variable).pack());
      this_variable = this_variable->next_token();
   }
   return true;
}


//  declare_statement
//     -> 'declare' (variable list ',') 'as'? 'global'? array_specifier? type?
//
//  array_specifier
//     -> 'array' '(' integer-expression ')' 'of'?
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


void compiler::declare_stmt() {
   trace_begin(parser, "declare_stmt");
   discard(); // 'declare'
   parse_declaration(true);
   trace_end(parser);
}


symbol *compiler::declare_label(const char *a_name, Boolean is_subroutine) {
   symbol *the_symbol = NULL;

   // See if name has been declared before
   the_symbol = the_symbol_table->lookup(a_name);
#if WANG
   { // Allow duplicate labels
#else
   if (the_symbol) {
      semantic_error(the_symbol->is.subroutine ? 33 : 20);
      if (user_options.compile())
         in_error_state = false;
      else
         return NULL;
   }
   else {
#endif
      // Add label to symbol table
      symbol_attributes is = initial_attributes;
      is.label = true;
      is.subroutine = is_subroutine;
      label_data *the_data = new label_data(the_emitter->current_offset());
      the_symbol = the_symbol_table->insert
         (the_token->lexeme(), is, the_scope->current_block(), the_data);

      // See if this label can resolve any references
      Boolean fixups_possible = true;
      fixup *ref;
      while (fixup_chain.fixups_pending() && fixups_possible) {
         ref = fixup_chain.lookup(the_token->lexeme());
         if (ref) {
            if (label_accessible(the_scope->current_block(), ref->ref_scope(),
                (Boolean)is.subroutine, ref->call(), true, ref->label_token()))
            {
               the_emitter->fixup(ref->ref_offset(), the_data->location());
               delete ref;
            }
            else {
               delete ref;
               if (user_options.compile())
                  in_error_state = false;
               else
                  return NULL;
            }
         }
         else
            fixups_possible = false;
      }
   }
   discard(); // label
   return the_symbol;
}
#endif





//
//	History:
//	$Log: p_decl.cpp,v $
//	Revision 1.7  1998/08/31 19:14:02  gsl
//	drcs update
//	
//

//	
//
//	Working file: p_decl.cpp
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
//	date: 1996-07-25 14:15:41-04;  author: gsl;  state: V4_3_00;  lines: +0 -0
//	Renamed from p_decl.cc to p_decl.cpp
//	----------------------------
//	revision 1.5
//	date: 1995-06-02 10:26:39-04;  author: gsl;  state: V3_3_19;  lines: +0 -1
//	fix warning
//	----------------------------
//	revision 1.4
//	date: 1995-04-25 06:00:10-04;  author: gsl;  state: V3_3_16;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:52:27-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:33:08-05;  author: gsl;  state: V3_3x12;  lines: +6 -7
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:17-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
