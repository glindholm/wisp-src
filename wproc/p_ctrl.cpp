//
//	Copyright (c) 1996-1998 NeoMedia Technologies Inc. All rights reserved.
//
//	Project:	WPROC
//	
//// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : p_ctrl.cpp
// Author : George Soules
// Date   : 4 April 1991

#if ! RUNTIME

// Specification
#include "compiler.hpp"

// Classes
#include "process.hpp"

// Definitions and subprograms
#include <string.h>
#include "debugaid.hpp"
#include "memory.hpp"
#include "utility.hpp"


#define require_statements(t)         \
   if (! statements(t)) {             \
      return;                         \
   }


#define require_statements_in_case(t) \
   if (! statements_in_case(t)) {     \
      exits.delete_chain();           \
      return;                         \
   }


void compiler::end_error(token *a_token) {
   token *eof_token = the_token;
   the_token = a_token;
   semantic_error(19);
   the_token = eof_token;
}


Boolean compiler::statements(token *a_token) {
   Boolean found = true;
   in_block += 1;
   while (! keyword_is("end")) {
      if (stmt()) {
         if (kind_is(tk_eof)) {
            end_error(a_token);
            found = false;
            break;
         }
      }
      else {
         found = false;
         break;
      }
   }
   if (found)
      discard(); // 'end'
   in_block -= 1;
   return found;
}


Boolean compiler::statements_in_case(token *a_token) {
   Boolean found = true;
   in_block += 1;
   while (! (keyword_is("when") || keyword_is("end"))) {
      if (stmt()) {
         if (kind_is(tk_eof)) {
            end_error(a_token);
            found = false;
            break;
         }
      }
      else {
         found = false;
         break;
      }
   }
   in_block -= 1;
   return found;
}


Boolean compiler::label_accessible(
   block    a_block,
   scope   *a_scope,
   Boolean  is_subroutine,
   Boolean  called,
   Boolean  issue_error,
   token   &a_token)
{
   int error = 0;

   if (! a_scope->can_see(a_block))
      error = 25;
   else if (is_subroutine && ! called)
      error = 30;
   else if (! a_scope->can_go_to(a_block) && ! is_subroutine)
      error = called ? 18 : 24;

   if (error) {
      if (issue_error) {
         token *tk = new token(a_token);
         delete the_token;
         the_token = tk;
         semantic_error(error);
      }
      return false;
   }
   else
      return true;
}


void compiler::resolve_label_offset(
   offset  an_offset,
   token  &a_token,
   Boolean called)
{
   // When Wang, label resolution is deferred until all labels have been seen.
   // Thus, no fixups are done--all refs added to fixup chain.
#if ! WANG
   symbol *the_symbol = the_symbol_table->lookup(a_token.lexeme());
   if (the_symbol) {
      if (label_accessible(the_symbol->decl_block(), the_scope,
          the_symbol->is.subroutine, called, true, a_token))
      {
         label_data &the_label = (label_data&) the_symbol->data_ref();
         the_emitter->fixup(an_offset, the_label.location());
      }
   }
   else
#endif
      fixup_chain.add_ref(an_offset, a_token, the_scope, called);
}


void compiler::resolve_label_fixups() {
   fixup *the_fixup = fixup_chain.next_in_chain();
   while (the_fixup) {
      if (! the_fixup->resolved()) {
         if (the_fixup->target_is_exp()) {
            the_emitter->fixup(the_fixup->ref_offset());
            symbol_id boundary = the_fixup->symbol_boundary();
            emit_labels_accessible_from
               (the_fixup->ref_scope(), the_fixup->call(), true, boundary);
            the_fixup->set_resolved();
         }
#if WANG
         else {
            if (resolve_static_label(the_fixup))
               the_fixup->set_resolved();
         }
#endif
      }
      the_fixup = the_fixup->next_in_chain();
   }
}


#if WANG
// Static label fixups only exist when Wang.  When not Wang, static
// labels are resolved by resolve_label_offset() when encountered.

Boolean compiler::resolve_static_label(fixup *a_fixup) {
   // Find the first symbol in the symbol table that matches this fixup
   symbol             *the_symbol = NULL;
   symbol_table_entry *the_entry  = NULL;

   while (1) {
      the_entry = the_symbol_table->entry_after(the_entry);
      if (the_entry) {
         the_symbol = the_entry->the_symbol;
         if (the_symbol->is.label)
            if (same_string(the_symbol->name(), a_fixup->label_token().lexeme())) {
               if (label_accessible(the_symbol->decl_block(), a_fixup->ref_scope(),
                  (Boolean)the_symbol->is.subroutine, 
		  a_fixup->call(), false, *the_token))
               {
                  label_data &the_data = (label_data&) the_symbol->data_ref();
                  the_emitter->fixup(a_fixup->ref_offset(), the_data.location());
                  return true;
               }
            }
      }
      else
         return false;
   }
}
#endif


void compiler::emit_labels_accessible_from(
   scope     *a_scope,
   Boolean    called,
   Boolean    source_order,
   symbol_id &boundary)
{
   symbol             *the_symbol     = NULL;
   symbol_table_entry *the_entry      = NULL;
   symbol_id           start_boundary = boundary;
   Boolean             first_label    = true;

   // Emit the ID of every label in the symbol table that is accessible
   // from a_scope.  Note that labels are inserted at the top of the symbol
   // table as they are encountered.  Thus, the order of labels in the table
   // is exactly opposite of the ordering in the source.

   // The passed value of boundary contains the last symbol to be ingnored
   // (values > boundary should be emitted).  The first symbol emitted is
   // returned via boundary.  The boundary is only needed in the Wang version,
   // but is not easily tagged out.

   while (true) {
      the_entry = source_order ? the_symbol_table->entry_before(the_entry)
                               : the_symbol_table->entry_after(the_entry);
      if (the_entry) {
         the_symbol = the_entry->the_symbol;
         if (the_symbol->is.label && the_symbol->id() > start_boundary) {
            if (label_accessible(the_symbol->decl_block(), a_scope,
               (Boolean)the_symbol->is.subroutine, called, false, *the_token))
            {
               symbol::emit_id(the_symbol->id(), *the_emitter);
               if (first_label) {
                  boundary = the_symbol->id();
                  first_label = false;
               }
            }
         }
      }
      else
         break;
   }

   symbol::emit_id((symbol_id) 0, *the_emitter);
}


//  block_statement
//     -> 'begin' statement+ 'end'

void compiler::block_stmt() {
   trace_begin(parser, "block_stmt");
   token start_token(*the_token);
   discard(); // 'block'
   require_semantics(the_scope->enter(), 31);
   require_statements(&start_token);
   trace_end(parser);
   the_scope->exit();
}


void compiler::call_builtin() {
   range the_range(the_token);
   symbol *the_symbol = lookup_or_create_symbol(the_token->lexeme());
   int args = parse_variable(the_symbol, the_range);
   if (args == -1)
      return;
   the_emitter->emit_opcode(builtin_op);
}


void compiler::call_stmt() {
   trace_begin(parser, "call_stmt");
   discard(); // 'call'
   if (kind_is(tk_identifier)) {
      resolve_label_offset
         (the_emitter->fixup_offset_for(call_op), *the_token, true);
      range the_range(the_token);
      the_emitter->emit_int_32(the_range.pack());
      discard(); // label
   }
   else if (kind_is_expression()) {
      Boolean is_expression = true;
      if (kind_is(tk_variable)) {
         symbol *the_symbol = the_symbol_table->lookup(the_token->lexeme());
         if (the_symbol) {
            if (the_symbol->is.builtin) {
               is_expression = false;
               call_builtin();
            }
         }
      }
      if (is_expression) {
         // Emit an expression fixup to be resolved after all labels seen
         require_nonterminal(expression(19));
         fixup_chain.add_exp_ref
            (the_emitter->fixup_offset_for(call_exp_op), the_scope, true);
#if WANG
         // Emit a list of symbols that are valid for a backward call
         symbol_id boundary = 0;
         emit_labels_accessible_from(the_scope, true, false, boundary);
         // Store into the fixup the ID of the first label before this call
         fixup_chain.next_in_chain()->set_symbol_boundary(boundary);
#endif
      }
   }
   else
      syntax_error(19);
   trace_end(parser);
}


//  case_statement
//     -> 'case' expression cases 'end'
//
//  cases
//     -> ('when' expression statement)* ('when' 'others' statement)?

void compiler::case_stmt() {
   trace_begin(parser, "case_stmt");

   Boolean others_seen = false;
   offset  exit_branch;
   offset  skip_branch;
   offset  match_branch;
   fixup   exits;


   token start_token(*the_token);
   discard(); // 'case'
   require_semantics(the_scope->enter(), 31);

   // Parse the case value
   require_nonterminal(expression(45));

   while (true) {
      if (keyword_is("when")) {
         if (others_seen) {
            // Report an error, but keep parsing
            semantic_error(17);
            in_error_state = false;
         }
         discard(); // 'when'
         if (keyword_is("others")) {
            others_seen = true;
            discard(); // 'others'

            // Parse the 'others' statements
            require_statements_in_case(&start_token);

            // Done with statements, branch to end of case
            exit_branch = the_emitter->fixup_offset_for(branch_always_op);
            exits.add_exit_from(the_scope->current_block(), exit_branch);
         }
         else {
            require_semantics(the_scope->enter(), 31);
            while (true) {
               // Emit checks that 'when' values match case value
               require_nonterminal(expression(46));
               the_emitter->emit_opcode(rel_case_eq_op);
               match_branch = the_emitter->fixup_offset_for(branch_true_op);
               exits.add_exit_from
                  (the_scope->current_block(), match_branch);
               if (lexeme_is("|"))
                  discard(); // '|'
               else
                  break;
            }
            // Control comes here if no match--emit branch to next 'when' case
            skip_branch = the_emitter->fixup_offset_for(branch_always_op);

            // Control comes here if match
            exits.fixup_exits_from
               (the_scope->current_block(), *the_emitter);
            the_scope->exit();

            // Pop the case value saved by the rel_case_eq_op
            the_emitter->emit_opcode(pop_top_op);

            // Parse the 'when' statements
            require_statements_in_case(&start_token);

            // Done with statements, branch to end of case
            exit_branch = the_emitter->fixup_offset_for(branch_always_op);
            exits.add_exit_from(the_scope->current_block(), exit_branch);

            // Control comes here if case value didn't match
            the_emitter->fixup(skip_branch);
         }
      }
      else if (keyword_is("end")) {
         discard(); // 'end'
         break;
      }
      else
         improper_syntax(28);
   }

   // Perform fixups for case exits
   exits.fixup_exits_from(the_scope->current_block(), *the_emitter);
   trace_end(parser);
   the_scope->exit();
}


void compiler::end_stmt() {
   trace_begin(parser, "end_stmt");
   if (! in_block) {
      the_emitter->emit_opcode(call_return_op);
      // Emit range of 'end' to allow error reporting
      range the_range(the_token);
      the_emitter->emit_int_32(the_range.pack());
      discard();
   }
   trace_end(parser);
}


//  exit_statement
//     -> 'exit'

void compiler::exit_stmt() {
   trace_begin(parser, "exit_stmt");
   offset exit_branch;
   if (the_scope->current_block() == outermost_block || ! exits_allowed) {
      semantic_error(12);
      discard();
      return;
   }
   discard(); // 'exit'
   exit_branch = the_emitter->fixup_offset_for(branch_always_op);
   fixup_chain.add_exit_from(the_scope->current_block(), exit_branch);
}


//  for_statement
//     -> 'for' variable '=' exp 'to' exp ('by' exp)? 'loop' statement* 'end'

void compiler::for_stmt() {
   trace_begin(parser, "for_stmt");
   token start_token(*the_token);
   discard(); // 'for'

   // Assign initial value to control variable
   require_kind(tk_variable, 9);
   range the_range(the_token);
   symbol *the_symbol = lookup_or_create_symbol(the_token->lexeme());
   if (parse_variable(the_symbol, the_range) == -1)
      return;
   require_syntax("=", 3);
   discard(); // '='
   require_nonterminal(expression(47));
   require_syntax("to", 27);
   discard(); // 'to'
   require_nonterminal(expression(48));
   if (keyword_is("by")) {
      discard(); // 'by'
      require_nonterminal(expression(61));
   }
   else {
      the_emitter->emit_opcode(push_int_8_op);
      the_emitter->emit_int_8(1);
   }
   the_emitter->emit_opcode(for_loop_init_op);

   // Top of loop (checks that variable in loop range)
   offset loop_top = the_emitter->current_offset();
   offset done_branch = the_emitter->fixup_offset_for(for_loop_test_op);

   // Parse the loop body
   require_syntax("loop", 26);
   discard(); // 'loop'
   require_semantics(the_scope->enter(true), 31);
   exits_allowed += 1;
   require_statements(&start_token);

   // Emit branch to top of loop
   the_emitter->emit_opcode(for_loop_incr_op);
   the_emitter->emit_opcode(branch_always_op);
   the_emitter->emit_offset(loop_top);

   // Control comes here when loop is exited via an Exit statement
   fixup_chain.fixup_exits_from(the_scope->current_block(), *the_emitter);
   the_emitter->emit_opcode(for_loop_exit_op);

   // Control comes here when loop terminates normally
   the_emitter->fixup(done_branch);

   trace_end(parser);
   exits_allowed -= 1;
   the_scope->exit();
}


//  goto_statement
//     -> 'goto' label-identifier

void compiler::goto_stmt() {
   trace_begin(parser, "goto_stmt");
   discard(); // 'goto'
   if (kind_is(tk_identifier)) {
      // Emit a branch
      resolve_label_offset
         (the_emitter->fixup_offset_for(branch_always_op), *the_token, false);
      discard(); // label
   }
   else {
      // Emit an expression fixup to be resolved after all labels seen
      require_nonterminal(expression(19));
      fixup_chain.add_exp_ref
         (the_emitter->fixup_offset_for(goto_exp_op), the_scope, false);
#if WANG
      // Emit a list of symbols that are valid for a backward goto
      symbol_id boundary = 0;
      emit_labels_accessible_from(the_scope, false, false, boundary);
      // Store into the fixup the ID of the first label before this goto
      fixup_chain.next_in_chain()->set_symbol_boundary(boundary);
#endif
   }
   trace_end(parser);
}


//  if_statement
//     -> 'if' cond-exp 'then'? statement ('else' statement)?

void compiler::if_stmt() {
   trace_begin(parser, "if_stmt");
   discard(); // 'if'
#if WANG
   Boolean if_exists = false;
   if (keyword_is("not"))
      if_exists = BOOLEAN(same_string(the_scanner->lookahead(1)->lexeme(), "exists"));
   else
      if_exists = Boolean(keyword_is("exists"));
   if (if_exists) {
      if (! parse_if_exists())
         return;
   }
   else
#endif
   require_nonterminal(expression(49));
   allow_syntax("then");
   offset then_branch = the_emitter->fixup_offset_for(branch_false_op);
   if (stmt()) {
      if (keyword_is("else")) {
         discard(); // 'else'
         offset else_branch = the_emitter->fixup_offset_for(branch_always_op);
         the_emitter->fixup(then_branch);
         if (stmt())
            the_emitter->fixup(else_branch);
      }
      else
         the_emitter->fixup(then_branch);
   }
   trace_end(parser);
}


#if WANG
void compiler::logoff_stmt() {
   trace_begin(parser, "logoff_stmt");
   discard();  // 'logoff'
   the_emitter->emit_opcode(logoff_op);
   trace_end(parser);
}
#endif


#if WANG
Boolean compiler::parse_if_exists() {
   Boolean ok;
   Boolean negate = false;
   if (keyword_is("not")) {
      negate = true;
      discard(); // 'not'
   }
   discard(); // 'exists'
   if (keyword_is("file")) {
      discard(); // 'file'
      ok = parse_filename();
   }
   else if (keyword_is("library")) {
      discard(); // 'library'
      ok = parse_libname();
   }
   else if (keyword_is("volume")) {
      discard(); // 'volume'
      ok = parse_volname();
      if (ok && keyword_is("vsid")) {
         discard(); // 'vsid'
         if (lexeme_is("="))
            discard(); // '='
         else {
            syntax_error(3);
            ok = false;
         }
         if (ok) {
            the_emitter->pause_emission();
            expression(8);
            the_emitter->resume_emission();
            ok = BOOLEAN(! in_error_state);
         }
      }
   }
   else {
      syntax_error(81);
      ok = false;
   }
   the_emitter->emit_opcode(exists_op);
   if (negate)
      the_emitter->emit_opcode(not_op);
   return ok;
}
#endif


//  leave_statement
//     -> 'leave'

void compiler::leave_stmt() {
   trace_begin(parser, "leave_stmt");
   offset leave_branch;
   if (! in_subroutine) {
      semantic_error(44);
      discard();
      return;
   }
   discard(); // 'leave'
   leave_branch = the_emitter->fixup_offset_for(branch_always_op);
   subroutine_fixup_chain.add_exit_from
      (the_scope->current_block(), leave_branch);
}


//  loop_statement
//     -> 'loop' statement+ 'end'

void compiler::loop_stmt() {
   trace_begin(parser, "loop_stmt");
   token start_token(*the_token);
   discard(); // 'loop'
   require_semantics(the_scope->enter(), 31);
   exits_allowed += 1;
   offset loop_top = the_emitter->current_offset();
   require_statements(&start_token);
   the_emitter->emit_opcode(branch_always_op);
   the_emitter->emit_offset(loop_top);
   fixup_chain.fixup_exits_from(the_scope->current_block(), *the_emitter);
   trace_end(parser);
   exits_allowed -= 1;
   the_scope->exit();
}


void compiler::return_stmt() {
   trace_begin(parser, "return_stmt");
   discard(); // 'return'
   if (keyword_is("code")) {
      discard(); // 'code'
      require_syntax("=", 3);
      discard(); // '='
      require_nonterminal(expression(50));
   }
   else {
      the_emitter->emit_opcode(push_int_8_op);
      the_emitter->emit_int_8(0);
   }

   the_emitter->emit_opcode(return_op);

   trace_end(parser);
}


//  subroutine_statement
//     -> 'subroutine' identifier statement+ 'end'

void compiler::subroutine_stmt() {
   trace_begin(parser, "subroutine_stmt");
   in_subroutine += 1;
   token start_token(*the_token);
   discard(); // 'subroutine'
   require_kind(tk_identifier, 33);

   // Emit branch to end of routine in case fallen into
   offset skip_branch = the_emitter->fixup_offset_for(branch_always_op);

   if (! declare_label(the_token->lexeme(), true))
      // Already declared error
      return;

   require_semantics(the_scope->enter(true), 31);
   int saved_exits_allowed = exits_allowed;
   exits_allowed = 0;

   require_statements(&start_token);

   exits_allowed = saved_exits_allowed;
   subroutine_fixup_chain.fixup_exits_from
      (the_scope->current_block(), *the_emitter);
   the_emitter->emit_opcode(call_return_op);
   the_emitter->emit_int_32(0); // dummy range
   the_emitter->fixup(skip_branch);
   trace_end(parser);
   the_scope->exit();
   in_subroutine -= 1;
}


//  while_statement
//     -> 'while' conditional-expression 'loop' statement+ 'end'

void compiler::while_stmt() {
   trace_begin(parser, "while_stmt");
   offset loop_top = the_emitter->current_offset();
   token start_token(*the_token);
   discard(); // 'while'
   require_nonterminal(expression(49));
   offset done_branch = the_emitter->fixup_offset_for(branch_false_op);
   require_syntax("loop", 26);
   discard(); // 'loop'
   require_semantics(the_scope->enter(), 31);
   exits_allowed += 1;
   require_statements(&start_token);
   the_emitter->emit_opcode(branch_always_op);
   the_emitter->emit_offset(loop_top);
   the_emitter->fixup(done_branch);
   fixup_chain.fixup_exits_from(the_scope->current_block(), *the_emitter);
   trace_end(parser);
   exits_allowed -= 1;
   the_scope->exit();
}
#endif





//
//	History:
//	$Log: p_ctrl.cpp,v $
//	Revision 1.7  1998/08/31 19:14:01  gsl
//	drcs update
//	
//

//	
//	RCS file: /disk1/neomedia/RCS/wisp/wproc/p_ctrl.cpp,v
//	Working file: p_ctrl.cpp
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
//	date: 1996-07-25 19:47:46-04;  author: gsl;  state: V4_3_00;  lines: +2 -2
//	NT
//	----------------------------
//	revision 1.5
//	date: 1996-07-25 14:15:39-04;  author: gsl;  state: Exp;  lines: +0 -0
//	Renamed from p_ctrl.cc to p_ctrl.cpp
//	----------------------------
//	revision 1.4
//	date: 1995-04-25 06:00:09-04;  author: gsl;  state: V3_3_19;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:52:26-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:33:07-05;  author: gsl;  state: V3_3x12;  lines: +25 -25
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:17-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
