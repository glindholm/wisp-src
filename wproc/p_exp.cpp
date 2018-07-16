//
//	Copyright (c) 1996-1998 NeoMedia Technologies Inc. All rights reserved.
//
//	Project:	WPROC
//	Id:		$Id:$
//	RCS:		$Source:$
//	
//// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : p_exp.cpp
// Author : George Soules
// Date   : 4 April 1991

#if RUNTIME
#else

// Specification
#include "compiler.hpp"

// Classes
#include "options.hpp"

// Definitions and subprograms
#include <ctype.h>
#include <string.h>
#include "colors.hpp"
#include "debugaid.hpp"
#include "environ.hpp"
#include "memory.hpp"
#include "txt.hpp"
#include "utility.hpp"


// Expression nonterminal parsers

Boolean expression_found;

void compiler::expression(int error_number) {
   trace_begin(parser, "expression");
   expression_found = true;
   require_nonterminal(logical());
   while (expression_found) {
      if (! keyword_is("or"))
         break;
      discard(); // 'or'
      require_nonterminal(logical());
      the_emitter->emit_opcode(or_op);
   }
   if (! expression_found)
      syntax_error(error_number);
   trace_end(parser);
}


void compiler::simple_expression(int error_number) {
   trace_begin(parser, "simple_expression");
   expression_found = true;
   require_nonterminal(catenation());
   if (! expression_found)
      syntax_error(error_number);
   trace_end(parser);
}


void compiler::logical() {
   trace_begin(parser, "logical");
   require_nonterminal(relation());
   while (expression_found) {
      if (! keyword_is("and"))
         break;
      discard(); // 'and'
      require_nonterminal(relation());
      the_emitter->emit_opcode(and_op);
   }
   trace_end(parser);
}


void compiler::relation() {
   trace_begin(parser, "relation");
   opcode the_operator;
   require_nonterminal(catenation());
   Boolean relop = true;
   while (expression_found) {
      switch (the_token->kind()) {
         case tk_eq: the_operator = rel_eq_op; break;
         case tk_ge: the_operator = rel_ge_op; break;
         case tk_gt: the_operator = rel_gt_op; break;
         case tk_le: the_operator = rel_le_op; break;
         case tk_lt: the_operator = rel_lt_op; break;
         case tk_ne: the_operator = rel_ne_op; break;
         default:
         {
            if (keyword_is("eq"))
               the_operator = rel_eq_op;
            else if (keyword_is("GE") || keyword_is("NLT"))
               the_operator = rel_ge_op;
            else if (keyword_is("GT"))
               the_operator = rel_gt_op;
            else if (keyword_is("LE") || keyword_is("NGT"))
               the_operator = rel_le_op;
            else if (keyword_is("LT"))
               the_operator = rel_lt_op;
            else if (keyword_is("NE") || keyword_is("NEQ"))
               the_operator = rel_ne_op;
            else
               relop = false;
         }
      }
      if (! relop)
         break;
      discard(); // relational operator
      require_nonterminal(catenation());
      the_emitter->emit_opcode(the_operator);
   }
   trace_end(parser);
}


void compiler::catenation() {
   trace_begin(parser, "catenation");
   require_nonterminal(binary_add_subtract());
   while (expression_found) {
#if WANG
      if (kind_is(tk_concat)) {
         discard(); // '!!'
#else
      if (kind_is(tk_concat)  || kind_is(tk_string)  ||
          kind_is(tk_integer) || kind_is(tk_variable) || kind_is(tk_parenleft))
      {
         if (kind_is(tk_concat))
            discard(); // '!!'
         else
            if (previous_token.row != the_token->row())
               break;
#endif
         require_nonterminal(binary_add_subtract());
         the_emitter->emit_opcode(catenate_op);
      }
      else
         break;
   }
   trace_end(parser);
}


void compiler::binary_add_subtract() {
   trace_begin(parser, "binary_add_subtract");
   opcode the_operator;
   require_nonterminal(binary_multiply_divide());
   while (expression_found) {
      if (kind_is(tk_plus))
         the_operator = add_op;
      else if (kind_is(tk_minus))
         the_operator = subtract_op;
      else
         break;
      discard(); // '+' or '-'
      require_nonterminal(binary_multiply_divide());
      the_emitter->emit_opcode(the_operator);
   }
   trace_end(parser);
}


void compiler::binary_multiply_divide() {
   trace_begin(parser, "binary_multiply_divide");
   opcode the_operator;
   require_nonterminal(unary_plus_minus());
   while (expression_found) {
      if (kind_is(tk_mult))
         the_operator = multiply_op;
      else if (kind_is(tk_div))
         the_operator = divide_op;
      else
         break;
      discard(); // '*' or '/'
      require_nonterminal(unary_plus_minus());
      the_emitter->emit_opcode(the_operator);
   }
   trace_end(parser);
}


void compiler::unary_plus_minus() {
   trace_begin(parser, "unary_plus_minus");
   if (kind_is(tk_minus)) {
      discard(); // '-'
      require_nonterminal(primary());
      the_emitter->emit_opcode(negate_op);
   }
   else if (keyword_is("not")) {
      discard(); // 'not'
      require_nonterminal(primary());
      the_emitter->emit_opcode(not_op);
   }
   else if (kind_is(tk_plus)) {
      discard(); // '+'
      require_nonterminal(primary());
   }
   else
      require_nonterminal(primary());
   trace_end(parser);
}


void compiler::primary() {
   trace_begin(parser, "primary");

   switch (the_token->kind()) {
      case tk_parenleft: {
         require_nonterminal(parens());
         break;
      }

      case tk_integer: {
         char *the_number = the_token->lexeme();
         if (in_int_8_range(the_number)) {
            the_emitter->emit_opcode(push_int_8_and_range_op);
            the_emitter->emit_int_8((int_8) atoi(the_number));
         }
         else if (in_int_16_range(the_number)) {
            the_emitter->emit_opcode(push_int_16_and_range_op);
            the_emitter->emit_int_16(atoi(the_number));
         }
         else if (in_int_32_range(the_number)) {
            the_emitter->emit_opcode(push_int_32_and_range_op);
            the_emitter->emit_int_32(atol(the_number));
         }
         else
            semantic_error(3);
         emit_range_of(the_token);
         discard(); // integer
         break;
      }

      case tk_string: {
         emit_string();
         discard(); // string
         break;
      }

      case tk_variable: {
         range the_range(the_token);
         symbol *the_symbol = lookup_or_create_symbol(the_token->lexeme());
#if WANG
         // Special handling for &label built-in.
         if (the_symbol->is.builtin)
            if (same_string(the_symbol->name(), "&label")) {
               // Create dummy fixup to force rest of source to be compiled.
               // When &label is executed, all labels will have been seen.
               fixup_chain.add_exp_ref(0, the_scope, false);
               fixup_chain.next_in_chain()->set_resolved();
            }
#endif
         require_semantics(the_scope->can_see(the_symbol->decl_block()), 16);
         if (parse_variable(the_symbol, the_range) == -1)
            return;
         break;
      }

      case tk_identifier: {
         range the_range(the_token);
         symbol *the_symbol = the_symbol_table->lookup(the_token->lexeme());
         require_semantics(the_symbol, 21);
         require_semantics(the_scope->can_see(the_symbol->decl_block()), 25);
         discard(); // label
         the_emitter->emit_opcode(label_ref_op);
         symbol::emit_id(the_symbol->id(), *the_emitter);
         the_emitter->emit_int_32(the_range.pack());
         break;
      }

      default:
         expression_found = false;
         trace(parser, ">>> no expression found");
         break;
   }
   trace_end(parser);
}


// Expression parsing support routines

void compiler::parens() {
   range parens_range;

   parens_range.row.first = the_token->row();
   parens_range.col.first = (usign_8)the_token->column();

#if WANG
   if (is_full_backward_ref()) {
      semantic_error(50);
      discard(); // '('
      discard(); // label
      discard(); // ')'
      return;
   }
#endif

   discard(); // '('

#if WANG
   // Check if '(' is begining of a partial backward reference
   if (kind_is(tk_identifier))
      if (the_scanner->lookahead(1)->kind() == tk_dot) {
         emit_string();
         discard(); // label
         discard(); // '.'
         require_kind(tk_identifier, 94);
         emit_string();
         the_emitter->emit_opcode(backref_partial_op);
         discard(); // keyword
         require_kind(tk_parenright, 72);
         discard(); // ')'
         return;
      }
#endif

   require_nonterminal(expression(8));
   require_kind(tk_parenright, 7);

   parens_range.row.last = the_token->row();
   parens_range.col.last = (usign_8)the_token->column();
   discard(); // ')'

   the_emitter->emit_opcode(sub_exp_range_op);
   the_emitter->emit_int_32(parens_range.pack());
   parens_range.row.first = parens_range.row.last;
   the_emitter->emit_int_32(parens_range.pack());
}


Boolean compiler::parse_yes_no() {
   if (kind_is(tk_identifier)) {
      if (keyword_is("yes") || keyword_is("no")) {
         emit_string();
         discard(); // 'yes' or 'no'
         return true;
      }
      else {
         syntax_error(25);
         return false;
      }
   }
   else {
      expression(25);
      return BOOLEAN(! in_error_state);
   }
}


Boolean compiler::parse_color() {
   if (kind_is(tk_identifier)) {
      color dummy;
      if (is_color(the_token->lexeme(), dummy)) {
         emit_string();
         discard(); // color
         return true;
      }
      else {
         syntax_error(34);
         return false;
      }
   }
   else {
      expression(34);
      return BOOLEAN(! in_error_state);
   }
}


void compiler::emit_string(token *a_token) {
   if (a_token == NULL)
      a_token = the_token;
   the_emitter->emit_opcode(push_string_and_range_op);
   the_emitter->emit_string(a_token->lexeme());
   emit_range_of(a_token);
}


void compiler::emit_range_of(token *a_token) {
   range the_range(a_token);
   the_emitter->emit_int_32(the_range.pack());
}


Boolean compiler::parse_identifier() {
   if (kind_is(tk_identifier)) {
      emit_string();
      discard(); // identifier
      return true;
   }
   return false;
}


#if WANG
Boolean compiler::parse_cobol_keyword() {
   if (! (kind_is(tk_identifier) || kind_is(tk_integer)))
      return false;

   token  *keyword;
   token  *temp;
   Boolean hyphen_required = true;
   int     last_col;

   keyword = new token(*the_token);
   discard();

   while (kind_is(tk_minus) || kind_is(tk_identifier) || kind_is(tk_integer)) {
      // Make sure at least one '-' separates alphanumeric tokens
      if (hyphen_required && ! kind_is(tk_minus))
         break;
      hyphen_required = BOOLEAN(! kind_is(tk_minus));

      // Make sure token is adjacent to previous token
      last_col = keyword->column() + keyword->lexeme_size() - 1;
      if (the_token->column() != (last_col % MAX_SOURCE_WIDTH) + 1)
         break;

      // Build new token
      temp = keyword;
      keyword = new token(*keyword, *the_token);
      delete temp;
      discard();
   }
   if (keyword->lexeme_size() > 8) {
      temp = the_token;
      the_token = keyword;
      semantic_warning(58);
      the_token = temp;
   }
   keyword->shift_to_upper_case();
   emit_string(keyword);
   delete keyword;
   return true;
}


Boolean compiler::parse_char_option(int error, char first, char last) {
   assert(kind_is(tk_identifier));
   Boolean ok = BOOLEAN(strlen(the_token->lexeme()) == 1);
   if (ok) {
      char c = toupper(the_token->lexeme()[0]);
      ok = BOOLEAN(c >= first && c <= last);
   }
   if (ok) {
      emit_string();
      discard(); // char
   }
   else
      syntax_error(error);

   return ok;
}


Boolean compiler::parse_keyword_option(
   int error,
   const char *s1,
   const char *s2,
   const char *s3)
{
   Boolean found;
   Boolean ok = true;

   if (kind_is(tk_identifier)) {
      found = BOOLEAN(lexeme_is(s1));
      if (! found && s2) {
         found = BOOLEAN(lexeme_is(s2));
         if (! found && s3)
            found = BOOLEAN(lexeme_is(s3));
      }
      if (found) {
         emit_string();
         discard(); // keyword
      }
      else {
         syntax_error(error);
         ok = false;
      }
   }
   else {
      expression(error);
      ok = BOOLEAN(! in_error_state);
   }
   return ok;
}


Boolean compiler::keyword_matches(
   const char *s1,
   const char *s2,
   const char *s3,
   const char *s4,
   const char *s5)
{
   Boolean found = false;
   if (kind_is(tk_identifier)) {
      found = BOOLEAN(lexeme_is(s1));
      if (! found && s2) {
         found = BOOLEAN(lexeme_is(s2));
         if (! found && s3)
            found = BOOLEAN(lexeme_is(s3));
            if (! found && s4)
               found = BOOLEAN(lexeme_is(s4));
               if (! found && s5)
                  found = BOOLEAN(lexeme_is(s5));
      }
   }
   return found;
}


Boolean compiler::parse_file_component(
   Boolean allow_fbr,
   Boolean &is_fbr,
   int     size,
   int     size_warning,
   int     component_error)
{
   if (allow_fbr)
      if (is_full_backward_ref()) {
         parse_full_backward_ref();
         the_emitter->emit_opcode(backref_full_op);
         is_fbr = true;
         return true;
      }

   is_fbr = false;
   Boolean ok;

   if (kind_is(tk_identifier)) {
      if (the_token->lexeme_size() > size)
         semantic_warning(size_warning);
      emit_string();
      discard(); // component
      ok = true;
   }
   else {
      expression(component_error);
      ok = BOOLEAN(! in_error_state);
   }
   return ok;
}


Boolean compiler::parse_filename() {
   Boolean is_fbr;
   Boolean ok = parse_file_component(true, is_fbr, WANG_FILENAME_SIZE, 52, 43);

   if (ok && !is_fbr) {
      if (keyword_is("in")) {
         discard(); // 'in'
         ok = parse_libname(false);
         if (ok)
            the_emitter->emit_opcode(catenate_op);
      }
      else if (keyword_is("on")) {
         discard(); // 'on'
         ok = parse_volname(false);
         if (ok)
            the_emitter->emit_opcode(catenate_op);
      }
   }
   return ok;
}

Boolean compiler::parse_libname(Boolean allow_fbr) {
   Boolean is_fbr;
   Boolean ok = parse_file_component(allow_fbr, is_fbr, WANG_LIBNAME_SIZE, 53, 73);
   the_emitter->emit_opcode(lib_name_op);

   if (ok && !is_fbr) {
      if (keyword_is("on")) {
         discard(); // 'on'
         ok = parse_volname(false);
         if (ok)
            the_emitter->emit_opcode(catenate_op);
      }
   }
   return ok;
}


Boolean compiler::parse_volname(Boolean allow_fbr) {
   Boolean is_fbr;
   Boolean ok = parse_file_component(allow_fbr, is_fbr, WANG_VOLNAME_SIZE, 54, 74);
   the_emitter->emit_opcode(vol_name_op);
   return ok;
}


void compiler::parse_full_backward_ref() {
   discard(); // '('
     if (the_token->lexeme_size() > WANG_LABELNAME_SIZE+1)   // lexeme includes the trailing ":" character
      semantic_warning(51);
   emit_string();
   discard(); // label
   discard(); // ')'
}


Boolean compiler::is_full_backward_ref() {
   if (kind_is(tk_parenleft))
      if (the_scanner->lookahead(1)->kind() == tk_identifier)
         if (the_scanner->lookahead(2)->kind() == tk_parenright)
            return true;

   return false;
}

#else
Boolean compiler::parse_filename() {
#if DOS
   if (kind_is(tk_label) && the_token->lexeme_size() == 2) {
       // Assume it's a DOS drive designator
      token *drive_token = the_token;
      keep();
      if ((kind_is(tk_identifier) || kind_is(tk_pathname)) &&
          (the_token->column() == drive_token->column() + 2))
      {
         token *filename_token = new token(*drive_token, *the_token);
         emit_string(filename_token);
         delete filename_token;
         discard(); // filename
         return true;
      }
      else {
         syntax_error(1);
         return false;
      }
   }
   else if (kind_is(tk_mult)) {
#else
   if (kind_is(tk_mult)) {
#endif
       // Assume it's a wildcard
      token *wildcard_token = the_token;
      keep();
      if ((kind_is(tk_identifier) || kind_is(tk_pathname)) &&
          (the_token->column() == wildcard_token->column() + 1))
      {
         token *filename_token = new token(*wildcard_token, *the_token);
         emit_string(filename_token);
         delete filename_token;
         discard(); // rest of pattern
      }
      else {
         emit_string(wildcard_token);
         delete wildcard_token;
      }
      return true;
   }
   else if (kind_is(tk_identifier) || kind_is(tk_pathname)) {
      emit_string();
      discard(); // filename
      return true;
   }
   else {
      expression(43);
      return BOOLEAN(! in_error_state);
   }
}

#endif

#endif





//
//	History:
//	$Log: p_exp.cpp,v $
//	Revision 1.7  1998-08-31 15:14:02-04  gsl
//	drcs update
//
//

//	
//	RCS file: /disk1/neomedia/RCS/wisp/wproc/p_exp.cpp,v
//	Working file: p_exp.cpp
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
//	date: 1997-10-01 18:43:25-04;  author: gsl;  state: V4_3_00;  lines: +2 -2
//	fix warnings
//	----------------------------
//	revision 1.5
//	date: 1996-07-25 14:15:43-04;  author: gsl;  state: V4_1_02;  lines: +0 -0
//	Renamed from p_exp.cc to p_exp.cpp
//	----------------------------
//	revision 1.4
//	date: 1995-04-25 06:00:11-04;  author: gsl;  state: V3_3_19;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:52:27-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:33:08-05;  author: gsl;  state: V3_3x12;  lines: +45 -45
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:17-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
