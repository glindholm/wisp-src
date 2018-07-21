//
//	Copyright (c) Shell Stream Software LLC, All Rights Reserved.
//
//	Project:	WPROC
//	
//// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : p_scrnio.cpp
// Author : George Soules
// Date   : 4 April 1991

#if RUNTIME
#else

// Specification
#include "compiler.hpp"

// Classes
#include "tables.hpp"

// Definitions and subprograms
#include <string.h>
#include "attribs.hpp"
#include "debugaid.hpp"
#include "subops.hpp"


class scrnio_option : public option_name {
   public:
      scrnio_option(
         char         *a_name,
         screen_opcode an_opcode,
         Boolean       is_yes_no,
         Boolean       is_list,
         Boolean       is_lvalue,
         Boolean       is_prompt_only)
         :
         option_name(a_name, an_opcode, is_yes_no, is_list)
      {
         prompt_only = is_prompt_only;
         lvalue      = is_lvalue;
      }
      Boolean is_prompt_only() {return prompt_only;}
      Boolean is_lvalue()      {return lvalue;}
   private:
      Boolean lvalue;
      Boolean prompt_only;
};


class attribute : public name_kind {
   public:
      attribute(
         char      *a_name,
         field_attr attr,
         Boolean    is_prompt_only) : name_kind(a_name, attr)
      {
         prompt_only = is_prompt_only;
      }
      Boolean is_prompt_only() {return prompt_only;}
   private:
      Boolean prompt_only;
};


table *scrnio_options = NULL;
table *attributes = NULL;


void init_scrnio_tables() {
   assert(! scrnio_options);
   scrnio_options = new table();

//      Name           Opcode                  YesNo  List   Lvalue Prompt
   scrnio_options->add(new scrnio_option
      ("alarm",        screen_alarm_op,        true,  false, false, false));
   scrnio_options->add(new scrnio_option
      ("border",       screen_border_op,       false, false, false, false));
   scrnio_options->add(new scrnio_option
      ("bordercolor",  screen_bordercolor_op,  false, true,  false, false));
   scrnio_options->add(new scrnio_option
      ("corner",       screen_corner_op,       false, true,  false, false));
   scrnio_options->add(new scrnio_option
      ("curcol",       screen_curcol_op,       false, false, true,  true));
   scrnio_options->add(new scrnio_option
      ("currow",       screen_currow_op,       false, false, true,  true));
   scrnio_options->add(new scrnio_option
      ("cursor",       screen_cursor_op,       false, true,  false, true));
   scrnio_options->add(new scrnio_option
      ("enable",       screen_enable_op,       false, true,  false, true));
   scrnio_options->add(new scrnio_option
      ("erase",        screen_erase_op,        true,  false, false, false));
   scrnio_options->add(new scrnio_option
      ("escape",       screen_escape_op,       false, true,  false, true));
   scrnio_options->add(new scrnio_option
      ("facs",         screen_facs_op,         true,  false, false, false));
   scrnio_options->add(new scrnio_option
      ("modchar",      screen_modchar_op,      false, false, false, true));
   scrnio_options->add(new scrnio_option
      ("pfkey",        screen_fkey_op,         false, false, true,  true));
   scrnio_options->add(new scrnio_option
      ("restore",      screen_restore_op,      true,  false, false, true));
   scrnio_options->add(new scrnio_option
      ("restrict",     screen_restrict_op,     true,  false, false, true));
#if !WANG
   scrnio_options->add(new scrnio_option
      ("row",          screen_row_op,          false, false, false, false));
#endif
   scrnio_options->add(new scrnio_option
      ("screencolor",  screen_screencolor_op,  false, true,  false, false));
   scrnio_options->add(new scrnio_option
      ("title",        screen_title_op,        false, false, false, false));
   scrnio_options->add(new scrnio_option
      ("titlecolor",   screen_titlecolor_op,   false, true,  false, false));

   assert(! attributes);
   attributes = new table();
//                                Name       Attribute      Prompt
   attributes->add(new attribute("alpha",    attr_alpha,    true));
   attributes->add(new attribute("alphanum", attr_alphanum, true));
   attributes->add(new attribute("blank",    attr_blank,    false));
   attributes->add(new attribute("blink",    attr_blink,    false));
   attributes->add(new attribute("bright",   attr_bright,   false));
   attributes->add(new attribute("color",    attr_color,    false));
   attributes->add(new attribute("digits",   attr_digits,   true));
   attributes->add(new attribute("dim",      attr_dim,      false));
   attributes->add(new attribute("first",    attr_first,    true));
   attributes->add(new attribute("hex",      attr_hex,      true));
   attributes->add(new attribute("line",     attr_line,     false));
   attributes->add(new attribute("lower",    attr_lower,    true));
   attributes->add(new attribute("numeric",  attr_numeric,  true));
   attributes->add(new attribute("reverse",  attr_reverse,  false));
   attributes->add(new attribute("tab",      attr_tab,      true));
   attributes->add(new attribute("uplow",    attr_uplow,    true));
   attributes->add(new attribute("upper",    attr_upper,    true));
}


void compiler::p_scrnio_cleanup() {
   if (scrnio_options) {
      delete scrnio_options;
      scrnio_options = NULL;
   }
   if (attributes) {
      delete attributes;
      attributes = NULL;
   }
}


//  message_statement
//     -> 'message' (option ',')* message_line+
//
//  option
//     -> 'alarm' '=' yes_no-exp
//     -> 'border' '=' borderstyle-exp
//     -> 'bordercolor' '=' '(' color-exp ',' color-exp ')'
//     -> 'corner' '=' '(' lc-exp ',' tr-exp ',' rc-exp ',' br-exp ')'
//     -> 'erase' '=' yes_no-exp
//     -> 'facs' '=' yes_no-exp
//     -> 'restore' '=' yes_no-exp
//     -> 'restrict' '=' yes_no-exp
//     -> 'row' '=' integer-exp
//     -> 'screencolor' '=' '(' color-exp ',' color-exp ')'
//     -> 'title' '=' string-exp
//     -> 'titlecolor' '=' '(' color-exp ',' color-exp ')'
//
//  message_line
//     -> ('show' 'when' '(' Boolean-exp ')')? 'center'?
//           ((attribute ('when' '(' Boolean-exp ')'))* exp list ',')
//     -> ';'
//
//  attribute
//     -> 'blank'
//     -> 'blink'
//     -> 'bright'
//     -> 'color' '(' color-exp ',' color-exp ')'
//     -> 'dim'
//     -> 'line'
//     -> 'reverse'

void compiler::message_stmt() {
   trace_begin(parser, "message_stmt");
   parse_screen(message_op);
   trace_end(parser);
}


//  prompt_statement
//     -> 'prompt' (option ',')* message_line+
//
//  option
//     -> 'alarm' '=' yes_no-exp
//     -> 'border' '=' borderstyle-exp
//     -> 'bordercolor' '=' '(' color-exp ',' color-exp ')'
//     -> 'corner' '=' '(' lc-exp ',' tr-exp ',' rc-exp ',' br-exp ')'
//     -> 'curcol' '=' variable
//     -> 'currow' '=' variable
//     -> 'cursor' '=' '(' col-exp ',' row-exp ')'
//     -> 'enable' '=' '(' (pfkey-exp list ',') ')'
//     -> 'erase' '=' yes_no-exp
//     -> 'escape' '=' '(' (pfkey-exp list ',') ')'
//     -> 'facs' '=' yes_no-exp
//     -> 'modchar '=' character-exp
//     -> 'pfkey' '=' variable
//     -> 'restore' '=' yes_no-exp
//     -> 'restrict' '=' yes_no-exp
//     -> 'row' '=' integer-exp
//     -> 'screencolor' '=' '(' color-exp ',' color-exp ')'
//     -> 'title' '=' string-exp
//     -> 'titlecolor' '=' '(' color-exp ',' color-exp ')'
//
//  message_line
//     -> ('show' 'when' '(' Boolean-exp ')')? 'center'?
//           ((attribute ('when' '(' Boolean-exp ')'))* exp list ',')
//     -> ';'
//
//  attribute
//     -> 'alpha'
//     -> 'alphanum'
//     -> 'blank'
//     -> 'blink'
//     -> 'bright'
//     -> 'color' '(' color-exp ',' color-exp ')'
//     -> 'digits'
//     -> 'dim'
//     -> 'first'
//     -> 'hex'
//     -> 'line'
//     -> 'lower'
//     -> 'numeric'
//     -> 'reverse'
//     -> 'tab'
//     -> 'uplow'
//     -> 'upper'

void compiler::prompt_stmt() {
   trace_begin(parser, "prompt_stmt");
   parse_screen(prompt_op);
   trace_end(parser);
}


Boolean compiler::parse_screen_field(opcode an_opcode) {
   trace(parser, "parse_screen_field");

   Boolean field_required   = false;
   Boolean is_tab           = false;
   Boolean modifiable_field = false;
   int     list_item_count  = 0;
   Boolean allowed          = true;
   Boolean cond_attr;

   // Parse attributes
   attribute *the_attribute;
   attributes->set_none_seen();

   while (allowed) {
      if (kind_is(tk_identifier))
         the_attribute = (attribute *) attributes->lookup(the_token->lexeme());
      else
         the_attribute = NULL;
      if (the_attribute) {
         if (the_attribute->is_prompt_only()) {
            modifiable_field = true;
            if (an_opcode == message_op) {
               semantic_error(23);
               return false;
            }
         }
         if (the_attribute->seen() > 1) {
            semantic_error(35);
            return false;
         }

         discard(); // attribute
         field_attr the_attr = (field_attr) the_attribute->kind();

         if (the_attr == attr_color) {
            if (! parse_color_list(list_item_count))
               return false;
         }
         if (the_attr == attr_tab)
            is_tab = true;

         if (keyword_is("when")) {
            discard(); // 'when'
            if (! parse_attribute_condition())
               return false;
            cond_attr = true;
         }
         else
            cond_attr = false;

         the_emitter->emit_opcode(scrnio_subop);
         the_emitter->emit_usign_8
            (cond_attr ? screen_attr_cond_op : screen_attr_op);
         the_emitter->emit_usign_8(the_attr);
         if (the_attr == attr_color)
            the_emitter->emit_usign_8(list_item_count);
         field_required = true;
      }
      else
         allowed = false;
   }

   if (modifiable_field && ! is_tab) {
      if (! kind_is(tk_variable)) {
         syntax_error(37);
         return false;
      }
      symbol *the_symbol = lookup_or_create_symbol(the_token->lexeme());
      range the_range(the_token);
      if (parse_variable(the_symbol, the_range) == -1)
         return false;
   }
   else if (kind_is(tk_string)   || kind_is(tk_integer) ||
            kind_is(tk_variable) || kind_is(tk_parenleft) ||
            (kind_is(tk_identifier) &&
            the_symbol_table->lookup(the_token->lexeme())))
   {
      expression(32);
      if (in_error_state)
         return false;
   }
   else {
      if (field_required)
         syntax_error(32);
      return false;
   }
   the_emitter->emit_opcode(scrnio_subop);
   the_emitter->emit_usign_8(screen_field_op);
   return true;
}


Boolean compiler::parse_screen_line(opcode an_opcode) {
   trace(parser, "parse_screen_line");
   Boolean field_required = false;
   int     field_count    = 0;

   if (keyword_is("show")) {
      discard(); // 'show'
      if (! keyword_is("when")) {
         syntax_error(71);
         return false;
      }
      discard(); // 'when'
      if (! parse_attribute_condition())
         return false;
      field_required = true;
      the_emitter->emit_opcode(scrnio_subop);
      the_emitter->emit_usign_8(screen_line_cond_op);
   }

   if (keyword_is("center")) {
      field_required = true;
      the_emitter->emit_opcode(scrnio_subop);
      the_emitter->emit_usign_8(screen_center_op);
      discard(); // 'center'
      if (keyword_is("when")) {
         discard(); // 'when'
         if (! parse_attribute_condition())
            return false;
         the_emitter->emit_opcode(scrnio_subop);
         the_emitter->emit_usign_8(screen_center_cond_op);
      }
   }

   while (parse_screen_field(an_opcode)) {
      if (in_error_state)
         return false;
      field_count += 1;
      if (kind_is(tk_comma)) {
         field_required = true;
         discard(); // ','
      }
      else {
         field_required = false;
         break;
      }
   }
   if (in_error_state)
      return false;
   if (field_count && ! field_required)
      return true;
   else {
      if (field_required)
         syntax_error(32);
      return false;
   }
}


void compiler::parse_screen(opcode an_opcode) {
   discard(); // 'message' or 'prompt'
   the_emitter->emit_opcode(an_opcode);

   symbol *the_symbol;
   int     list_item_count;
   Boolean options_allowed = true;

   // Parse options
   if (! scrnio_options)
      init_scrnio_tables();
   scrnio_option *the_option;
   scrnio_options->set_none_seen();

   while (options_allowed) {
      if (kind_is(tk_identifier))
         the_option =
            (scrnio_option *) scrnio_options->lookup(the_token->lexeme());
      else
         the_option = NULL;
      if (the_option) {
         if (the_option->is_prompt_only()) {
            if (an_opcode == message_op) {
               semantic_error(29);
               return;
            }
         }
         require_semantics(the_option->seen() == 1, 22);

         discard(); // option
         require_syntax("=", 3);
         discard(); // '='

         if (the_option->is_lvalue()) {
            require_kind(tk_variable, 9);
            the_symbol = lookup_or_create_symbol(the_token->lexeme());
            range the_range(the_token);
            if (parse_variable(the_symbol, the_range) == -1)
               return;
            the_emitter->emit_opcode(scrnio_subop);
            the_emitter->emit_usign_8(the_option->kind());
         }
         else {
            if (the_option->is_yes_no()) {
               if (! parse_yes_no())
                  return;
            }
            else if (the_option->is_list()) {
               if (the_option->kind() == screen_screencolor_op ||
                   the_option->kind() == screen_bordercolor_op ||
                   the_option->kind() == screen_titlecolor_op)
               {
                  if (! parse_color_list(list_item_count))
                     return;
               }
               else if (the_option->kind() == screen_enable_op ||
                        the_option->kind() == screen_escape_op)
               {
                  if (! parse_fkey_list(list_item_count))
                     return;
               }
               else if (the_option->kind() == screen_corner_op ||
                        the_option->kind() == screen_cursor_op)
               {
                  list_item_count =
                     the_option->kind() == screen_corner_op ? 4 : 2;
                  if (! parse_corners_list(list_item_count))
                     return;
               }
               else
                  assert(UNREACHABLE);
            }
            else {
               require_nonterminal(expression(58));
            }
            the_emitter->emit_opcode(scrnio_subop);
            the_emitter->emit_usign_8(the_option->kind());
            if (the_option->is_list())
               the_emitter->emit_usign_8(list_item_count);
         }
#if WANG
         if (kind_is(tk_comma))
            discard(); // ','
         else
            options_allowed = false;
#else
         require_kind(tk_comma, 31);
         discard(); // ','
#endif
      }
      else
         options_allowed = false;
   }

#if WANG
   if (keyword_is("row")) {
      discard(); // 'row'
      require_nonterminal(expression(58));
      the_emitter->emit_opcode(scrnio_subop);
      the_emitter->emit_usign_8(screen_row_op);
   }
#endif

   the_emitter->emit_opcode(scrnio_subop);
   the_emitter->emit_usign_8(screen_options_end_op);

   // Parse message lines
   Boolean has_lines = false;
   Boolean done      = true;

   while (kind_is(tk_semicolon)) {
      has_lines = true;
      the_emitter->emit_opcode(scrnio_subop);
      the_emitter->emit_usign_8(screen_newline_op);
      discard(); // ';'
   }

   while (parse_screen_line(an_opcode)) {
      if (in_error_state)
         return;
      has_lines = true;
      done = true;
      while (kind_is(tk_semicolon)) {
         done = false;
         the_emitter->emit_opcode(scrnio_subop);
         the_emitter->emit_usign_8(screen_newline_op);
         discard(); // ';'
      }
      if (done)
         break;
   }
   if (! in_error_state) {
      if (! has_lines) {
         syntax_error(42);
         return;
      }
      the_emitter->emit_opcode(statement_end_op);
   }
   trace_end(parser);
}


Boolean compiler::parse_color_list(int &count) {
   if (! kind_is(tk_parenleft)) {
      syntax_error(35);
      return false;
   }
   discard(); // '('
   if (! parse_color())
      return false;
   count = 1;
   if (kind_is(tk_comma)) {
      discard(); // ','
      if (! parse_color())
         return false;
      count += 1;
   }
   if (! kind_is(tk_parenright)) {
      syntax_error(7);
      return false;
   }
   discard(); // ')'
   return true;
}


Boolean compiler::parse_fkey_list(int &count) {
   count = 0;
   if (! kind_is(tk_parenleft)) {
      syntax_error(36);
      return false;
   }
   discard(); // '('
   while (true) {
      expression(59);
      if (in_error_state)
         return false;
      count += 1;
      if (kind_is(tk_comma))
         discard(); // ','
      else
         break;
   }
   if (! kind_is(tk_parenright)) {
      syntax_error(7);
      return false;
   }
   discard(); // ')'
   return true;
}


Boolean compiler::parse_corners_list(int &count) {
   Boolean done      = false;
   int     max_count = count;
   count = 0;
   if (! kind_is(tk_parenleft)) {
      syntax_error(40);
      return false;
   }
   discard(); // '('
   while (! done) {
      expression(60);
      if (in_error_state)
         return false;
      if (! kind_is(tk_comma)) {
         syntax_error(41);
         return false;
      }
      discard(); // ','
      expression(60);
      if (in_error_state)
         return false;
      count += 2;
      if (count < max_count && kind_is(tk_comma))
         discard(); // ','
      else
         done = true;
   }
   if (! kind_is(tk_parenright)) {
      syntax_error(7);
      return false;
   }
   discard(); // ')'
   return true;
}


Boolean compiler::parse_attribute_condition() {
   if (! kind_is(tk_parenleft)) {
      syntax_error(38);
      return false;
   }
   discard(); // '('
   expression(49);
   if (in_error_state)
      return false;
   if (! kind_is(tk_parenright)) {
      syntax_error(7);
      return false;
   }
   discard(); // ')'
   return true;
}


void compiler::screen_stmt() {
   trace_begin(parser, "screen_stmt");
   discard(); // 'screen'

   usign_8 fcn = 0;

   if (keyword_is("clear"))
      fcn = 1;
   else if (keyword_is("save"))
      fcn = 2;
   else if (keyword_is("restore"))
      fcn = 3;
   else if (keyword_is("cursor")) {
      discard(); // 'cursor'
      if (keyword_is("on"))
         fcn = 4;
      else if (keyword_is("off"))
         fcn = 5;
   }

   if (fcn) {
      the_emitter->emit_opcode(screen_op);
      the_emitter->emit_usign_8(fcn);

      // Emit range for fcn name to allow a runtime error to be reported
      range the_range(the_token);
      the_emitter->emit_int_32(the_range.pack());
      discard();
   }
   else
      syntax_error(70);

   trace_end(parser);
}
#endif





//
//	History:
//	$Log: p_scrnio.cpp,v $
//	Revision 1.6  1998/08/31 19:14:04  gsl
//	drcs update
//	
//

//	
//	RCS file: /disk1/neomedia/RCS/wisp/wproc/p_scrnio.cpp,v
//	Working file: p_scrnio.cpp
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
//	date: 1996-07-25 14:15:49-04;  author: gsl;  state: V4_3_00;  lines: +0 -0
//	Renamed from p_scrnio.cc to p_scrnio.cpp
//	----------------------------
//	revision 1.4
//	date: 1995-04-25 06:00:12-04;  author: gsl;  state: V3_3_19;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:52:28-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:33:10-05;  author: gsl;  state: V3_3x12;  lines: +33 -31
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:18-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
