//
//	Copyright (c) 1996-1998 NeoMedia Technologies Inc. All rights reserved.
//
//	Project:	WPROC
//	
//// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : e_scrnio.cpp
// Author : George Soules
// Date   : 5 April 1991

// Specification
#include "machine.hpp"

// Classes
#include "screen.hpp"

// Definitions and subprograms
#include "attribs.hpp"
#include "cancel.hpp"
#include "crt_io.hpp"
#include "memory.hpp"
#include "subops.hpp"
#include "txt.hpp"
#include "utility.hpp"
#include "wisp_rts.h"

void init_mod_attributes(field_attributes &is) {
   if (is.modifiable) {
      is.alpha        = false;
      is.alphanumeric = false;
      is.digits       = false;
      is.hex          = false;
      is.numeric      = false;
      is.tab          = false;
   }
   else
      is.modifiable = true;
}


void init_intensity_attributes(field_attributes &is) {
   is.bright = false;
   is.dim    = false;
}


void set_field_attr(field_attributes &is, field_attr an_attr) {
   switch (an_attr) {
      case attr_alpha :
         init_mod_attributes(is);
         is.alpha = true;
         break;

      case attr_alphanum :
         init_mod_attributes(is);
         is.alphanumeric = true;
         break;

      case attr_blank :
         is.blank = true;
         break;

      case attr_blink :
         is.blink = true;
         break;

      case attr_bright :
         init_intensity_attributes(is);
         is.bright = true;
         break;

      case attr_digits :
         init_mod_attributes(is);
         is.digits = true;
         break;

      case attr_dim :
         init_intensity_attributes(is);
         is.dim = true;
         break;

      case attr_first :
         is.first = true;
         if (! is.modifiable) {
            // defaults to 'uplow' if 'first' specified alone
            is.uplow      = true;
            is.modifiable = true;
         }
         break;

      case attr_hex :
         init_mod_attributes(is);
         is.hex = true;
         break;

      case attr_line :
         is.line = true;
         break;

      case attr_lower :
         is.modifiable = true;
         is.lower = true;
         break;

      case attr_numeric :
         init_mod_attributes(is);
         is.numeric = true;
         break;

      case attr_reverse :
         is.reverse = true;
         break;

      case attr_tab :
         init_mod_attributes(is);
         is.tab = true;
         break;

      case attr_uplow :
         is.modifiable = true;
         is.uplow = true;
         break;

      case attr_upper :
         is.modifiable = true;
         is.upper = true;
         break;

      default :
         assert(UNREACHABLE);
   }
}


void machine::exec_screen_io(opcode an_opcode) {
//   symbol       *the_symbol;
//   int_32        symbol_range;
   opcode        the_machine_op;
   screen_opcode the_screen_op;
   Boolean       yes_no_option;
   Boolean       in_error_state = false;
#if WANG
   Boolean       using_facs = true;
#else
   Boolean       using_facs = false;
#endif
   int_32        first_row = 1;
   expression   *var_fkey   = NULL;
   expression   *var_curcol = NULL;
   expression   *var_currow = NULL;
   int           left_col;
   int           right_col;
   int           top_row;
   int           bottom_row;
   int           screen_height;
   int           screen_width;
   int           physical_screen_height;
   int           physical_screen_width;
   Boolean       fit_border_to_text = false;
   Boolean       center_border_horizontally = false;
   Boolean       center_border_vertically = false;
   Boolean       border_color_set = false;
   Boolean       title_color_set = false;

   if (in_graphics_mode())
      report_fatal_error(6);

   // Get dimensions of the screen
   get_screen_dimensions(physical_screen_height, physical_screen_width);
   screen_height = physical_screen_height;
   screen_width  = physical_screen_width;
   left_col   = 1;
   right_col  = screen_width;
   top_row    = 1;
   bottom_row = screen_height;

   // Create the screen object
   screen the_screen;

   // Process options
   while (! in_error_state) {
      the_machine_op = execute();

      if (the_machine_op == halt_op) {
         // The machine encountered a fatal error
         in_error_state = true;
         break;
      }
      the_screen_op = (screen_opcode) (the_pcode_reader->get_usign_8());
      if (the_screen_op == screen_options_end_op)
         break;

      switch (the_screen_op) {
         case screen_alarm_op :
            if (get_yes_no_option(yes_no_option))
               the_screen.set_alarm(yes_no_option);
            else
               in_error_state = true;
            break;

         case screen_border_op : {
            pop_expressions(1);
            int_32 border = exp[1].integer(0, 9, 33, txt_border);
            if (exp[1].is_bad())
               in_error_state = true;
            else
               the_screen.set_border(border);
            break;
         }

         case screen_bordercolor_op :
         case screen_screencolor_op :
         case screen_titlecolor_op :
         {
            color  the_color;
            colors coloring = default_colors;
            int_8  args = the_pcode_reader->get_int_8();

            pop_expressions(args);

            for (int i = 1; i <= args && ! in_error_state; i++) {
               while (! is_color(exp[i].string(), the_color)) {
                  exp[i].request_string(33, txt_color);
                  if (exp[i].is_bad()) {
                     in_error_state = true;
                     break;
                  }
               }
               if (i == 1)
                  coloring.foreground = the_color;
               else if (i == 2)
                  coloring.background = the_color;
               else
                  assert(UNREACHABLE);
            }
            if (! in_error_state) {
               if (the_screen_op == screen_screencolor_op) {
                  the_screen.set_screen_colors(coloring);
                  if (! border_color_set)
                     the_screen.set_border_colors(coloring);
                  if (! title_color_set)
                     the_screen.set_title_colors(coloring);
               }
               else if (the_screen_op == screen_bordercolor_op) {
                  the_screen.set_border_colors(coloring);
                  border_color_set = true;
               }
               else if (the_screen_op == screen_titlecolor_op) {
                  the_screen.set_title_colors(coloring);
                  title_color_set = true;
               }
            }
            break;
         }

         case screen_corner_op : {
            int_8 args = the_pcode_reader->get_int_8();
            pop_expressions(args);
            if (args == 2)
               fit_border_to_text = true;

            for (int i = 1; i <= args && ! in_error_state; i++) {
               switch (i) {
                  case 1 :
                     left_col =
                        exp[i].integer(0, screen_width - 4, 41, txt_col);
                     if (fit_border_to_text && left_col == 0) {
                        left_col = 1;
                        center_border_horizontally = true;
                     }
                     break;
                  case 2 :
                     top_row =
                        exp[i].integer(0, screen_height - 2, 41, txt_row);
                     if (fit_border_to_text && top_row == 0) {
                         top_row = 1;
                         center_border_vertically = true;
                     }
                     break;
                  case 3 :
                     right_col = exp[i].integer
                        (left_col + 4, screen_width, 41, txt_col);
                     break;
                  case 4 :
                     bottom_row = exp[i].integer
                        (top_row + 2, screen_height, 41, txt_row);
                     break;
                  default :
                     assert(UNREACHABLE);
               }
               if (bad_exp(i))
                  in_error_state = true;
            }
            if (! in_error_state) {
               screen_width  = right_col - left_col + 1;
               screen_height = bottom_row - top_row + 1;
            }
            break;
         }

         case screen_curcol_op : {
            pop_expressions(1);
            var_curcol = exp.remove_exp(1);
            break;
         }

         case screen_currow_op : {
            pop_expressions(1);
            var_currow = exp.remove_exp(1);
            break;
         }

         case screen_cursor_op : {
            the_pcode_reader->get_int_8(); // throw away item count
            pop_expressions(2);
            int_32 x;
            int_32 y;
            x = exp[1].integer(46, txt_int_value);
            if (exp[1].is_bad())
               in_error_state = true;
            else {
               y = exp[2].integer(46, txt_int_value);
               if (exp[2].is_bad())
                  in_error_state = true;
            }
            if (! in_error_state)
               the_screen.set_cursor(x, y);
            break;
         }

         case screen_enable_op :
         case screen_escape_op :
         {
            int     fkey;
            Boolean enable = BOOLEAN(the_screen_op == screen_enable_op);
            int_8   args = the_pcode_reader->get_int_8();

            if (enable)
               the_screen.disable_all_fkeys();

            pop_expressions(args);

            for (int i = 1; i <= args && ! in_error_state; i++) {
               fkey = exp[i].integer(-1, 33, 33, txt_fkey);
               if (bad_exp(i))
                  in_error_state = true;
               else {
                  if (enable)
                     the_screen.enable_fkey(fkey);
                  else
                     the_screen.set_escape_fkey(fkey);
               }
            }
            break;
         }

         case screen_erase_op :
            if (get_yes_no_option(yes_no_option))
               the_screen.set_erase(yes_no_option);
            else
               in_error_state = true;
            break;

         case screen_facs_op :
            if (get_yes_no_option(yes_no_option))
               using_facs = yes_no_option;
            else
               in_error_state = true;
            break;

         case screen_fkey_op : {
            pop_expressions(1);
            var_fkey = exp.remove_exp(1);
            break;
         }

         case screen_modchar_op : {
            pop_expressions(1);
            the_screen.set_modchar(exp[1].string()[0]);
            break;
         }

         case screen_restore_op :
            if (get_yes_no_option(yes_no_option))
               the_screen.set_restore(yes_no_option);
            else
               in_error_state = true;
            break;

         case screen_restrict_op :
            if (get_yes_no_option(yes_no_option))
               the_screen.set_restrict(yes_no_option);
            else
               in_error_state = true;
            break;

         case screen_row_op : {
            pop_expressions(1);
            first_row = exp[1].integer(1, screen_height, 46, txt_int_value);
            if (exp[1].is_bad())
               in_error_state = true;
            else
               the_screen.set_first_row(first_row);
            break;
         }

         case screen_title_op : {
            pop_expressions(1);
            the_screen.set_title(exp[1].string());
            break;
         }

         default:
            assert(UNREACHABLE);
      }
   }

   // Update screen height and width in case the border option was used
   screen_width  -= the_screen.vborder_size() * 2;
   screen_height -= the_screen.hborder_size() * 2;

   // Process lines
   int              last_row           = first_row;
   int              last_col           = 0;
   int              max_col            = 0;
   Boolean          centered           = false;
   field_attributes is                 = default_field_attributes;
   colors           coloring           = the_screen.screen_colors();
   Boolean          last_field_had_fac = false;
   Boolean          need_fac           = false;
   Boolean          attr_enabled       = true;
   Boolean          line_enabled       = true;
   Boolean          dangling_newline   = false;

   while (! in_error_state) {
      the_machine_op = execute();

      if (the_machine_op == statement_end_op)
         break;

      if (the_machine_op == halt_op) {
         // The machine encountered a fatal error
         in_error_state = true;
         break;
      }
      the_screen_op = (screen_opcode) (the_pcode_reader->get_usign_8());

      switch (the_screen_op) {
         case screen_attr_cond_op :
            pop_expressions(1);
            attr_enabled = line_enabled ? BOOLEAN(exp[1].integer(37, txt_cond)) : false;
            if (bad_exp(1)) {
               in_error_state = true;
               break;
            }
         // fall through
         case screen_attr_op : {
            if (! line_enabled)
               attr_enabled = false;
            need_fac = true;
            field_attr attr = (field_attr) (the_pcode_reader->get_usign_8());
            if (attr == attr_color)
               in_error_state = BOOLEAN(! set_field_colors(coloring, attr_enabled));
            else {
               if (attr_enabled)
                  set_field_attr(is, attr);
            }
            attr_enabled = true;
            break;
         }

         case screen_center_op : {
            if (line_enabled)
               centered = true;
            break;
         }

         case screen_center_cond_op : {
            pop_expressions(1);
            if (line_enabled) {
               centered = BOOLEAN(exp[1].integer(37, txt_cond));
               if (bad_exp(1))
                  in_error_state = true;
            }
            break;
         }

         case screen_field_op :
         {
            pop_expressions(1);
            if (! line_enabled)
               break;
            dangling_newline = false;

            Boolean modifiable = BOOLEAN(is.modifiable || is.tab);
            Boolean integer_variable = BOOLEAN(exp[1].kind() == expression::integer_kind);
            Boolean insert_fac = BOOLEAN(using_facs && need_fac);

            int size;
            if (modifiable && integer_variable) {
               if (is.digits || is.numeric)
                  size = 11;
               else {
                  exp[1].fatal_error(36, (char *) NULL, NULL);
                  in_error_state = true;
                  break;
               }
            }
            else
               size = strlen(exp[1].string());
            last_col += size;
            if (is.tab)
               last_col += 1;

            if (insert_fac && ! last_field_had_fac) {
               // Put in a fake leading FAC (Wang VS Field Attribute Char)
               the_screen.add_field(new field
                  (last_row, " ", default_field_attributes, centered, coloring));
               last_col += 1;
            }

            if (last_row > screen_height || last_col > screen_width) {
               exp[1].fatal_error
                  (last_row > screen_height ? 39 : 35, exp[1].string(), NULL);
               in_error_state = true;
               break;
            }

            if (modifiable) {
               if (! is.tab) {
                  lvalue &the_lvalue = exp[1].lvalue_ref();
                  symbol &the_symbol = the_lvalue.symbol_ref();
                  if (strlen(exp[1].string()) == 0 || the_symbol.is.builtin) {
                     int error = the_symbol.is.builtin ? 64 : 42;
                     fatal_error(the_lvalue.symbol_range(), error, the_symbol.name());
                     in_error_state = true;
                     break;
                  }
               }

               expression *variable = exp.remove_exp(1);
               if (integer_variable) {
                  the_screen.add_mod_field(new mod_field
                     (last_row, variable, "           ", is, centered, coloring));
               }
               else {
                  the_screen.add_mod_field(new mod_field
                     (last_row, variable, is, centered, coloring));
               }
            }
            else {
               the_screen.add_field(new field
                  (last_row, exp[1].string(), is, centered, coloring));
            }

            if (insert_fac) {
               // Put in a fake trailing FAC
               the_screen.add_field(new field
                  (last_row, " ", default_field_attributes, centered, coloring));
               last_col += 1;
               last_field_had_fac = true;
            }
            else
               last_field_had_fac = false;

            is       = default_field_attributes;
            need_fac = false;
            coloring = the_screen.screen_colors();
            break;
         }

         case screen_line_cond_op : {
            pop_expressions(1);
            line_enabled = BOOLEAN(exp[1].integer(37, txt_cond));
            if (bad_exp(1)) {
               in_error_state = true;
               break;
            }
            break;
         }

         case screen_newline_op : {
            if (line_enabled) {
               dangling_newline = true;
               last_row += 1;
               max_col = max(max_col, last_col);
               last_col = 0;
               centered = false;
               last_field_had_fac = false;
            }
            else
               line_enabled = true;
            break;
         }

         default:
            assert(UNREACHABLE);
      }
   }

   if (! in_error_state) {
      max_col = max(max_col, last_col);

      if (dangling_newline)
         last_row -= 1;
      if (last_row > screen_height)
         last_row = screen_height;

      if (fit_border_to_text) {
         right_col -= (screen_width - max_col);
         if (right_col < left_col)
            right_col = left_col;
         bottom_row -= (screen_height - last_row);
         if (center_border_horizontally) {
            int shift = (physical_screen_width - right_col) / 2;
            left_col += shift;
            right_col += shift;
         }
         if (center_border_vertically) {
            int shift = (physical_screen_height - bottom_row) / 2;
            top_row += shift;
            bottom_row += shift;
         }
         the_screen.set_first_row(0);
      }

      the_screen.set_corners(left_col, top_row, right_col, bottom_row);
      the_screen.set_rows(min(last_row, screen_height));

      // Display the screen
      Boolean read = BOOLEAN(an_opcode == prompt_op);

      if (message_op == an_opcode && WL_wbackground())
      {
         // If in background then MESSAGE does not display
      }
      else
      {
         the_screen.display(read);
      }

      if (the_screen.cancelled())
         cancel_handler();

      if (read && ! in_error_state) {
         if (the_screen.assign_fields(this)) {
            if (var_fkey) {
               assign(var_fkey, the_screen.fkey());
               var_fkey = NULL;
            }
            if (var_curcol) {
               assign(var_curcol, the_screen.curcol());
               var_curcol = NULL;
            }
            if (var_currow) {
               assign(var_currow, the_screen.currow());
               var_currow = NULL;
            }
         }
         else
            in_error_state = true;
      }
   }
   if (in_error_state) {
      if (var_fkey)
         delete var_fkey;
      if (var_curcol)
         delete var_curcol;
      if (var_currow)
         delete var_currow;
      enter_cancel_state();
   }
}


Boolean machine::get_yes_no_option(Boolean &an_option) {
   pop_expressions(1);
   while (! is_yes_or_no(exp[1].string(), an_option)) {
      exp[1].request_string(4, txt_option);
      if (exp[1].is_bad())
         return false;
   }
   return true;
}


Boolean machine::set_field_colors(colors &coloring, Boolean attr_enabled) {
   color the_color;
   int_8 args = the_pcode_reader->get_int_8();
   pop_expressions(args);
   if (attr_enabled) {
      for (int i = 1; i <= args; i++) {
         while (! is_color(exp[i].string(), the_color)) {
            exp[i].request_string(33, txt_color);
            if (exp[i].is_bad())
               return false;
         }
         if (i == 1)
            coloring.foreground = the_color;
         else if (i == 2)
            coloring.background = the_color;
         else
            assert(UNREACHABLE);
      }
   }
   return true;
}


void machine::exec_screen() {
   usign_8 fcn       = the_pcode_reader->get_usign_8();
   int_32  fcn_range = the_pcode_reader->get_int_32();
   int     error     = 0;

   switch (fcn) {
      case 1 :
         // clear
         clear_screen();
         break;

      case 2 :
         // save
         if (screen_depth < screen_stack_size - 1) {
            screen_contents *the_contents = new screen_contents();
            if (the_contents->ok()) {
               screen_depth += 1;
               screen_stack[screen_depth] = the_contents;
            }
            else {
               delete the_contents;
               error = 1;
            }
         }
         else
            error = 2;

         if (error) {
            fatal_error(fcn_range, error == 1 ? 51 : 49, screen_depth + 1);
            enter_cancel_state();
         }
         break;

      case 3 :
         // restore
         if (screen_depth >= 0) {
            screen_stack[screen_depth]->restore_screen();
            delete screen_stack[screen_depth];
            screen_depth -= 1;
         }
         else {
            fatal_error(fcn_range, 50);
            enter_cancel_state();
         }
         break;

      case 4 :
         // cursor on
         cursor_visible(true);
         break;

      case 5 :
         // cursor off
         cursor_visible(false);
         break;

      default :
         assert(UNREACHABLE);
   }
}

//
//	History:
//	$Log: e_scrnio.cpp,v $
//	Revision 1.9  2002/07/10 21:06:28  gsl
//	Fix globals WL_ to make unique
//	
//	Revision 1.8  1998/08/31 19:13:43  gsl
//	drcs update
//	
//

//	
//	RCS file: /disk1/neomedia/RCS/wisp/wproc/e_scrnio.cpp,v
//	Working file: e_scrnio.cpp
//	head: 1.7
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
//	total revisions: 7;	selected revisions: 7
//	description:
//	----------------------------
//	revision 1.7
//	date: 1997-05-08 14:59:55-04;  author: gsl;  state: V4_3_00;  lines: +11 -2
//	FIxed MESSAGE so that if in background it does not attempt to display
//	----------------------------
//	revision 1.6
//	date: 1996-07-25 14:14:52-04;  author: gsl;  state: V3_9_92;  lines: +0 -0
//	Renamed from e_scrnio.cc to e_scrnio.cpp
//	----------------------------
//	revision 1.5
//	date: 1995-06-02 10:47:44-04;  author: gsl;  state: V3_3_19;  lines: +2 -2
//	fix warning
//	----------------------------
//	revision 1.4
//	date: 1995-04-25 05:59:51-04;  author: gsl;  state: V3_3_16;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:52:09-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:32:47-05;  author: gsl;  state: V3_3x12;  lines: +26 -29
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:05-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
