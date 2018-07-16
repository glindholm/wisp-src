// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : screen.cpp
// Author : George Soules
// Date   : 24 May 1991

// Specification
#include "screen.hpp"

// Classes
// (none)

// Definitions and subprograms
#include <ctype.h>
#include <string.h>
#include "cancel.hpp"
#include "crt_io.hpp"
#include "debugaid.hpp"
#include "keyboard.hpp"
#include "machine.hpp"
#include "memory.hpp"
#include "utility.hpp"

extern "C" void ws_help(int);

screen_info global_screen_info;

field_attributes default_field_attributes =
   {0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, (Boolean)0};

colors default_colors = {color_white, color_black};

Boolean in_insert_mode = false;


// screen_info member functions

screen_info::screen_info() {
   left         = 0;
   top          = 0;
   right        = 0;
   bottom       = 0;
   cursor_shape = 0;
}


// screen_contents member functions

screen_contents::screen_contents(
   int left_col, int top_row, int right_col, int bottom_row)
{
   trace(object, "screen_contents");
   left         = left_col;
   top          = top_row;
   right        = right_col;
   bottom       = bottom_row;

#if DOS || DOS_HOST
   const int min_heap = 4 * 1024;
   int       size     = (bottom - top + 1) * (right - left + 1) * 2;

   if (memory_left(min_heap + size)) {
      the_contents = new usign_8[size];
      if (the_contents) {
         the_ok = true;
              get_region(left, top, right, bottom, the_contents);
         if (the_process->cursor_off) {
            cursor_x = 1;
            cursor_y = SCREEN_HEIGHT;
         }
         else {
            cursor_x = where_x();
            cursor_y = where_y();
         }
      }
      else
         the_ok = false;
   }
   else {
      the_ok = false;
      the_contents = NULL;
   }
#else
#if WANG
        get_region(left, top, right, bottom, the_contents);
        the_ok = BOOLEAN(the_contents != NULL);
#endif
#endif
}


screen_contents::~screen_contents() {
   if (the_contents)
#if DOS || DOS_HOST
      delete the_contents;
#else
#if WANG
           the_contents = NULL;
#endif
#endif
   trace(object, "~screen_contents");
}


void screen_contents::restore_screen() {
   if (the_contents) {
#if DOS || DOS_HOST
      put_region(left, top, right, bottom, the_contents);
      goto_xy(cursor_x, cursor_y);
#else
#if WANG
      put_region(the_contents);
#endif
#endif
   }
}


// field member functions

field::field(
   int              a_row,
   const char      *text,
   field_attributes attributes,
   Boolean          center_option,
   colors           coloring)
{
   trace_ss(object, "field ", text);
   the_row          = a_row;

   if (attributes.tab) {
      // Prepend space for a pseudoblank
      the_text = new_string(strlen(text) + 2);
      the_text[0] = ' ';
      the_text[1] = '\0';
      strcat(the_text, text);
   }
   else
      the_text = dup_string(text);

   the_text_size    = strlen(the_text);
   needs_centering  = center_option;
   the_first_col    = 1;
   the_last_col     = the_text_size;
   next             = NULL;
   the_modchar      = '?';

   set_attributes(attributes, coloring);
}


field::~field() {
   delete the_text;
   if (next)
      delete next;
   trace(object, "~field");
}


void field::set_attributes(
   field_attributes attributes,
   colors coloring)
{
   is = attributes;

   if (is.line) {
      coloring.foreground = color_blue;
      coloring.background = color_black;
   }
   if (is.reverse) {
      color temp = coloring.foreground;
      coloring.foreground = coloring.background;
      coloring.background = temp;
   }
   else {
      if (is.bright || (is.modifiable && ! is.dim && ! is.tab))
         // Make bright
         coloring.foreground = (color) (coloring.foreground | ATTR_BRIGHT);
   }
   if (coloring.background >= FIRST_LT_COLOR)
      // Disallow bright color as background
      coloring.background = (color) (coloring.background - FIRST_LT_COLOR);
   if (is.blink)
      coloring.foreground = (color) (coloring.foreground | ATTR_BLINK);
   the_video_attribute = coloring.foreground + (coloring.background << 4);
}


void field::display() {
   if (needs_centering)
      center();
   screen_put();
}


void field::screen_put(Boolean highlight) {
   int col_offset = global_screen_info.left - 1;
   int row_offset = global_screen_info.top - 1;
   int left_col   = the_first_col + col_offset;
   int right_col  = the_last_col + col_offset;
   int row        = the_row + row_offset;

   if (left_col > global_screen_info.right ||
       row < global_screen_info.top || row > global_screen_info.bottom)
   {
      return;
   }
   if (left_col < global_screen_info.left)
      left_col = global_screen_info.left;
   if (right_col > global_screen_info.right)
      right_col = global_screen_info.right;

   char *buffer = new char[the_text_size + 1];
   int i = 0;
   int c = the_text[i];

   usign_8 attr =
      highlight ? the_video_attribute | ATTR_BRIGHT : the_video_attribute;

   while (c) {
      if (is.blank)
         c = ' ';
      buffer[i] = is.modifiable && ! is.tab && c == ' ' ? the_modchar : c;
      c = the_text[++i];
   }
   buffer[right_col - left_col + 1] = '\0';

   put_text(left_col, row, buffer, attr);
   delete buffer;
}


void field::center() {
   Boolean in_row   = true;
   int     last_col = 0;
   int     fields   = 0;

   // Find last field in this row
   field *fld = this;
   while (in_row && fld) {
      fields += 1;
      fld->needs_centering = false;
      last_col = fld->the_last_col;
      fld = fld->next;
      if (fld)
         in_row = BOOLEAN(fld->the_row == the_row);
   }

   // Center fields in row
   int offset =
      global_screen_info.right - global_screen_info.left + 1 - last_col;
   if (offset > 0) {
      offset /= 2;
      fld = this;
      while (fields--) {
         fld->the_first_col += offset;
         fld->the_last_col  += offset;
         fld = fld->next;
      }
   }
}


void field::append(field *a_field) {
   next = a_field;
   if (the_row == a_field->the_row) {
      a_field->the_first_col += the_last_col;
      a_field->the_last_col  += the_last_col;
   }
}


int field::last_nonblank_col() {
   int col = the_last_col;
   while (col > the_first_col && the_text[col - the_first_col] == ' ')
      col -= 1;
   if (col != the_last_col && (col != the_first_col || the_text[0] != ' '))
      col += 1;
   return col;
}


void field::increment_rows_by(int a_row) {
   the_row += a_row;
   if (next)
      next->increment_rows_by(a_row);
}


void field::set_modchar(char c) {
   the_modchar = c;
   if (is.tab)
      the_text[0] = the_modchar;
}


char field::first_letter() {
   assert(is.tab);
   int i = 1;
   int c = the_text[i];
   while (c) {
      if (c == ' ')
         c = the_text[++i];
      else
         return c;
   }
   return '\0';
}


// mod_field member functions

mod_field::mod_field(
   int              a_row,
   expression      *a_variable,
   field_attributes attributes,
   Boolean          center_option,
   colors           coloring)
   :
   field(a_row, a_variable->string(), attributes, center_option, coloring)
{
   trace(object, "mod_field (string)");
   the_exp = a_variable;
   next    = NULL;
   prev    = NULL;
}


mod_field::mod_field(
   int              a_row,
   expression      *a_variable,
   const char      *text,
   field_attributes attributes,
   Boolean          center_option,
   colors           coloring)
   :
   field(a_row, text, attributes, center_option, coloring)
{
   trace(object, "mod_field (integer)");
   the_exp = a_variable;
   next    = NULL;
   prev    = NULL;

   const char *s = a_variable->string();
   int len = strlen(s);
   for (int i = 0; i < len; i++)
      the_text[i] = s[i];
}


mod_field::mod_field(
   int              a_row,
   const char      *text,
   field_attributes attributes,
   Boolean          center_option,
   colors           coloring)
   :
   field(a_row, text, attributes, center_option, coloring)
{
   trace(object, "mod_field (string)");
   the_exp = NULL;
   next    = NULL;
   prev    = NULL;
}


mod_field::~mod_field() {
   if (the_exp)
      delete the_exp;
   trace(object, "~mod_field");
}


void mod_field::append(mod_field *a_field) {
   next = a_field;
   a_field->prev = this;
}


mod_field *mod_field::initial_cursor_field() {
   mod_field *fld = this;
   while (fld) {
      if (fld->is.first)
         return fld;
      fld = fld->next;
   }
   return NULL;
}


Boolean mod_field::put_char(int c, int a_col, Boolean insert) {
   assert(a_col >= the_first_col && a_col <= the_last_col);

   if (is.alpha && ! (isalpha(c) || c == ' '))
      return false;
   else if (is.alphanumeric && ! (isalnum(c) || c == ' '))
      return false;
   else if (is.hex && ! isxdigit(c))
      return false;
   else if (is.digits && ! isdigit(c))
      return false;
   else if (is.lower)
      c = tolower(c);
   else if (is.numeric && ! (isdigit(c) || c == ' ' || c == '+' || c == '-'))
      return false;
   else if (is.tab)
      return false;
   else if (is.upper)
      c = toupper(c);

   int index;
   if (insert) {
      index = the_last_col - the_first_col;
      if (the_text[index] != ' ')
         return false;
      for (int i = a_col; i < the_last_col; i++) {
         the_text[index] = the_text[index - 1];
         index -= 1;
      }
   }

   index = a_col - the_first_col;
   the_text[index] = c;
   screen_put();
   return true;
}


Boolean mod_field::delete_char_at(int a_col) {
   if (is.tab)
      return false;
   int index = a_col - the_first_col;
   for (int i = a_col; i < the_last_col; i++) {
      the_text[index] = the_text[index + 1];
      index += 1;
   }
   the_text[index] = ' ';
   screen_put();
   return true;
}


Boolean mod_field::erase_from(int a_col) {
   if (is.tab)
      return false;
   int index = a_col - the_first_col;
   for (int i = a_col; i <= the_last_col; i++) {
      the_text[index] = ' ';
      index += 1;
   }
   screen_put();
   return true;
}


Boolean mod_field::assign(machine *a_machine) {
   assert(the_exp);
   if (! is.tab) {
      if (the_exp->lvalue_ref().symbol_ref().is.integer) {
         int_32 i;
         if (! string_to_int_32(the_text, i)) {
            the_exp->fatal_error(40, the_text, NULL);
            return false;
         }
      }
      a_machine->assign(the_exp, the_text);
      the_exp = NULL;
   }
   return next ? next->assign(a_machine) : true;
}


mod_field *screen::mod_field_closest_to_cursor() {
   int        row;
   mod_field *fld   = the_first_mod_field;

   while (fld) {
      row = fld->row();
      if (row >= y) {
         if (row > y)
            return fld;
         else if (fld->last_col() >= x)
            return fld;
      }
      fld = fld->next_field();
   }
   return NULL;
}


mod_field *screen::mod_field_at_cursor() {
   mod_field *fld = the_first_mod_field;

   while (fld) {
      if (fld->row() == y && x >= fld->first_col() && x <= fld->last_col())
         return fld;
      else
         fld = fld->next_field();
   }
   return NULL;
}


// screen member functions

screen::screen() {
   trace(object, "screen");
   x                     = 0;
   y                     = 0;
   the_fkey              = 0;
   cancel                = false;
   alarm_option          = false;
   erase_option          = true;
   restore_option        = false;
#if WANG
   restrict_option       = false;
#else
   restrict_option       = true;
#endif
#if DOS
   the_modchar           = 'þ';
#else
   the_modchar           = '_';
#endif
   the_screen_colors     = default_colors;
   the_border_colors     = default_colors;
   the_title_colors      = default_colors;
   the_first_row         = 0;
   the_first_field       = NULL;
   the_last_field        = NULL;
   the_first_mod_field   = NULL;
   the_last_mod_field    = NULL;
   the_active_field      = NULL;
   rows                  = 0;
   the_border            = 0;
   the_title             = NULL;
   saved_screen_contents = NULL;

   // set_corners() must be called to initialize these
   the_left_col          = 0;
   the_right_col         = 0;
   the_top_row           = 0;
   the_bottom_row        = 0;

   // display_border() must be called to initialize these
   the_width             = 0;
   the_height            = 0;

   init_fkey_map();
}


screen::~screen() {
   if (the_first_field)
      delete the_first_field;
   if (the_title)
      delete the_title;
   trace(object, "~screen");
}


void screen::add_field(field *a_field) {
   if (the_last_field)
      the_last_field->append(a_field);
   else
      the_first_field = a_field;
   the_last_field = a_field;
}


void screen::add_mod_field(mod_field *a_field) {
   a_field->set_modchar(the_modchar);
   add_field(a_field);
   if (the_last_mod_field)
      the_last_mod_field->append(a_field);
   else
      the_first_mod_field = a_field;
   the_last_mod_field = a_field;
}


void screen::display(Boolean read) {
   // Initialize screen
   save_video_environment();
   set_cursor_shape(in_insert_mode ? CURSOR_SHAPE_INSERT : CURSOR_SHAPE_NORMAL);
   text_background(the_screen_colors.background);
   text_foreground(the_screen_colors.foreground);
   display_border();

   if (alarm_option)
      ring_bell();

   // Center rows vertically
   if (the_first_field && the_first_row == 0) {
      the_first_row = ((the_height - rows) / 2) + 1;
      the_first_field->increment_rows_by(the_first_row - 1);
   }

   // Display fields
   field *fld = the_first_field;
   while (fld) {
      fld->display();
      fld = fld->next_field();
   }

   if (read) {
      // Set initial cursor position
      if (x < 1 || y < 1 || x > the_width || y > the_height) {
         if (the_first_mod_field)
            the_active_field = the_first_mod_field->initial_cursor_field();
         if (the_active_field == NULL)
            the_active_field = the_first_mod_field;
         x = the_active_field ? the_active_field->first_col() : 1;
         y = the_active_field ? the_active_field->row() : 1;
      }
      goto_xy(x, y);

      wait_for_fkey();
   }

   restore_video_environment();
}


void screen::display_border() {
   int left   = the_left_col;
   int top    = the_top_row;
   int right  = the_right_col;
   int bottom = the_bottom_row;

   draw_box(the_border, left, top, right, bottom, erase_option,
      the_title, the_border_colors, the_title_colors);

   the_height = bottom - top + 1;
   the_width  = right - left + 1;

   if (the_border > 0) {
      left      += vborder_size();
      right     -= vborder_size();
      the_width  = right - left + 1;
   }

   if (the_border > 1) {
      top        += 1;
      bottom     -= 1;
      the_height = bottom - top + 1;
   }

   // Set the virtual window to the inner bounds of the border
   define_window(left, top, right, bottom);

   // Make screen info available to field objects
   global_screen_info.left   = left;
   global_screen_info.top    = top;
   global_screen_info.right  = right;
   global_screen_info.bottom = bottom;
}


int screen::vborder_size() {
   if (the_border == 0)
      return 0;
   else if (the_border == 1)
      return 1;
   else
      return the_border % 2 == 0 ? 1 : 2;
}


int screen::hborder_size() {
   return the_border >= 2 ? 1 : 0;
}


void screen::wait_for_fkey() {
   int        the_key;
   Boolean    fkey_pressed = false;
   mod_field *fld = NULL;

   while (! fkey_pressed) {
      // Dim last field that was highlighted
      if (fld)
         fld->screen_put(false);

      // Highlight field if tab
      fld = mod_field_at_cursor();
      if (fld) {
         if (fld->attributes().tab)
            fld->screen_put(true);
      }

#ifdef DOS
      // Wait until a key is pressed
      while (! key_pressed(1)) {
         if (cancel_requested()) {
            cancel = true;
            break;
         }
      }

      // Identify key
      the_key = cancel ? KEY_CTRLC : get_key_pressed();
#endif
#ifdef WANG
      	the_key = get_key_pressed();
	if (cancel_requested())
	{
		cancel = true;
		the_key = KEY_CTRLC;
	}
#endif

      // Act on key
      if (the_key == KEY_CTRLC)
         break;
      if (convert_to_function_key(the_key, the_fkey))
         fkey_pressed = enabled_function_key(the_fkey);
      else
         handle_key(the_key);
   }
}


void screen::type_key(int a_key) {
   mod_field *fld = the_active_field;
   Boolean no_action = true;

   if (fld) {
      // First see if cursor in the active field
      if (! (y == fld->row() && x >= fld->first_col() && x <= fld->last_col())) {
         // See if cursor in any mod field
         fld = mod_field_at_cursor();
         if (! fld) {
            // Move cursor to closest mod field
            fld = mod_field_closest_to_cursor();
            if (fld) {
               move_cursor_to(fld);
               no_action = false;
            }
         }
      }

      if (fld->attributes().tab) {
         tab_magic(a_key, fld, no_action);
         return;
      }
      else {
         // Attempt to display the character and move cursor past it
         the_active_field = fld;
         if (the_active_field->put_char(a_key, x, in_insert_mode)) {
            handle_key(KEY_CURRT);
            return;
         }
      }
   }
   // Either no eligible mod field or a_key not allowed in the active field
   ring_bell();
}


void screen::tab_magic(int a_key, mod_field *a_field, Boolean first_action) {
   mod_field *start_field = a_field;

   if (first_action)
      a_field = a_field->next_field();
   if (! a_field)
      a_field = the_first_mod_field;

   while (a_field) {
      if (a_field->attributes().tab) {
         if (a_key == KEY_SPACE ||
             toupper(a_key) == toupper(a_field->first_letter()))
         {
            move_cursor_to(a_field);
            return;
         }
      }
      a_field = a_field->next_field();
      if (! a_field)
         a_field = the_first_mod_field;
      if (a_field == start_field)
         break;
   }
   ring_bell();
}


void screen::handle_key(int a_key) {
   Boolean move_cursor = false;
   Boolean bad_key     = false;

   switch (a_key) {

      case KEY_CTRLHOME :
         the_active_field = mod_field_at_cursor();
         if (the_active_field) {
            x = the_active_field->first_col();
            move_cursor = true;
         }
         else
            bad_key = true;
         break;

      case KEY_CTRLEND :
         the_active_field = mod_field_at_cursor();
         if (the_active_field) {
            x = the_active_field->last_nonblank_col();
            move_cursor = true;
         }
         else
            bad_key = true;
         break;

      case KEY_CURUP :
         if (restrict_option)
            handle_key(KEY_REVTAB);
         else {
            y = y == 1 ? the_height : y - 1;
            move_cursor = true;
         }
         break;

      case KEY_CURDN :
         if (restrict_option)
            handle_key(KEY_TAB);
         else {
            y = y == the_height ? 1 : y + 1;
            move_cursor = true;
         }
         break;

      case KEY_CURLF :
         if (restrict_option) {
            the_active_field = mod_field_at_cursor();
            if (the_active_field) {
               if (the_active_field->attributes().tab) {
                  handle_key(KEY_REVTAB);
                  break;
               }
            }
            x -= 1;
            the_active_field = mod_field_at_cursor();
            if (the_active_field)
               move_cursor = true;
            else {
               x += 1;
               handle_key(KEY_REVTAB);
               if (the_active_field) {
                  x = the_active_field->last_restricted_col();
                  move_cursor = true;
               }
            }
         }
         else {
            if (x == 1) {
               x = the_width;
               handle_key(KEY_CURUP);
            }
            else {
               x -= 1;
               move_cursor = true;
            }
         }
         break;

      case KEY_CURRT :
         if (restrict_option) {
            the_active_field = mod_field_at_cursor();
            if (the_active_field) {
               if (x < the_active_field->last_restricted_col()) {
                  if (! the_active_field->attributes().tab) {
                     x += 1;
                     move_cursor = true;
                     break;
                  }
               }
            }
            handle_key(KEY_TAB);
         }
         else {
            if (x == the_width) {
               x = 1;
               handle_key(KEY_CURDN);
            }
            else {
               x += 1;
               move_cursor = true;
            }
         }
         break;

      case KEY_HELP :
	 {
		screen_contents *saved_screen;
		saved_screen = new screen_contents(1, 1, SCREEN_WIDTH, SCREEN_HEIGHT);
	 	ws_help(1);
          	saved_screen->restore_screen();
          	delete saved_screen;
	 }
	 break;

      case KEY_HOME :
         the_active_field = the_first_mod_field;
         move_cursor_to(the_active_field);
         break;

      case KEY_END :
         the_active_field = the_last_mod_field;
         move_cursor_to(the_active_field);
         break;

      case KEY_TAB :
         the_active_field = mod_field_at_cursor();
         if (the_active_field)
            the_active_field = the_active_field->next_field();
         else
            the_active_field = mod_field_closest_to_cursor();
         if (! the_active_field)
            the_active_field = the_first_mod_field;
         move_cursor_to(the_active_field);
         break;

      case KEY_REVTAB :
         the_active_field = mod_field_at_cursor();
         if (the_active_field)
            the_active_field = the_active_field->prev_field();
         else {
            the_active_field = mod_field_closest_to_cursor();
            if (the_active_field)
               the_active_field = the_active_field->prev_field();
            else
               the_active_field = the_last_mod_field;
         }
         if (! the_active_field)
            the_active_field = the_last_mod_field;
         move_cursor_to(the_active_field);
         break;

      case KEY_BACKSPACE :
         x -= 1;
         the_active_field = mod_field_at_cursor();
         if (the_active_field) {
            goto_xy(x, y);
            bad_key = BOOLEAN(! the_active_field->delete_char_at(x));
         }
         else {
            x += 1;
            bad_key = true;
         }
         break;

      case KEY_DELETE :
         the_active_field = mod_field_at_cursor();
         if (the_active_field)
            bad_key = BOOLEAN(! the_active_field->delete_char_at(x));
         else
            bad_key = true;
         break;

      case KEY_AF10 :
         // This is the erase key on a Wang keyboard
         the_active_field = mod_field_at_cursor();
         if (the_active_field)
            bad_key = BOOLEAN(! the_active_field->erase_from(x));
         else
            bad_key = true;
         break;

      case KEY_INSERT :
         in_insert_mode = BOOLEAN(! in_insert_mode);
         set_cursor_shape(
            in_insert_mode ? CURSOR_SHAPE_INSERT : CURSOR_SHAPE_NORMAL);
         break;

      case KEY_MOUSE_CLICK :
         bad_key = true;
	 break;

      default :
         type_key(a_key);
         break;
   }
   if (bad_key)
      ring_bell();
   else if (move_cursor)
      goto_xy(x, y);
}


void screen::move_cursor_to(mod_field *a_field) {
   if (a_field) {
      x = a_field->first_col();
      y = a_field->row();
      goto_xy(x, y);
   }
   else
      ring_bell();
}


Boolean screen::assign_fields(machine *a_machine) {
   if (the_first_mod_field && ! escape_function_key(the_fkey))
      return the_first_mod_field->assign(a_machine);
   else
      return true;
}


void screen::save_video_environment() {
   if (restore_option) {
      saved_screen_contents = new screen_contents
         (the_left_col, the_top_row, the_right_col, the_bottom_row);
   }

   struct crt_info crt;
   get_crt_info(&crt);
   saved_screen_info.left         = crt.left;
   saved_screen_info.top          = crt.top;
   saved_screen_info.right        = crt.right;
   saved_screen_info.bottom       = crt.bottom;
   saved_screen_info.cursor_shape = cursor_shape();
}

void screen::restore_video_environment() {
   define_window(saved_screen_info.left, saved_screen_info.top,
      saved_screen_info.right, saved_screen_info.bottom);
   set_cursor_shape(saved_screen_info.cursor_shape);
   normal_video();
   if (restore_option) {
      assert(saved_screen_contents);
      saved_screen_contents->restore_screen();
      delete saved_screen_contents;
      saved_screen_contents = NULL;
   }
   else
   // goto_xy(1, saved_screen_info.bottom - 1); // how it was before UNIX
      goto_xy(1, saved_screen_info.bottom);
}


Boolean screen::convert_to_function_key(int a_key, int &an_fkey) {
   switch (a_key) {
      case KEY_F11  : a_key = KEY_AF1; break;
      case KEY_F12  : a_key = KEY_AF2; break;
      case KEY_SF11 : a_key = KEY_CF1; break;
      case KEY_SF12 : a_key = KEY_CF2; break;
      // Do nothing if not one of the above
   }

   switch (a_key) {
      case KEY_RETURN :
      case KEY_ESCAPE :
      case KEY_F1     :
      case KEY_F2     :
      case KEY_F3     :
      case KEY_F4     :
      case KEY_F5     :
      case KEY_F6     :
      case KEY_F7     :
      case KEY_F8     :
      case KEY_F9     :
      case KEY_F10    :
      case KEY_SF1    :
      case KEY_SF2    :
      case KEY_SF3    :
      case KEY_SF4    :
      case KEY_SF5    :
      case KEY_SF6    :
      case KEY_SF7    :
      case KEY_SF8    :
      case KEY_SF9    :
      case KEY_SF10   :
      case KEY_CF1    :
      case KEY_CF2    :
      case KEY_CF3    :
      case KEY_CF4    :
      case KEY_CF5    :
      case KEY_CF6    :
      case KEY_AF1    :
      case KEY_AF2    :
      case KEY_AF3    :
      case KEY_AF4    :
      case KEY_AF5    :
      case KEY_AF6    :
      {
         for (int k = 0; k <= last_fkey; k++)
            if (a_key == key_map[k].code) {
               an_fkey = k;
               return true;
            }
      }
   }
   return false;
}


Boolean screen::enabled_function_key(int an_fkey) {
   assert(an_fkey >= 0 && an_fkey <= last_fkey);
   if (key_map[an_fkey].enabled)
      return true;
   else {
      ring_bell();
      return false;
   }
}


Boolean screen::escape_function_key(int an_fkey) {
   assert(an_fkey >= 0 && an_fkey <= last_fkey);
   return (key_map[an_fkey].escape)? true:false;
}


void screen::disable_all_fkeys() {
   for (int k = 0; k <= last_fkey; k++)
      key_map[k].enabled = false;
}


void screen::enable_fkey(int an_fkey) {
   if (an_fkey >= 0 && an_fkey <= last_fkey)
      key_map[an_fkey].enabled = true;
}


void screen::set_escape_fkey(int an_fkey) {
   if (an_fkey >= 0 && an_fkey <= last_fkey)
      key_map[an_fkey].escape = true;
}


void screen::set_corners(int lc, int tr, int rc, int br) {
   the_left_col   = lc;
   the_right_col  = rc;
   the_top_row    = tr;
   the_bottom_row = br;
}


void screen::set_title(const char *a_title) {
   the_title = dup_string(a_title);
}


void screen::init_fkey_map() {
   // Based on Wang Local Office Connection User's Guide key maps
   key_map[0].code  = KEY_RETURN;
   key_map[1].code  = KEY_F1;
   key_map[2].code  = KEY_F2;
   key_map[3].code  = KEY_F3;
   key_map[4].code  = KEY_F4;
   key_map[5].code  = KEY_F5;
   key_map[6].code  = KEY_F6;
   key_map[7].code  = KEY_F7;
   key_map[8].code  = KEY_F8;
   key_map[9].code  = KEY_F9;
   key_map[10].code = KEY_F10;
   key_map[11].code = KEY_AF1;
   key_map[12].code = KEY_AF2;
   key_map[13].code = KEY_AF3;
   key_map[14].code = KEY_AF4;
   key_map[15].code = KEY_AF5;
   key_map[16].code = KEY_AF6;
   key_map[17].code = KEY_SF1;
   key_map[18].code = KEY_SF2;
   key_map[19].code = KEY_SF3;
   key_map[20].code = KEY_SF4;
   key_map[21].code = KEY_SF5;
   key_map[22].code = KEY_SF6;
   key_map[23].code = KEY_SF7;
   key_map[24].code = KEY_SF8;
   key_map[25].code = KEY_SF9;
   key_map[26].code = KEY_SF10;
   key_map[27].code = KEY_CF1;
   key_map[28].code = KEY_CF2;
   key_map[29].code = KEY_CF3;
   key_map[30].code = KEY_CF4;
   key_map[31].code = KEY_CF5;
   key_map[32].code = KEY_CF6;
   key_map[33].code = KEY_ESCAPE;

   for (int k = 0; k <= last_fkey; k++) {
      key_map[k].enabled = true;
      key_map[k].escape  = false;
   }
}
