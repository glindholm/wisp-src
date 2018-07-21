//
//	Copyright (c) 1996-1998 NeoMedia Technologies Inc. All rights reserved.
//
//	Project:	WPROC
//	
//// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : screen.hpp
// Author : George Soules
// Date   : 24 May 1991

#ifndef SCREEN__HPP
#define SCREEN__HPP

// Classes
#include "exp.hpp"
class machine;
#include "object.hpp"
#include "symbols.hpp"
#include "crt_io.hpp"

// Definitions and subprograms
#include "colors.hpp"


class field_attributes {
   public:
      Boolean alphanumeric : 1;
      Boolean alpha        : 1;
      Boolean blank        : 1;
      Boolean blink        : 1;
      Boolean bright       : 1;
      Boolean digits       : 1;
      Boolean dim          : 1;
      Boolean first        : 1;
      Boolean hex          : 1;
      Boolean line         : 1;
      Boolean lower        : 1;
      Boolean modifiable   : 1;
      Boolean numeric      : 1;
      Boolean reverse      : 1;
      Boolean tab          : 1;
      Boolean uplow        : 1;
      Boolean upper        : 1;
      int     spare        : 15;
};


class screen_info {
   public:
      screen_info();
      usign_8  left;
      usign_8  top;
      usign_8  right;
      usign_8  bottom;
      usign_16 cursor_shape;
};


class screen_contents {
   public:
      screen_contents(
         int left_col   =  1,
         int top_row    =  1,
         int right_col  = SCREEN_WIDTH,
         int bottom_row = SCREEN_HEIGHT);

      ~screen_contents();

      Boolean ok() {return the_ok;}
      void    restore_screen();

   private:
      usign_8  left;
      usign_8  top;
      usign_8  right;
      usign_8  bottom;
      usign_8  cursor_x;
      usign_8  cursor_y;
      Boolean  the_ok;
      usign_8 *the_contents;
      w4w_handler  saved_w4w_handler;	// Save the w4w handler because it has a map of the hotspots
};


extern field_attributes default_field_attributes;
extern colors           default_colors;


class field : public object {
   public:
      field(
         int              a_row,
         const char      *text,
         field_attributes attributes,
         Boolean          center_option,
         colors           coloring);

      virtual ~field();
      void    display();
      void    screen_put(Boolean highlight = false);
      int     row()                 {return the_row;}
      int     first_col()           {return the_first_col;}
      int     last_col()            {return the_last_col;}
      int     last_restricted_col()
                 {return is.tab ? the_first_col : the_last_col;}
      int     last_nonblank_col();
      void    append(field *a_field);
      field  *next_field()          {return next;}
      void    increment_rows_by(int a_row);
      void    set_modchar(char c);
      char    first_letter();
      field_attributes attributes() {return is;}
      void set_attributes(
         field_attributes attributes,
         colors coloring = default_colors);

   protected:
      void    center();
      char   *text()       {return the_text;}

      field_attributes is;
      Boolean needs_centering;
      usign_8 the_video_attribute;
      char    the_modchar;
      int     the_first_col;
      int     the_last_col;
      int     the_row;
      int     the_text_size;
      char   *the_text;
      field  *next;
};


class mod_field : public field {
   public:
      mod_field(
         int              a_row,
         expression      *a_variable,
         field_attributes attributes,
         Boolean          center_option,
         colors           coloring);

      mod_field(
         int              a_row,
         expression      *a_variable,
         const char      *text,
         field_attributes attributes,
         Boolean          center_option,
         colors           coloring);

      mod_field(
         int              a_row,
         const char      *text,
         field_attributes attributes,
         Boolean          center_option,
         colors           coloring);

      ~mod_field();

      void        append(mod_field *a_field);
      mod_field  *next_field() {return next;}
      mod_field  *prev_field() {return prev;}
      mod_field  *initial_cursor_field();
      Boolean     put_char(int c, int a_col, Boolean insert);
      Boolean     delete_char_at(int a_col);
      Boolean     erase_from(int a_col);
      Boolean     assign(machine *a_machine);
      const char* text() {return the_text;}

   private:
      expression *the_exp;
      mod_field  *next;
      mod_field  *prev;
};


const int last_fkey = 33;

class screen : public object {
   public:
      screen();
      ~screen();
      void        add_field(field *a_field);
      void        add_mod_field(mod_field *a_field);
      void        display(Boolean read);
      Boolean     assign_fields(machine *a_machine);
      Boolean     cancelled()                  {return cancel;}
      int         fkey()                       {return the_fkey;}
      int         curcol()                     {return x;}
      int         currow()                     {return y;}
      int         border()                     {return the_border;}
      int         vborder_size();
      int         hborder_size();
      colors      screen_colors()              {return the_screen_colors;}
      void        set_rows(int a_row)          {rows = a_row;}
      void        set_alarm(Boolean an_option) {alarm_option = an_option;}
      void        set_erase(Boolean an_option) {erase_option = an_option;}
      void        set_first_row(int a_row)     {the_first_row = a_row;}
      void        set_border(int a_border)     {the_border = a_border;}
      void        set_modchar(char c)          {the_modchar = c;}
      void        set_cursor(int a_col, int a_row)
                     {x = a_col; y = a_row;}
      void        set_restore(Boolean an_option)
                     {restore_option = an_option;}
      void        set_restrict(Boolean an_option)
                     {restrict_option = an_option;}
      void        set_screen_colors(colors coloring)
                     {the_screen_colors = coloring;}
      void        set_border_colors(colors coloring)
                     {the_border_colors = coloring;}
      void        set_title_colors(colors coloring)
                     {the_title_colors = coloring;}
      void        disable_all_fkeys();
      void        enable_fkey(int an_fkey);
      void        set_escape_fkey(int an_fkey);
      void        set_title(const char *a_title);
      void        set_corners(int lc, int tr, int rc, int br);

   private:
      void        display_border();
      void        wait_for_fkey();
      mod_field  *mod_field_closest_to_cursor();
      mod_field  *mod_field_at_cursor();
      Boolean     handle_key(int a_key);	// Returns true if a function key was pressed
      void        type_key(int a_key);
      void        tab_magic(int a_key, mod_field *a_field, Boolean first_action);
      void        move_cursor_to(mod_field *a_field);
      void        save_video_environment();
      void        restore_video_environment();
      Boolean     convert_to_function_key(int a_key, int &an_fkey);
      Boolean     enabled_function_key(int an_fkey);
      Boolean     escape_function_key(int an_fkey);
      void        init_fkey_map();

      int         x;
      int         y;
      int         the_fkey;
      char        the_modchar;
      Boolean     cancel;
      Boolean     alarm_option;
      Boolean     erase_option;
      Boolean     restore_option;
      Boolean     restrict_option;
      colors      the_screen_colors;
      colors      the_border_colors;
      colors      the_title_colors;
      int         the_first_row;
      field      *the_first_field;
      field      *the_last_field;
      mod_field  *the_first_mod_field;
      mod_field  *the_last_mod_field;
      mod_field  *the_active_field;
      int         rows;
      int         the_border;
      char       *the_title;
      int         the_init_col;
      int         the_init_row;
      int         the_left_col;
      int         the_right_col;
      int         the_top_row;
      int         the_bottom_row;
      int         the_height;
      int         the_width;
      screen_info saved_screen_info;
      screen_contents *saved_screen_contents;

      struct fkey_info {
         int_16  code;
         Boolean enabled : 1;
         Boolean escape  : 1;
      };

      fkey_info key_map[1 + last_fkey];
};

#endif

//
//	History:
//	$Log: screen.hpp,v $
//	Revision 1.6  1998/10/02 19:26:12  gsl
//	Add a saved_w4w_handler to the screen_contents class.
//	Change screen::handle_key() to return true if a function key was press.
//	This is needed because a mouse click (key) can simulate a pfkey.
//	
//	Revision 1.5  1998-08-31 15:14:15-04  gsl
//	drcs update
//
//

//	
//	RCS file: /disk1/neomedia/RCS/wisp/wproc/screen.hpp,v
//	Working file: screen.hpp
//	head: 1.4
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
//	total revisions: 4;	selected revisions: 4
//	description:
//	----------------------------
//	revision 1.4
//	date: 1995-04-25 06:00:23-04;  author: gsl;  state: V4_3_00;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:52:38-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:33:23-05;  author: gsl;  state: V3_3x12;  lines: +48 -48
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:26-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
