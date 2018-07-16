//
//	Copyright (c) 1996-1998 NeoMedia Technologies Inc. All rights reserved.
//
//	Project:	WPROC
//	Id:		$Id:$
//	RCS:		$Source:$
//	
//// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : tracer.hpp
// Author : George Soules
// Date   : 20 March 1991

#ifndef TRACER__HPP
#define TRACER__HPP

// Classes
#include "exp.hpp"
#include "object.hpp"
#include "reader.hpp"
#include "screen.hpp"

// Definitions and subprograms
#include "environ.hpp"

const int max_vars = 100;
const int max_display_vars = 7;  // lines in data window


class scroll_region : public object {
   public:
      scroll_region(int lines);
      ~scroll_region();
      void clear_region();
      void put_line(char *text);
      void highlight_line(int line);
   private:
      int    max_lines;
      int    line_index;
      int    highlighted_line;
      char **buffer;
};

void tracer_prepare_for_link();

class tracer : public object {
   public:
      tracer(const char *source_file);
      void trace_interactive();
      int  trace_into(const char *into_file, int resources, int scratch, int statements, int variables);
      void check_restart();
      ~tracer();

      void    display_variable(expression &a_variable);
      Boolean permission_to_execute(usign_16 a_source_row);
      void    log_return_code(int_32 rc);

   private:
      struct var_info {
         symbol *the_symbol;
         int     the_array_index;
      };

      file_reader    *the_reader;
      int             var_index;
      int             var_count;
      var_info        vars[max_vars];

      int              first_displayed_row;
      int              last_displayed_row;
      char            *the_source_name;
      Boolean          source_modified;
      screen_contents *the_source_window;
      screen_contents *the_data_window;
      screen_contents *the_user_screen;
      scroll_region   *scrolling_data;
      scroll_region   *scrolling_source;

      void  display_header(char *text);
      void  display_footer();
      void  log_start();
      void  log_source(int row = 0);
      void  log_data(char *text = NULL);
      void  display_source(int row = 0);
      void  display_data(char *text = NULL);
      void  show_trace_windows(char *source_name);
      void  hide_trace_windows();
      void  delete_trace_windows();
      void  display_vars();
      void  log_vars();
      char *symbol_data(symbol &a_symbol, int array_index);
};


#endif


//
//	History:
//	$Log: tracer.hpp,v $
//	Revision 1.5  1998-08-31 15:14:24-04  gsl
//	drcs update
//
//

//	
//	RCS file: /disk1/neomedia/RCS/wisp/wproc/tracer.hpp,v
//	Working file: tracer.hpp
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
//	date: 1995-04-25 06:00:31-04;  author: gsl;  state: V4_3_00;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:52:46-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:33:32-05;  author: gsl;  state: V3_3x12;  lines: +13 -4
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:32-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
