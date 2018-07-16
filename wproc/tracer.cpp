/* 
	Copyright (c) 1995 DevTech Migrations, All rights reserved.
	$Id:$
*/

// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : tracer.cpp
// Author : George Soules
// Date   : 20 March 1991


#include <sys/types.h>
#include <time.h>

// Specification
#include "tracer.hpp"

// Classes
#include "process.hpp"
#include "symbols.hpp"

// Definitions and subprograms
#include "crt_io.hpp"
#include "debugaid.hpp"
#include "keyboard.hpp"
#include "memory.hpp"
#include "product.hpp"
#include "utility.hpp"
#include "fileinfo.hpp"
#include "wangfile.hpp"

#include "envs.h"

Boolean continue_requested = false;

static int trace_resources = 0;
static int trace_all_statements = 1;
static int trace_variables = 0;
static int scratch_into_file = 0;
static int interactive_trace = 0;

static FILE *log_file = NULL;
static char *log_filename = NULL;

const int footer_bottom = SCREEN_HEIGHT;
const int footer_top    = footer_bottom - 1;
const int data_bottom   = footer_top - 1;
const int data_top      = 16;
const int data_lines    = data_bottom - data_top;
const int source_bottom = 15;
const int source_top    = 3;
const int source_lines  = source_bottom - source_top;
const int header_bottom = 2;
const int header_top    = 1;


int fkey() {
   int the_key;
   while (true) {
      // Wait for a valid function key to be pressed
      the_key = get_key_pressed();
      if (the_key == KEY_F1 || the_key == KEY_F6 || the_key == KEY_F8 ||
          the_key == KEY_F9 || the_key == KEY_F10)
      {
         return the_key;
      }
      ring_bell();
   }
   return 0; /* NEVER WILL OCCUR */
}


scroll_region::scroll_region(int lines) {
   // Note: the scrolling region begins one line below the currently defined
   // window (the first window line is assumed to be a title line).  The
   // scroll lines begin with 1, but map to a 0 based buffer array.  Thus,
   // add 1 to a region line to get its window line, subtract 1 to get its
   // buffer pointer.

   max_lines = lines;
   buffer = new char*[max_lines];
   for (int i = 0; i < max_lines; i++)
      buffer[i] = new_string(SCREEN_WIDTH + 1);
   line_index = 0;
   highlighted_line = 0;
}


scroll_region::~scroll_region() {
   for (int i = 0; i < max_lines; i++)
      delete_string(buffer[i]);
   delete buffer;
}


void scroll_region::clear_region() {
   for (int i = 0; i < max_lines; i++) {
      goto_xy(1, i + 2);
      clear_to_eol();
   }
   line_index = 0;
   highlighted_line = 0;
}


void scroll_region::put_line(char *text) {
  int i;
   if (text) {
      if (strlen(text) > SCREEN_WIDTH)
         text[SCREEN_WIDTH] = '\0';
      line_index += 1;
      if (line_index > max_lines) {
         // Scroll everything up one line
         char *temp = buffer[0];
         for (i = 0; i < max_lines - 1; i++) {
            buffer[i] = buffer[i + 1];
            goto_xy(1, i + 2);
            clear_to_eol();
            put_text(buffer[i]);
         }
         buffer[i] = temp;
         line_index = max_lines;
      }
      strcpy(buffer[line_index - 1], text);
      goto_xy(1, line_index + 1);
      clear_to_eol();
      put_text(text);
   }
}


void scroll_region::highlight_line(int line) {
   if (highlighted_line) {
      // Un-highlight
      goto_xy(1, highlighted_line + 1);
      clear_to_eol();
      put_text(buffer[highlighted_line - 1]);
   }

   char s[SCREEN_WIDTH + 1];
   init_string_with_blank_pad(s, SCREEN_WIDTH, buffer[line - 1]);
   text_foreground(color_black);
   text_background(color_white);
   goto_xy(1, line + 1);
   put_text(s);
   goto_xy(1, line + 1);
   normal_video();
   highlighted_line = line;
}

tracer::tracer(const char *source_file) {
   trace_begin(object, "tracer");
   the_source_name     = dup_string(source_file);
   the_source_window   = NULL;
   the_data_window     = NULL;
   the_user_screen     = NULL;
   first_displayed_row = 0;
   last_displayed_row  = 0;
   var_count           = 0;
   var_index           = -1;
   the_reader          = new file_reader(source_file, false);
   source_modified     = BOOLEAN(the_process->the_source_timestamp != the_reader->timestamp());

   if (!the_process->trace_active)
   {
	// If trace_active then the trace_level will be set externally via an option of other link-level
	the_process->trace_active = true;
	the_process->trace_level = the_process->base_level + the_process->nesting_level;
   }

   if (0 == the_process->trace_level)
   {
	interactive_trace = 1;
   }

   scrolling_source    = new scroll_region(source_lines);
   scrolling_data      = new scroll_region(data_lines);

   log_start();

   trace_end(object);
}

void tracer::trace_interactive()
{
	// Once you turn on interactive tracing it stays on until the tracer 
	// that started it is deleted.

	interactive_trace = 1;
}

extern "C" char *wfname(int_32 *mode, const char *vol, const char *lib, const char *file, char *native);
extern "C" int makepath(const char *filepath);

int tracer::trace_into(const char *coded_into_file, 
			int resources, int scratch, int statements, int variables)
{
	char	into_file[256];
	char	*ptr;
	int	rc = 0;

	strcpy(into_file,"TRACELOG");

	if (coded_into_file)
	{
		wang_filename name(coded_into_file);
		int_32	mode = 3;

		ptr = wfname(&mode, name.volname, name.libname, name.filename, into_file);
		*ptr = (char) 0;
	}
	else if (!log_file)
	{
		int_32	mode = 3;

		ptr = wfname(&mode, "      ", "        ", the_process->the_top_filename, into_file);
		*ptr = (char) 0;
	}

	/*
	**	Possible values are
	**		0	default setting
	**		1	other setting
	**		-1	don't change
	*/
	if (resources >= 0) 
		trace_resources = resources;
	if (scratch >= 0) 
		scratch_into_file = scratch;
	if (statements >= 0) 
		trace_all_statements = !statements;
	if (variables >= 0) 
		trace_variables = variables;

	if (log_file && !coded_into_file)
	{
		rc = 4;
	}

	if (log_file && coded_into_file)
	{
		fclose(log_file);
		log_file = NULL;
		delete_string(log_filename);
	}

	if (!log_file)
	{
		makepath(into_file);

		if (scratch_into_file)
		{
			log_file = fopen(into_file,"w");
		}
		else
		{
			log_file = fopen(into_file,"a");
		}

		log_filename = dup_string(into_file);
	}

	log_start();

	if (!log_file) rc = 20;

	return( rc );
}

void tracer::check_restart()
{
	// This is only called the first time thru a run-unit and only if trace_active
	char	*ptr;

	if ((ptr = getenv(WPROC_TRACEFLAGS_ENV)) && *ptr)
	{
		if ('Y' == ptr[0])	interactive_trace = 1;
		if ('Y' == ptr[1])	continue_requested = true;
		if ('Y' == ptr[2])	trace_resources = 1;
		if ('Y' == ptr[3])	trace_all_statements = 1;
		if ('Y' == ptr[4])	trace_variables = 1;
	}

	if ((ptr = getenv(WPROC_TRACEFILE_ENV)) && *ptr)
	{
		if (*ptr)
		{
			log_filename = dup_string(ptr);
			log_file = fopen(log_filename,"a");
			log_start();
		}
	}
}

void tracer_prepare_for_link()
{
	char	temp[265];

	if (log_file)
	{
		sprintf(temp,"%s=%s",WPROC_TRACEFILE_ENV,log_filename);
		putenv(dup_string(temp));
	}

	sprintf(temp,"%s=%c%c%c%c%c",WPROC_TRACEFLAGS_ENV,
		(interactive_trace)	? 'Y' : 'N',
		(continue_requested)	? 'Y' : 'N',
		(trace_resources)	? 'Y' : 'N',
		(trace_all_statements)	? 'Y' : 'N',
		(trace_variables)	? 'Y' : 'N');
	putenv(dup_string(temp));
}

tracer::~tracer() {
   char buffer[256];
   sprintf(buffer,"End tracing %s",the_source_name);
   log_data(buffer);

   if (the_process->base_level + the_process->nesting_level == the_process->trace_level) 
   {
	if (log_file)
	{
		fclose(log_file);
		log_file = NULL;
		delete_string(log_filename);
	}

	trace_resources = 0;
	trace_all_statements = 1;
	trace_variables = 0;
	scratch_into_file = 0;
	interactive_trace = 0;
	continue_requested = false;

	the_process->trace_active = false;
	the_process->trace_level = 0;
   }

   delete the_source_name;
   delete the_reader;
   delete_trace_windows();
   delete scrolling_data;
   delete scrolling_source;
   trace(object, "~tracer");
}


void tracer::display_header(char *text) {
   define_window(1, header_top, SCREEN_WIDTH, header_bottom);

   clear_screen();
   put_text(product_copyright());
   goto_xy(1, 2);
   if (the_reader->ok()) {
      put_text("Procedure ");
      if (source_modified) {
         text_foreground(color_lt_white);
         put_text("(modified) ");
         text_foreground(color_white);
      }
   }
   else {
      text_foreground(color_lt_white);
      put_text("Could not open procedure ");
   }
   put_text(text);
   text_foreground(color_white);
}


void tracer::display_footer() {
   define_window(1, footer_top, SCREEN_WIDTH, footer_bottom);

   clear_screen();
#if DOS
   put_text("ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ");
#else
   put_text("--------------------------------------------------------------------------------");
#endif
   goto_xy(1, 2);
   put_text("(1) Step  (6) User screen  (8) Continue  (9) Return  (10) Quit");
}

void tracer::log_start()
{
	char buffer[256];
	sprintf(buffer,"Tracing procedure %s",the_source_name);
	log_data(buffer);

	if (source_modified)
	{
		log_data("**** SOURCE HAS BEEN MODIFIED ****\n");
		log_data("**** UNABLE TO TRACE          ****\n");
	}
}

void tracer::log_source(int row)
{
	if (row && the_reader->ok() && log_file && !source_modified)
	{
		int	the_level;
		char	*time_stamp;
		char 	*the_text;
		time_t	the_time;

		the_level = the_process->nesting_level + the_process->base_level;
		the_time = time(NULL);
		time_stamp = ctime(&the_time);
		the_text = the_reader->read_absolute_record(row);
		fprintf(log_file,"%-2d  %8.8s  %06d %.59s\n", 
			the_level, &time_stamp[11], row, the_text);
		fflush(log_file);
		delete the_text;
	}
}

void tracer::log_data(char *text)
{
	if (log_file)
	{
		int	the_level;
		the_level = the_process->nesting_level + the_process->base_level;
		fprintf(log_file,"%-2d\t\t\t\t\t\t\t\t\t\t %.51s\n", the_level, text);
		fflush(log_file);
	}
}

void tracer::display_source(int row) {
   const int first_row = 2;
   const int last_row  = source_bottom - source_top + 1;

   if (continue_requested) return;
   if (!interactive_trace) return;

   define_window(1, source_top, SCREEN_WIDTH, source_bottom);

   if (the_source_window)
      the_source_window->restore_screen();
   else {
      clear_screen();
#if DOS
      put_text("ÍÍ Line ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ");
#else
      put_text("== Line ========================================================================");
#endif
      goto_xy(1, 1);
   }
   if (the_source_window)
      delete the_source_window;

   if (row && the_reader->ok()) {
      char s[INT_32_STRING_SIZE];
      int_32_to_ascii(row, s);
      goto_xy(9, 1);
#if DOS
      put_text("ÍÍÍÍÍÍ");
#else
      put_text("======");
#endif
      goto_xy(9, 1);
      put_text(s);
      put_text(" ");

      if (row >= first_displayed_row && row <= last_displayed_row) {
         int window_row = row - first_displayed_row + 2;
         goto_xy(1, window_row);
      }
      else {
         int row_offset = row == 1 ? 0 : 1;
         row -= row_offset;
         first_displayed_row = row;
         scrolling_source->clear_region();
         for (int i = first_row; i <= last_row; i++) {
            char *the_text = the_reader->read_absolute_record(row++);
            scrolling_source->put_line(the_text);
            delete the_text;
         }
         last_displayed_row = row - 1;
         goto_xy(1, first_row + row_offset);
      }
      scrolling_source->highlight_line(where_y() - 1);
   }

   the_source_window =
      new screen_contents(1, source_top, SCREEN_WIDTH, source_bottom);
}


void tracer::display_data(char *text) {
   define_window(1, data_top, SCREEN_WIDTH, data_bottom);

   if (the_data_window) {
      the_data_window->restore_screen();
      scrolling_data->put_line(text);
   }
   else {
      clear_screen();
#if DOS
      put_text("ÍÍ Data ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ");
#else
      put_text("== Data ========================================================================");
#endif
      goto_xy(1, 1);
   }
   if (the_data_window)
      delete the_data_window;
   the_data_window =
      new screen_contents(1, data_top, SCREEN_WIDTH, data_bottom);
}


void tracer::show_trace_windows(char *source_name) {
   if (continue_requested) return;
   if (!interactive_trace) return;

   assert(! the_user_screen);
   the_user_screen = new screen_contents();
   text_foreground(color_white);
   display_header(source_name);
   display_footer();
   display_data();
   display_source();
}


void tracer::hide_trace_windows() {
   define_window(1, 1, SCREEN_WIDTH, SCREEN_HEIGHT);
   the_user_screen->restore_screen();
   assert(the_user_screen);
   delete the_user_screen;
   the_user_screen = NULL;
}


void tracer::delete_trace_windows() {
   if (the_user_screen) {
      delete the_user_screen;
      the_user_screen = NULL;
   }
   if (the_source_window) {
      delete the_source_window;
      the_source_window = NULL;
   }
   if (the_data_window) {
      delete the_data_window;
      the_data_window = NULL;
   }
}

/*
**	permission_to_execute
**
**		This routine is called before each statement.
*/
Boolean tracer::permission_to_execute(usign_16 a_source_row) {

   if (log_file)
   {
      log_vars();
      log_source(a_source_row);
   }

   if (interactive_trace && !continue_requested)
   {
      show_trace_windows(the_source_name);
      display_vars();
      display_source(a_source_row);
   }

   var_count = 0;

   if (continue_requested) return true;
   if (!interactive_trace) return true;

   int the_key = fkey();
   while (the_key == KEY_F6) {
      hide_trace_windows();

      // Wait until any key is hit, then discard key
      /* while (! key_pressed()); */
      the_key = get_key_pressed();

      show_trace_windows(the_source_name);
      the_key = fkey();
   }
   hide_trace_windows();

   switch (the_key) {
      case KEY_F1 :
         return true;
      case KEY_F8 :
         continue_requested = true;
         return true;
      case KEY_F9 :
         return false;
      case KEY_F10 :
         restore_video_state();
         exit(0);
   }
   assert(UNREACHABLE);
   return false;
}

void tracer::log_return_code(int_32 rc)
{
	char	buffer[80];

	sprintf(buffer,"Return Code = %ld",(long)rc);
	log_data(buffer);
}


void tracer::display_variable(expression &a_variable) {

   if (!((log_file && trace_variables) ||
         (interactive_trace && !continue_requested)))
	return;

   if (var_count < max_vars) {
      lvalue &the_lvalue = a_variable.lvalue_ref();
      symbol *new_symbol = the_lvalue.symbol_address();

      for (int i = 0; i < var_count; i++) {
         // Ignore symbols already seen
         if (new_symbol->is.builtin || (new_symbol == vars[i].the_symbol &&
             the_lvalue.array_index() == vars[i].the_array_index))
         {
            return;
         }
      }
      vars[var_count].the_symbol      = new_symbol;
      vars[var_count].the_array_index = the_lvalue.array_index();
      var_count += 1;
   }
}


void tracer::display_vars() {
   const int max_data = SCREEN_WIDTH;
   char buffer[max_data + 1];

   if (var_count)
      display_data(" ");

   for (int i = 0; (i < var_count) && (i < max_display_vars); i++) {
      symbol &the_symbol = *(vars[i].the_symbol);
      if (the_symbol.is.builtin)
         continue;

      strcpy(buffer, the_symbol.name());

      if (the_symbol.is.array) {
         strcat(buffer, "(");
         char s[INT_32_STRING_SIZE];
         int_32_to_ascii(vars[i].the_array_index, s);
         strcat(buffer, s);
         strcat(buffer, ")");
      }

      strcat(buffer, " = ");

      if (the_symbol.is.string)
         strcat(buffer, "\"");

      char *the_data = symbol_data(the_symbol, vars[i].the_array_index);
      int data_len    = strlen(the_data);
      int buffer_left = max_data - strlen(buffer);
      if (data_len > buffer_left) {
         the_data[buffer_left] = '\0';
      }
      buffer_left -= data_len;
      strcat(buffer, the_data);

      if (the_symbol.is.string && buffer_left)
         strcat(buffer, "\"");

      display_data(buffer);

      delete_string(the_data);
   }
}

void tracer::log_vars() {
   char buffer[256];

   if (!trace_variables) return;

   for (int i = 0; i < var_count; i++) {
      symbol &the_symbol = *(vars[i].the_symbol);
      if (the_symbol.is.builtin)
         continue;

      strcpy(buffer, the_symbol.name());

      if (the_symbol.is.array) {
         strcat(buffer, "(");
         char s[INT_32_STRING_SIZE];
         int_32_to_ascii(vars[i].the_array_index, s);
         strcat(buffer, s);
         strcat(buffer, ")");
      }

      strcat(buffer, " = ");

      if (the_symbol.is.string)
         strcat(buffer, "\"");

      char *the_data = symbol_data(the_symbol, vars[i].the_array_index);
      int data_len    = strlen(the_data);
      int buffer_left = sizeof(buffer) - strlen(buffer) - 2;
      if (data_len > buffer_left) {
         the_data[buffer_left] = '\0';
      }
      strcat(buffer, the_data);

      if (the_symbol.is.string)
         strcat(buffer, "\"");

      log_data(buffer);

      delete_string(the_data);
   }
}

char *tracer::symbol_data(symbol &a_symbol, int array_index) {
   symbol_attributes is = a_symbol.is;
   char *the_string;

   if (is.integer) {
      int_32 i;
      if (is.array) {
         integer_array_data &the_data =
            (integer_array_data&) a_symbol.data_ref();
         i = the_data[array_index - 1];
      }
      else {
         integer_data &the_data = (integer_data&) a_symbol.data_ref();
         i = the_data.contents();
      }
      the_string = new char[INT_32_STRING_SIZE];
      int_32_to_ascii(i, the_string);
   }
   else {
      if (is.array) {
         string_array_data &the_data = (string_array_data&) a_symbol.data_ref();
         the_string = the_data[array_index - 1].contents();
      }
      else {
         string_data &the_data = (string_data&) a_symbol.data_ref();
         the_string = the_data.contents();
      }
   }
   return the_string;
}

/*
**	History:
**	$Log: tracer.cpp,v $
**	Revision 1.8  1996-07-25 19:48:17-04  gsl
**	NT
**
**	Revision 1.7  1996-07-25 11:16:32-07  gsl
**	Renamed from tracer.cc to tracer.cpp
**
**	Revision 1.6  1996-04-18 10:00:57-07  jockc
**	moved declaration of for loop index from for loop to function
**	auto decl area
**
// Revision 1.5  1995/10/12  12:18:12  gsl
// Moved the env defines to envs.h
//
**
**
*/
