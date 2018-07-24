//
//	Copyright (c) Shell Stream Software LLC, All Rights Reserved.
//
//	Project:	WPROC
//	
//// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : report.cpp
// Author : George Soules
// Date   : 12 February 1991

// Specification
#include "report.hpp"

// Classes
#include "colors.hpp"
#include "options.hpp"
#include "process.hpp"
#include "reader.hpp"
#include "screen.hpp"

// Definitions and subprograms
#include <string.h>
#include "crt_io.hpp"
#include "debugaid.hpp"
#include "environ.hpp"
#include "ext.hpp"
#if WANG
#include "wangfile.hpp"
#endif
#include "install.hpp"
#include "memory.hpp"
#include "product.hpp"


extern "C" void WL_wtrace(const char* routine, const char* code, const char* format, ... /* args */);
extern "C"void WL_werr_write(const char* buff);


char *const error_label   = "ERROR ";
char *const warning_label = "WARNING ";
char *const arrow_label   = "   -> ";
char *const blank_label   = "      ";
int         indent        = 0;

const int   syntax_messages_100         = 100;
const int   semantic_messages_200       = 200;
const int   static_runtime_messages_300 = 300;
const int   runtime_messages_400        = 400;
const int   GENERAL_MESSAGES_500        = 500;
const int   unexpected_messages_600     = 600;
const int   usage_messages_700          = 700;

const int   max_column   = 67;
int         max_text_len = max_column;
int         column       = indent;

const  int  max_word = 31;
static char word[max_word + 1];
static int  word_index = 0;

#if DOS
const  char truncation_mark  = 'þ';
const  char left_range_mark  = 'À';
const  char right_range_mark =  'Ù';
const  char mid_range_mark   = 'Ä';
#else
const  char truncation_mark  = '#';
const  char left_range_mark  = '+';
const  char right_range_mark =  '+';
const  char mid_range_mark   = '-';
#endif

Boolean highlighting    = false;
Boolean warning         = false;

int msg_number;

colors          coloring = {color_black, color_white};
message_reader *the_message_reader = NULL;
screen         *the_scrn = NULL;
int             the_scrn_row;


void put(const char *text) {
   if (user_options.compile() || the_scrn == NULL)
      write_stdout(text);
   else {
      the_scrn->add_field(new field(the_scrn_row, text,
         default_field_attributes, false, coloring));
   }
}


void put_newline(int how_many = 1) {
   while (how_many) {
      if (user_options.compile() || the_scrn == NULL)
         write_stdout("\n");
      else
         the_scrn_row += 1;
      how_many -= 1;
   }
}


void create_screen(Boolean fatal) {
   the_scrn = new screen();
   the_scrn_row = 1;

   if (fatal)
      the_scrn->set_title(" Error ");
   else
      the_scrn->set_title(" Correction required ");

   put("Procedure ");
   put(the_process->the_input_pathname);
   put(" has been interrupted");
   put_newline();
}

void init_screen(Boolean fatal) {
   the_scrn->disable_all_fkeys();
   if (fatal) {
      put("F10 - Cancel procedure");
      put_newline();
   }
   else
      the_scrn->enable_fkey(0);
   the_scrn->enable_fkey(10);
   the_scrn->set_screen_colors(coloring);
   the_scrn->set_border_colors(coloring);
   the_scrn->set_title_colors(coloring);
   the_scrn->set_border(7);
   the_scrn->set_restore(true);
   the_scrn->set_restrict(true);
   the_scrn->set_first_row(1);
   the_scrn->set_corners(5, 5, 75, 5 + the_scrn_row);
}


void cleanup_after_error() {
   delete the_scrn;
   the_scrn = NULL;
}


void locate_message(int message_number) {
   msg_number = message_number;
   column = indent;
   char *messages_filename =
      installation_pathname(MESSAGES_NAME, MESSAGES_EXT);
   the_message_reader = new message_reader(messages_filename);
   delete_string(messages_filename);
   if (the_message_reader->ok()) {
      the_message_reader->position_to(message_number);
      if (the_message_reader->ok())
         return;
   }
}


void show_new_line(int how_many = 1) {
   put_newline(how_many);
   column = 1;
}


void show_new_indented_line() {
   put_newline();
   put(blank_label);
   column = indent;
}


void show(const char *text) {
   int text_len = strlen(text);
   if (text_len + column > max_column) {
      if (column > indent + 1)
         show_new_indented_line();
      if (text_len > max_text_len) {
         char* truncated = new_string(max_text_len + 1);
         strncpy(truncated, text, max_text_len - 1);
         truncated[max_text_len - 1] = truncation_mark;
         truncated[max_text_len] = '\0';
         show(truncated);
         delete_string(truncated);
         return;
      }
   }
   // Prevent first character in a message line from being blank
   int offset = 0;
   if (text[0] == ' ' && column == indent && ! highlighting)
      offset = 1;

   put(text + offset);
   column += text_len - offset;
}


void show(char c) {
   char s[2];
   s[0] = c;
   s[1] = '\0';
   show(s);
}


void show(usign_16 a_number) {
   char ascii_number[INT_16_STRING_SIZE];
   int_32_to_ascii((int_32) a_number, ascii_number);
   show(ascii_number);
}


void show_word() {
   word[word_index] = '\0';
   show(word);
   word_index = 0;
}


void show_message_text(
   const char *sub1 = NULL,
   const char *sub2 = NULL,
   const char *sub3 = NULL,
   const char *sub4 = NULL)
{
   const int subs_allowed = 4;

   if (! the_message_reader->ok()) {
      show_new_line();
      show("Unable to open message file to report error ");
      show((usign_16) abs(msg_number));
      show_new_line();
      show("(this problem can occur if the message file is missing,");
      show_new_line();
      show("too many files are open, or there is insufficienct memory).");
   }
   else {
      char c;
      int  sub_count = 0;
      char msgbuf[4096];
      int msgpos = 0;
      const char *subs[subs_allowed];
      subs[0] = sub1;
      subs[1] = sub2;
      subs[2] = sub3;
      subs[3] = sub4;

      strcpy(msgbuf,"(WPROC) %WPROC-E-MESSAGE ");
      msgpos = strlen(msgbuf);

      while ((c = the_message_reader->next_char()) != EOF) {
         if (c == substitution_char) {
            // show substitution value
            show_word();  // flush
            while (subs[sub_count] == NULL)
               sub_count += 1;
            assert(sub_count < subs_allowed);

	    memcpy(&msgbuf[msgpos], subs[sub_count], strlen(subs[sub_count]));
	    msgpos += strlen(subs[sub_count]);

            show(subs[sub_count++]);
         }
         else {
		msgbuf[msgpos++] = c;

            // append char to word
            assert(word_index < max_word);
            word[word_index++] = c;
            // show word when fully built
            if (c == ' ')
               show_word();
         }
      }
      assert(word_index < max_word);
      word[word_index++] = '.';
      show_word(); // last one

      msgbuf[msgpos++] = '\n';
      msgbuf[msgpos++] = '\0';
      WL_werr_write(msgbuf);
   }
   delete the_message_reader;
   the_message_reader = NULL;
}


void show_error_line_number(location *a_location) {
   usign_16 first = a_location->the_range->row.first;
   usign_16 last  = a_location->the_range->row.last;
   if (first == 0)
      // Source is not a file
      first = last = 1;
   show_new_line();
   if (warning)
      show(warning_label);
   else
      show(error_label);
   warning = false;
   if (first == last) {
      show("on line ");
      show(first);
   }
   else {
      show("on lines ");
      show(first);
      show(" through ");
      show(last);
   }
   show(".  ");
   indent = strlen(error_label);
   max_text_len = max_column - indent;
}


void show_copyright() {
#if DEMO
   show_new_line();
   put(demo_notice());
#endif
   show_new_line();
   put(product_copyright());
}


void show_source_info(location *a_location) {
   int     record_len;
   Boolean is_file = BOOLEAN(a_location->the_range->row.first != 0);
   Boolean ok      = true;

   // Show the source text
   show_new_line(2);
   if (is_file) {
      file_reader the_reader(a_location->the_source);
      ok = the_reader.ok();
      if (! ok) {
         show("Unable to open source procedure ");
         show(a_location->the_source);
         show("");
         show_new_line();
      }
      else {
         if (the_process->the_source_timestamp == the_reader.timestamp()) {
            char *the_record =
               the_reader.read_absolute_record(a_location->the_range->row.first);
            record_len = strlen(the_record);
            show(arrow_label);
            show(the_record);
            delete_string(the_record);
         }
         else {
            show("The source procedure ");
            show(a_location->the_source);
            show_new_line();
            show("is not shown because it was modified since last compiled");
            show_new_line();
            ok = false;
         }
      }
   }
   else {
      record_len = strlen(a_location->the_source);
      show(arrow_label);
      show(a_location->the_source);
   }

   // Highlight the error range
   if (ok && a_location->the_range->col.first <= max_text_len) {
      highlighting = true;
      char   *bar = new_string(max_text_len + 1);
      Boolean truncated = false;
      int     i;

      // Insert leading blanks up to start of error range
      show_new_indented_line();
      for (i = 0; i < a_location->the_range->col.first - 1; i++)
         bar[i] = ' ';

      // Determine end of range
      int end = a_location->the_range->col.last;
      if (end > record_len ||
          a_location->the_range->row.first != a_location->the_range->row.last)
      {
         end = record_len;
         truncated = true;
      }
      if (end > max_text_len) {
         end = max_text_len;
         truncated = true;
      }

      // Draw the bar
      if (a_location->the_range->col.first == a_location->the_range->col.last)
         bar[i++] = '^';
      else {
         if (i < end - 1) {
            bar[i++] = left_range_mark;
            while(i < end - 1)
               bar[i++] = mid_range_mark;
            bar[i++] = truncated ? mid_range_mark : right_range_mark;
         }
      }
      assert(i <= max_text_len);
      bar[i] = '\0';
      show(bar);
      show_new_line();
      delete_string(bar);
      highlighting = false;
   }
   else if (ok)
      // Error is beyond max_txt_len
      show_new_line(2);
}


void init_error(int error_number, location *a_location) {
   if (! user_options.compile())
      create_screen(true);
   locate_message(error_number);
   show_error_line_number(a_location);
}


void display_error(location *a_location, const char *sub = NULL) {
   Boolean cursor_on = BOOLEAN(! the_process->cursor_off);
   show_message_text(sub);
   show_source_info(a_location);
   if (! user_options.compile()) {
      put_newline();
      init_screen(true);
      cursor_visible(false);
      the_scrn->display(true);
      cursor_visible(cursor_on);
      cleanup_after_error();
   }
}


void report_syntax_error
   (int error_number, location *a_location, const char *sub)
{
   init_error(error_number + syntax_messages_100, a_location);
   show("Found ");
   if (sub) {
      show("'");
      show(sub);
      show("'");
   }
   else
      show("end of procedure");
   show(" but require ");
   display_error(a_location);
}


void report_semantic_error
   (int error_number, location *a_location, const char *sub)
{
   warning = BOOLEAN(error_number < 0);
   init_error(abs(error_number) + semantic_messages_200, a_location);
   display_error(a_location, sub);
}


mod_field *init_correction(
   Boolean     fatal,
   Boolean     is_integer,
   Boolean    &no_correction,
   int         error_number,
   location   *a_location,
   const char *bad_value,
   const char *value_kind,
   const char *first,
   const char *last)
{
   mod_field *fld = NULL;

   create_screen(fatal);

   locate_message(error_number + runtime_messages_400);
   show_error_line_number(a_location);
   show_message_text(bad_value, value_kind, first, last);
   show_source_info(a_location);
   put_newline();

   no_correction = true;

   if (! fatal) {
      field_attributes is = default_field_attributes;
      is.modifiable = true;
      put("New ");
      put(value_kind);
      put(" = ");
      if (is_integer) {
         is.numeric = is_integer;
         fld = new mod_field(the_scrn_row, "           ",
            is, false, coloring);
      }
      else
         fld = new mod_field(the_scrn_row,
            "                                                  ",
            is, false, coloring);
      the_scrn->add_mod_field(fld);
      put_newline(2);
      put("Enter - Continue with new ");
      put(value_kind);
      put_newline();
      put("F10   - Cancel procedure");
      put_newline();
   }
   init_screen(fatal);
   return fld;
}


#if WANG
void init_filename_correction(
   Boolean     fatal,
   Boolean    &no_correction,
   int         error_number,
   location   *a_location,
   const char *bad_value,
   const char *value_kind,
   const char *first,
   const char *last,
   mod_field *&native_fld,
   mod_field *&file_fld,
   mod_field *&lib_fld,
   mod_field *&vol_fld)
{
   create_screen(fatal);

   locate_message(error_number + runtime_messages_400);
   show_error_line_number(a_location);
   wang_filename name(bad_value);
   char in_on[31];  // 8 + " IN " + 8 + " ON " + 6 + 1
   char *temp = strip(name.filename);
   strcpy(in_on, temp);
   delete_string(temp);
   if (name.libname[0] != ' ') {
      temp = strip(name.libname);
      strcat(in_on, " in ");
      strcat(in_on, temp);
      delete_string(temp);
   }
   if (name.volname[0] != ' ') {
      temp = strip(name.volname);
      strcat(in_on, " on ");
      strcat(in_on, temp);
      delete_string(temp);
   }
   show_message_text(in_on, value_kind, first, last);
   show_source_info(a_location);
   put_newline();

   no_correction = true;

   if (! fatal) {
      field_attributes is = default_field_attributes;
      is.modifiable = true;
      is.upper = true;
      put("Respecify the file name:");
      put_newline(2);
      put("   FILE = ");
      file_fld = new mod_field(the_scrn_row, "        ", is, false, coloring);
      the_scrn->add_mod_field(file_fld);
      put("  LIBRARY = ");
      lib_fld = new mod_field(the_scrn_row, "        ", is, false, coloring);
      the_scrn->add_mod_field(lib_fld);
      put("  VOLUME = ");
      vol_fld = new mod_field(the_scrn_row, "      ", is, false, coloring);
      the_scrn->add_mod_field(vol_fld);
      put_newline(2);
      is.upper = false;
      put("Or native file name: ");
      native_fld = new mod_field(the_scrn_row,
         "                                            ",
         is, false, coloring);
      the_scrn->add_mod_field(native_fld);
      put_newline(2);
      put("Enter - Continue with new ");
      put(value_kind);
      put_newline();
      put("F10   - Cancel procedure");
      put_newline();
   }
   init_screen(fatal);
}
#endif


void report_correction(
   int_32     &new_value,
   Boolean    &no_correction,
   int         error_number,
   location   *a_location,
   const char *bad_value,
   const char *value_kind,
   const char *first,
   const char *last)
{
   mod_field *fld;

   fld = init_correction(
      BOOLEAN(new_value == error_is_fatal),
      true,
      no_correction,
      error_number,
      a_location,
      bad_value,
      value_kind,
      first,
      last);

   Boolean cursor_on = BOOLEAN(! the_process->cursor_off);

   while (true) {
      cursor_visible(BOOLEAN(new_value != error_is_fatal));
      the_scrn->display(true);
      cursor_visible(cursor_on);
      if (the_scrn->fkey() == 10 || the_scrn->cancelled())
         break;
      assert(fld);
      if (string_to_int_32(fld->text(), new_value)) {
         no_correction = false;
         break;
      }
      field_attributes is = fld->attributes();
      is.blink = true;
      fld->set_attributes(is, coloring);
      the_scrn->set_alarm(true);
   }
   cleanup_after_error();
}


void report_correction(
   char      *&new_value,
   Boolean    &no_correction,
   int         error_number,
   location   *a_location,
   const char *bad_value,
   const char *value_kind)
{

   mod_field *fld;
   fld = init_correction(
      false,
      false,
      no_correction,
      error_number,
      a_location,
      bad_value,
      value_kind,
      NULL,
      NULL);

   Boolean cursor_on = BOOLEAN(! the_process->cursor_off);

   cursor_visible(true);
   the_scrn->display(true);
   cursor_visible(cursor_on);

   if (! (the_scrn->fkey() == 10 || the_scrn->cancelled())) {
      assert(fld);
      new_value = trim(fld->text());
      no_correction = false;
   }
   cleanup_after_error();
}


#if WANG
void report_filename_correction(
   char      *&new_value,
   Boolean    &no_correction,
   int         error_number,
   location   *a_location,
   const char *bad_value,
   const char *value_kind)
{
   mod_field *native_fld = NULL;
   mod_field *file_fld   = NULL;
   mod_field *lib_fld    = NULL;
   mod_field *vol_fld    = NULL;

   init_filename_correction(
      false,
      no_correction,
      error_number,
      a_location,
      bad_value,
      value_kind,
      NULL,
      NULL,
      native_fld,
      file_fld,
      lib_fld,
      vol_fld);

   Boolean cursor_on = BOOLEAN(! the_process->cursor_off);

   cursor_visible(true);
   the_scrn->display(true);
   cursor_visible(cursor_on);

   if (! (the_scrn->fkey() == 10 || the_scrn->cancelled())) {
      if (file_fld) {
         if (strcmp(file_fld->text(), "        ") == 0)
            new_value = trim(native_fld->text());
         else {
            char *file = trim(file_fld->text());
            char *lib  = trim(lib_fld->text());
            char *vol  = trim(vol_fld->text());
            char flv[8 + 1 + 8 + 1 + 6 + 1];
            strcpy(flv, file);
            strcat(flv, "\x01");
            strcat(flv, lib);
            strcat(flv, "\x02");
            strcat(flv, vol);
            delete_string(file);
            delete_string(lib);
            delete_string(vol);
            new_value = dup_string(flv);
         }
      }
      else
         new_value = trim(native_fld->text());
      no_correction = false;
   }
   cleanup_after_error();
}
#endif


void report_general_error(int error_number, const char *sub) {
   show_copyright();
   show_new_line();
   locate_message(error_number + GENERAL_MESSAGES_500);
   show_message_text(sub);
   show_new_line();
}


void report_fatal_error(int error_number, const char *sub) {
   show_copyright();
   show_new_line();
   locate_message(error_number + unexpected_messages_600);
   show_message_text(sub);
   show_new_line();
   restore_video_state();
   exit(1);
}


void report_status(const char *msg1, const char *msg2, const char *msg3) {
   put(msg1);
   if (msg2)
      put(msg2);
   if (msg3)
      put(msg3);
   put_newline();
}

//
//	History:
//	$Log: report.cpp,v $
//	Revision 1.8  2004/01/06 18:48:35  gsl
//	in show_message_text() write the message to wisperr.log
//	
//	Revision 1.7  2004/01/06 18:25:11  gsl
//	in show_message_text() write the message with WL_wtrace()
//	
//	Revision 1.6  1998/08/31 19:14:11  gsl
//	drcs update
//	
//

//	
//
//	Working file: report.cpp
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
//	date: 1996-07-25 14:16:09-04;  author: gsl;  state: V4_3_00;  lines: +0 -0
//	Renamed from report.cc to report.cpp
//	----------------------------
//	revision 1.4
//	date: 1995-04-25 06:00:19-04;  author: gsl;  state: V3_3_19;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:52:35-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:33:18-05;  author: gsl;  state: V3_3x12;  lines: +24 -24
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:23-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
