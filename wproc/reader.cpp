//
//	Copyright (c) 1996-1998 NeoMedia Technologies Inc. All rights reserved.
//
//	Project:	WPROC
//	
//// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : reader.cpp
// Author : George Soules
// Date   : 30 January 1991

// Specification
#include "reader.hpp"

// Classes
// (none)

// Definitions and subprograms
#include <ctype.h>
#if DOS || DOS_HOST
#include <io.h>
#endif
#include <string.h>
#if WANG
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#endif

#ifndef SEEK_SET
#define SEEK_SET 0
#endif

#include "debugaid.hpp"
#include "environ.hpp"
#include "memory.hpp"
#include "report.hpp"
#include "utility.hpp"

const int tab_width = 8;

// reader member functions

reader::reader() {
   unread             = false;
   the_current_column = 0;
   the_current_row    = 0;
   skipping_comments  = true;
}


// file_reader member functions

file_reader::file_reader(const char *a_filename, Boolean skip_comments) {
   trace_begin(object, "file reader");
   skipping_comments = skip_comments;

   the_file = fopen(a_filename, "r");
   if (the_file == NULL) {
      the_ok = false;
      the_timestamp = 0;
   }
   else {
      the_ok = true;
      record_len   = 0;
      record_index = 0;

#if DOS || DOS_HOST
      union {
         ftime  fields;
         int_32 number;
      } timestamp;

      getftime(fileno(the_file), &timestamp.fields);
      the_timestamp = timestamp.number;
#else
#if WANG
      struct stat buf;
      stat(a_filename, &buf);
      the_timestamp = (int_32) (buf.st_mtime);
#endif
#endif
   }
   trace_end(object);
}


file_reader::~file_reader() {
   fclose(the_file);
   trace(object, "~file_reader");
}


char file_reader::next_char() {
   if (unread)
      unread = false;
   else {
      char c;
      int  tab_offset;

      if (record_index == record_len)
         read_next_record();

      c = record[record_index++];
      the_current_column += 1;

      /* Adjust for tabs if necessary */
      if (c == '\t') {
         tab_offset = the_current_column % tab_width;
         if (tab_offset)
            the_current_column += tab_width - tab_offset;
      }
      last_char_read = c;
   }
   return last_char_read;
}


char *file_reader::read_absolute_record(usign_16 a_row_number) {
   if (a_row_number < the_current_row) {
      fseek(the_file, 0, SEEK_SET);
      the_current_row = 0;
   }
   while (the_current_row != a_row_number)
      read_next_record();
   if (record[record_len - 1] != '\n')
      record_len += 1; // Record was truncated to MAX_SOURCE_WIDTH
   char *the_record = new_string(record_len);
   strncpy(the_record, record, record_len);
   the_record[record_len - 1] = '\0';  // replaces '\n'
   return the_record;
}


void file_reader::read_next_record() {
   int c;
   do {
      /* Read one "record" or "row" from a file */
      record_index = 0;
      while (record_index <= MAX_FILE_RECORD_SIZE
             && (c = fgetc(the_file)) != EOF && c != '\n')
      {
         if (c == '\t') {
            int tab_offset = (record_index + 1) % tab_width;
            int spaces = tab_width - tab_offset + 1;
            for (int i = 1; i <= spaces; i++)
               record[record_index++] = ' ';
         }
         else
            record[record_index++] = (char) c;
      }

      if (c != EOF) {
         /* Verify that max record limit has not been exceeded */
         if (the_current_row == MAX_FILE_RECORDS)
            report_fatal_error(8);

         /* Verify that max line length has not been exceeded */
         if (c != '\n')
            report_fatal_error(1);
      }

      the_current_row += 1;
#if WANG
      if (record_index > MAX_SOURCE_WIDTH)
         record_index = MAX_SOURCE_WIDTH;
#endif
      record[record_index++] = (char) c; // Add the NewLine

      record_len = record_index;
      record_index = 0;
      the_current_column = 0;

   } while (skipping_comments && record[0] == '*'); /* throw out line comments */
}


// string_reader member functions

string_reader::string_reader(const char *a_string) {
   trace(object, "string reader");
   the_string         = dup_string(a_string);
   the_string_len     = strlen(the_string);
   the_ok             = true;
}


string_reader::~string_reader() {
   delete_string(the_string);
   trace(object, "~string_reader");
}


char string_reader::next_char() {
   if (unread)
      unread = false;
   else {
      if (the_current_column < the_string_len)
         last_char_read = the_string[the_current_column++];
      else {
         if (the_current_column == the_string_len)
            the_current_column += 1;
         last_char_read = EOF;
      }
   }
   return last_char_read;
}


void string_reader::unread_last_char() {
   if (the_current_column > 0)
      the_current_column -= 1;
}


// message_reader member functions

message_reader::message_reader(const char *a_filename) {
   trace_begin(object, "message reader");
   the_file           = fopen(a_filename, "r");
   the_ok             = BOOLEAN(the_file != NULL);
   trace_end(object);
}


message_reader::~message_reader() {
   fclose(the_file);
   trace(object, "~file_reader");
}


char message_reader::next_char() {
   int c;
   c = fgetc(the_file);
   if (c == '\n')
      c = ' ';
   else if (c == '?')
      c = substitution_char;
   else if (c == '\\') {
      c = fgetc(the_file);
      if (c != '?')  // allows \? to be returned as ?
         c = EOF;
   }
   return (char) c;
}


void message_reader::position_to(int a_message_number) {
   char number_found[8];
   int c;
   char *number_wanted = int_32_to_string(a_message_number);
   while (true) {
      // Position to next formfeed
      while ((c = fgetc(the_file)) != EOF && c != '\f');
      if (c == EOF) {
         the_ok = false;
         delete_string(number_wanted);
         return;
      }
      // Keep looking for formfeeds until first digits match
      c = fgetc(the_file);
      if (c == number_wanted[0]) {
         // Now we're in the right section, find an exact match
         int index = 0;
         number_found[index++] = c;
         while (isdigit(c = fgetc(the_file)))
            number_found[index++] = c;
         number_found[index] = '\0';
         if (strcmp(number_wanted, number_found) == 0) {
            delete_string(number_wanted);
            return;
         }
      }
   }
}

//
//	History:
//	$Log: reader.cpp,v $
//	Revision 1.7  1998/08/31 19:14:10  gsl
//	drcs update
//	
//

//	
//	RCS file: /disk1/neomedia/RCS/wisp/wproc/reader.cpp,v
//	Working file: reader.cpp
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
//	date: 1996-07-25 19:47:50-04;  author: gsl;  state: V4_3_00;  lines: +2 -2
//	NT
//	----------------------------
//	revision 1.5
//	date: 1996-07-25 14:16:07-04;  author: gsl;  state: Exp;  lines: +0 -0
//	Renamed from reader.cc to reader.cpp
//	----------------------------
//	revision 1.4
//	date: 1995-04-25 06:00:18-04;  author: gsl;  state: V3_3_19;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:52:34-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:33:17-05;  author: gsl;  state: V3_3x12;  lines: +10 -3
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:23-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
