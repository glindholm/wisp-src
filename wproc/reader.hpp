//
//	Copyright (c) Shell Stream Software LLC, All Rights Reserved.
//
//	Project:	WPROC
//	
//
// Module : reader.hpp
// Author : George Soules
// Date   : 30 January 1991

#ifndef READER__HPP
#define READER__HPP

// Classes
#include "object.hpp"

// Definitions and subprograms
#include <limits.h>
#include <stdio.h>
#include "environ.hpp"


class reader : public object {
   public:
      reader();
      virtual ~reader() {};
      virtual  char   next_char() = 0;
      virtual  int_32 timestamp()        {return 0;}
      void            unread_last_char() {unread = true;}
      usign_16        current_row()      {return the_current_row;}
      usign_16        current_column()   {return the_current_column;}
      Boolean         ok()               {return the_ok;}
//FIX002
      Boolean ignoring_line_comments() {return skipping_comments;}
      void    ignore_line_comments(Boolean ignore) {skipping_comments = ignore;}

   protected:
      usign_16        the_current_row;
      usign_16        the_current_column;
      Boolean         unread;
      char            last_char_read;
      Boolean         the_ok;
      Boolean skipping_comments;
};


#define MAX_FILE_RECORD_SIZE  255
#define MAX_FILE_RECORDS      USHRT_MAX

class file_reader : public reader {
   public:
      file_reader(const char *a_filename, Boolean skip_comments = true);
      ~file_reader();
      char    next_char();
      void    unread_last_char();
      char   *read_absolute_record(usign_16 a_row_number);
      int_32  timestamp() {return the_timestamp;}

   private:
      FILE   *the_file;
      char    record[MAX_FILE_RECORD_SIZE];
      int     record_len;
      int     record_index;
      int_32  the_timestamp;
      void    read_next_record();
};


class string_reader : public reader {
   public:
      string_reader(const char *a_string);
      ~string_reader();
      char next_char();
      void unread_last_char();
   private:
      char *the_string;
      int  the_string_len;
};


const char substitution_char = '\f';

class message_reader : public reader {
   public:
      message_reader(const char *a_filename);
      ~message_reader();
      char next_char();
      void position_to(int a_message_number);
   private:
      FILE *the_file;
};

#endif

//
//	History:
//	$Log: reader.hpp,v $
//	Revision 1.6  2003/02/11 19:05:26  gsl
//	Remove unneeded #ifdef's for DEBUG
//	
//	Revision 1.5  1998/08/31 19:14:11  gsl
//	drcs update
//	
//

//	
//
//	Working file: reader.hpp
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
//	date: 1995-04-25 06:00:19-04;  author: gsl;  state: V4_3_00;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:52:35-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:33:18-05;  author: gsl;  state: V3_3x12;  lines: +10 -5
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:23-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
