// Copyright (c) Lexical Software, 1991.  All rights reserved.
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
#if ! AIX
      typedef FILE source_file;
#endif
      file_reader(const char *a_filename, Boolean skip_comments = true);
      ~file_reader();
      char    next_char();
      void    unread_last_char();
      char   *read_absolute_record(usign_16 a_row_number);
      int_32  timestamp() {return the_timestamp;}

   private:
#if ! AIX
      source_file *the_file;
#else
      FILE   *the_file;
#endif
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
#if ! AIX
      typedef FILE message_file;
#endif
      message_reader(const char *a_filename);
      ~message_reader();
      char next_char();
      void position_to(int a_message_number);
   private:
#if ! AIX
      message_file *the_file;
#else
      FILE *the_file;
#endif
};

#endif
