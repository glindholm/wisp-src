//
//	Copyright (c) 1996-1998 NeoMedia Technologies Inc. All rights reserved.
//
//	Project:	WPROC
//	Id:		$Id:$
//	RCS:		$Source:$
//	
//// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : files.cpp
// Author : George Soules
// Date   : 5 July 1991

// Specification
#include "files.hpp"

// Classes
#include "process.hpp"

// Definitions and subprograms
#include <string.h>
#include "debugaid.hpp"
#include "memory.hpp"
#include "utility.hpp"


class file_entry : public object {
   public:
      file_entry(file *a_file, Boolean global = false) {
         the_file      = a_file;
         nesting_level = the_process->nesting_level;
         is_global     = global;
         next          = NULL;
         prev          = NULL;
      };

      ~file_entry() {
         delete the_file;
      }

      file       *the_file;
      int         nesting_level;
      Boolean     is_global;
      file_entry *next;
      file_entry *prev;
};

file_entry *first_entry = NULL;


void add_entry(file_entry *an_entry) {
   trace_ss(general, "open ", an_entry->the_file->name());
   if (first_entry) {
      an_entry->next = first_entry;
      first_entry->prev = an_entry;
   }
   first_entry = an_entry;
}


void remove_entry(file_entry *an_entry) {
   trace_ss(general, "close ", an_entry->the_file->name());
   if (an_entry == first_entry) {
      first_entry = an_entry->next;
      if (first_entry)
         first_entry->prev = NULL;
   }
   else {
      an_entry->prev->next = an_entry->next;
      if (an_entry->next)
         an_entry->next->prev = an_entry->prev;
   }
   delete an_entry;
}


file_entry *lookup_file(const char *a_name) {
   file_entry *entry = first_entry;
   char *fname = strip(a_name);
//.
   trace_ss(general, "Looking up file: ", fname);
   while (entry) {
      if (entry->nesting_level==the_process->nesting_level || entry->is_global)
         if (same_string(fname, entry->the_file->name()))
//.
{
   trace_ss(general, "Found file: ", entry->the_file->name());
            break;
}
      entry = entry->next;
   }
   delete fname;
   return entry;
}


file *open_file(const char *a_name, file::file_mode a_mode, Boolean global) {
   file_entry *entry = lookup_file(a_name);

   if (entry && global)
      // Can't make an open file global
      return NULL;

   if (! entry) {
      char *fname = strip(a_name); // fopen hangs if trailing blanks
//.
      trace_ss(general, "Opening file: ", fname);
      FILE *fp = fopen(fname, a_mode == file::mode_read ? "r" : "a");
      if (fp) {
         entry = new file_entry(new file(fname, fp, a_mode), global);
         add_entry(entry);
      }
      delete fname;
      if (! fp)
         return NULL;
   }
   return entry->the_file;
}


int close_file(const char *a_name) {
   file_entry *entry = lookup_file(a_name);
   if (entry) {
      // File is closed by the_file's destructor
      remove_entry(entry);
      return 0;
   }
   else
      return 1;
}


void flush_open_files(int nesting_level) {
   file_entry *entry = first_entry;
   while (entry) {
      if (entry->nesting_level == nesting_level &&
          entry->the_file->mode() == file::mode_write)
      {
         fflush(entry->the_file->fp());
      }
      entry = entry->next;
   }
}


void files_cleanup(int nesting_level) {
   file_entry *entry = first_entry;
   file_entry *next;
   while (entry) {
      next = entry->next;
      if (entry->nesting_level == nesting_level)
         remove_entry(entry);
      entry = next;
   }
}


file::file(const char *a_name, FILE *a_file, file_mode a_mode) {
   the_name = upper_case(strip(a_name));
   the_fp   = a_file;
   the_mode = a_mode;
}


file::~file() {
   fclose(the_fp);
   delete the_name;
}


char *file::read(Boolean read_line, int chars) {
   char buffer[256];
   int  i = 0;
   int  c;

//.
trace(general, "Reading file: " );
   if (read_line)
      chars = 255;

   while (i < chars) {
      c = fgetc(the_fp);
      if (c == EOF || c == '\n')
         break;
      buffer[i++] = (char) c;
   }

   if (c == EOF && i == 0)
      return NULL;
   else {
      buffer[i] = '\0';
      return dup_string(buffer);
   }
}


void file::write(Boolean write_line, const char *s) {
//.
trace(general, "Writing to file: ");
   fputs(s, the_fp);
   if (write_line)
      fputc('\n', the_fp);
}




//
//	History:
//	$Log: files.cpp,v $
//	Revision 1.6  1998-08-31 15:13:50-04  gsl
//	drcs update
//
//

//	
//	RCS file: /disk1/neomedia/RCS/wisp/wproc/files.cpp,v
//	Working file: files.cpp
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
//	date: 1996-07-25 14:15:13-04;  author: gsl;  state: V4_3_00;  lines: +0 -0
//	Renamed from files.cc to files.cpp
//	----------------------------
//	revision 1.4
//	date: 1995-04-25 05:59:58-04;  author: gsl;  state: V3_3_19;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:52:16-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:32:54-05;  author: gsl;  state: V3_3x12;  lines: +5 -5
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:09-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
