// Copyright (c) Lexical Design, 1991
//
// Module : files.hpp
// Author : George Soules
// Date   : 5 July 1991

#ifndef FILES__HPP
#define FILES__HPP

// Classes
#include "object.hpp"

// Definitions and subprograms
#include <stdio.h>
#include "environ.hpp"


class file : public object {
   public:
      enum file_mode {mode_read, mode_write, mode_unknown};

      file(const char *a_name, FILE *a_file, file_mode a_mode);
      ~file();
      const char *name() {return the_name;}
      FILE       *fp()   {return the_fp;}
      file_mode   mode() {return the_mode;}
      char       *read(Boolean read_line, int chars);
      void        write(Boolean write_line, const char *s);
   private:
      char       *the_name;
      FILE       *the_fp;
      file_mode   the_mode;
};


file *open_file(const char *a_name, file::file_mode a_mode, Boolean global = false);

int close_file(const char *a_name);

void flush_open_files(int nesting_level);

void files_cleanup(int nesting_level);

#endif

