//
//	Copyright (c) Shell Stream Software LLC, All Rights Reserved.
//
//	Project:	WPROC
//	
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


//
//	History:
//	$Log: files.hpp,v $
//	Revision 1.5  1998/08/31 19:13:50  gsl
//	drcs update
//	
//

//	
//
//	Working file: files.hpp
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
//	date: 1995-04-25 05:59:59-04;  author: gsl;  state: V4_3_00;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:52:16-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:32:55-05;  author: gsl;  state: V3_3x12;  lines: +3 -3
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:09-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
