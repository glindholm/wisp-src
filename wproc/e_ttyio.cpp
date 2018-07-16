//
//	Copyright (c) 1996-1998 NeoMedia Technologies Inc. All rights reserved.
//
//	Project:	WPROC
//	Id:		$Id:$
//	RCS:		$Source:$
//	
//// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : e_ttyio.cpp
// Author : George Soules
// Date   : 15 June 1991

// Specification
#include "machine.hpp"

// Classes
#include "files.hpp"

// Definitions and subprograms
#include "crt_io.hpp"
#include "txt.hpp"
#include "utility.hpp"


void machine::exec_close() {
   pop_expressions(1);
   stmt_return_code = close_file(exp[1].string());
}


void machine::exec_open() {
   usign_8         fcn    = the_pcode_reader->get_usign_8();
   file::file_mode mode   = fcn & 0x02 ? file::mode_read : file::mode_write;
   Boolean         global = BOOLEAN(fcn & 0x01);

   pop_expressions(1);
   file *the_file = open_file(exp[1].string(), mode, global);
   if (the_file)
       stmt_return_code = the_file->mode() == mode ? 0 : 2;
   else
       stmt_return_code = 1;
}


void machine::exec_read() {
   usign_8    fcn       = the_pcode_reader->get_usign_8();
   Boolean    read_line = BOOLEAN(fcn & 0x01);
   Boolean    read_file = BOOLEAN(fcn & 0x02);

   pop_expressions(read_file ? 2 : 1);

   expression *the_variable = exp.remove_exp(1);
   symbol     &the_symbol   = the_variable->lvalue_ref().symbol_ref();
   int         rc           = 0;
   int         chars;
   char       *s;

   if (read_line || the_symbol.is.dynamic)
      chars = 127;
   else if (the_symbol.is.integer)
      chars = 11;
   else
      chars = strlen(the_variable->string());

   if (read_file) {
      file *the_file = open_file(exp[2].string(), file::mode_read);
      if (the_file) {
         if (the_file->mode() == file::mode_write)
            rc = 3; // file open for writing
         else {
            s = the_file->read(read_line, chars);
            if (! s)
               rc = 2; // eof
         }
      }
      else
         rc = 1; // could not open
   }
   else
      s = read_stdin(chars);

   if (rc)
      delete the_variable;
   else {
      if (the_symbol.is.integer) {
         int_32 i;
         if (string_to_int_32(s, i))
            assign(the_variable, i);
         else {
            the_variable->fatal_error(47, s, NULL);
            delete the_variable;
            enter_cancel_state();
         }
      }
      else
         assign(the_variable, s);

      delete s;
   }
   stmt_return_code = rc;
}


void machine::exec_write() {
   usign_8 fcn        = the_pcode_reader->get_usign_8();
   Boolean write_line = BOOLEAN(fcn & 0x01);
   Boolean write_file = BOOLEAN(fcn & 0x02);
   int     rc         = 0;

   pop_expressions(write_file ? 2 : 1);

   if (write_file) {
      file *the_file = open_file(exp[2].string(), file::mode_write);
      if (the_file) {
         if (the_file->mode() == file::mode_read)
            rc = 4; // file open for reading
         else {
            the_file->write(write_line, exp[1].string());
            flush_open_files(the_process->nesting_level);
         }
      }
      else
         rc = 1; // could not open

   }
   else {
      write_stdout(exp[1].string());
      if (write_line)
         write_stdout("\n");
   }
   stmt_return_code = rc;
}




//
//	History:
//	$Log: e_ttyio.cpp,v $
//	Revision 1.6  1998/08/31 19:13:44  gsl
//	drcs update
//	
//

//	
//	RCS file: /disk1/neomedia/RCS/wisp/wproc/e_ttyio.cpp,v
//	Working file: e_ttyio.cpp
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
//	date: 1996-07-25 14:14:56-04;  author: gsl;  state: V4_3_00;  lines: +0 -0
//	Renamed from e_ttyio.cc to e_ttyio.cpp
//	----------------------------
//	revision 1.4
//	date: 1995-04-25 05:59:52-04;  author: gsl;  state: V3_3_19;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:52:10-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:32:48-05;  author: gsl;  state: V3_3x12;  lines: +5 -5
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:06-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
