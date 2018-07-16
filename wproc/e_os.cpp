//
//	Copyright (c) 1996-1998 NeoMedia Technologies Inc. All rights reserved.
//
//	Project:	WPROC
//	Id:		$Id:$
//	RCS:		$Source:$
//	
//// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : e_os.cpp
// Author : George Soules
// Date   : 24 June 1991

// Specification
#include "machine.hpp"

// Classes
#include "process.hpp"

// Definitions and subprograms
#if DOS || DOS_HOST
#include <dir.h>
#include <dos.h>
#include <io.h>
#endif
#include <errno.h>
#include <stdio.h>
#include "debugaid.hpp"
#include "fileinfo.hpp"
#include "memory.hpp"
#include "subops.hpp"
#include "txt.hpp"
#include "utility.hpp"


#if ! WANG
#if DOS || DOS_HOST
void machine::exec_print() {
   opcode       the_machine_op;
   print_opcode the_print_op;
   Boolean      yes_no_option;
   Boolean      in_error_state = false;
   char        *file_name;
   int          copies = 1;
   Boolean      eject  = true;
   Boolean      spool  = false;

   pop_expressions(1);
   file_name = dup_string(exp[1].stripped_string());

   // Process options
   while (! in_error_state) {
      the_machine_op = execute();

      if (the_machine_op == statement_end_op)
         break;

      if (the_machine_op == halt_op) {
         // The machine encountered a fatal error
         in_error_state = true;
         break;
      }
      the_print_op = (print_opcode) (the_pcode_reader->get_usign_8());

      switch (the_print_op) {
         case print_copies_op :
            pop_expressions(1);
            copies = exp[1].integer(0, 32767, 46, txt_int_value);
            if (exp[1].is_bad())
               in_error_state = true;
            break;

         case print_eject_op :
            if (get_yes_no_option(yes_no_option))
               eject = yes_no_option;
            else
               in_error_state = true;
            break;

         case print_spool_op :
            if (get_yes_no_option(yes_no_option))
               spool = yes_no_option;
            else
               in_error_state = true;
            break;

         default:
            assert(UNREACHABLE);
      }
   }
   if (in_error_state)
      enter_cancel_state();
   else
      stmt_return_code = print(file_name, copies, eject, spool);

   delete file_name;
}
#else
void machine::exec_print() {
   opcode       the_machine_op;
   print_opcode the_print_op;
   Boolean      yes_no_option;
   Boolean      in_error_state = false;
   char        *file_name;
   int          copies = 1;
   Boolean      eject  = true;
   Boolean      spool  = false;

   pop_expressions(1);
   file_name = dup_string(exp[1].stripped_string());

   stmt_return_code = 1;
}
#endif
#endif


#if ! WANG
#if DOS || DOS_HOST
void machine::exec_protect() {
   opcode         the_machine_op;
   protect_opcode the_protect_op;
   Boolean        in_error_state = false;
   ffblk          info;
   int            status = 0;
   int            done;
   Boolean        ok;
   int            len;
   const char    *file_name;
   char          *pattern;
   char           dir[MAXPATH];
   Boolean        set;
   int            attrib;
   int            change_archive  = 0;
   int            change_hidden   = 0;
   int            change_readonly = 0;
   int            change_system   = 0;

   pop_expressions(1);
   pattern = dup_string(exp[1].stripped_string());
   get_dir(dir, pattern);
   len = strlen(dir);

   // Process options
   while (! in_error_state) {
      the_machine_op = execute();

      if (the_machine_op == statement_end_op)
         break;

      if (the_machine_op == halt_op) {
         // The machine encountered a fatal error
         in_error_state = true;
         break;
      }
      the_protect_op = (protect_opcode) (the_pcode_reader->get_usign_8());

      if (! get_yes_no_option(set))
         in_error_state = true;
      else {
         switch (the_protect_op) {
            case protect_archive_op  : change_archive  = set ? 1 : 2; break;
            case protect_hidden_op   : change_hidden   = set ? 1 : 2; break;
            case protect_readonly_op : change_readonly = set ? 1 : 2; break;
            case protect_system_op   : change_system   = set ? 1 : 2; break;
            default: assert(UNREACHABLE);
         }
      }
   }
   if (in_error_state) {
      delete_string(pattern);
      enter_cancel_state();
      return;
   }

   // Set attributes for each file in pattern
   done = findfirst(pattern, &info, FA_HIDDEN + FA_SYSTEM);
   ok   = BOOLEAN( ! done);
   while (! done) {
      // Append the file name to the drive and dir of the pattern
      file_name = strcat(dir, info.ff_name);
      attrib = _chmod(file_name, 0);

      if (change_archive)
         attrib = change_archive == 1 ? attrib | FA_ARCH : attrib & ~FA_ARCH;
      if (change_hidden)
         attrib = change_hidden == 1 ? attrib | FA_HIDDEN : attrib & ~FA_HIDDEN;
      if (change_readonly)
         attrib = change_readonly == 1 ? attrib | FA_RDONLY : attrib & ~FA_RDONLY;
      if (change_system)
         attrib = change_system == 1 ? attrib | FA_SYSTEM : attrib & ~FA_SYSTEM;

      status = _chmod(file_name, 1, attrib);
      if (status == -1)
         ok = false;
      dir[len] = '\0';
      done = findnext(&info);
   }
   delete_string(pattern);
   stmt_return_code = ok ? 0 : 1;
}
#else
void machine::exec_protect() {
   stmt_return_code = 1;
}
#endif
#endif


#if ! WANG
//#if DOS || DOS_HOST
void machine::exec_rename() {
   pop_expressions(2);
   stmt_return_code =
      rename(exp[1].stripped_string(), exp[2].stripped_string());
   if (stmt_return_code) {
      switch (errno) {
         case ENOENT :
            stmt_return_code = 1;
            break;
         case EACCES :
            stmt_return_code = 2;
            break;
#if DOS || DOS_HOST
         case ENOTSAM :
            stmt_return_code = 3;
            break;
#endif
      }
   }
}
//#else
//void machine::exec_rename() {
   //pop_expressions(2);
   //stmt_return_code = 1;
//}
//#endif
#endif


#if ! WANG
#if DOS || DOS_HOST
void machine::exec_scratch() {
   ffblk       info;
   int         status;
   int         done;
   Boolean     ok;
   int         len;
   const char *pattern;
   char        dir[MAXPATH];

   pop_expressions(1);
   pattern = exp[1].stripped_string();
   get_dir(dir, pattern);
   len = strlen(dir);
   done = findfirst(pattern, &info, 0);
   ok   = BOOLEAN(! done);
   while (! done) {
      // Append the file name to the drive and dir of the pattern
      status = remove(strcat(dir, info.ff_name));
      dir[len] = '\0';
      if (status)
         ok = false;
      done = findnext(&info);
   }
   stmt_return_code = ok ? 0 : 1;
}
#else
// supports deleting a single file only - no wildcards.
void machine::exec_scratch() {
   const char *pattern;
   int         status;

   pop_expressions(1);
   pattern = exp[1].stripped_string();

   status = remove(pattern);
   if (status == 0)
      stmt_return_code = 0;
   else
      stmt_return_code = 1;
}
#endif
#endif


#if ! WANG
void machine::exec_set() {
   pop_expressions(2);
   const char *name = exp[1].stripped_string();
   stmt_return_code = environment->set(name, exp[2].string());
}
#endif





//
//	History:
//	$Log: e_os.cpp,v $
//	Revision 1.6  1998-08-31 15:13:42-04  gsl
//	drcs update
//
//

//	
//	RCS file: /disk1/neomedia/RCS/wisp/wproc/e_os.cpp,v
//	Working file: e_os.cpp
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
//	date: 1996-07-25 14:14:48-04;  author: gsl;  state: V4_3_00;  lines: +0 -0
//	Renamed from e_os.cc to e_os.cpp
//	----------------------------
//	revision 1.4
//	date: 1995-04-25 05:59:50-04;  author: gsl;  state: V3_3_19;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:52:08-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:32:46-05;  author: gsl;  state: V3_3x12;  lines: +14 -14
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:04-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
