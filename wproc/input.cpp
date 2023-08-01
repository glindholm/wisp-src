//
//	Copyright (c) Shell Stream Software LLC, All Rights Reserved.
//
//	Project:	WPROC
//	
//
// Module : input.cpp
// Author : George Soules
// Date   : 11 February 1991

// Specification
#include "input.hpp"

// Classes
#include "procedur.hpp"
#include "reader.hpp"

// Definitions and subprograms
#ifdef UNIX
#include <unistd.h>
#endif

#include <errno.h>
#include "cancel.hpp"
#include "crt_io.hpp"
#include "debugaid.hpp"
#include "fileinfo.hpp"
#if DOS
#include "holdev.h"
#include <process.h>
#endif
#include "memory.hpp"
#include "report.hpp"
#include "utility.hpp"
#include "screen.hpp"
#if WANG
#include "wang_os.hpp"
#endif

#ifdef WIN32
#define unlink(file)			_unlink(file)
#endif


// Free subprograms that support input objects

input *create_input_object(const char *a_filename, arguments *args) {
   input    *the_input_object;
   char     *fname = strip(a_filename);
   char     *path;
   file_ext  ext;
   Boolean   source_file = false;
   Boolean   pcode_file = false;

#if DOS || DOS_HOST
   fname = upper_case(fname);
#endif
   path = find_valid_input_file(fname);

#if WANG
   if (the_process->nesting_level == 1)
      wang_os_first_procedure_name(path);
#endif

   if (path) {
      the_process->set_top_filename(path);

      ext = file_extension(path);

      trace_ss(object, "creating input object from ", path);

      switch (ext) {
         case ext_src :
         case ext_unknown :
            the_input_object = new source_file_input(path, args);
            source_file = true;
            break;

         case ext_pcode :
            the_input_object = new state_file_input(path, args);
            pcode_file = true;
            break;

         case ext_com :
         case ext_exe :
            the_input_object = new spawnable_input(path, args);
            break;

         case ext_bat :
            the_input_object = new system_command_input(path, args);
            break;

         default :
            assert(UNREACHABLE);
            break;
      }

      if (the_process->nesting_level == 1 && ! source_file) {
         if (user_options.compile())
            report_fatal_error(15, path);
         if (user_options.trace_execution() && ! pcode_file)
            report_fatal_error(19, path);
      }
   }
   else
      the_input_object = new nonexistent_input(fname, args);

   delete fname;
   delete path;

   return the_input_object;
}


// Bodies of input members

input::input() {
   trace(object, "input");
   the_return_code = 0;
   swapping        = false;
   the_name        = NULL;
   the_args        = NULL;
   is_linked_to    = false;

   const int min_heap  = 16 * 1024;
   const int min_stack =  1 * 1024;
   the_status =
      memory_left(min_heap, min_stack) ? status_ok : status_memory_error;
}


input::~input() {
   if (the_name)
      delete_string(the_name);
   trace(object, "~input");
}


// Bodies of nonexistent_input members

nonexistent_input::nonexistent_input(const char *a_name, arguments *args) {
   trace(object, "nonexistent_input");
   the_name = dup_string(a_name);
   the_args = args;
}


nonexistent_input::~nonexistent_input() {
   trace(object, "~nonexistent_input");
}


void nonexistent_input::run() {
   the_status = status_file_not_found;
}


#if DOS
Boolean input::swap_and_spawn(int_32 &rc) {
   Boolean ok;
   Boolean use_ems = false;
   Boolean use_xms = false;

   setenv(environ);
   char *swappath = getenv("RUNSWAP");

   if (swappath) {
      if (stricmp(swappath, "XMS") == 0)
         use_xms = true;
      else if (stricmp(swappath, "EMS") == 0)
         use_ems = true;
      if (use_xms || use_ems)
         swappath = NULL;
   }
   if (! swappath)
      swappath = "";

   setems(use_ems);
   setxms(use_xms);
   char *command = the_args->command(the_name);
   rc = holdev(swappath, 0, command);
   delete command;
   ok = Boolean(rc == 0);

   if (ok)
      rc = childret();
   else {
      // Test for unsafe interrupt error
      if (((unsigned) rc) > 255) {
         // Set rc to interrupt # + 200
         rc = (rc >> 8) + 200;
      }
      else
         rc = rc + 100;
   }
   return ok;
}
#endif


// Bodies of system_command_input members

system_command_input::system_command_input(
   const char *a_filename,
   arguments  *args)
{
   trace(object, "system_command_input");
   the_name = args->command(a_filename);
   the_args = args;
}

system_command_input::system_command_input(const char *a_command) {
   trace(object, "system_command_input");
   the_name = dup_string(a_command);
}


system_command_input::~system_command_input() {
   trace(object, "~system_command_input");
}


void system_command_input::run() {
   trace_ss(object, "running system_command_input ", the_name);

   save_full_screen_state();

   if (swapping)
#if DOS
      the_status =
         swap_and_spawn(the_return_code) ? status_ok : status_swap_error;
#else
      ;
#endif
   else {
      if (system(the_name) == -1)
         the_status =
            errno == ENOMEM ? status_memory_error : status_runtime_error;
      else
         the_status = status_ok;
   }

   restore_full_screen_state();
   set_cancel_handler();
}


// Bodies of spawnable_input members

spawnable_input::spawnable_input(const char *a_filename, arguments *args) {
   trace(object, "spawnable_input");
   the_name = dup_string(a_filename);
   the_args = args;
   is_linked_to = true;
}


spawnable_input::~spawnable_input() {
   trace(object, "~spawnable_input");
}


void spawnable_input::run() {
   trace_ss(object, "running spawnable_input ", the_name);

   save_full_screen_state();

   Boolean ok;
   int_32  rc;

#if WANG
   int_32 completion_status;

   the_process->prepare_for_link();
   
   /*
	Support for the CANCEL EXIT clause on the RUN statement is 
	not implemented and always set to false (3rd argument).
   */
   rc = wang_os_link(the_name, the_args, false, false, completion_status);
   ok = Boolean (completion_status == 0);
   the_return_code = rc;
#else
#if DOS
   if (swapping)
      ok = swap_and_spawn(rc);
   else {
      rc = spawnv(P_WAIT, the_name, the_args->vector());
      ok = Boolean(rc != -1);
   }
#endif
#endif

   if (ok) {
      trace_si(general, "spawn ok with rc = ", rc);
      the_status = status_ok;
      the_return_code = rc;
   }
   else {
      trace_si(general, "spawn failed with completion_status = ", completion_status);
      trace_si(general, "spawn failed with rc = ", rc);
#if DOS
      if (swapping) {
         the_status = status_swap_error;
         the_return_code = rc;
      }
      else {
         the_status = status_open_error;
      }
#endif
#if WANG
      // completion_status is 8 or 16
      if (completion_status == 16)
            the_status = status_cancel;
      else if (rc == 4 || rc == 16 || rc == 20)
            the_status = status_file_not_found;
      else
            the_status = status_link_failed;
#endif
   }

#if WANG
   restore_full_screen_state(false);
#else
   restore_full_screen_state();
#endif
   set_cancel_handler();
}


// Bodies of procedure_input members

procedure_input::procedure_input() {
   trace(object, "procedure_input");
}


procedure_input::~procedure_input() {
   trace(object, "~procedure_input");
}

extern "C" int WL_wbackground();

void procedure_input::run() {
   screen_contents *saved_screen;
   trace_begin(object, "running procedure_input");
   if (the_status == status_ok) {
#if WANG
      Boolean change_link_level = BOOLEAN(the_process->nesting_level > 1);
      if (change_link_level)
         wang_os_increment_link_level();
#endif

      saved_screen = NULL;
      if (! user_options.compile() && !WL_wbackground())
      {
          saved_screen = new screen_contents(1, 1, SCREEN_WIDTH, SCREEN_HEIGHT);

          clear_screen();

          char message[80];
          sprintf(message, "Procedure %.45s in progress", name() );
          put_text(10,12,message,ATTR_NORMAL);
      }

      the_procedure->interpret();

      if (saved_screen)
      {
          saved_screen->restore_screen();
          delete saved_screen;
      }

      the_status = the_procedure->status();
      the_return_code = the_procedure->return_code();
#if WANG
      if (change_link_level)
         wang_os_decrement_link_level();
#endif
   }
   trace_end(object);
}


// Bodies of source_input members

source_input::source_input() {
   trace(object, "source_input");
}


source_input::~source_input() {
   trace(object, "~source_input");
}


void source_input::run() {
   trace(object, "running source_input");
#if RUNTIME
#else
   if (the_status == status_ok) {
      the_procedure = new compilable_procedure(the_reader, the_name, the_args);
      procedure_input::run();
      delete the_procedure;
   }
#endif
}


// Bodies of source_file_input members

source_file_input::source_file_input(const char *a_filename, arguments *args) {
   trace(object, "source_file_input");
#if RUNTIME
   the_status = status_source_input;
   the_name   = dup_string(a_filename);
   the_reader = NULL;
   the_args   = args;
#else
   if (the_status == status_ok) {
      the_name   = dup_string(a_filename);
      the_reader = new file_reader(the_name);
      the_args   = args;
      if (! the_reader->ok())
         the_status = status_open_error;
      the_process->the_source_pathname = dup_string(a_filename);
      the_process->the_input_pathname  = dup_string(a_filename);
   }
#endif
}


source_file_input::~source_file_input() {
   if (the_reader)
      delete the_reader;
   trace(object, "~source_file_input");
}


int source_file_input::destroy() {
   delete the_reader;
   the_reader = NULL;
   return abs(unlink(the_name));
}


// Bodies of source_string_input members

source_string_input::source_string_input(const char *a_string) {
   trace(object, "source_string_input");
   the_name   = dup_string(a_string);
   the_reader = new string_reader(the_name);
   the_args   = new arguments(0);
}


source_string_input::~source_string_input() {
   delete the_reader;
   delete the_args;
   the_args = NULL;
   trace(object, "~source_string_input");
}


// Bodies of state_file_input members

state_file_input::state_file_input(const char *a_filename, arguments *args) {
   trace(object, "state_file_input");
   the_name = dup_string(a_filename);
   the_process->the_input_pathname = dup_string(a_filename);
   the_args = args;
}


state_file_input::~state_file_input() {
   trace(object, "~state_file_input");
}


void state_file_input::run() {
   trace(object, "running state_file_input");
   the_procedure = new executable_procedure(the_name, the_args);
   procedure_input::run();
   delete the_procedure;
}


int state_file_input::destroy() {
   return abs(unlink(the_name));
}


//
//	History:
//	$Log: input.cpp,v $
//	Revision 1.10  2011/10/29 20:09:14  gsl
//	Fix ISO routine name warnins on WIN32
//	
//	Revision 1.9  2002/07/10 21:06:28  gsl
//	Fix globals WL_ to make unique
//	
//	Revision 1.8  1998/08/31 19:13:52  gsl
//	drcs update
//	
//

//	
//
//	Working file: input.cpp
//	head: 1.7
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
//	total revisions: 7;	selected revisions: 7
//	description:
//	----------------------------
//	revision 1.7
//	date: 1997-05-13 10:44:24-04;  author: gsl;  state: V4_3_00;  lines: +5 -1
//	Change the RUN command to not set CANCEL EXIT
//	----------------------------
//	revision 1.6
//	date: 1996-07-25 19:46:46-04;  author: gsl;  state: V3_9_92;  lines: +3 -0
//	NT
//	----------------------------
//	revision 1.5
//	date: 1996-07-25 14:15:17-04;  author: gsl;  state: Exp;  lines: +0 -0
//	Renamed from input.cc to input.cpp
//	----------------------------
//	revision 1.4
//	date: 1995-04-25 06:00:00-04;  author: gsl;  state: V3_3_19;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:52:18-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:32:56-05;  author: gsl;  state: V3_3x12;  lines: +54 -16
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:11-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
