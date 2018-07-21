//
//	Copyright (c) Shell Stream Software LLC, All Rights Reserved.
//
//	Project:	WPROC
//	
//// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : e_run.cpp
// Author : George Soules
// Date   : 5 April 1991

// Specification
#include "machine.hpp"

// Classes
#include "input.hpp"

// Definitions and subprograms
#if UNIX
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#endif
#if WIN32
#include <io.h>
#endif

#include "wang_os.hpp"

#include <fcntl.h>
#include <string.h>
#include "debugaid.hpp"
#include "memory.hpp"
#include "subops.hpp"
#include "txt.hpp"
#include "utility.hpp"


void machine::exec_interpret() {
   pop_expressions(1);
#if RUNTIME
   exp[1].fatal_error(66, NULL, NULL);
   enter_cancel_state(status_runtime_error);
   return;
#else
   save_process_state();
   input *the_input = new source_string_input(exp[1].string());
   the_process->the_input_pathname  = dup_string("(unnamed)");

   // Copy enough of the input to the_source_pathname to
   // allow a source error to be reported if one occurs
   the_process->the_source_pathname = new_string(80);
   strncpy(the_process->the_source_pathname, exp[1].string(), 79);
   the_process->the_source_pathname[79] = '\0';

   the_input->run();
   restore_process_state();
   stmt_return_code = the_input->return_code();
   delete the_input;
#endif
}


void machine::exec_run() {
   trace_begin(machine, "run statement");

   // Statement returns codes if Error Exit taken
   const int    rc_program_open  = 1;
   const int    rc_program_error = 2;
   const int    rc_memory        = 5;
#if RUNTIME
   const int    rc_source_input  = 11;
#endif

   // Statement returns codes if Cancel Exit taken
   const int    rc_cancel        = 1;
   const int    rc_runtime_error = 2;
   const int    rc_logoff        = 3;

   Boolean      in_error_state   = false;
   Boolean      exiting          = false;
   Boolean      run_pending      = true;
   Boolean      swap             = false;

   Boolean      cexit_handler    = false;
   Boolean      eexit_handler    = false;
   Boolean      cexit_taken      = false;
   Boolean      eexit_taken      = false;

   status_kind  status;
   opcode       the_machine_op;
   run_opcode   the_run_op;
   input       *the_input        = NULL;
   int_8        arg_count        = the_pcode_reader->get_usign_8();
   arguments   *using_args       = NULL;

   stmt_return_code = 0;

   // Get 'using' parameters + file name
   pop_expressions(arg_count + 1);
   expression *filename_exp = exp.remove_exp(1);

   // Build vector of parameters
   using_args = new arguments(arg_count);
   for (int i = 1; i <= arg_count; i++) {
      if (exp[i + 1].is_lvalue())
         using_args->set(i, exp[i + 1]);
      else {
         expression::exp_kind kind = exp[i + 1].kind();
         using_args->set(i, exp[i + 1].string(), kind);
      }
   }

   // Process clauses
   while (! in_error_state) {
      the_machine_op = execute();

      if (the_machine_op == statement_end_op)
         break;

      if (the_machine_op == halt_op) {
         // The machine encountered a fatal error
         in_error_state = true;
         break;
      }
      the_run_op = (run_opcode) (the_pcode_reader->get_usign_8());

      switch (the_run_op) {

         case run_cexit_op : {
            cexit_handler = true;
            break;
         }

         case run_eexit_op : {
            eexit_handler = true;
            break;
         }

         case run_display_op :
         case run_enter_op :
         {
            in_error_state =
               BOOLEAN(! exec_display_enter(BOOLEAN(the_run_op == run_enter_op)));
            break;
         }

         default:
            assert(UNREACHABLE);
      }
   }

   while (run_pending && ! in_error_state && ! exiting) {

      save_process_state();

      char proglib[8];
      char progvol[6];
      wang_os_extract_alpha("PL", proglib);
      wang_os_extract_alpha("PV", progvol);

      the_input =
         create_input_object(filename_exp->stripped_string(), using_args);

      if (swap)
         the_input->enable_swapping();

      wang_os_access_to_machine(this);

      the_input->run();

      wang_os_set_alpha("PL", proglib);
      wang_os_set_alpha("PV", progvol);

      restore_process_state();

      status = the_input->status();

      if (eexit_handler) {
         eexit_taken = true;
         switch (status) {
            case status_file_not_found :
               stmt_return_code = rc_program_open;
               break;
            case status_open_error :
               stmt_return_code = rc_program_error;
               break;
            case status_memory_error :
               stmt_return_code = rc_memory;
               break;
            case status_swap_error :
               stmt_return_code = the_input->return_code();
               break;
#if RUNTIME
            case status_source_input :
               stmt_return_code = rc_source_input;
               break;
#endif
            default :
               eexit_taken = false;
               break;
         }
      }

      if (cexit_handler) {
         cexit_taken = true;
         switch (status) {
            case status_cancel :
               stmt_return_code = rc_cancel;
               break;
            case status_runtime_error :
               stmt_return_code = rc_runtime_error;
               break;
            case status_logoff :
               stmt_return_code = rc_logoff;
               break;
            default :
               cexit_taken = false;
               break;
         }
      }

      if (stmt_return_code)
         exiting = true;
      else {

         stmt_return_code = the_input->return_code();		// Always get the return code.

         switch (status) {
            case status_ok :
               stmt_return_code = the_input->return_code();
               run_pending = false;
               break;

            case status_cancel :
            case status_runtime_error :
            case status_logoff :
               in_error_state = true;
               break;

            case status_link_failed :
               filename_exp->request_filename(77, txt_filename); // 477
               if (filename_exp->is_bad())
                  in_error_state = true;
               break;

            case status_file_not_found :
               filename_exp->request_filename(20, txt_filename); // 420
               if (filename_exp->is_bad())
                  in_error_state = true;
               break;

            case status_open_error :
               filename_exp->request_filename(61, txt_filename); // 461
               if (filename_exp->is_bad())
                  in_error_state = true;
               break;

            case status_memory_error :
               filename_exp->fatal_error(59, filename_exp->string(), NULL);
               in_error_state = true;
               break;

            case status_swap_error :
               filename_exp->fatal_error
                  (62, the_input->return_code(), filename_exp->string());
               in_error_state = true;
               break;
#if RUNTIME
            case status_source_input :
               filename_exp->fatal_error(65, filename_exp->string(), NULL);
               in_error_state = true;
               break;
#endif
            case status_destroy :
               stmt_return_code = the_input->destroy();
               run_pending = false;
               break;

            default:
               assert(UNREACHABLE);
         }
      }
      delete the_input;
   }
   delete using_args;

   delete filename_exp;

   if (in_error_state) {
      if (!(status == status_cancel || status == status_logoff))
         status = status_runtime_error;
      enter_cancel_state(status);
   }

   if (cexit_handler && ! (eexit_handler && eexit_taken))
      the_stack->push(new expression(cexit_taken));
   if (eexit_handler)
      the_stack->push(new expression(eexit_taken));

   trace_end(machine);
}


void machine::exec_system() {
   trace_begin(machine, "system statement");
   save_process_state();
   pop_expressions(1);
   input *the_input = new system_command_input(exp[1].string());
   the_input->run();
   restore_process_state();
   if (the_input->status() == status_memory_error) {
      exp[1].fatal_error(67, exp[1].string(), NULL);
      enter_cancel_state(status_runtime_error);
   }
   stmt_return_code = the_input->status() != status_ok;
   delete the_input;
   trace_end(machine);
}


void machine::exec_using() {
   exec_declare(true);
}




//
//	History:
//	$Log: e_run.cpp,v $
//	Revision 1.11  2001/09/24 13:39:14  gsl
//	Fix warnings
//	
//	Revision 1.10  1998-08-31 15:13:43-04  gsl
//	drcs update
//
//

//	
//	RCS file: /disk1/neomedia/RCS/wisp/wproc/e_run.cpp,v
//	Working file: e_run.cpp
//	head: 1.9
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
//	total revisions: 9;	selected revisions: 9
//	description:
//	----------------------------
//	revision 1.9
//	date: 1998-04-22 15:49:10-04;  author: gsl;  state: V4_3_00;  lines: +6 -2
//	Fix warnings
//	----------------------------
//	revision 1.8
//	date: 1996-07-25 19:45:54-04;  author: gsl;  state: V4_2_02;  lines: +10 -7
//	NT
//	----------------------------
//	revision 1.7
//	date: 1996-07-25 14:14:50-04;  author: gsl;  state: Exp;  lines: +0 -0
//	Renamed from e_run.cc to e_run.cpp
//	----------------------------
//	revision 1.6
//	date: 1995-06-10 13:59:27-04;  author: gsl;  state: V3_3_19;  lines: +0 -6
//	move the dump_globals() and update_globals() to wang_os_link()
//	----------------------------
//	revision 1.5
//	date: 1995-06-02 10:40:46-04;  author: gsl;  state: Exp;  lines: +2 -0
//	fix warning
//	----------------------------
//	revision 1.4
//	date: 1995-04-25 05:59:51-04;  author: gsl;  state: V3_3_16;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:52:08-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:32:47-05;  author: gsl;  state: V3_3x12;  lines: +44 -18
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:05-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
