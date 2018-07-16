//
//	Copyright (c) 1996-1998 NeoMedia Technologies Inc. All rights reserved.
//
//	Project:	WPROC
//	Id:		$Id:$
//	RCS:		$Source:$
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
#if DOS || DOS_HOST
#include <dos.h>
#include <io.h>
#include <sys\stat.h>
#include "holdev.h"
#else
#if UNIX
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#endif
#endif
#if WIN32
#include <io.h>
#endif
#if WANG
#include "wang_os.hpp"
#endif
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
#if ! WANG
   const int    rc_input_open    = 3;
   const int    rc_output_open   = 4;
   const int    rc_swap_option   = 6;
#endif
   const int    rc_memory        = 5;
#if RUNTIME
   const int    rc_source_input  = 11;
#endif

   // Statement returns codes if Cancel Exit taken
   const int    rc_cancel        = 1;
   const int    rc_runtime_error = 2;
#if WANG
   const int    rc_logoff        = 3;
#endif

   Boolean      in_error_state   = false;
   Boolean      exiting          = false;
   Boolean      run_pending      = true;
#if (! WANG) && DOS
   Boolean      type_ahead       = false;
#endif
   Boolean      swap             = false;

#if !WANG
   int          access;
#endif
   const int    std_in           = 0;
   const int    std_out          = 1;
   int          new_std_in       = -1;
   int          new_std_out      = -1;
#if !WANG
   int          old_std_in;
   int          old_std_out      = false;
   Boolean      redirect_std_in  = false;
   Boolean      redirect_std_out = false;
#endif /* !WANG */
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
#if WANG
         expression::exp_kind kind = exp[i + 1].kind();
         using_args->set(i, exp[i + 1].string(), kind);
#else
         using_args->set(i, exp[i + 1].string());
#endif
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
#if ! WANG
         case run_append_op :
         case run_output_op :
         {
            // Get the output file name (ignore null input)
            pop_expressions(1);
            if (exp[1].string()[0]) {
               // Set the access bits
               if (the_run_op == run_append_op)
#if DOS || DOS_HOST || WIN32
                  access = O_RDWR | O_CREAT | O_TEXT;
#else
#if UNIX
                  access = O_RDWR | O_CREAT;
#endif
#endif
               else
#if DOS || DOS_HOST || WIN32
                  access = O_RDWR | O_CREAT | O_TRUNC | O_TEXT;
#else
#if UNIX
                  access = O_RDWR | O_CREAT | O_TRUNC;
#endif
#endif

               // Attempt to open the file
               while (! exiting && new_std_out == -1) {
                  new_std_out =
                     open(exp[1].stripped_string(), access, S_IWRITE);
                  if (new_std_out == -1) {
                     // Open failed
                     if (eexit_handler) {
                        stmt_return_code = rc_output_open;
                        exiting = true;
                        eexit_taken = true;
                        break;
                     }
                     else {
                        exp[1].request_string(21, txt_filename);
                        if (exp[1].is_bad()) {
                           in_error_state = true;
                           break;
                        }
                     }
                  }
               }
               if (! in_error_state && ! exiting) {
                  // If appending, position to end of file
                  if (the_run_op == run_append_op)
                     lseek(new_std_out, 0, SEEK_END);

                  // Save current standard output
                  old_std_out = dup(std_out);
                  redirect_std_out = true;
               }
            }
            break;
         }

         case run_input_op : {
            // Get the input file name (ignore null input name)
            pop_expressions(1);
            if (exp[1].string()[0]) {
               // Set the access bits
#if DOS || DOS_HOST || WIN32
               access = O_RDONLY | O_TEXT;
#else
#if UNIX
               access = O_RDONLY;
#endif
#endif

               // Attempt to open the file
               while (! exiting && new_std_in == -1) {
                  new_std_in = open(exp[1].stripped_string(), access);
                  if (new_std_in == -1) {
                     // Open failed
                     if (eexit_handler) {
                        stmt_return_code = rc_input_open;
                        exiting = true;
                        eexit_taken = true;
                        break;
                     }
                     else {
                        exp[1].request_string(22, txt_filename);
                        if (exp[1].is_bad()) {
                           in_error_state = true;
                           break;
                        }
                     }
                  }
               }
               if (! in_error_state && ! exiting) {
                  // Save current standard output
                  old_std_in = dup(std_in);
                  redirect_std_in = true;
               }
            }
            break;
         }
#endif

         case run_cexit_op : {
            cexit_handler = true;
            break;
         }

         case run_eexit_op : {
            eexit_handler = true;
            break;
         }

#if (! WANG) && DOS
         case run_swap_op : {
            pop_expressions(1);
            while (! is_yes_or_no(exp[1].string(), swap)) {
               if (eexit_handler) {
                  stmt_return_code = rc_swap_option;
                  exiting = true;
                  eexit_taken = true;
                  break;
               }
               else {
                  exp[1].request_string(4, txt_swap_opt);
                  if (exp[1].is_bad()) {
                     in_error_state = true;
                     break;
                  }
               }
            }
            break;
         }

         case run_type_op : {
            int_8 type_arg_count = the_pcode_reader->get_int_8();
            pop_expressions(type_arg_count);

            if (! exiting) {
               // Flush type-ahead buffer
               _AH = 0x0C;
               _AL = 0x00;
               geninterrupt(0x21);

               // Load buffer with type clause characters
               for (int i = 1; i <= type_arg_count; i++) {
                  if (exp[i].kind() == expression::integer_kind) {
                     int_16 the_key = exp[i].integer();
                     if (the_key >= 0) {
                        // ASCII decimal codes
                        _CL = the_key;
                        _CH = 0;
                        _AH = 0x05;
                        geninterrupt(0x16);
                     }
                     else {
                        // Extended ASCII decimal codes
                        the_key = - the_key;
                        _CL = 0;
                        _CH = the_key;
                        _AH = 0x05;
                        geninterrupt(0x16);
                     }
                  }
                  else {
                     // String -- Normal ASCII characters
                     const char *s = exp[i].string();
                     int size = strlen(s);
                     for (int j = 0; j < size; j++) {
                        _CL = s[j];
                        _CH = 0;
                        _AH = 0x05;
                        geninterrupt(0x16);
                        if (_AL) break;
                     }
                  }
               }
               type_ahead = true;
            }
            break;
         }
#endif

#if WANG
         case run_display_op :
         case run_enter_op :
         {
            in_error_state =
               BOOLEAN(! exec_display_enter(BOOLEAN(the_run_op == run_enter_op)));
            break;
         }
#endif

         default:
            assert(UNREACHABLE);
      }
   }

   while (run_pending && ! in_error_state && ! exiting) {

#if !WANG
      // Enable IO redirection
      if (redirect_std_in)
         dup2(new_std_in, std_in);
      if (redirect_std_out)
         dup2(new_std_out, std_out);
#endif /* !WANG */

      save_process_state();

#if WANG
      char proglib[8];
      char progvol[6];
      wang_os_extract_alpha("PL", proglib);
      wang_os_extract_alpha("PV", progvol);
#endif

      the_input =
         create_input_object(filename_exp->stripped_string(), using_args);

      if (swap)
         the_input->enable_swapping();

#if WANG
      wang_os_access_to_machine(this);
#endif

      the_input->run();

#if WANG
      wang_os_set_alpha("PL", proglib);
      wang_os_set_alpha("PV", progvol);
#endif

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
#if WANG
            case status_logoff :
               stmt_return_code = rc_logoff;
               break;
#endif
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
#if WANG
            case status_logoff :
#endif
               in_error_state = true;
               break;

#if WANG
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
#endif

#if DOS
            case status_file_not_found :
            case status_open_error :
               filename_exp->request_string(
                  status == status_file_not_found ? 20 : 61, txt_filename);
               if (filename_exp->is_bad())
                  in_error_state = true;
               break;
#endif

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

#if (! WANG) && DOS
   // Undo IO redirection
   if (redirect_std_in) {
      dup2(old_std_in, std_in);
      close(new_std_in);
      close(old_std_in);
   }
   if (redirect_std_out) {
      dup2(old_std_out, std_out);
      close(new_std_out);
      close(old_std_out);
   }

   if (type_ahead) {
      // Flush type-ahead buffer
      _AH = 0x0C;
      _AL = 0x00;
      geninterrupt(0x21);
   }
#endif

   delete filename_exp;

   if (in_error_state) {
#if WANG
      if (!(status == status_cancel || status == status_logoff))
#else
      if (status != status_cancel)
#endif
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
