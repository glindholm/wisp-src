// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : driver.cpp
// Author : George Soules
// Date   : 2 February 1991

// Specification
// (none)

// Classes
#include "input.hpp"
#include "options.hpp"
#include "process.hpp"

// Definitions and subprograms
#if DOS || DOS_HOST
#include <alloc.h>
#include <dos.h>
#endif
#include <signal.h>
#include <stdio.h>
#include <string.h>
#include "cancel.hpp"
#include "crt_io.hpp"
#include "debugaid.hpp"
#include "environ.hpp"
#include "ext.hpp"
#include "install.hpp"
#include "memcheck.hpp"
#include "memory.hpp"
#include "report.hpp"
#include "status.hpp"
#include "utility.hpp"
#if WANG
#include "wang_os.hpp"
#include "wisp_rts.h"  // Only included for LOGOFF()
#endif

#if DOS
// Set stack to 8K (default is 4K)
extern unsigned _stklen = 8192U;
#endif

int main(int argc, char *argv[]) {
   save_memory_state();

   int        argv_index = 1;
   input     *the_input;
   arguments *the_args;
   int        rc = 1;
   int_8      test_signed_int_8;
#if WANG
   Boolean    logoff = false;
#endif

   /*
      If the following generates a error then chars are not signed
      and things will not work.
      
      CHARS MUST BE SIGNED !!!!
   */
   test_signed_int_8 = -1;
   assert(-1 == test_signed_int_8);
   
#if DOS
   if (_osmajor < 3)
      report_fatal_error(9);
#endif

#if WANG
   wang_os_init();
#endif

   the_process = new process();

   if (! installation_ok()) {
      report_general_error(5);
      exit(0);
   }

   // Process user options and validate with respect to kind of input
   user_options.process_command_line(argv_index, argc, argv);
   if (argv_index >= argc)
      report_fatal_error(14); // No file name

   // Create the arguments
#if WANG
   if (user_options.fetched_args())
      the_args = wang_os_fetched_args_for(argv[argv_index]);
   else
      the_args = new arguments(&argv[argv_index]);
#else
   the_args = new arguments(&argv[argv_index]);
#endif
   the_input = create_input_object(argv[argv_index], the_args);
   if (the_input->status() == status_memory_error)
      report_fatal_error(7);

#if WANG
   // Initialize full screen IO services
   if (!wbackground()) clear_screen();
#endif

   // Set up handler to intercept Ctrl-c
   set_cancel_handler();

   // Run the input and evaluate the result
   the_input->run();

   int general_error = 0;
   switch (the_input->status()) {
      case status_ok :
         rc = the_input->return_code();
         break;
      case status_runtime_error :
         break;
      case status_cancel :
         general_error = 2;
         break;
      case status_destroy :
         rc = the_input->destroy();
         break;
      case status_file_not_found :
         general_error = 4;
         break;
      case status_open_error :
         general_error = 3;
         break;
      case status_memory_error :
         general_error = 1;
         break;
#if RUNTIME
      case status_source_input :
         general_error = 6;
         break;
#endif
#if WANG
      case status_logoff :
         logoff = true;
         break;
#endif
      default:
         assert(UNREACHABLE);
         break;
   }
   if (general_error)
#if DOS
      report_general_error(general_error, upper_case(the_input->name()));
#else
#if WANG
      report_general_error(general_error, the_input->name());
#endif
#endif

   trace_si(general, "driver return code = ", rc);

   restore_video_state();

#if WANG
   wang_os_update_fetched_args();
#endif

   // Clean up
   delete the_args;
   delete the_input;
   delete the_process;
   user_options.options::~options();

   check_memory_state();

#if WANG
   wispexit_cleanup();

   if (logoff)
      LOGOFF();
#endif

   exit(rc);
   return 0;  // Never reached (avoids compiler warning)
}
