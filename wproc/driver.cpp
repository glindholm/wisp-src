//
//	Copyright (c) Shell Stream Software LLC, All Rights Reserved.
//
//	Project:	WPROC
//	
//// Copyright (c) Lexical Software, 1991.  All rights reserved.
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
   if (!WL_wbackground()) 
   {
	if (user_options.compile() || user_options.syntax_check())
	{
		/* Don't need to clear the screen if compiling or checking */
	}
	else
	{
	        vwang_title("WPROC - WISP Procedure Interpreter");
		clear_screen();
	}
   }
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
   {
	   char buff[20];
	   sprintf(buff,"%03d",rc);
	   SETRETCODE(buff);
   }
   wang_os_update_fetched_args();
#endif

   // Clean up
   delete the_args;
   delete the_input;
   delete the_process;
   user_options.options::~options();


#if WANG
   wang_os_decrement_link_level();
   wispexit_cleanup();

   if (logoff)
      LOGOFF();
#endif

   exit(rc);
   return 0;  // Never reached (avoids compiler warning)
}

//
//	History:
//	$Log: driver.cpp,v $
//	Revision 1.16  2003/07/03 18:54:55  gsl
//	Fix RETURN-CODE processing, call SETRETCODE before exiting
//	
//	Revision 1.15  2003/02/11 19:05:26  gsl
//	Remove unneeded #ifdef's for DEBUG
//	
//	Revision 1.14  2002/07/10 21:06:28  gsl
//	Fix globals WL_ to make unique
//	
//	Revision 1.13  1999/08/29 17:19:37  gsl
//	Move the vwang_title() call out of wang_os_init() so it doesn't get
//	called while in CoSTAR if just checking the version.
//	
//	Revision 1.12  1999-01-29 18:58:33-05  gsl
//	Add decrement link level before call to wispexit_cleanup().
//
//	Revision 1.11  1998-09-28 17:08:09-04  gsl
//	If compiling or syntax checking then don't go into full screen IO.
//	Just use the stdout IO.
//	THis allows you to redirect errors to a file.
//
//	Revision 1.10  1998-08-31 15:13:40-04  gsl
//	drcs update
//
//

//	
//
//	Working file: driver.cpp
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
//	date: 1997-06-10 01:27:15-04;  author: scass;  state: V4_3_00;  lines: +1 -1
//	Removed #else if so is #else, to remove warning.
//	----------------------------
//	revision 1.8
//	date: 1996-11-11 20:13:14-05;  author: gsl;  state: V3_3_93;  lines: +2 -0
//	Add call to wispexit_cleanup() before exit
//	----------------------------
//	revision 1.7
//	date: 1996-07-25 19:45:48-04;  author: gsl;  state: Exp;  lines: +6 -11
//	NT
//	----------------------------
//	revision 1.6
//	date: 1996-07-25 14:14:37-04;  author: gsl;  state: Exp;  lines: +0 -0
//	Renamed from driver.cc to driver.cpp
//	----------------------------
//	revision 1.5
//	date: 1995-06-02 09:00:05-04;  author: gsl;  state: V3_3_19;  lines: +10 -0
//	Add test to ensure char's are signed.
//	----------------------------
//	revision 1.4
//	date: 1995-04-25 05:59:48-04;  author: gsl;  state: V3_3_16;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:52:05-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:32:44-05;  author: gsl;  state: V3_3x12;  lines: +2 -2
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:03-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
