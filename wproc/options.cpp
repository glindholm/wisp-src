//
//	Copyright (c) 1996-1998 NeoMedia Technologies Inc. All rights reserved.
//
//	Project:	WPROC
//	Id:		$Id:$
//	RCS:		$Source:$
//	
//// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : options.cpp
// Author : George Soules
// Date   : 9 February 1991

// Specification
#include "options.hpp"

// Classes
// (none)

// Definitions and subprograms
#include <ctype.h>
#if DOS || DOS_HOST
#include <dos.h>
#include <io.h>
#include <sys\stat.h>
#else
#if UNIX
#include <sys/types.h>
#endif
#endif
#include <limits.h>
#include <string.h>
#include "crt_io.hpp"
#include "debugaid.hpp"
#include "install.hpp"
#include "memory.hpp"
#include "process.hpp"
#include "product.hpp"
#include "report.hpp"


// This is the only place that actual option values are
// defined.  They should never be hard-coded anywhere else.

const char compile_letter       = 'c';
const char syntax_letter        = 's';
const char trace_letter         = 't';
const char version_letter       = 'v';
#if WANG
const char fetched_args_letter  = 'p';
#endif
const char debug_letter         = 'd';
const char debug_emitter_letter = 'e';
const char debug_general_letter = 'g';
const char debug_heap_letter    = 'h';
const char debug_machine_letter = 'x';
const char debug_memory_letter  = 'm';
const char debug_object_letter  = 'o';
const char debug_parser_letter  = 'p';
const char debug_scanner_letter = 's';
const char debug_stack_letter   = 'k';
const char debug_symbols_letter = 'y';
const char debug_all_letter     = 'a';
const char debug_zero_letter    = '0';   // Smart debug trace

options  user_options;
usign_8  saved;
usign_16 option_bits;

void show_usage();


options::options() {
   for (int i = 0; i < last_opt; i++)
      option[i] = false;
   option[debug_always_opt] = true;
}


options::~options() {
}


void options::process_command_line(int &arg, int argc, char *argv[]) {
   char    the_opt;
   Boolean bad_opt = false;
   Boolean opt_specified = false;
   int     index;
   char    sub[2];

   if (arg >= argc) {
      // No arguments were specified
      show_usage();
      exit(0);
   }

   while (arg <= argc && argv[arg][0] == '-') {
      index = 1;

      switch (the_opt = tolower(argv[arg][index])) {
#if ! RUNTIME
         case compile_letter :
            if (opt_specified) report_fatal_error(17);
            option[compile_opt] = true;
            opt_specified = true;
            break;

         case syntax_letter :
            if (opt_specified) report_fatal_error(17);
            option[syntax_opt] = true;
            option[compile_opt] = true;
            opt_specified = true;
            break;
#endif
         case trace_letter :
            if (opt_specified) report_fatal_error(17);
            the_process->trace_active = true;
	    the_process->trace_level = 0;
            option[trace_opt] = true;
            opt_specified = true;
            break;
         case version_letter :
	    write_stdout(product_copyright());
	    exit(0);
            break;
#if WANG
         case fetched_args_letter :
            option[fetched_args_opt] = true;
            break;
#endif
         case debug_letter :
            while('\0' != argv[arg][index+1])
	    {
               index += 1;
               switch (argv[arg][index]) {
               case debug_emitter_letter :
                  option[debug_emitter_opt] = true;
                  break;
               case debug_general_letter :
                  option[debug_general_opt] = true;
                  break;
               case debug_heap_letter :
                  option[debug_heap_opt] = true;
                  break;
               case debug_machine_letter :
                  option[debug_machine_opt] = true;
                  break;
               case debug_memory_letter :
                  option[debug_memory_opt] = true;
                  break;
               case debug_object_letter :
                  option[debug_object_opt] = true;
                  break;
               case debug_parser_letter :
                  option[debug_parser_opt] = true;
                  break;
               case debug_scanner_letter :
                  option[debug_scanner_opt] = true;
                  break;
               case debug_stack_letter :
                  option[debug_stack_opt] = true;
                  break;
               case debug_symbols_letter :
                  option[debug_symbols_opt] = true;
                  break;
               case debug_all_letter :
                  option[debug_emitter_opt] = true;
                  option[debug_general_opt] = true;
                  option[debug_heap_opt] = true;
                  option[debug_machine_opt] = true;
                  option[debug_memory_opt] = true;
                  option[debug_object_opt] = true;
                  option[debug_parser_opt] = true;
                  option[debug_scanner_opt] = true;
                  option[debug_stack_opt] = true;
                  option[debug_symbols_opt] = true;
                  break;
               case debug_zero_letter :
                  option[debug_emitter_opt] = true;
                  option[debug_general_opt] = true;
                  option[debug_machine_opt] = true;
                  option[debug_object_opt] = true;
                  option[debug_scanner_opt] = true;
                  option[debug_symbols_opt] = true;
                  break;
               default :
                  bad_opt = true;
               }
	    }
            break;
         default :
            bad_opt = true;
      }
      sub[0] = the_opt;
      sub[1] = '\0';
      if (bad_opt) {
         int error = 18;
#if RUNTIME
         if (the_opt == compile_letter || the_opt == syntax_letter)
            error = 20;
#endif
         report_fatal_error(error, sub);
      }
      else if (argv[arg][++index] != '\0')
         report_fatal_error(16, sub);
      else
         arg += 1;
   }
}


void show_usage() {
#if DEMO
   write_stdout(demo_notice());
   write_stdout("\n");
#endif
   write_stdout(product_copyright());
#if RUNTIME
   write_stdout("\nUsage: wproc [-t] file [parameters]\n\n");
#else
   write_stdout("\nUsage: wproc [option] file [parameters]\n\n");
   write_stdout("Options:\n");
   write_stdout("   -s = Syntax check\n");
   write_stdout("   -c = Compile\n");
   write_stdout("   -t = Trace execution\n");
   write_stdout("   -v = Version numbers\n");
#endif
}

//
//	History:
//	$Log: options.cpp,v $
//	Revision 1.10.2.1  2003/02/11 18:52:00  gsl
//	Removed unneeded #ifdef code for AIX and DEBUG
//	
//	Revision 1.10  1998/09/08 18:50:22  gsl
//	Fix the "only one options" allowed logic to not count debug
//	
//	Revision 1.9  1998-09-02 17:28:59-04  gsl
//	changed processing of the debug (-d) switch to allow multiple flags.
//	you can now say -dkos instead of -dk -do -ds this was needed to
//	handle passing multiple debug flags via WPROCDEBUG envvar.
//
//	Revision 1.8  1998-08-31 15:14:00-04  gsl
//	drcs update
//
//

//	
//	RCS file: /disk1/neomedia/RCS/wisp/wproc/options.cpp,v
//	Working file: options.cpp
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
//	date: 1996-07-25 14:15:35-04;  author: gsl;  state: V4_3_00;  lines: +0 -0
//	Renamed from options.cc to options.cpp
//	----------------------------
//	revision 1.6
//	date: 1995-08-09 12:56:24-04;  author: gsl;  state: V3_3_19;  lines: +9 -0
//	Add a new debug option -d0 which uses a "smart" set of trace options.
//	----------------------------
//	revision 1.5
//	date: 1995-06-15 06:44:37-04;  author: gsl;  state: V3_3_18;  lines: +8 -2
//	Add -v to report version numbers
//	----------------------------
//	revision 1.4
//	date: 1995-04-25 06:00:08-04;  author: gsl;  state: V3_3_16;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:52:25-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:33:05-05;  author: gsl;  state: V3_3x12;  lines: +18 -4
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:16-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
