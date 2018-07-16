//
//	Copyright (c) 1996-1998 NeoMedia Technologies Inc. All rights reserved.
//
//	Project:	WPROC
//	Id:		$Id:$
//	RCS:		$Source:$
//	
//// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : options.hpp
// Author : George Soules
// Date   : 9 February 1991

#ifndef OPTIONS__HPP
#define OPTIONS__HPP

// Classes
#include "object.hpp"

// Definitions and subprograms
#include "environ.hpp"


class options : public object {
   public:
      options();
      ~options();
      void process_command_line(int &arg, int argc, char *argv[]);
      Boolean compile()             {return option[compile_opt];}
      Boolean syntax_check()        {return option[syntax_opt];}
      Boolean trace_execution()     {return option[trace_opt];}
#if WANG
      Boolean fetched_args()        {return option[fetched_args_opt];}
#endif
      Boolean debug_trace_always()  {return option[debug_always_opt];}
      Boolean debug_trace_emitter() {return option[debug_emitter_opt];}
      Boolean debug_trace_general() {return option[debug_general_opt];}
      Boolean debug_trace_heap()    {return option[debug_heap_opt];}
      Boolean debug_trace_machine() {return option[debug_machine_opt];}
      Boolean debug_trace_memory()  {return option[debug_memory_opt];}
      Boolean debug_trace_object()  {return option[debug_object_opt];}
      Boolean debug_trace_parser()  {return option[debug_parser_opt];}
      Boolean debug_trace_scanner() {return option[debug_scanner_opt];}
      Boolean debug_trace_stack()   {return option[debug_stack_opt];}
      Boolean debug_trace_symbols() {return option[debug_symbols_opt];}
   private:
      enum option_kind {
         compile_opt,
         syntax_opt,
         trace_opt,
#if WANG
         fetched_args_opt,
#endif
         debug_always_opt,
         debug_emitter_opt,
         debug_general_opt,
         debug_heap_opt,
         debug_machine_opt,
         debug_memory_opt,
         debug_object_opt,
         debug_parser_opt,
         debug_scanner_opt,
         debug_stack_opt,
         debug_symbols_opt,
         last_opt
      };
      Boolean option[last_opt];
};

extern options user_options;

#endif


//
//	History:
//	$Log: options.hpp,v $
//	Revision 1.5  1998-08-31 15:14:00-04  gsl
//	drcs update
//
//

//	
//	RCS file: /disk1/neomedia/RCS/wisp/wproc/options.hpp,v
//	Working file: options.hpp
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
//	date: 1995-04-25 06:00:08-04;  author: gsl;  state: V4_3_00;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:52:25-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:33:06-05;  author: gsl;  state: V3_3x12;  lines: +16 -16
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:16-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
