//
//	Copyright (c) Shell Stream Software LLC, All Rights Reserved.
//
//	Project:	WPROC
//	
//// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : process.hpp
// Author : George Soules
// Date   : 24 February 1991

#ifndef PROCESS__HPP
#define PROCESS__HPP

// Classes
#include "object.hpp"
#include "symbols.hpp"

// Definitions and subprograms
// (none)


class process : public object {
   public:
      process();
      process(process &a_process);
      ~process();
      void set_top_filename(const char *path);
      void prepare_for_link();
      void load_from_env();

      int           nesting_level;
      int           base_level;		// The starting nesting_level from previous wproc
      int           trace_level;
      Boolean       trace_active;
      char         *the_top_filename;	// The highest level procedure name
      char         *the_parent_pathname;
      char         *the_input_pathname;
      char         *the_source_pathname;
      int_32        the_source_timestamp;
      symbol_table *global_symbol_table;
      Boolean       cursor_off;
};

extern process *the_process;

#endif


//
//	History:
//	$Log: process.hpp,v $
//	Revision 1.5  1998/08/31 19:14:08  gsl
//	drcs update
//	
//

//	
//
//	Working file: process.hpp
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
//	date: 1995-04-25 06:00:16-04;  author: gsl;  state: V4_3_00;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:52:32-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:33:15-05;  author: gsl;  state: V3_3x12;  lines: +9 -2
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:21-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
