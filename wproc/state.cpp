//
//	Copyright (c) 1996-1998 NeoMedia Technologies Inc. All rights reserved.
//
//	Project:	WPROC
//	Id:		$Id:$
//	RCS:		$Source:$
//	
//// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : state.cpp
// Author : George Soules
// Date   : 18 February 1991

// Specification
#include "state.hpp"

// Classes
#include "pcode.hpp"
#include "process.hpp"
#include "stack.hpp"
#include "symbols.hpp"

// Definitions and subprograms
#include "debugaid.hpp"
#include "product.hpp"
#include "report.hpp"


usign_16 version;

state::state() {
   trace_begin(object, "state");
   the_pcode        = new pcode();
   the_symbol_table = new symbol_table();
   the_stack        = new stack();
   link_states();
   trace_end(object);
}


state::~state() {
   delete the_pcode;
   delete the_symbol_table;
   delete the_stack;
   trace(object, "~state");
}


void state::link_states() {
   this->next_state_object      = the_pcode;
   the_pcode->next_state_object = the_symbol_table;
}


void state::save_state() {
   trace_begin(object, "saving state");

   version = product_version();
   save(&version, sizeof(version));

   // Save each state object
   state_object *next_object = this->next_state_object;
   while (next_object) {
      next_object->save_state();
      next_object = next_object->next_state_object;
   }
   state_object::close_state_file();
   trace_end(object);
}


void state::restore_state() {
   trace_begin(object, "restoring state");

   restore(&version, sizeof(version));
   usign_16 this_version = product_version();
   if (version > this_version || ((version & 0x0F00) != (this_version & 0x0F00)))
      report_fatal_error(10);

   // Restore each state object
   state_object *next_object = this->next_state_object;
   while (next_object) {
      next_object->restore_state();
      next_object = next_object->next_state_object;
   }
   state_object::close_state_file();
   trace_end(object);
}


Boolean state::save_state(char *a_filename) {
   if (state_object::open_state_file(a_filename, state_object::save_mode)) {
      save_state();
      return true;
   }
   else
      return false;
}


Boolean state::restore_state(char *a_filename) {
   if (state_object::open_state_file(a_filename, state_object::restore_mode)) {
      restore_state();
      return true;
   }
   else
      return false;
}

//
//	History:
//	$Log: state.cpp,v $
//	Revision 1.6  1998/08/31 19:14:16  gsl
//	drcs update
//	
//

//	
//	RCS file: /disk1/neomedia/RCS/wisp/wproc/state.cpp,v
//	Working file: state.cpp
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
//	date: 1996-07-25 14:16:20-04;  author: gsl;  state: V4_3_00;  lines: +0 -0
//	Renamed from state.cc to state.cpp
//	----------------------------
//	revision 1.4
//	date: 1995-04-25 06:00:25-04;  author: gsl;  state: V3_3_19;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:52:40-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:33:24-05;  author: gsl;  state: V3_3x12;  lines: +2 -2
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:27-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
