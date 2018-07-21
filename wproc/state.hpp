//
//	Copyright (c) Shell Stream Software LLC, All Rights Reserved.
//
//	Project:	WPROC
//	
//// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : state.hpp
// Author : George Soules
// Date   : 18 February 1991

#ifndef STATEMGR__HPP
#define STATEMGR__HPP

// Classes
#include "stateobj.hpp"
class pcode;
class stack;
class symbol_table;

// Definitions and subprograms
// (none)


class state : public state_object {
   public:
      state();
      virtual ~state();
      virtual void save_state();
      virtual void restore_state();
      Boolean      save_state(char *a_filename);
      Boolean      restore_state(char *a_filename);
      pcode        *the_pcode;
      symbol_table *the_symbol_table;
      stack        *the_stack;
   private:
      void         link_states();
};

#endif


//
//	History:
//	$Log: state.hpp,v $
//	Revision 1.5  1998/08/31 19:14:17  gsl
//	drcs update
//	
//

//	
//	RCS file: /disk1/neomedia/RCS/wisp/wproc/state.hpp,v
//	Working file: state.hpp
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
//	date: 1995-04-25 06:00:25-04;  author: gsl;  state: V4_3_00;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:52:40-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:33:25-05;  author: gsl;  state: V3_3x12;  lines: +2 -2
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:28-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
