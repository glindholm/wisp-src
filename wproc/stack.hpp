//
//	Copyright (c) 1996-1998 NeoMedia Technologies Inc. All rights reserved.
//
//	Project:	WPROC
//	Id:		$Id:$
//	RCS:		$Source:$
//	
//// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : stack.hpp
// Author : George Soules
// Date   : 7 March 1991

#ifndef STACK__HPP
#define STACK__HPP

// Classes
#include "exp.hpp"
#include "object.hpp"

// Definitions and subprograms
#include "environ.hpp"


class stack_entry : public object {
   public:
      stack_entry();
      ~stack_entry();
      expression  *the_exp;
      stack_entry *next;
      stack_entry *prev;
   private:
      int id;
};


class stack : public object {
   public:
      stack();
      ~stack();
      void push(expression *an_exp);
      expression *pop();
      expression *top(int offset = 0);
   private:
      stack_entry *the_top;
      stack_entry *the_bottom;
      int          entry_count;
      int          id;
};

#endif


//
//	History:
//	$Log: stack.hpp,v $
//	Revision 1.6  1998/09/03 12:21:12  gsl
//	Add an "id" field to stack and stack_entry
//	
//	Revision 1.5  1998-08-31 15:14:16-04  gsl
//	drcs update
//
//

//	
//	RCS file: /disk1/neomedia/RCS/wisp/wproc/stack.hpp,v
//	Working file: stack.hpp
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
//	date: 1995-04-25 06:00:24-04;  author: gsl;  state: V4_3_00;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:52:40-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:33:24-05;  author: gsl;  state: V3_3x12;  lines: +0 -0
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:27-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
