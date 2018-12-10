//
//	Copyright (c) Shell Stream Software LLC, All Rights Reserved.
//
//	Project:	WPROC
//	
//// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : stack.cpp
// Author : George Soules
// Date   : 7 March 1991

// Specification
#include "stack.hpp"

// Classes
// (none)

// Definitions and subprograms
#include "debugaid.hpp"
#include "memory.hpp"
#include "report.hpp"

static int g_stack_id = 0;
static int g_stack_entry_id = 0;

stack_entry::stack_entry()
{
   trace_begin(object, "stack_entry");
   id = ++g_stack_entry_id;
   the_exp = NULL;
   next = NULL;
   prev = NULL;
   trace_si(stack, "create stack_entry ", id);
   trace_end(object);
}
stack_entry::~stack_entry()
{
   trace_begin(object, "~stack_entry");
   trace_si(stack, "destroy stack_entry ", id);
   trace_end(object);
}

stack::stack() {
   trace_begin(object, "stack");
   the_bottom = new stack_entry;
   the_bottom->prev = NULL;
   the_bottom->next = NULL;
   the_bottom->the_exp = NULL;
   the_top = the_bottom;
   entry_count = 0;
   id = ++g_stack_id;
   trace_si(stack, "Create stack ", id);
   trace_end(object);
}


stack::~stack() {
   trace(object, "~stack");
   stack_entry *prev;
   while (the_bottom) {
      prev = the_bottom->prev;
      if (the_bottom->the_exp)
         delete the_bottom->the_exp;
      delete the_bottom;
      the_bottom = prev;
   }
   trace_si(stack, "destroy stack ", id);
}


void stack::push(expression *an_exp) {
   entry_count += 1;
   trace_si(stack, "push entry ", entry_count);
   if (the_top->prev)
      the_top = the_top->prev;
   else {
      the_top->prev = new stack_entry;
      the_top->prev->next = the_top;
      the_top = the_top->prev;
      the_top->prev = NULL;
   }
   the_top->the_exp = an_exp;
}


expression *stack::pop() {
   trace_si(stack, "pop entry  ", entry_count);
   entry_count -= 1;
   assert(the_top != the_bottom);
   expression* the_exp = the_top->the_exp;
   the_top->the_exp = NULL;
   the_top = the_top->next;
   return the_exp;
}


expression *stack::top(int offset) {
   // The offset is used to get to top + n
   assert(the_top != the_bottom);
   stack_entry *this_entry = the_top;
   for (int i = 0; i < offset; i++) {
      this_entry = this_entry->next;
      assert(this_entry);
   }
  return this_entry->the_exp;
}






//
//	History:
//	$Log: stack.cpp,v $
//	Revision 1.7  1998/09/03 12:28:11  gsl
//	Enhanced the stack trace logic to print the id's
//	
//	Revision 1.6  1998-08-31 15:14:16-04  gsl
//	drcs update
//
//

//	
//
//	Working file: stack.cpp
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
//	date: 1996-07-25 14:16:18-04;  author: gsl;  state: V4_3_00;  lines: +0 -0
//	Renamed from stack.cc to stack.cpp
//	----------------------------
//	revision 1.4
//	date: 1995-04-25 06:00:24-04;  author: gsl;  state: V3_3_19;  lines: +0 -0
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
