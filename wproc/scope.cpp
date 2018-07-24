//
//	Copyright (c) Shell Stream Software LLC, All Rights Reserved.
//
//	Project:	WPROC
//	
//// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : scope.cpp
// Author : George Soules
// Date   : 19 July 1991

// Specification
#include "scope.hpp"

// Classes
// (none)

// Definitions and subprograms
#include "debugaid.hpp"


scope::scope(int size) {
   entry_count  = size;
   the_stack    = new block[entry_count];
   the_top      = -1;
   active_block = outermost_block;
   block_count  = outermost_block;
}


scope::scope(scope &a_scope) {
   entry_count  = a_scope.entry_count;
   the_stack    = new block[entry_count];
   the_top      = a_scope.the_top;
   active_block = a_scope.active_block;
   block_count  = a_scope.block_count;
   for (int i = 0; i < entry_count; i++)
      the_stack[i] = a_scope.the_stack[i];
}


scope::~scope() {
   delete the_stack;
}


Boolean scope::enter(Boolean restricted) {
   if (the_top < entry_count - 1) {
      the_top += 1;
      the_stack[the_top] = active_block;
      if (restricted)
         // Restrict GOTOs to scopes between top and first negative block
         the_stack[the_top] *= -1;
      block_count += 1;
      active_block = block_count;
      trace_si(parser, "enter block ", active_block);
      return true;
   }
   else
      return false;
}


void scope::exit() {
   assert(the_top >= 0);
   active_block = abs(the_stack[the_top]);
   the_top -= 1;
   trace_si(parser, "back to block ", active_block);
}


block scope::current_block() {
   return active_block;
}


Boolean scope::can_see(block a_block) {
   if (a_block == active_block)
      return true;
   for (int i = the_top; i >= 0; i--) {
      if (abs(the_stack[i]) == a_block)
         return true;
   }
   return false;
}


Boolean scope::can_go_to(block a_block) {
   if (a_block == active_block)
      return true;
   for (int i = the_top; i >= 0; i--) {
      if (the_stack[i] == a_block)
         return true;
      if (the_stack[i] < 0)
         return false;
   }
   return false;
}

//
//	History:
//	$Log: scope.cpp,v $
//	Revision 1.6  1998/08/31 19:14:13  gsl
//	drcs update
//	
//

//	
//
//	Working file: scope.cpp
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
//	date: 1996-07-25 14:16:13-04;  author: gsl;  state: V4_3_00;  lines: +0 -0
//	Renamed from scope.cc to scope.cpp
//	----------------------------
//	revision 1.4
//	date: 1995-04-25 06:00:21-04;  author: gsl;  state: V3_3_19;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:52:37-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:33:21-05;  author: gsl;  state: V3_3x12;  lines: +3 -3
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:25-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
