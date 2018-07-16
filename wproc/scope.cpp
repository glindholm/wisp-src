// Copyright (c) Lexical Software, 1991.  All rights reserved.
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
