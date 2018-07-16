// Copyright (c) Lexical Software, 1991.  All rights reserved.
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


stack::stack() {
   trace_begin(object, "stack");
   the_bottom = new stack_entry;
   the_bottom->prev = NULL;
   the_bottom->next = NULL;
   the_bottom->the_exp = NULL;
   the_top = the_bottom;
   entry_count = 0;
   trace_end(object);
}


stack::~stack() {
   trace(object, "~stack");
   stack_entry *prev;
   while (the_bottom) {
      trace_si(stack, "delete entry ", entry_count--);
      prev = the_bottom->prev;
      if (the_bottom->the_exp)
         delete the_bottom->the_exp;
      delete the_bottom;
      the_bottom = prev;
   }
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





