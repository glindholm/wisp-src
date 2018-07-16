// Copyright (c) Lexical Software, 1991.  All rights reserved.
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
      expression  *the_exp;
      stack_entry *next;
      stack_entry *prev;
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
};

#endif

