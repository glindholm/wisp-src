// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : scope.hpp
// Author : George Soules
// Date   : 19 July 1991

#ifndef SCOPE__HPP
#define SCOPE__HPP

// Classes
#include "object.hpp"

// Definitions and subprograms
#include "environ.hpp"


typedef int_16 block;
const block outermost_block = 1;

class scope : public object {
   public:
      scope(int size);
      scope(scope &a_scope);
      ~scope();
      Boolean enter(Boolean restricted = false);
      void    exit();
      block   current_block();
      Boolean can_see(block a_block);
      Boolean can_go_to(block a_block);
   private:
      block *the_stack;
      int    the_top;
      int    entry_count;
      block  active_block;
      int    block_count;
};

#endif

