// Copyright (c) Lexical Software, 1991.  All rights reserved.
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

