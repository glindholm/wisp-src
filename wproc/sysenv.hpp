// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : sysenv.hpp
// Author : George Soules
// Date   : 27 June 1991

#ifndef SYSENV__HPP
#define SYSENV__HPP

// Classes
#include "object.hpp"

// Definitions and subprograms
// (none)


class system_environment : public object {
   public:
      system_environment();
      ~system_environment();
      int         set(const char* a_name, const char* a_value);
      char       *get(const char* a_name);
      void        restore();
   private:
      void        create_vector();
      int         lookup(const char* a_name);
      char      **the_vector;
      int         entry_count;
      int         entries_allocated;
};


#endif

