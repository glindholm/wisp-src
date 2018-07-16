// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : stateobj.hpp
// Author : George Soules
// Date   : 21 March 1991

#ifndef STATEOBJ__HPP
#define STATEOBJ__HPP

// Classes
#include "object.hpp"

// Definitions and subprograms
#include "environ.hpp"
#include <stdio.h>


typedef FILE state_file;

class state_object : public object {
   public:
      state_object();
      virtual void save_state() = 0;
      virtual void restore_state() = 0;
      state_object *next_state_object;
   protected:
      enum state_mode {save_mode, restore_mode};
      static Boolean open_state_file(const char *a_filename, state_mode a_mode);
      static void    close_state_file();
      void           save(const void *ptr, size_t size);
      void           save(const char *a_string);
      void           restore(void *ptr, size_t size);
      void           restore(char *&a_string);
   private:
      static state_file *the_file;
};

#endif

