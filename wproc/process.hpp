// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : process.hpp
// Author : George Soules
// Date   : 24 February 1991

#ifndef PROCESS__HPP
#define PROCESS__HPP

// Classes
#include "object.hpp"
#include "symbols.hpp"

// Definitions and subprograms
// (none)


class process : public object {
   public:
      process();
      process(process &a_process);
      ~process();
      void set_top_filename(const char *path);
      void prepare_for_link();
      void load_from_env();

      int           nesting_level;
      int           base_level;		// The starting nesting_level from previous wproc
      int           trace_level;
      Boolean       trace_active;
      char         *the_top_filename;	// The highest level procedure name
      char         *the_parent_pathname;
      char         *the_input_pathname;
      char         *the_source_pathname;
      int_32        the_source_timestamp;
      symbol_table *global_symbol_table;
      Boolean       cursor_off;
};

extern process *the_process;

#endif

