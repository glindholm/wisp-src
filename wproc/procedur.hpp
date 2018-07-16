// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : procedur.hpp
// Author : George Soules
// Date   : 13 March 1991

#ifndef PROCEDUR__HPP
#define PROCEDUR__HPP

// Classes
#include "args.hpp"
#include "compiler.hpp"
#include "object.hpp"
#include "machine.hpp"
#include "pcode.hpp"
#include "state.hpp"

// Definitions and subprograms
#include "status.hpp"


class procedure : public object {
   public:
      procedure(char *a_name, arguments *args);
      virtual ~procedure();
      status_kind      status()      {return the_status;};
      int_32           return_code() {return the_return_code;};
      virtual void     interpret() = 0;
   protected:
      status_kind      the_status;
      int_32           the_return_code;
      char            *the_name;
      arguments       *the_args;
      state           *the_state;
      machine         *the_machine;
      pcode_reader    *the_pcode_reader;

      void             create_machine();
      void             destroy_machine();
};

#if RUNTIME
#else
class compilable_procedure : public procedure {
   public:
      compilable_procedure(
         reader    *a_source_reader,
         char      *a_name,
         arguments *args);
      ~compilable_procedure();
      void interpret();
   private:
      compiler      *the_compiler;
      pcode_emitter *the_emitter;
      void compile();
      void compile_and_execute();
};
#endif

class executable_procedure : public procedure {
   public:
      executable_procedure(char *a_name, arguments *args);
      ~executable_procedure();
      void interpret();
};

#endif

