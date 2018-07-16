// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : args.hpp
// Author : George Soules
// Date   : 8 July 1991

#ifndef ARGS__HPP
#define ARGS__HPP

// Classes
#include "exp.hpp"
#include "object.hpp"

// Definitions and subprograms
#include "environ.hpp"


class arguments : public object {
   public:
      arguments(char **vector);
      arguments(int arg_count);

      ~arguments();

#if WANG
      expression::exp_kind value_kind(int index);
      void set(
         int                  index,
         const char          *a_value,
         expression::exp_kind a_kind = expression::string_kind);
      void        raw_set(int index, const char *a_value);
      int         formals_count();
      void        increment_formals_count(int by = 1);
#else
      void        set(int index, const char *a_value);
#endif
      void        set(int index, expression &an_exp);
      const char *value(int index);
      expression *exp(int index);
      char      **vector();
      char       *command(const char *name);
      int         count();

   private:
      char       **the_values;
#if WANG
      expression::exp_kind *the_values_kind;
      int          formals;
#endif
      expression **the_expressions;
      int          the_dimension;
      Boolean      own_vector;
};


#endif

