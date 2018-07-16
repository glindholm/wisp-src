// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : args.cpp
// Author : George Soules
// Date   : 8 July 1991

// Specification
#include "args.hpp"

// Classes
// (none)

// Definitions and subprograms
#include <string.h>
#include "debugaid.hpp"
#include "memory.hpp"
#include "utility.hpp"


arguments::arguments(char **vector) {
   trace(object, "arguments (initialized)");
   the_values = vector;
   the_dimension = 1; // Count terminating NULL first
   while (*the_values++)
      the_dimension += 1;
   the_values = vector;
#if WANG
   the_values_kind = NULL;
   formals         = 0;
#endif
   the_expressions = NULL;
   own_vector      = false;
}


arguments::arguments(int arg_count) {
   trace(object, "arguments (uninitialized)");
   the_dimension      = arg_count + 2; // Args + Name + terminating NULL
   the_values         = new char*[the_dimension];
   the_expressions    = new expression*[the_dimension];
#if WANG
   the_values_kind    = new expression::exp_kind[the_dimension];
   formals            = 0;
#endif
   the_values[0]      = ""; // needed for Spawn()
   the_expressions[0] = NULL;
   for (int i = 1; i < the_dimension; i++) {
      the_values[i] = NULL;
#if WANG
      the_values_kind[i] = expression::string_kind;
#endif
      the_expressions[i] = NULL;
   }
   own_vector = true;
}


arguments::~arguments() {
   trace(object, "~arguments");
   if (own_vector) {
      int i = 1;
      while (the_values[i]) {
         delete_string(the_values[i]);
         if (the_expressions && the_expressions[i])
            delete the_expressions[i];
         i += 1;
      }
      delete the_values;
#if WANG
      if (the_values_kind)
         delete the_values_kind;
#endif
      if (the_expressions)
         delete the_expressions;
   }
}

#if WANG
expression::exp_kind arguments::value_kind(int index) {
   assert(index >= 1 && index < the_dimension - 1);
   assert(the_values_kind);
   return the_values_kind[index];
}


void arguments::set(
   int                  index,
   const char          *a_value,
   expression::exp_kind a_kind)
{
   assert(index >= 1 && index < the_dimension - 1);
   assert(own_vector);
   the_values[index]      = dup_string(a_value);
   the_expressions[index] = NULL;
   the_values_kind[index] = a_kind;
}


void arguments::raw_set(int index, const char *a_value) {
   assert(index >= 1 && index < the_dimension - 1);
   assert(own_vector);
   the_values[index] = (char *) a_value;
   the_expressions[index] = NULL;
   the_values_kind[index] = expression::undefined_kind;
}


int arguments::formals_count() {
   return formals;
}


void arguments::increment_formals_count(int by) {
   formals += by;
}

#else

void arguments::set(int index, const char *a_value) {
   assert(index >= 1 && index < the_dimension - 1);
   assert(own_vector);
   the_values[index]      = dup_string(a_value);
   the_expressions[index] = NULL;
}
#endif


void arguments::set(int index, expression &an_exp) {
   assert(index >= 1 && index < the_dimension - 1);
   assert(own_vector);
   assert(the_expressions);
   the_expressions[index] = new expression(an_exp);
   the_values[index]      = dup_string(an_exp.string());
}


const char *arguments::value(int index) {
   assert(index >= 1 && index < the_dimension - 1);
   return the_values[index];
}


expression *arguments::exp(int index) {
   assert(index >= 1 && index < the_dimension - 1);
   return the_expressions ? the_expressions[index] : (expression*)NULL;
}


char **arguments::vector() {
   return the_values;
}


char *arguments::command(const char *name) {
   char   buffer[128];
   int    index = 0;
   int    len;
   char **arg = &the_values[1];

   strcpy(buffer, name);
   index = strlen(name);
   buffer[index++] = ' ';

   while (*arg) {
      len = min(strlen(*arg), sizeof(buffer) - index - 1);
      strncpy(buffer+index, *arg, len);
      index += len;
      if (++arg && (index < (int)sizeof(buffer) - 1))
         buffer[index++] = ' ';
   }
   buffer[index] = '\0';
   return dup_string(buffer);
}


int arguments::count() {
   return the_dimension - 2;
}



