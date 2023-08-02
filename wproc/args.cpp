//
//	Copyright (c) Shell Stream Software LLC, All Rights Reserved.
//
//	Project:	WPROC
//	
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




//
//	History:
//	$Log: args.cpp,v $
//	Revision 1.7  1998/08/31 19:12:08  gsl
//	Add log
//	
//

//	
//
//	Working file: args.cpp
//	head: 1.6
//	branch:
//	locks: strict
//	access list:
//		gsl
//		scass
//		ljn
//		jockc
//		jlima
//	symbolic names:
//	keyword substitution: kv
//	total revisions: 6;	selected revisions: 6
//	description:
//	----------------------------
//	revision 1.6
//	date: 1997-10-01 08:14:56-04;  author: gsl;  state: V4_3_00;  lines: +2 -2
//	Fix warnings
//	----------------------------
//	revision 1.5
//	date: 1996-07-25 14:14:17-04;  author: gsl;  state: V4_1_02;  lines: +0 -0
//	Renamed from args.cc to args.cpp
//	----------------------------
//	revision 1.4
//	date: 1995-04-25 05:59:38-04;  author: gsl;  state: V3_3_19;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:51:55-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:32:32-05;  author: gsl;  state: V3_3x12;  lines: +0 -0
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:50:55-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
