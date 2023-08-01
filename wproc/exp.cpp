//
//	Copyright (c) Shell Stream Software LLC, All Rights Reserved.
//
//	Project:	WPROC
//	
//
// Module : exp.cpp
// Author : George Soules
// Date   : 23 April 1991

// Specification
#include "exp.hpp"

// Classes
#include "process.hpp"

// Definitions and subprograms
#include "debugaid.hpp"
#include "memory.hpp"
#include "report.hpp"
#include "utility.hpp"


// lavlue member functions

lvalue::lvalue(
   symbol *a_symbol,
   int_32 a_range,
   int    args,
   int    an_index,
   int    a_start,
   int    a_length)
{
   the_symbol       = a_symbol;
   the_symbol_range = a_range;
   the_args         = args;
   the_array_index  = an_index;
   the_start        = a_start;
   the_length       = a_length;
}

lvalue::lvalue(lvalue &an_lvalue) {
   the_symbol       = an_lvalue.the_symbol;
   the_symbol_range = an_lvalue.the_symbol_range;
   the_args         = an_lvalue.the_args;
   the_array_index  = an_lvalue.the_array_index;
   the_start        = an_lvalue.the_start;
   the_length       = an_lvalue.the_length;
}


// expression member functions

expression::expression(int_32 a_value, expression &exp1) {
   trace_si(object, "expression (int_32) ", a_value);
   the_kind     = integer_kind;
   range_first  = exp1.range_first;
   range_last   = exp1.range_last;
   the_integer  = a_value;
   value_is_bad = false;
   the_lvalue    = NULL;
}


expression::expression(char  *a_value, expression &exp1) {
   trace_ss(object, "expression (string) ", a_value);
   the_kind     = string_kind;
   range_first  = exp1.range_first;
   range_last   = exp1.range_last;
   the_string   = a_value;
   value_is_bad = false;
   the_lvalue    = NULL;
}

expression::expression(int_32 a_value, expression &exp1, expression &exp2) {
   trace_si(object, "expression (int_32) ", a_value);
   the_kind      = integer_kind;
   range_first   = exp1.range_first;
   range_last    = exp2.range_last;
   the_integer   = a_value;
   value_is_bad  = false;
   the_lvalue    = NULL;
}


expression::expression(char  *a_value, expression &exp1, expression &exp2) {
   trace_ss(object, "expression (string) ", a_value);
   the_kind      = string_kind;
   range_first   = exp1.range_first;
   range_last    = exp2.range_last;
   the_string    = a_value;
   value_is_bad  = false;
   the_lvalue    = NULL;
}


expression::expression(int_32 a_value, int_32 a_range, lvalue *an_lvalue) {
   trace_si(object, "expression (int_32) ", a_value);
   the_kind      = integer_kind;
   range_first   = a_range;
   range_last    = a_range;
   the_integer   = a_value;
   value_is_bad  = false;
   the_lvalue    = an_lvalue;
}


expression::expression(char *a_value, int_32 a_range, lvalue *an_lvalue) {
   trace_ss(object, "expression (string) ", a_value);
   the_kind      = string_kind;
   range_first   = a_range;
   range_last    = a_range;
   the_string    = a_value;
   value_is_bad  = false;
   the_lvalue    = an_lvalue;
}


expression::expression(expression &an_expression) {
   trace(object, "expression (copy)");
   the_kind      = an_expression.the_kind;
   range_first   = an_expression.range_first;
   range_last    = an_expression.range_last;
   if (the_kind == string_kind)
      the_string = dup_string(an_expression.the_string);
   else
      the_integer = an_expression.the_integer;
   value_is_bad  = an_expression.value_is_bad;
   the_lvalue    = new lvalue(*an_expression.the_lvalue);
}


expression::~expression() {
   if (the_kind == string_kind && the_string)
      delete_string(the_string);
   if (the_lvalue)
      delete the_lvalue;
   trace(object, "~expression");
}


// The string() and integer() routines below will convert
// the kind of an expression, if necessary, to the kind requested.
// Once the conversion is done, a subsequent call on the same
// object will simply return the value.

const char *expression::string() {
   if (the_kind == integer_kind) {
      int_32 i = the_integer;
      the_string = new_string(INT_32_STRING_SIZE);
      int_32_to_ascii(i, the_string);
      trace(object, "converting integer expression to string");
      the_kind = string_kind;
   }
   return the_string;
}


const char *expression::stripped_string() {
   char *old_string = (char *) (this->string());
   the_string = strip(the_string);
   delete_string(old_string);
   return the_string;
}


void expression::request_string(
   int   error_number,
   char *value_kind)
{
   range the_range(range_first, range_last);
   location the_location(the_process->the_source_pathname, &the_range);
   char *new_value;
   report_correction(
      new_value, value_is_bad, error_number,
      &the_location, the_string, value_kind);
   if (! value_is_bad) {
      delete the_string;
      the_string = new_value;
   }
}


#if WANG
void expression::request_filename(
   int   error_number,
   char *value_kind)
{
   range the_range(range_first, range_last);
   location the_location(the_process->the_source_pathname, &the_range);
   char *new_value;
   report_filename_correction(
      new_value, value_is_bad, error_number,
      &the_location, the_string, value_kind);
   if (! value_is_bad) {
      delete the_string;
      the_string = new_value;
   }
}
#endif


int_32 expression::integer(
   int_32 first,
   int_32 last,
   int    error_number,
   char  *value_kind)
{
   if (the_kind == string_kind)
      convert_to_integer(error_number, value_kind);

   if (! value_is_bad)
      validate_integer(first, last, 5, value_kind);

   return the_integer;
}


void expression::request_integer(
   int   error_number,
   char *value_kind)
{
   range the_range(range_first, range_last);
   location the_location(the_process->the_source_pathname, &the_range);
   the_integer = 0;
   report_correction(
      the_integer, value_is_bad, error_number,
      &the_location, NULL, value_kind);
}


void expression::convert_to_integer(
   int   error_number,
   char *value_kind)
{
   int_32 i = 0;
   if (! string_to_int_32(the_string, i)) {
      range the_range(range_first, range_last);
      location the_location(the_process->the_source_pathname, &the_range);
      report_correction(
         i, value_is_bad, error_number,
         &the_location, the_string, value_kind);
   }
   if (! value_is_bad) {
      delete_string(the_string);
      the_kind = integer_kind;
      the_integer = i;
   }
}


void expression::validate_integer(
   int_32 first,
   int_32 last,
   int    error_number,
   char  *value_kind)
{
   while (the_integer < first || the_integer > last) {
      range the_range(range_first, range_last);
      location the_location(the_process->the_source_pathname, &the_range);

      char s1[INT_32_STRING_SIZE];
      char s2[INT_32_STRING_SIZE];
      char s3[INT_32_STRING_SIZE];

      int_32_to_ascii(the_integer, s1);
      int_32_to_ascii(first,       s2);
      int_32_to_ascii(last,        s3);

      report_correction
         (the_integer, value_is_bad, error_number, &the_location,
          s1, value_kind, s2, s3);

      if (value_is_bad)
         break;
   }
}


void expression::fatal_error(
   int         error_number,
   int_32      subs,
   const char *value_kind)
{
   char s[INT_32_STRING_SIZE];
   int_32_to_ascii(subs, s);
   fatal_error(error_number, s, value_kind);
}


void expression::fatal_error(
   int         error_number,
   const char *subs,
   const char *value_kind)
{
   range the_range(range_first, range_last);
   location the_location(the_process->the_source_pathname, &the_range);
   int_32 new_value = error_is_fatal;
   report_correction(
      new_value, value_is_bad, error_number,
      &the_location, subs, value_kind);
}


// expression_array member functions

expression_array::expression_array() {
   the_dimension             = 0;
   the_array                 = NULL;
   the_last_index_referenced = 0;
}


expression_array::~expression_array() {
   delete_array();
}


void expression_array::delete_array() {
   if (the_array) {
      for (int i = 0; i < the_dimension; i++) {
         if (the_array[i])
            delete the_array[i];
      }
   }
   delete the_array;
   the_array = NULL;
}


void expression_array::new_array(int a_dimension) {
   if (the_array)
      delete_array();
   the_dimension = a_dimension;
   if (a_dimension) {
      the_array = new expression*[the_dimension];
      for (int i = 0; i < the_dimension; i++)
         the_array[i] = NULL;
   }
   the_last_index_referenced = 0;
}


expression *expression_array::remove_exp(int an_index) {
   assert(an_index >= 1 && an_index <= the_dimension);
   expression *the_exp = the_array[an_index - 1];
   the_array[an_index - 1] = NULL;
   return the_exp;
}




//
//	History:
//	$Log: exp.cpp,v $
//	Revision 1.6  1998/08/31 19:13:47  gsl
//	drcs update
//	
//

//	
//
//	Working file: exp.cpp
//	head: 1.5
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
//	total revisions: 5;	selected revisions: 5
//	description:
//	----------------------------
//	revision 1.5
//	date: 1996-07-25 14:15:09-04;  author: gsl;  state: V4_3_00;  lines: +0 -0
//	Renamed from exp.cc to exp.cpp
//	----------------------------
//	revision 1.4
//	date: 1995-04-25 05:59:56-04;  author: gsl;  state: V3_3_19;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:52:14-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:32:52-05;  author: gsl;  state: V3_3x12;  lines: +0 -0
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:08-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
