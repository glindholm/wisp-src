//
//	Copyright (c) 1996-1998 NeoMedia Technologies Inc. All rights reserved.
//
//	Project:	WPROC
//	Id:		$Id:$
//	RCS:		$Source:$
//	
//// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : exp.hpp
// Author : George Soules
// Date   : 23 April 1991

#ifndef EXP__HPP
#define EXP__HPP

// Classes
#include "object.hpp"
#include "symbols.hpp"

// Definitions and subprograms
#include <limits.h>
#include <string.h>
#include "assert.hpp"
#include "environ.hpp"


class lvalue : public object {
   public:
      lvalue(
         symbol *a_symbol,
         int_32 a_range,
         int    args     = 0,
         int    an_index = 0,
         int    a_start  = 0,
         int    a_length = 0);

      lvalue(lvalue &an_lvalue);

      symbol &symbol_ref()     {return *the_symbol;}
      symbol *symbol_address() {return the_symbol;}
      int_32 symbol_range()    {return the_symbol_range;}
      int    args()            {return the_args;}
      int    array_index()     {return the_array_index;}
      int    start()           {return the_start;}
      int    length()          {return the_length;}
   private:
      symbol *the_symbol;
      int_32  the_symbol_range;
      int     the_args;
      int     the_array_index;
      int     the_start;
      int     the_length;
};


class expression : public object {
   public:
      enum exp_kind {integer_kind, string_kind, undefined_kind};

      expression(int_32 a_value, expression &exp1);
      expression(char  *a_value, expression &exp1);

      expression(char  *a_value, expression &exp1, expression &exp2);
      expression(int_32 a_value, expression &exp1, expression &exp2);

      expression(int_32 a_value, int_32 a_range = 0, lvalue *an_lvalue = NULL);
      expression(char  *a_value, int_32 a_range = 0, lvalue *an_lvalue = NULL);

      expression(expression &an_expression);

      ~expression();

      lvalue &lvalue_ref() {assert(the_lvalue); return *the_lvalue;}

      const char *string();
      const char *stripped_string();

      void request_string(
         int   error_number,
         char *value_kind);

#if WANG
      void request_filename(
         int     error_number,
         char   *value_kind);
#endif

      int_32 integer() {
         assert(the_kind == integer_kind);
         return integer(INT_MIN, INT_MAX, 0, "");
      }

      int_32 integer(
         int    error_number,
         char  *value_kind)
      {
         return integer(INT_MIN, INT_MAX, error_number, value_kind);
      }

      int_32 integer(
         int_32 first,
         int_32 last,
         int    error_number,
         char  *value_kind);

      void request_integer(
         int   error_number,
         char *value_kind);

      void convert_to_integer(
         int   error_number,
         char *value_kind);

      void validate_integer(
         int_32 first,
         int_32 last,
         int    error_number,
         char  *value_kind);

      void fatal_error(
         int         error_number,
         int_32      subs,
         const char *value_kind);

      void fatal_error(
         int         error_number,
         const char *subs,
         const char *value_kind);

      Boolean  is_bad()    {return value_is_bad;}
      Boolean  is_lvalue() {return BOOLEAN(the_lvalue != NULL);}
      exp_kind kind()      {return the_kind;}

      void     set_range_first(int_32 a_range) {range_first = a_range;}
      void     set_range_last(int_32 a_range)  {range_last = a_range;}

   private:
      exp_kind  the_kind;
      int_32    range_first;
      int_32    range_last;
      union {
         int_32 the_integer;
         char  *the_string;
      };
      Boolean   value_is_bad;
      lvalue   *the_lvalue;
};


// Elements in this array start at 1, not 0

class expression_array : public object {
   public:
      expression_array();
      ~expression_array();
      void delete_array();
      void new_array(int a_dimension);

      void set_exp(int an_index, expression *an_exp) {
         assert(an_index >= 1 && an_index <= the_dimension);
         the_array[an_index - 1] = an_exp;
      }

      expression *remove_exp(int an_index);

      expression &operator[] (int an_index) {
         assert(an_index >= 1 && an_index <= the_dimension);
         assert(the_array[an_index -1]);
         the_last_index_referenced = an_index;
         return *the_array[an_index - 1];
      }

      int last_index_referenced() {return the_last_index_referenced;}
      Boolean exists(int an_index) {
         return BOOLEAN(an_index >= 1 && an_index <= the_dimension);
      }
      int dimension() {return the_dimension;}
   private:
      int          the_dimension;
      expression **the_array;
      int          the_last_index_referenced;
};


#endif


//
//	History:
//	$Log: exp.hpp,v $
//	Revision 1.7  2003/02/06 18:12:01  gsl
//	Fix qualified name warning on
//	 expression::validate_integer()
//	
//	Revision 1.6  1998/08/31 19:13:48  gsl
//	drcs update
//	
//

//	
//	RCS file: /disk1/neomedia/RCS/wisp/wproc/exp.hpp,v
//	Working file: exp.hpp
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
//	date: 1997-06-10 12:05:21-04;  author: scass;  state: V4_3_00;  lines: +2 -2
//	Corrected LONG_INT and LONG_MAX
//	----------------------------
//	revision 1.4
//	date: 1995-04-25 05:59:56-04;  author: gsl;  state: V3_3_93;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:52:15-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:32:52-05;  author: gsl;  state: V3_3x12;  lines: +5 -5
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:08-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
