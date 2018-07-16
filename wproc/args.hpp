//
//	Copyright (c) 1996-1998 NeoMedia Technologies Inc. All rights reserved.
//
//	Project:	WPROC
//	Id:		$Id:$
//	RCS:		$Source:$
//	
//// Copyright (c) Lexical Software, 1991.  All rights reserved.
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


//
//	History:
//	$Log: args.hpp,v $
//	Revision 1.5  1998-08-31 15:13:31-04  gsl
//	drcs update
//
//

//	
//	RCS file: /disk1/neomedia/RCS/wisp/wproc/args.hpp,v
//	Working file: args.hpp
//	head: 1.4
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
//	total revisions: 4;	selected revisions: 4
//	description:
//	----------------------------
//	revision 1.4
//	date: 1995-04-25 05:59:38-04;  author: gsl;  state: V4_3_00;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:51:55-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:32:32-05;  author: gsl;  state: V3_3x12;  lines: +1 -1
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:50:56-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
