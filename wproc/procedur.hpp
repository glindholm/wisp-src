//
//	Copyright (c) Shell Stream Software LLC, All Rights Reserved.
//
//	Project:	WPROC
//	
//// Copyright (c) Lexical Software, 1991.  All rights reserved.
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


//
//	History:
//	$Log: procedur.hpp,v $
//	Revision 1.5  1998/08/31 19:14:08  gsl
//	drcs update
//	
//

//	
//
//	Working file: procedur.hpp
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
//	date: 1995-04-25 06:00:15-04;  author: gsl;  state: V4_3_00;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:52:31-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:33:14-05;  author: gsl;  state: V3_3x12;  lines: +0 -0
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:20-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
