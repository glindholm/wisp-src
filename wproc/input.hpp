//
//	Copyright (c) Shell Stream Software LLC, All Rights Reserved.
//
//	Project:	WPROC
//	
//// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : input.hpp
// Author : George Soules
// Date   : 11 February 1991

#ifndef INPUT__HPP
#define INPUT__HPP

// Classes
#include "args.hpp"
#include "object.hpp"
#include "procedur.hpp"
#include "reader.hpp"
#include "state.hpp"

// Definitions and subprograms
#include <stdio.h>
#include "environ.hpp"
#include "status.hpp"


class input : public object {
   public:
      input();
      virtual ~input();
      virtual void run() = 0;
      virtual int  destroy()          {return 0;}
      int_32       return_code()      {return the_return_code;}
      status_kind  status()           {return the_status;}
      char        *name()             {return the_name;}
      void         enable_swapping()  {swapping = true;}
      Boolean      linked_to()        {return is_linked_to;}
   protected:
#if DOS
      Boolean      swap_and_spawn(int_32 &rc);
#endif
      int_32       the_return_code;
      status_kind  the_status;
      Boolean      swapping;
      char        *the_name;
      arguments   *the_args;
      Boolean      is_linked_to;
};


class nonexistent_input : public input {
   public:
      nonexistent_input(const char *a_name, arguments *args);
      ~nonexistent_input();
      void run();
};


class system_command_input : public input {
   public:
      system_command_input(const char *a_filename, arguments *args);
      system_command_input(const char *a_command);
      ~system_command_input();
      void run();
};


class spawnable_input : public input {
   public:
      spawnable_input(const char *a_filename, arguments *args);
      ~spawnable_input();
      void run();
};


class procedure_input : public input {
   public:
      procedure_input();
      ~procedure_input();
      void run();
   protected:
      procedure *the_procedure;
};


class source_input : public procedure_input {
   public:
      source_input();
      ~source_input();
      void run();
   protected:
      reader    *the_reader;
};


class source_file_input : public source_input {
   public:
      source_file_input(const char *a_filename, arguments *args);
      ~source_file_input();
      virtual int destroy();
};


class source_string_input : public source_input {
   public:
      source_string_input(const char *a_string);
      ~source_string_input();
};


class state_file_input : public procedure_input {
   public:
      state_file_input(const char *a_filename, arguments *args);
      ~state_file_input();
      void run();
      virtual int destroy();
};


input *create_input_object(const char *a_filename, arguments *args);

#endif


//
//	History:
//	$Log: input.hpp,v $
//	Revision 1.5  1998/08/31 19:13:52  gsl
//	drcs update
//	
//

//	
//
//	Working file: input.hpp
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
//	date: 1995-04-25 06:00:01-04;  author: gsl;  state: V4_3_00;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:52:18-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:32:57-05;  author: gsl;  state: V3_3x12;  lines: +4 -2
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:11-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
