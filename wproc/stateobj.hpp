//
//	Copyright (c) 1996-1998 NeoMedia Technologies Inc. All rights reserved.
//
//	Project:	WPROC
//	Id:		$Id:$
//	RCS:		$Source:$
//	
//// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : stateobj.hpp
// Author : George Soules
// Date   : 21 March 1991

#ifndef STATEOBJ__HPP
#define STATEOBJ__HPP

// Classes
#include "object.hpp"

// Definitions and subprograms
#include "environ.hpp"
#include <stdio.h>


typedef FILE state_file;

class state_object : public object {
   public:
      state_object();
      virtual ~state_object();
      virtual void save_state() = 0;
      virtual void restore_state() = 0;
      state_object *next_state_object;
   protected:
      enum state_mode {save_mode, restore_mode};
      static Boolean open_state_file(const char *a_filename, state_mode a_mode);
      static void    close_state_file();
      void           save(const void *ptr, size_t size);
      void           save(const char *a_string);
      void           restore(void *ptr, size_t size);
      void           restore(char *&a_string);
   private:
      static state_file *the_file;
};

#endif


//
//	History:
//	$Log: stateobj.hpp,v $
//	Revision 1.6  2010/01/10 18:07:33  gsl
//	fix missing virtual destructor warnings
//	
//	Revision 1.5  1998/08/31 19:14:18  gsl
//	drcs update
//	
//

//	
//	RCS file: /disk1/neomedia/RCS/wisp/wproc/stateobj.hpp,v
//	Working file: stateobj.hpp
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
//	date: 1995-04-25 06:00:26-04;  author: gsl;  state: V4_3_00;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:52:41-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:33:26-05;  author: gsl;  state: V3_3x12;  lines: +1 -1
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:28-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
