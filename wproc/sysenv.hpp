//
//	Copyright (c) Shell Stream Software LLC, All Rights Reserved.
//
//	Project:	WPROC
//	
//// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : sysenv.hpp
// Author : George Soules
// Date   : 27 June 1991

#ifndef SYSENV__HPP
#define SYSENV__HPP

// Classes
#include "object.hpp"

// Definitions and subprograms
// (none)


class system_environment : public object {
   public:
      system_environment();
      ~system_environment();
      int         set(const char* a_name, const char* a_value);
      char       *get(const char* a_name);
      void        restore();
   private:
      void        create_vector();
      int         lookup(const char* a_name);
      char      **the_vector;
      int         entry_count;
      int         entries_allocated;
};


#endif


//
//	History:
//	$Log: sysenv.hpp,v $
//	Revision 1.5  1998/08/31 19:14:21  gsl
//	drcs update
//	
//

//	
//	RCS file: /disk1/neomedia/RCS/wisp/wproc/sysenv.hpp,v
//	Working file: sysenv.hpp
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
//	date: 1995-04-25 06:00:28-04;  author: gsl;  state: V4_3_00;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:52:44-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:33:29-05;  author: gsl;  state: V3_3x12;  lines: +0 -0
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:30-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
