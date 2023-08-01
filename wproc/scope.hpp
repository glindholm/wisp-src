//
//	Copyright (c) Shell Stream Software LLC, All Rights Reserved.
//
//	Project:	WPROC
//	
//
// Module : scope.hpp
// Author : George Soules
// Date   : 19 July 1991

#ifndef SCOPE__HPP
#define SCOPE__HPP

// Classes
#include "object.hpp"

// Definitions and subprograms
#include "environ.hpp"


typedef int_16 block;
const block outermost_block = 1;

class scope : public object {
   public:
      scope(int size);
      scope(scope &a_scope);
      ~scope();
      Boolean enter(Boolean restricted = false);
      void    exit();
      block   current_block();
      Boolean can_see(block a_block);
      Boolean can_go_to(block a_block);
   private:
      block *the_stack;
      int    the_top;
      int    entry_count;
      block  active_block;
      int    block_count;
};

#endif


//
//	History:
//	$Log: scope.hpp,v $
//	Revision 1.5  1998/08/31 19:14:14  gsl
//	drcs update
//	
//

//	
//
//	Working file: scope.hpp
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
//	date: 1995-04-25 06:00:21-04;  author: gsl;  state: V4_3_00;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:52:37-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:33:21-05;  author: gsl;  state: V3_3x12;  lines: +3 -3
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:25-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
