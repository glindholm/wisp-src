//
//	Copyright (c) Shell Stream Software LLC, All Rights Reserved.
//
//	Project:	WPROC
//	
//// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : fixup.hpp
// Author : George Soules
// Date   : 9 May 1991

#if RUNTIME
#else

#ifndef FIXUP__HPP
#define FIXUP__HPP

// Classes
#include "data.hpp"
#include "object.hpp"
#include "pcode.hpp"
#include "scope.hpp"
#include "symbols.hpp"
#include "token.hpp"

// Definitions and subprograms
// (none)


class fixup : public object {
   public:
      fixup();
      fixup(block a_block, offset an_offset, fixup *a_fixup);

      fixup(
         token   &a_token,
         offset   an_offset,
         fixup   *a_fixup,
         scope   *a_scope,
         Boolean  call);

      fixup(
         offset   an_offset,
         fixup   *a_fixup,
         scope   *a_scope,
         Boolean  call);

      ~fixup();

      void add_ref(
         offset   an_offset,
         token   &a_token,
         scope   *a_scope,
         Boolean  call);

      void add_exp_ref(
         offset   an_offset,
         scope   *a_scope,
         Boolean  call);

      void      add_exit_from(block a_block, offset an_offset);
      void      fixup_exits_from(int depth, pcode_emitter &an_emitter);
      token    *pending_fixup();
      void      delete_chain();
      token    &label_token()     {return *the_label_token;}
      scope    *ref_scope()       {return  the_scope;}
      offset    ref_offset()      {return  the_offset;}
      Boolean   call()            {return  is_call;}
      Boolean   target_is_exp()   {return  is_exp;}
      fixup    *lookup(const char *a_name);
      Boolean   fixups_pending();
      fixup    *next_in_chain()   {return  next;}
      Boolean   resolved()        {return  is_resolved;}
      void      set_resolved()    {is_resolved = true;}
      symbol_id symbol_boundary() {return boundary;}
      void      set_symbol_boundary(symbol_id a_boundary) {boundary = a_boundary;}
   private:
      block     the_block;
      scope*    the_scope;
      offset    the_offset;
      token    *the_label_token;
      Boolean   is_call;
      Boolean   is_exp;
      Boolean   is_resolved;
      symbol_id boundary;
      fixup    *next;
};

#endif
#endif


//
//	History:
//	$Log: fixup.hpp,v $
//	Revision 1.5  1998/08/31 19:13:51  gsl
//	drcs update
//	
//

//	
//	RCS file: /disk1/neomedia/RCS/wisp/wproc/fixup.hpp,v
//	Working file: fixup.hpp
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
//	date: 1995-04-25 05:59:59-04;  author: gsl;  state: V4_3_00;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:52:17-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:32:56-05;  author: gsl;  state: V3_3x12;  lines: +11 -11
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:10-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
