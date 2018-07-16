//
//	Copyright (c) 1996-1998 NeoMedia Technologies Inc. All rights reserved.
//
//	Project:	WPROC
//	Id:		$Id:$
//	RCS:		$Source:$
//	
//// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : fixup.cpp
// Author : George Soules
// Date   : 9 May 1991

#if RUNTIME
#else

// Specification
#include "fixup.hpp"

// Classes
// (none)

// Definitions and subprograms
#include <string.h>
#include "debugaid.hpp"
#include "utility.hpp"


fixup::fixup() {
   the_block       = 0;
   the_scope       = NULL;
   the_offset      = 0;
   the_label_token = NULL;
   is_call         = false;
   is_exp          = false;
   is_resolved     = false;
   boundary        = 0;
   next            = NULL;
}


fixup::fixup(block a_block, offset an_offset, fixup *a_fixup) {
   the_block       = a_block;
   the_scope       = NULL;
   the_offset      = an_offset;
   the_label_token = NULL;
   is_call         = false;
   is_exp          = false;
   is_resolved     = false;
   boundary        = 0;
   next            = a_fixup;
}


fixup::fixup(
   token   &a_token,
   offset   an_offset,
   fixup   *a_fixup,
   scope   *a_scope,
   Boolean  call)
{
   the_block       = 0; // indicates this is not an exit fixup
   the_scope       = new scope(*a_scope);
   the_offset      = an_offset;
   the_label_token = new token(a_token);
   is_call         = call;
   is_exp          = false;
   is_resolved     = false;
   boundary        = 0;
   next            = a_fixup;
}


fixup::fixup(
   offset   an_offset,
   fixup   *a_fixup,
   scope   *a_scope,
   Boolean  call)
{
   the_block       = 0; // indicates this is not an exit fixup
   the_scope       = new scope(*a_scope);
   the_offset      = an_offset;
   the_label_token = NULL; // label was an expression
   is_call         = call;
   is_exp          = true;
   is_resolved     = false;
   boundary        = 0;
   next            = a_fixup;
}


fixup::~fixup() {
   if (the_label_token)
      delete the_label_token;
   if (the_scope)
      delete the_scope;
}


void fixup::add_exit_from(block a_block, offset an_offset) {
   next = new fixup(a_block, an_offset, next);
}


void fixup::add_ref(
   offset   an_offset,
   token   &a_token,
   scope   *a_scope,
   Boolean  call)
{
   next = new fixup(a_token, an_offset, next, a_scope, call);
}


void fixup::add_exp_ref(
   offset   an_offset,
   scope   *a_scope,
   Boolean  call)
{
   next = new fixup(an_offset, next, a_scope, call);
}


void fixup::fixup_exits_from(int a_block, pcode_emitter &an_emitter) {
   fixup *prev_fixup = this;
   fixup *this_fixup = next;
   while (this_fixup) {
      if (this_fixup->the_block >= a_block) {
         an_emitter.fixup(this_fixup->the_offset);
         prev_fixup->next = this_fixup->next;
         delete this_fixup;
         this_fixup = prev_fixup->next;
      }
      else {
         prev_fixup = this_fixup;
         this_fixup = this_fixup->next;
      }
   }
}


fixup *fixup::lookup(const char *a_name) {
   fixup *prev_fixup = this;
   fixup *this_fixup = next;

   while (this_fixup) {
      if (! this_fixup->is_exp && this_fixup->the_block == 0) {
         if (same_string(this_fixup->the_label_token->lexeme(), a_name)) {
            prev_fixup->next = this_fixup->next;
            return this_fixup;
         }
      }
      prev_fixup = this_fixup;
      this_fixup = this_fixup->next;
   }
   return NULL;
}


token *fixup::pending_fixup() {
   fixup *this_fixup;
   while (fixups_pending()) {
      this_fixup = next;
      next = this_fixup->next;
      if (! this_fixup->is_resolved) {
         token *the_label_token = this_fixup->the_label_token;
         this_fixup->the_label_token = NULL;
         delete this_fixup;
         return the_label_token;
      }
      delete this_fixup;
   }
   return NULL;
}


void fixup::delete_chain() {
   fixup *this_fixup;
   while (next) {
      this_fixup = next;
      next = this_fixup->next;
      delete this_fixup;
   }
}


Boolean fixup::fixups_pending() {
   return Boolean (next != NULL);
}

#endif



//
//	History:
//	$Log: fixup.cpp,v $
//	Revision 1.6  1998-08-31 15:13:50-04  gsl
//	drcs update
//
//

//	
//	RCS file: /disk1/neomedia/RCS/wisp/wproc/fixup.cpp,v
//	Working file: fixup.cpp
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
//	date: 1996-07-25 14:15:15-04;  author: gsl;  state: V4_3_00;  lines: +0 -0
//	Renamed from fixup.cc to fixup.cpp
//	----------------------------
//	revision 1.4
//	date: 1995-04-25 05:59:59-04;  author: gsl;  state: V3_3_19;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:52:17-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:32:55-05;  author: gsl;  state: V3_3x12;  lines: +6 -6
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:10-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
