// Copyright (c) Lexical Software, 1991.  All rights reserved.
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

