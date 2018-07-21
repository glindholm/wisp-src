//
//	Copyright (c) Shell Stream Software LLC, All Rights Reserved.
//
//	Project:	WPROC
//	
//// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : pcode.hpp
// Author : George Soules
// Date   : 25 February 1991

#ifndef PCODE__HPP
#define PCODE__HPP

// Classes
#include "stateobj.hpp"

// Definitions and subprograms
#include <stdio.h>
#include "assert.hpp"
#include "environ.hpp"
#include "opcodes.hpp"


class pcode_reader;
#if RUNTIME
#else
class pcode_emitter;
#endif

typedef usign_16 offset;


class pcode : public state_object {
   public:
      friend class pcode_reader;
#if RUNTIME
#else
      friend class pcode_emitter;
#endif
      pcode();
      virtual ~pcode();
      virtual void save_state();
      virtual void restore_state();
   protected:
      void     resize_code_buffer();
      usign_8 *code_buffer;
      offset   the_buffer_index;
      offset   the_buffer_size;
};


class pcode_access : public object {
   public:
      pcode_access();
      ~pcode_access();
      char   *offset_image(offset an_offset);
   protected:
      pcode  *the_pcode;
      offset  the_pc; // program counter
};


class pcode_reader : public pcode_access {
   public:
      pcode_reader(pcode *a_pcode_object);
      ~pcode_reader();
      opcode    get_opcode();
      usign_8   get_usign_8();
      usign_16  get_usign_16();
      int_8     get_int_8();
      int_16    get_int_16();
      int_32    get_int_32();
      char     *get_string();
      offset    get_offset();
      offset    current_offset();
      void      reset_pc_to(offset an_offset);
      char     *pc_image();
};

#if RUNTIME
#else
class pcode_emitter : public pcode_access {
   public:
      pcode_emitter(pcode *a_pcode_object);
      ~pcode_emitter();
      void   emit_opcode    (opcode    an_opcode);
      void   emit_usign_8   (usign_8   a_usign_8);
      void   emit_usign_16  (usign_16  a_usign_16);
      void   emit_int_8     (int_8     an_int_8);
      void   emit_int_16    (int_16    an_int_16);
      void   emit_int_32    (int_32    an_int_32);
      void   emit_string    (char     *a_string);
      void   emit_offset    (offset    an_offset);
      offset current_offset();
      offset fixup_offset_for(opcode an_opcode);
      void   fixup(offset at_offset, offset using_offset = 0);
      void   reset_pc() {the_pc = (offset)-1;};
      char  *pc_image();
#if WANG
      void   pause_emission();
      void   resume_emission();
#endif

      void alloc_bytes(int how_many) {
         the_pcode->the_buffer_index += how_many;
         if (the_pcode->the_buffer_index >= the_pcode->the_buffer_size)
            the_pcode->resize_code_buffer();
      }
#if WANG
   private:
      Boolean  emission_paused;
      offset   paused_buffer_index;
      offset   paused_pc;
#endif
};

#endif
#endif

//
//	History:
//	$Log: pcode.hpp,v $
//	Revision 1.6  2001/08/22 20:14:20  gsl
//	add friend class keyword
//	
//	Revision 1.5  1998-08-31 15:14:07-04  gsl
//	drcs update
//
//

//	
//	RCS file: /disk1/neomedia/RCS/wisp/wproc/pcode.hpp,v
//	Working file: pcode.hpp
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
//	date: 1995-04-25 06:00:14-04;  author: gsl;  state: V4_3_00;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:52:31-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:33:13-05;  author: gsl;  state: V3_3x12;  lines: +1 -1
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:20-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
