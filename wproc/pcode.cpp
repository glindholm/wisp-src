//
//	Copyright (c) Shell Stream Software LLC, All Rights Reserved.
//
//	Project:	WPROC
//	
//
// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : pcode.cpp
// Author : George Soules
// Date   : 25 February 1991

// Specification
#include "pcode.hpp"

// Classes
// (none)

// Definitions and subprograms
#include <ctype.h>
#include <string.h>
#include "debugaid.hpp"
#include "opcodes.hpp"
#include "memory.hpp"
#include "report.hpp"
#include "utility.hpp"


const offset initial_pc = 0xffff;
Boolean tracing = true;

union {
   int_32   i32;
   usign_16 u16;
   offset   os;
   int_16   i16;
   usign_8  u8[4];
} bits;


// Bodies of pcode members

pcode::pcode() {
   trace_begin(object, "pcode");
   the_buffer_index = 0;
   the_buffer_size  = 0;
   code_buffer      = NULL;
   trace_end(object);
}


pcode::~pcode() {
   delete code_buffer;
   trace(object, "~pcode");
}


void pcode::save_state() {
   trace(object, "saving pcode");
   save(&the_buffer_index, sizeof(the_buffer_index));
   save(code_buffer, the_buffer_index);
}


void pcode::restore_state() {
   restore(&the_buffer_size, sizeof(the_buffer_size));
   trace_si(object, "restoring pcode, bytes = ", the_buffer_size);
   code_buffer = new usign_8[the_buffer_size];
   if (code_buffer) {
      restore(code_buffer, the_buffer_size);
      the_buffer_index = the_buffer_size;
   }
   else
      report_fatal_error(12, int_32_to_string(the_buffer_size));
}


void pcode::resize_code_buffer() {
   const usign_32 large_model_limit = 0xFFFFL;
   const usign_16 increment = 1024;

   usign_32 new_size = the_buffer_size + increment;

   if (new_size <= large_model_limit) {
      usign_8 *new_buffer = new usign_8[new_size];
      if (new_buffer) {
#if DOS
         _fmemcpy(new_buffer, code_buffer, the_buffer_size);
#else
         memcpy(new_buffer, code_buffer, the_buffer_size);
#endif
         delete code_buffer;
         code_buffer = new_buffer;
         the_buffer_size = (usign_16)new_size;
         trace_si(object, "resized pcode buffer to ", the_buffer_size);
         return;
      }
      else
         report_fatal_error(11, int_32_to_string(new_size));
   }
   else
      report_fatal_error(13);
}


// Bodies of pcode_access members

pcode_access::pcode_access() {
   trace_begin(object, "pcode_access");
   trace_end(object);
}


pcode_access::~pcode_access() {
   trace(object, "~pcode_access");
}


char *pcode_access::offset_image(offset a_pc) {
   static char s[] = "000xxxx ";
   int i;
#if DOS
   itoa(a_pc, s + 3, 16);
#else
   sprintf(s + 3, "%x", a_pc);
#endif
   for (i = 0; i < 4; i++)
      s[i + 3] = toupper(s[i + 3]);
   for (i = 4; i < 8; i++)
      if (s[i] == '\0') {
         s[i] = ' ';
         s[i + 1] = '\0';
         return s + i - 4;
      }
   assert(UNREACHABLE);
   return NULL;
}


// Bodies of pcode_reader members

pcode_reader::pcode_reader(pcode *a_pcode_object) {
   trace_begin(object, "pcode_reader");
   the_pcode = a_pcode_object;
   the_pc = initial_pc;
   trace_end(object);
}


pcode_reader::~pcode_reader() {
   trace(object, "~pcode_reader");
}


opcode pcode_reader::get_opcode() {
   if (++the_pc >= the_pcode->the_buffer_index) {
      the_pc -= 1;
      return eoc_op; // end of code
   }
   else
      return (opcode) the_pcode->code_buffer[the_pc];
}


usign_8 pcode_reader::get_usign_8() {
   return the_pcode->code_buffer[++the_pc];
}


usign_16 pcode_reader::get_usign_16() {
   bits.u8[1] = the_pcode->code_buffer[++the_pc];
   bits.u8[0] = the_pcode->code_buffer[++the_pc];
   return bits.u16;
}


int_8 pcode_reader::get_int_8() {
   return (int_8) the_pcode->code_buffer[++the_pc];
}


int_16 pcode_reader::get_int_16() {
   bits.u8[1] = the_pcode->code_buffer[++the_pc];
   bits.u8[0] = the_pcode->code_buffer[++the_pc];
   return bits.i16;
}


int_32 pcode_reader::get_int_32() {
   bits.u8[3] = the_pcode->code_buffer[++the_pc];
   bits.u8[2] = the_pcode->code_buffer[++the_pc];
   bits.u8[1] = the_pcode->code_buffer[++the_pc];
   bits.u8[0] = the_pcode->code_buffer[++the_pc];
   return bits.i32;
}


char *pcode_reader::get_string() {
  int i;
   usign_16 size = get_usign_16();
   char *s = new_string(size + 1);
   for (i = 0; i < size; i++)
      s[i] = (char) the_pcode->code_buffer[++the_pc];
   s[i] = '\0';
   return s;
}


offset pcode_reader::current_offset() {
   return the_pc + 1;
}


offset pcode_reader::get_offset() {
   return get_usign_16();
}


void pcode_reader::reset_pc_to(offset an_offset) {
   the_pc = an_offset - 1;
}


char *pcode_reader::pc_image() {
   // Show the one last read
   return pcode_access::offset_image(the_pc);
}

#if RUNTIME
#else
// Bodies of pcode_emitter members

pcode_emitter::pcode_emitter(pcode *a_pcode_object) {
   trace_begin(object, "pcode_emitter");
   the_pcode = a_pcode_object;
   the_pc = initial_pc;
#if WANG
   emission_paused = false;
#endif
   trace_end(object);
}


pcode_emitter::~pcode_emitter() {
   trace(object, "~pcode_emitter");
}


void pcode_emitter::emit_opcode(opcode an_opcode) {
   trace_ss(emitter, pc_image(), opcode_name(an_opcode));
   alloc_bytes(1);
   the_pcode->code_buffer[++the_pc] = (usign_8) an_opcode;
}


void pcode_emitter::emit_usign_8(usign_8 a_usign_8) {
   trace_si(emitter, pc_image(), a_usign_8);
   alloc_bytes(1);
   the_pcode->code_buffer[++the_pc] = a_usign_8;
}


void pcode_emitter::emit_usign_16(usign_16 a_usign_16) {
   if (tracing)
      trace_si(emitter, pc_image(), a_usign_16);
   bits.u16 = a_usign_16;
   alloc_bytes(2);
   the_pcode->code_buffer[++the_pc] = bits.u8[1];
   the_pcode->code_buffer[++the_pc] = bits.u8[0];
}


void pcode_emitter::emit_int_8(int_8 an_int_8) {
   trace_si(emitter, pc_image(), an_int_8);
   alloc_bytes(1);
   the_pcode->code_buffer[++the_pc] = (usign_8) an_int_8;
}


void pcode_emitter::emit_int_16(int_16 an_int_16) {
   trace_si(emitter, pc_image(), an_int_16);
   bits.i16 = an_int_16;
   alloc_bytes(2);
   the_pcode->code_buffer[++the_pc] = bits.u8[1];
   the_pcode->code_buffer[++the_pc] = bits.u8[0];
}


void pcode_emitter::emit_int_32(int_32 an_int_32) {
   trace_si(emitter, pc_image(), an_int_32);
   bits.i32 = an_int_32;
   alloc_bytes(4);
   the_pcode->code_buffer[++the_pc] = bits.u8[3];
   the_pcode->code_buffer[++the_pc] = bits.u8[2];
   the_pcode->code_buffer[++the_pc] = bits.u8[1];
   the_pcode->code_buffer[++the_pc] = bits.u8[0];
}


void pcode_emitter::emit_string(char *a_string) {
   trace_sq(emitter, pc_image(), a_string);
   usign_16 size = strlen(a_string);
   tracing = false;
   emit_usign_16(size);
   tracing = true;
   alloc_bytes(size);
   for (int i = 0; i < size; i++)
      the_pcode->code_buffer[++the_pc] = (usign_8) a_string[i];
}


offset pcode_emitter::current_offset() {
   return the_pc + 1;
}


void pcode_emitter::emit_offset(offset an_offset) {
   emit_usign_16(an_offset);
}


offset pcode_emitter::fixup_offset_for(opcode an_opcode) {
   emit_opcode(an_opcode);
   trace_ss(emitter, pc_image(), "<fixup>");
   tracing = false;
   emit_offset(0);
   tracing = true;
   offset fixup_offset = the_pc - sizeof(offset) + 1;
   return fixup_offset;
}


void pcode_emitter::fixup(offset at_offset, offset using_offset) {
   bits.os = using_offset ? using_offset : the_pc + 1;
   trace_ss(
      emitter, 
      (using_offset) ? "fixup using " : "fixup ",
      (using_offset) ? offset_image(using_offset) : offset_image(at_offset)
      );
   the_pcode->code_buffer[at_offset++] = bits.u8[1];
   the_pcode->code_buffer[at_offset]   = bits.u8[0];
}


char *pcode_emitter::pc_image() {
#if WANG
   if (emission_paused)
      return "  -- ";
#endif
   // Show the next one to be written
   return pcode_access::offset_image(the_pc + 1);
}


#if WANG
void pcode_emitter::pause_emission() {
   assert(! emission_paused);
   emission_paused = true;
   paused_buffer_index = the_pcode->the_buffer_index;
   paused_pc = the_pc;
}


void pcode_emitter::resume_emission() {
   assert(emission_paused);
   emission_paused = false;
   the_pcode->the_buffer_index = paused_buffer_index;
   the_pc = paused_pc;
}
#endif

#endif

//
//	History:
//	$Log: pcode.cpp,v $
//	Revision 1.10  2000/08/28 14:12:47  gsl
//	Fix test is pcode::resize_code_buffer() for 64K limit on code size.
//	
//	Revision 1.9  1998-08-31 15:50:35-04  gsl
//	drcs update
//
//	Revision 1.8  1998-08-31 15:14:06-04  gsl
//	drcs update
//
//

//	
//
//	Working file: pcode.cpp
//	head: 1.7
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
//	total revisions: 7;	selected revisions: 7
//	description:
//	----------------------------
//	revision 1.7
//	date: 1997-10-01 13:33:01-04;  author: gsl;  state: V4_3_00;  lines: +7 -5
//	Fix warnings
//	----------------------------
//	revision 1.6
//	date: 1996-07-25 14:15:57-04;  author: gsl;  state: V4_1_02;  lines: +1 -1
//	Renamed from pcode.cc to pcode.cpp
//	----------------------------
//	revision 1.5
//	date: 1996-04-18 13:00:30-04;  author: jockc;  state: Exp;  lines: +6 -1
//	moved declaration of for loop index from for loop to function
//	auto decl area
//	----------------------------
//	revision 1.4
//	date: 1995-04-25 06:00:14-04;  author: gsl;  state: V3_3_19;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:52:30-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:33:13-05;  author: gsl;  state: V3_3x12;  lines: +2 -2
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:20-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
