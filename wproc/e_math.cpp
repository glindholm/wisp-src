//
//	Copyright (c) 1996-1998 NeoMedia Technologies Inc. All rights reserved.
//
//	Project:	WPROC
//	Id:		$Id:$
//	RCS:		$Source:$
//	
//// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : e_math.cpp
// Author : George Soules
// Date   : 5 April 1991


// Specification
#if DOS || DOS_HOST
#include <dos.h>
#endif
#include "machine.hpp"

// Classes
// (none)

// Definitions and subprograms
#include "debugaid.hpp"
#include "txt.hpp"

// Spontaneous Assembly routines
extern "C" void movs_qd(void);
extern "C" void muls_qd(void);


void machine::integer_add() {
   pop_expressions(2);
   reg1 = exp[1].integer(13, txt_operand);
   if (bad_exp(1)) return;
   reg2 = exp[2].integer(13, txt_operand);
   if (bad_exp(2)) return;
   reg3 = reg1 + reg2;
   expression *the_exp = new expression(reg3, exp[1], exp[2]);

   int math_error = (
      (reg1 > 0 && reg2 > 0 && reg3 < 0) ||
      (reg1 < 0 && reg2 < 0 && reg3 > 0) ||
      (reg3 == INT_MIN));

   if (math_error) {
      the_exp->request_integer(2, txt_result);
      if (the_exp->is_bad()) {
         delete the_exp;
         enter_cancel_state();
         return;
      }
   }
   the_stack->push(the_exp);
}


void machine::integer_divide() {
   pop_expressions(2);
   reg1 = exp[1].integer(13, txt_operand);
   if (bad_exp(1)) return;
   reg2 = exp[2].integer(13, txt_divisor);
   if (bad_exp(2)) return;
   while (reg2 == 0) {
      exp[2].request_integer(1, txt_divisor);
      if (bad_exp(2)) return;
      reg2 = exp[2].integer();
   }
   the_stack->push(new expression(reg1 / reg2, exp[1], exp[2]));
}


void machine::integer_multiply() {
   pop_expressions(2);
   reg1 = exp[1].integer(13, txt_operand);
   if (bad_exp(1)) return;
   reg2 = exp[2].integer(13, txt_operand);
   if (bad_exp(2)) return;

   int math_error;

#if DOS || DOS_HOST
   // There has got to be a better way to detect overflow without using
   // floating point (the Borland floating point libs drags in 14.5k).

   union {
      struct {
         usign_16 low;
         usign_16 high;
      } word;
      int_32 dword;
      char   byte[4];
   } bits_32;

   union {
      struct {
         usign_32 low;
         usign_32 high;
      } dword;
      char qword[8];
   } bits_64;

   asm push ds
   asm push di
   _DS = FP_SEG(bits_64.qword);
   _DI = FP_OFF(bits_64.qword);

   // Move reg1 into quad word
   bits_32.dword = reg1;
   _DX = bits_32.word.high;
   _AX = bits_32.word.low;
   asm call far ptr movs_qd

   // Multiply quad word * reg2
   bits_32.dword = reg2;
   _CX = bits_32.word.high;
   _BX = bits_32.word.low;
   asm call far ptr muls_qd
   asm pop  di
   asm pop  ds

   reg3 = bits_64.dword.low;

   math_error = (
      (bits_64.dword.high != 0 && bits_64.dword.high != -1 ) ||
      (reg1 > 0 && reg2 > 0 && reg3 < 0) ||
      (reg1 > 0 && reg2 < 0 && reg3 > 0) ||
      (reg1 < 0 && reg2 > 0 && reg3 > 0) ||
      (reg1 < 0 && reg2 < 0 && reg3 < 0) ||
      (reg3 == INT_MIN));
#else
   double d1 = reg1;
   double d2 = reg2;
   double d3 = d1 * d2;
   reg3 = (int_32)d3;
   math_error = (reg3 != d3);
#endif

   expression *the_exp = new expression(reg3, exp[1], exp[2]);

   if (math_error) {
      the_exp->request_integer(2, txt_result);
      if (the_exp->is_bad()) {
         delete the_exp;
         enter_cancel_state();
         return;
      }
   }
   the_stack->push(the_exp);
}


void machine::integer_subtract() {
   pop_expressions(2);
   reg1 = exp[1].integer(13, txt_operand);
   if (bad_exp(1)) return;
   reg2 = exp[2].integer(13, txt_operand);
   if (bad_exp(2)) return;
   reg3 = reg1 - reg2;
   expression *the_exp = new expression(reg3, exp[1], exp[2]);

   int math_error = (
      (reg1 > 0 && reg2 < 0 && reg3 < 0) ||
      (reg1 < 0 && reg2 > 0 && reg3 > 0) ||
      (reg3 == INT_MIN));

   if (math_error) {
      the_exp->request_integer(2, txt_result);
      if (the_exp->is_bad()) {
         delete the_exp;
         enter_cancel_state();
         return;
      }
   }
   the_stack->push(the_exp);
}




//
//	History:
//	$Log: e_math.cpp,v $
//	Revision 1.8  1998-08-31 15:13:42-04  gsl
//	drcs update
//
//

//	
//	RCS file: /disk1/neomedia/RCS/wisp/wproc/e_math.cpp,v
//	Working file: e_math.cpp
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
//	date: 1997-06-10 12:04:58-04;  author: scass;  state: V4_3_00;  lines: +3 -3
//	Corrected LONG_MIN and LONG_MAX
//	----------------------------
//	revision 1.6
//	date: 1996-08-13 13:48:51-04;  author: gsl;  state: V3_3_93;  lines: +0 -1
//	Remove inline pragma
//	----------------------------
//	revision 1.5
//	date: 1996-07-25 14:14:45-04;  author: gsl;  state: Exp;  lines: +0 -0
//	Renamed from e_math.cc to e_math.cpp
//	----------------------------
//	revision 1.4
//	date: 1995-04-25 05:59:50-04;  author: gsl;  state: V3_3_19;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:52:07-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:32:46-05;  author: gsl;  state: V3_3x12;  lines: +5 -5
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:04-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
