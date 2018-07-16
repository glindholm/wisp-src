// Copyright (c) Lexical Software, 1991.  All rights reserved.
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



