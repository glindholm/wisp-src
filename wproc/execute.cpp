//
//	Copyright (c) Shell Stream Software LLC, All Rights Reserved.
//
//	Project:	WPROC
//	
//// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : execute.cpp
// Author : George Soules
// Date   : 8 April 1991

// Specification
#include "machine.hpp"

// Classes
#include "data.hpp"
#include "input.hpp"
#include "process.hpp"

// Definitions and subprograms
#include <string.h>
#include "cancel.hpp"
#if DOS
#include "crt_io.hpp"
#endif
#include "debugaid.hpp"
#include "files.hpp"
#include "install.hpp"
#include "memory.hpp"
#include "tracer.hpp"
#include "txt.hpp"
#include "utility.hpp"

#if DOS
// Allow user to ctrl-break out of an infinite loop
// by doing some IO so DOS can check for ctrl-break
#define check_for_interrupt() key_pressed(0)
#else
#define check_for_interrupt()
#endif


opcode machine::execute() {
   trace_begin(machine, "execute");

   if (the_status == status_need_code)
      the_status = status_ok;

   halt = false;
   the_return_code = 0;

//   process *saved_process;
   opcode   the_opcode = the_pcode_reader->get_opcode();
   Boolean  done       = false;
   usign_16 label_id   = 0;

   while (! done) {
      if (cancel_requested()) {
#if WANG
         if (the_status != status_logoff)
            the_status = status_cancel;
#else
         the_status = status_cancel;
#endif
         the_opcode = halt_op;
      }

      if (user_options.debug_trace_machine())
         trace_ss(machine, the_pcode_reader->pc_image(), opcode_name(the_opcode));

      switch (the_opcode) {
         case add_op :
            integer_add();
            break;

         case and_op :
            pop_expressions(2);
            reg1 = exp[1].integer(12, txt_operand);
            if (bad_exp(1)) break;
            reg2 = exp[2].integer(12, txt_operand);
            if (bad_exp(2)) break;
            the_stack->push(new expression(reg1 && reg2, exp[1], exp[2]));
            break;

         case assign_op :
            exec_assign();
            break;
#if WANG
         case backref_full_op :
            resolve_full_backref();
            break;

         case backref_partial_op :
            resolve_partial_backref();
            break;
#endif
         case branch_always_op : {
            offset branch_target = the_pcode_reader->get_offset();
            the_pcode_reader->reset_pc_to(branch_target);
            check_for_interrupt();
            break;
         }

         case branch_false_op : {
            offset branch_target = the_pcode_reader->get_offset();
            pop_expressions(1);
            reg1 = exp[1].integer(14, txt_cond);
            if (bad_exp(1)) break;
            if (! reg1)
               the_pcode_reader->reset_pc_to(branch_target);
            check_for_interrupt();
            break;
         }

         case branch_true_op : {
            offset branch_target = the_pcode_reader->get_offset();
            pop_expressions(1);
            reg1 = exp[1].integer(14, txt_cond);
            if (bad_exp(1)) break;
            if (reg1)
               the_pcode_reader->reset_pc_to(branch_target);
            check_for_interrupt();
            break;
         }

         case builtin_op:
            exec_builtin();
            break;

         case call_op :
         case call_exp_op : {
            offset target;
            int_32 symbol_range;
	    offset return_offset;

            if (the_opcode == call_exp_op) {
	       /*
	        *	The locate_label_offset() routine messes up the pc so we need to 
		*	save then fixup the pc so call_return knows where to return to.
		*
		*	<call_exp_op> <fixup offset> <label(s) offset> <0 offset>
	       */
	       return_offset = the_pcode_reader->current_offset();

               pop_expressions(1);
               if (! locate_label_offset(exp[1].stripped_string(), target, true)) {
                  enter_cancel_state();
                  break;
               }

	       return_offset += sizeof(offset); // step over the <fixup offset>
   	       the_pcode_reader->reset_pc_to(return_offset);
	       while( 0 != symbol::get_id(*the_pcode_reader) ) 
	       { 
			/* step over the label offset table */
	       }
	       return_offset = the_pcode_reader->current_offset();
            }
            else {
               target = the_pcode_reader->get_offset();
               symbol_range = the_pcode_reader->get_int_32();
	       return_offset = the_pcode_reader->current_offset();
            }

            if (call_depth >= call_stack_size) {
               if (the_opcode == call_exp_op)
                  exp[1].fatal_error(32, call_stack_size, txt_label);
               else
                  fatal_error(symbol_range, 32, call_stack_size);
               enter_cancel_state();
               break;
            }
            call_stack[call_depth].return_offset = return_offset;
            call_stack[call_depth].for_loop_depth = for_loop_depth;

      	    if (user_options.debug_trace_machine())
	    {
		char mess[120], target_s[20], offset_s[20];

		strcpy(target_s,the_pcode_reader->offset_image(target));
		strcpy(offset_s,the_pcode_reader->offset_image(call_stack[call_depth].return_offset));

		sprintf(mess, "target=%s return_offset=%s for_loop_depth=%d call_depth=%d",
			target_s, offset_s, for_loop_depth, call_depth);
	        trace_ss(machine, "call ", mess);
           }

            call_depth += 1;

            the_pcode_reader->reset_pc_to(target);
            check_for_interrupt();
            break;
         }

         case call_return_op : {
            int_32 symbol_range = the_pcode_reader->get_int_32();
            if (call_depth > 0) {
               call_depth -= 1;

      	       if (user_options.debug_trace_machine())
	       {
		  char mess[120], offset_s[20];

		  strcpy(offset_s,the_pcode_reader->offset_image(call_stack[call_depth].return_offset));

		  sprintf(mess, "call_depth=%d call_stack.return_offset=%s call_stack.for_loop_depth=%d for_loop_depth=%d",
			call_depth, 
			offset_s, 
			call_stack[call_depth].for_loop_depth,
			for_loop_depth);
	          trace_ss(machine, "call_return ", mess);
               }

               the_pcode_reader->reset_pc_to
                  (call_stack[call_depth].return_offset);

               // Reset for_loop_depth in case a LEAVE inside a For loop
               // kept For loop(s) from terminating normally.
               while (for_loop_depth > call_stack[call_depth].for_loop_depth)
                  delete for_loop_stack[for_loop_depth--];
            }
            else {
               fatal_error(symbol_range, 60);
               enter_cancel_state();
            }
            break;
         }

         case catenate_op : {
            pop_expressions(2);
            str3 = new_string
               (strlen(exp[1].string()) + strlen(exp[2].string()) + 1);
            strcpy(str3, exp[1].string());
            strcat(str3, exp[2].string());
            the_stack->push(new expression(str3, exp[1], exp[2]));
            break;
         }

         case close_op:
            exec_close();
            break;

         case copy_and_push_top_op :
            the_stack->push(new expression(*the_stack->top()));
            break;

         case declare_op:
            exec_declare();
            break;

         case destroy_op :
            the_status = status_destroy;
            halt_machine();
            break;

         case divide_op :
            integer_divide();
            break;

         case eoc_op :
            the_status = status_need_code;
            halt_machine();
            break;
#if WANG
         case exists_op :
            file_lib_vol_exists();
            break;

         case extract_op :
            exec_extract();
            break;

         case extract_records_op :
            exec_extract_records();
            break;
#endif
         case for_loop_exit_op : {
            assert(for_loop_depth >= 0);
            delete for_loop_stack[for_loop_depth];
            for_loop_depth -= 1;
            break;
         }

         case for_loop_incr_op : {
            assert(for_loop_depth >= 0);
            for_loop_range *loop_range = for_loop_stack[for_loop_depth];
            loop_range->index += loop_range->increment;
            the_stack->push(new expression(*loop_range->variable));
            the_stack->push(new expression(loop_range->index));
            exec_assign();
            break;
         }

         case for_loop_init_op : {
            pop_expressions(4);

            for_loop_range *loop_range = new for_loop_range;
            loop_range->variable = exp.remove_exp(1);

            if (for_loop_depth + 1 >= for_loop_stack_size) {
               lvalue &the_lvalue = loop_range->variable->lvalue_ref();
               fatal_error(the_lvalue.symbol_range(), 44, for_loop_stack_size);
               enter_cancel_state();
               delete loop_range;
               break;
            }
            for_loop_depth += 1;
            for_loop_stack[for_loop_depth] = loop_range;

            loop_range->first = exp[2].integer(45, txt_parameter);
            if (bad_exp(2))
               break;
            loop_range->index = loop_range->first;
            loop_range->last = exp[3].integer(45, txt_parameter);
            if (bad_exp(3))
               break;
            loop_range->increment = exp[4].integer(45, txt_parameter);
            if (bad_exp(4))
               break;

            the_stack->push(new expression(*loop_range->variable));
            the_stack->push(new expression(loop_range->first));
            exec_assign();
            break;
         }

         case for_loop_test_op : {
            assert(for_loop_depth >= 0);
            offset loop_end = the_pcode_reader->get_offset();
            for_loop_range *loop_range = for_loop_stack[for_loop_depth];

            Boolean reverse = BOOLEAN(loop_range->increment < 0);
            int_32 first = reverse ? loop_range->last  : loop_range->first;
            int_32 last  = reverse ? loop_range->first : loop_range->last;

            if (loop_range->index > last || loop_range->index < first) {
               the_pcode_reader->reset_pc_to(loop_end);
               delete for_loop_stack[for_loop_depth];
               for_loop_depth -= 1;
            }
            break;
         }

         case goto_exp_op : {
            pop_expressions(1);
            offset target;
            if (locate_label_offset(exp[1].stripped_string(), target, true)) {
               the_pcode_reader->reset_pc_to(target);
               check_for_interrupt();
            }
            else
               enter_cancel_state();
            break;
         }

         case halt_op :
            halt_machine();
            break;

         case interpret_op :
            exec_interpret();
            break;

         case label_assign_op : {
            if (the_tracer)
	    {
		the_tracer->log_return_code(stmt_return_code);
	    }

            if (label_id) {
               symbol *the_symbol = the_symbol_table->lookup(label_id);
               label_data &the_data = (label_data&) the_symbol->data_ref();
               the_data.set_value(stmt_return_code);
            }
            stmt_return_code = 0;
            break;
         }

         case label_decl_op : {
            label_id = the_pcode_reader->get_usign_16();
            break;
         }

         case label_ref_op : {
            symbol *the_symbol = symbol_from_pcode();
            int_32  symbol_range = the_pcode_reader->get_int_32();
            label_data &the_data = (label_data&) the_symbol->data_ref();
            the_stack->push(new expression(the_data.value(), symbol_range));
            break;
         }
#if WANG
         case lib_name_op :
         case vol_name_op :
            pop_expressions(1);
            str3 = new_string(1 + strlen(exp[1].string()) + 1);
            strcpy(str3, the_opcode == lib_name_op ? "\x01" : "\x02");
            strcat(str3, exp[1].string());
            the_stack->push(new expression(str3, exp[1]));
            break;
#endif
#if WANG
         case logoff_op :
            the_status = status_logoff;
            cancel_handler();
            break;
#endif
         case message_op :
            exec_screen_io(message_op);
            break;

         case multiply_op :
            integer_multiply();
            break;

         case negate_op : {
            pop_expressions(1);
            reg1 = exp[1].integer(13, txt_operand);
            if (bad_exp(1)) break;
            the_stack->push(new expression(- reg1, exp[1]));
            break;
         }

         case not_op : {
            pop_expressions(1);
            reg1 = exp[1].integer(12, txt_operand);
            if (bad_exp(1)) break;
            the_stack->push(new expression(! reg1, exp[1]));
            break;
         }

         case open_op:
            exec_open();
            break;

         case or_op :
            pop_expressions(2);
            reg1 = exp[1].integer(12, txt_operand);
            if (bad_exp(1)) break;
            reg2 = exp[2].integer(12, txt_operand);
            if (bad_exp(2)) break;
            the_stack->push(new expression(reg1 || reg2, exp[1], exp[2]));
            break;

         case pop_top_op :
            pop_expressions(1);
            break;

         case print_op :
            exec_print();
            break;

         case print_subop :
            done = true;
            break;

         case prompt_op :
            exec_screen_io(prompt_op);
            break;

#if ! WANG
         case protect_op :
            exec_protect();
            break;
#endif

         case protect_subop :
            done = true;
            break;

         case push_int_8_op :
         case push_int_8_and_range_op :
            reg1 = the_pcode_reader->get_int_8();
            rng1 = the_opcode == push_int_8_and_range_op ?
               the_pcode_reader->get_int_32() : 0;
            the_stack->push(new expression(reg1, rng1));
            break;

         case push_int_16_op :
         case push_int_16_and_range_op :
            reg1 = the_pcode_reader->get_int_16();
            rng1 = the_opcode == push_int_16_and_range_op ?
               the_pcode_reader->get_int_32() : 0;
            the_stack->push(new expression(reg1, rng1));
            break;

         case push_int_32_op :
         case push_int_32_and_range_op :
            reg1 = the_pcode_reader->get_int_32();
            rng1 = the_opcode == push_int_32_and_range_op ?
               the_pcode_reader->get_int_32() : 0;
            the_stack->push(new expression(reg1, rng1));
            break;

         case push_string_op :
         case push_string_and_range_op :
            str1 = the_pcode_reader->get_string();
            rng1 = the_opcode == push_string_and_range_op ?
               the_pcode_reader->get_int_32() : 0;
            the_stack->push(new expression(str1, rng1));
            break;

         case push_variable_and_range_op :
            if (variable_reference()) {
               if (the_tracer)
                  the_tracer->display_variable(*the_stack->top());
            }
            else
               enter_cancel_state();
            break;

         case read_op :
            exec_read();
            break;

         case rename_op :
            exec_rename();
            break;

         case rel_case_eq_op :
         case rel_eq_op :
         case rel_ge_op :
         case rel_gt_op :
         case rel_le_op :
         case rel_lt_op :
         case rel_ne_op :
         {
            pop_expressions(2);
            expression::exp_kind the_kind = expression::undefined_kind;
            if (exp[1].kind() == expression::integer_kind) {
               if (exp[2].kind() == expression::integer_kind)
                  the_kind = expression::integer_kind;
            }
            else {
               assert(exp[1].kind() == expression::string_kind);
               if (exp[2].kind() == expression::string_kind)
                  the_kind = expression::string_kind;
            }

            int cmp_rc;
            if (the_kind == expression::integer_kind) {
               // They are known integers so no error check done
               reg1 = exp[1].integer();
               reg2 = exp[2].integer();
            }
            else if (the_kind == expression::string_kind) {
               char *s1 = trim(exp[1].string());
               char *s2 = trim(exp[2].string());
               cmp_rc = strcmp(s1, s2);
               delete s1;
               delete s2;
            }
            else {
               int error_number = the_opcode == rel_case_eq_op ? 28 : 26;
               if (exp[2].kind() == expression::string_kind)
                  error_number += 1;
               exp[2].fatal_error(error_number, (char *) NULL, NULL);
               enter_cancel_state();
               break;
            }

            Boolean integers = BOOLEAN(the_kind == expression::integer_kind);
            switch (the_opcode) {
               case rel_case_eq_op :
               case rel_eq_op :
                  reg3 = integers ? reg1 == reg2 : cmp_rc == 0;
                  break;
               case rel_ge_op :
                  reg3 = integers ? reg1 >= reg2 : cmp_rc >= 0;
                  break;
               case rel_gt_op :
                  reg3 = integers ? reg1 > reg2 : cmp_rc > 0;
                  break;
               case rel_le_op :
                  reg3 = integers ? reg1 <= reg2 : cmp_rc <= 0;
                  break;
               case rel_lt_op :
                  reg3 = integers ? reg1 < reg2 : cmp_rc < 0;
                  break;
               case rel_ne_op :
                  reg3 = integers ? reg1 != reg2 : cmp_rc != 0;
                  break;
               default :
                  assert(UNREACHABLE);
            }
            expression *the_result = new expression(reg3, exp[1], exp[2]);

            // If the comparison is for a Case statement, push the first
            // operand back on the stack so it can be used in the next case.
            if (the_opcode == rel_case_eq_op)
               the_stack->push(exp.remove_exp(1));

            the_stack->push(the_result);
            break;
         }

         case return_op :
            pop_expressions(1);
#if DOS
            reg1 = exp[1].integer(0, 255, 15, txt_rtn_code);
#else
            reg1 = exp[1].integer(15, txt_rtn_code);
#endif
            if (bad_exp(1)) break;
            the_return_code = reg1;
            halt_machine();
            break;

         case run_op :
            exec_run();
            break;

         case run_subop :
            done = true;
            break;

         case scratch_op :
            exec_scratch();
            break;

         case screen_op :
            exec_screen();
            break;

         case scrnio_subop :
            done = true;
            break;

         case set_op :
            exec_set();
            break;

         case source_name_op : {
            the_status = status_ok;

            char *name = the_pcode_reader->get_string();
            if (the_process->the_source_pathname)
               // Input is source--already know name
               delete name;
            else
               // Input is pcode
               the_process->the_source_pathname = name;
#if RUNTIME
#if DEMO
            if (! runtime_demo_ok(the_process->the_source_pathname)) {
               halt_machine();
               break;
            }
#endif
#endif
            the_process->the_source_timestamp =
               the_pcode_reader->get_int_32();

            Boolean source_is_file = BOOLEAN(the_process->the_source_pathname[0] != '(');

            if (the_process->trace_active && source_is_file)
	    {
                the_tracer = new tracer(the_process->the_source_pathname);
		if (1 == the_process->nesting_level)
		{
			the_tracer->check_restart();
		}
	    }
            break;
         }

         case statement_op : {
            label_id = 0;
            usign_16 stmt_number = the_pcode_reader->get_usign_16();
	    trace_si(machine, "line ", stmt_number);
            if (the_tracer)
               if (! the_tracer->permission_to_execute(stmt_number))
                  halt_machine();
            break;
         }

         case statement_end_op :
            done = true;
            break;

         case sub_exp_range_op :
            the_stack->top()->set_range_first(the_pcode_reader->get_int_32());
            the_stack->top()->set_range_last(the_pcode_reader->get_int_32());
            break;
#if WANG
         case submit_op :
            exec_submit();
            break;

         case submit_subop :
            done = true;
            break;
#endif
         case subtract_op :
            integer_subtract();
            break;

         case system_op :
            exec_system();
            break;

         case trace_op :
            exec_trace();
            break;

         case trace_subop :
            done = true;
            break;

         case using_op :
            exec_using();
            break;
#if WANG
         case using_end_op : {
            if (1 == the_process->nesting_level && user_options.fetched_args())
               if (the_args->formals_count() != the_args->count())
                  report_fatal_error
                     (the_args->formals_count() > the_args->count() ? 22 : 23);
            break;
         }
#endif
         case write_op :
            exec_write();
            break;

         default:
            assert(UNREACHABLE);
      }
      if (done)
         break;
      else if (halt) {
         the_opcode = halt_op;
         break;
      }
      else
         the_opcode = the_pcode_reader->get_opcode();
   }

   trace_end(machine);
   return the_opcode;
}






//
//	History:
//	$Log: execute.cpp,v $
//	Revision 1.12  2003/02/11 19:05:26  gsl
//	Remove unneeded #ifdef's for DEBUG
//	
//	Revision 1.11  1998/09/08 18:08:30  gsl
//	For call_exp_op had to save the current offset then calculate the
//	return offset.
//	CALL <exp> was failing to correctly set the return offset
//	
//	Revision 1.10  1998-09-04 13:16:17-04  gsl
//	Add debug tracing to the call_op and call_return_op, also print the line
//	number on a statement_op
//
//	Revision 1.9  1998-08-31 15:13:47-04  gsl
//	drcs update
//
//

//	
//	RCS file: /disk1/neomedia/RCS/wisp/wproc/execute.cpp,v
//	Working file: execute.cpp
//	head: 1.8
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
//	total revisions: 8;	selected revisions: 8
//	description:
//	----------------------------
//	revision 1.8
//	date: 1997-04-17 17:53:47-04;  author: gsl;  state: V4_3_00;  lines: +1 -1
//	Add check nesting_level==1 to test of fetched_args
//	----------------------------
//	revision 1.7
//	date: 1996-07-25 19:46:05-04;  author: gsl;  state: V3_9_91;  lines: +2 -2
//	NT
//	----------------------------
//	revision 1.6
//	date: 1996-07-25 14:15:06-04;  author: gsl;  state: Exp;  lines: +0 -0
//	Renamed from execute.cc to execute.cpp
//	----------------------------
//	revision 1.5
//	date: 1995-06-02 11:29:21-04;  author: gsl;  state: V3_3_19;  lines: +1 -1
//	fix warning
//	----------------------------
//	revision 1.4
//	date: 1995-04-25 05:59:55-04;  author: gsl;  state: V3_3_16;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:52:13-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:32:51-05;  author: gsl;  state: V3_3x12;  lines: +24 -10
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:07-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
