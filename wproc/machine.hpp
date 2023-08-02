//
//	Copyright (c) Shell Stream Software LLC, All Rights Reserved.
//
//	Project:	WPROC
//	
//
// Module : machine.hpp
// Author : George Soules
// Date   : 24 February 1991

#ifndef MACHINE__HPP
#define MACHINE__HPP

// Classes
#include "args.hpp"
#include "colors.hpp"
class input;
#include "object.hpp"
#include "pcode.hpp"
#include "process.hpp"
#include "range.hpp"
class screen_contents;
#include "stack.hpp"
#include "symbols.hpp"
#include "sysenv.hpp"
class tracer;

// Definitions and subprograms
#include "opcodes.hpp"
#include "report.hpp"
#include "status.hpp"


const int call_stack_size     = 32;
const int for_loop_stack_size = 32;
const int screen_stack_size   = 32;


class machine : public object {
   public:
      machine(
         arguments    *args,
         pcode_reader *a_pcode_reader,
         symbol_table *a_symbol_table,
         stack        *a_stack);

      ~machine();

      opcode      execute();
      void        abort_execution();
      status_kind status()      {return the_status;};
      int_32      return_code() {return the_return_code;};
      void        assign(expression *a_variable, char *text);
      void        assign(expression *a_variable, int_32 a_number);
      void        assign(expression *a_variable, expression *a_value);

      symbol_table *symbol_table_ptr() {return the_symbol_table;};

   private:
      void    halt_machine();
      void    enter_cancel_state(status_kind a_status = status_runtime_error);
      Boolean bad_exp(int an_exp);
      symbol *symbol_from_pcode();
      void    pop_expressions(int how_many);
      void    save_process_state();
      void    restore_process_state();

      void fatal_error(
         int_32      a_range,
         int         error_number,
         int_32      subs,
         const char *value_kind = NULL);

      void fatal_error(
         int_32      a_range,
         int         error_number,
         const char *subs = NULL,
         const char *value_kind = NULL);

      Boolean valid_ref(int args, symbol *a_symbol, int_32 a_range);

      Boolean valid_substring(
         int          args,
         int_32       &start,
         int_32       &length,
         string_index a_size);

      void args_error(int args, int min_args, int max_args, int_32 a_range);

#if NETWORK
      Boolean verify_network_available(int_32 a_range);
      Boolean verify_user_logged_in(int_32 a_range);
#endif

      Boolean builtin_value(
         int_32       &value,
         int           args,
         builtin_data &a_builtin,
         int_32        a_range);

      Boolean builtin_value(
         char        *&value,
         int           args,
         builtin_data &a_builtin,
         int_32        a_range);

      void    integer_add();
      void    integer_divide();
      void    integer_multiply();
      void    integer_subtract();

      Boolean get_yes_no_option(Boolean &an_option);
      Boolean set_field_colors(colors &coloring, Boolean attr_enabled);
      Boolean variable_reference();

      Boolean locate_label_offset(
         const char *name,
         offset     &an_offset,
         Boolean     issue_error);

      symbol *locate_label_symbol(const char *name, offset label_table);

      void    exec_assign();
      void    exec_builtin();
      void    exec_close();
      void    exec_declare(Boolean formal_parameter = false);
      void    exec_open();
#if ! WANG
      void    exec_print();
      void    exec_protect();
#endif
      void    exec_read();
      void    exec_rename();
      void    exec_scratch();
      void    exec_screen();
      void    exec_screen_io(opcode an_opcode);
#if ! WANG
      void    exec_set();
#endif
      void    exec_trace();
      void    exec_using();
      void    exec_write();
      void    exec_run();
      void    exec_system();
      void    exec_interpret();

#if WANG
      Boolean exec_display_enter(Boolean is_enter);
      void    exec_extract();
      void    exec_extract_records();
      void    exec_submit();
      void    exec_print();
      void    exec_set();
      void    resolve_full_backref();
      void    resolve_partial_backref();
      void    file_lib_vol_exists();
#endif

      expression_array exp;

      arguments    *the_args;
      pcode_reader *the_pcode_reader;
      symbol_table *the_symbol_table;
      stack        *the_stack;
      tracer       *the_tracer;
      status_kind   the_status;
      int_32        the_return_code;
      int           the_arg_index;
      process      *the_saved_process;

      Boolean       halt;
      int_32        stmt_return_code;

      char         *str1;
      char         *str2;
      char         *str3;
      int_32        reg1;
      int_32        reg2;
      int_32        reg3;
      int_32        rng1;

      class call_state : public object {
         public:
             offset return_offset;
             int    for_loop_depth;
      };

      int              call_depth;
      call_state       call_stack[call_stack_size];

      class for_loop_range : public object {
         public:
            for_loop_range()  {}
            ~for_loop_range() {delete variable;}
            expression    *variable;
            int_32  first;
            int_32  last;
            int_32  index;
            int_32  increment;
      };

      int              for_loop_depth;
      for_loop_range  *for_loop_stack[for_loop_stack_size];

      int              screen_depth;
      screen_contents *screen_stack[screen_stack_size];

      system_environment *environment;
};

#endif

//
//	History:
//	$Log: machine.hpp,v $
//	Revision 1.7  1998/09/08 18:05:32  gsl
//	Rename the stack_state return offset
//	
//	Revision 1.6  1998-08-31 15:13:55-04  gsl
//	drcs update
//
//

//	
//
//	Working file: machine.hpp
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
//	date: 1995-06-10 13:56:58-04;  author: gsl;  state: V4_3_00;  lines: +2 -0
//	Added public function symbol_table_ptr() which returns a pointer
//	to the_symbol_table for that machine.  Needed to fix the globals
//	problem and used in wang_os_link()
//	----------------------------
//	revision 1.4
//	date: 1995-04-25 06:00:03-04;  author: gsl;  state: V3_3_16;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:52:20-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:33:00-05;  author: gsl;  state: V3_3x12;  lines: +16 -15
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:13-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
