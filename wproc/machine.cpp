//
//	Copyright (c) 1996-1998 NeoMedia Technologies Inc. All rights reserved.
//
//	Project:	WPROC
//	Id:		$Id:$
//	RCS:		$Source:$
//	
//// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : machine.cpp
// Author : George Soules
// Date   : 24 February 1991

// Specification
#include "machine.hpp"

// Classes
#include "process.hpp"

// Definitions and subprograms
#include "crt_io.hpp"
#include "debugaid.hpp"
#include "files.hpp"
#include "memory.hpp"
#include "screen.hpp"
#include "tracer.hpp"
#include "txt.hpp"


machine::machine(
   arguments    *args,
   pcode_reader *a_pcode_reader,
   symbol_table *a_symbol_table,
   stack        *a_stack)
{
   trace_begin(object, "machine");
   the_args         = args;
   the_pcode_reader = a_pcode_reader;
   the_symbol_table = a_symbol_table;
   the_stack        = a_stack;
   the_tracer       = NULL;
   the_status       = status_none;
   the_return_code  = 0;
   the_arg_index    = 0;
   call_depth       = 0;
   for_loop_depth   = -1;
   screen_depth     = -1;
   environment      = new system_environment();
   trace_end(object);
}


machine::~machine() {
   if (the_tracer)
      delete the_tracer;
   exp.delete_array();
   while (for_loop_depth >= 0)
      delete for_loop_stack[for_loop_depth--];
   while (screen_depth >= 0)
      delete screen_stack[screen_depth--];
   delete environment;
   files_cleanup(the_process->nesting_level);
   trace(object, "~machine");
}


void machine::pop_expressions(int how_many) {
   exp.new_array(how_many);
   while (how_many)
      exp.set_exp(how_many--, the_stack->pop());
}


void machine::halt_machine() {
   halt = true;
}


void machine::enter_cancel_state(status_kind a_status) {
   the_status = a_status;
   halt = true;
}


Boolean machine::bad_exp(int an_exp) {
   if (exp[an_exp].is_bad()) {
      enter_cancel_state();
      return true;
   }
   else
      return false;
}


symbol *machine::symbol_from_pcode() {
   symbol_id the_id = symbol::get_id(*the_pcode_reader);
   assert(the_id);
   symbol *the_symbol = the_symbol_table->lookup(the_id);
   assert(the_symbol);
   return the_symbol;
}


void machine::fatal_error(
   int_32      a_range,
   int         error_number,
   int_32      subs,
   const char *value_kind)
{
   expression the_ref((int_32) 0, a_range);
   the_ref.fatal_error(error_number, subs, value_kind);
}


void machine::fatal_error(
   int_32      a_range,
   int         error_number,
   const char *subs,
   const char *value_kind)
{
   expression the_ref((int_32) 0, a_range);
   the_ref.fatal_error(error_number, subs, value_kind);
}


Boolean machine::valid_ref(int args, symbol *a_symbol, int_32 a_range)
{
   symbol_attributes is = a_symbol->is;
   int error_number = 0;

   if (is.undeclared)
      error_number = 6;
   else if (! is.allocated && ! is.builtin)
      error_number = 30;
   else if (is.global && is.not_local)
   {
      // Perform parameter checks that the compiler couldn't
      // because it didn't know the type of this global variable

      if (is.array) {
         if (! (args == 1 || args < 0))
            // No array index
            error_number = 7;
         else if (is.integer && args != 1)
            // Parameter list after array index
            error_number = 9;
         else if (is.string && args < -3)
            // More than two parameters after array index
            error_number = 10;
      }
      else {
         if (args < 0)
            // Array index on non-array
            error_number = 8;
         else if (is.integer && args != 0)
            // Parameters on an integer
            error_number = 11;
         else if (is.string && args > 2)
            // More than two parameters
            error_number = 10;
      }
   }

   if (error_number) {
      fatal_error(a_range, error_number, a_symbol->name());
      return false;
   }
   else
      return true;
}


void machine::abort_execution() {
   the_status = status_runtime_error;
}

/*
**	1	TRACE BEGIN
**	2	TRACE END
**	3	TRACE INTO file [IN library [ON volume]]
**                    RESOURCES={yes|NO} SCRATCH={yes|NO} STATEMENTS={run|ALL} VARIABLES={yes|NO}
*/
void machine::exec_trace() {

	switch (the_pcode_reader->get_usign_8()) 
	{
	case 1:	// Trace Begin
		{
	      		Boolean source_is_file = BOOLEAN(the_process->the_source_pathname[0] != '(');

      			if (source_is_file)
			{
				if (!the_tracer)
				{
	         			the_tracer = new tracer(the_process->the_source_pathname);
				}
				the_tracer->trace_interactive();
			}
		}
   		break;

	case 2:	// Trace End
		if (the_tracer) 
		{
      			if (the_process->base_level + the_process->nesting_level == the_process->trace_level) 
			{
         			delete the_tracer;
         			the_tracer = NULL;
      			}
   		}
		break;

	case 3:	// Trace Into
		{
			char	*file_name;
			int	v_resources, v_scratch, v_statements, v_variables;

			file_name = NULL;
			if (the_pcode_reader->get_usign_8()) 
			{
				pop_expressions(1);
				file_name = dup_string(exp[1].stripped_string());
			}

			v_resources  = the_pcode_reader->get_int_8(); 
			v_scratch    = the_pcode_reader->get_int_8(); 
			v_statements = the_pcode_reader->get_int_8(); 
			v_variables  = the_pcode_reader->get_int_8(); 

			Boolean source_is_file = BOOLEAN(the_process->the_source_pathname[0] != '(');

			if (source_is_file)
			{
				if (!the_tracer)
				{
					the_tracer = new tracer(the_process->the_source_pathname);
				}

				stmt_return_code = the_tracer->trace_into(file_name, 
					v_resources, v_scratch, v_statements, v_variables);
			}

			if (file_name)
			{
				delete file_name;
			}
		}
		break;
	}
}


void machine::save_process_state() {
   flush_open_files(the_process->nesting_level);
   the_saved_process = the_process;
   the_process = new process(*the_process);
   if (the_saved_process->cursor_off)
      cursor_visible(true);
}


void machine::restore_process_state() {
   delete the_process;
   the_process = the_saved_process;
   cursor_visible(Boolean(! the_saved_process->cursor_off));
   environment->restore();
}


Boolean machine::locate_label_offset(
   const char *name,
   offset     &an_offset,
   Boolean     issue_error)
{
   // Search the pcode labels tables to find the first label that matches name.
   // The backward label table (Wang version only) is in the pcode at the
   // point of transfer (e.g. goto or call statement).  The forward label
   // table is at the end of the code (not generated until all labels in
   // the source were seen).

   offset forward_label_table  = the_pcode_reader->get_offset();
#if WANG
   offset backward_label_table = the_pcode_reader->current_offset();
#endif
   symbol *label_symbol;

   while (true) {
      label_symbol = locate_label_symbol(name, forward_label_table);
#if WANG
      if (label_symbol == NULL)
         label_symbol = locate_label_symbol(name, backward_label_table);
#endif
      if (label_symbol)
         break;
      else {
         if (issue_error) {
            label_symbol = the_symbol_table->lookup(name);
            int error = label_symbol ? 76 : 75;
            exp[1].request_string(error, txt_label);
            if (exp[1].is_bad())
               return false;
            name = exp[1].string();
         }
         else
            return false;
      }
   }

   // Obtain offset of target label
   assert(label_symbol->is.label);
   data &data_ref = label_symbol->data_ref();
   label_data &the_label = (label_data&) data_ref;
   an_offset = the_label.location();

   return true;
}


symbol *machine::locate_label_symbol(const char *name, offset label_table) {
   // Position to a pcode label table (a list of symbol ID's terminated by
   // a 0 ID.  Starting with the first table entry, look up each symbol until
   // one is found that matches name.

   symbol_id  id;
   symbol    *the_symbol = NULL;

   the_pcode_reader->reset_pc_to(label_table);

   while (1) {
      id = symbol::get_id(*the_pcode_reader);
      if (id) {
         the_symbol = the_symbol_table->lookup(id);
         assert(the_symbol);
         if (same_string(name, the_symbol->name()))
            return the_symbol;
      }
      else
         // At end of table
         return NULL;
   }
}


//
//	History:
//	$Log: machine.cpp,v $
//	Revision 1.7  1998-08-31 15:13:54-04  gsl
//	drcs update
//
//

//	
//	RCS file: /disk1/neomedia/RCS/wisp/wproc/machine.cpp,v
//	Working file: machine.cpp
//	head: 1.6
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
//	total revisions: 6;	selected revisions: 6
//	description:
//	----------------------------
//	revision 1.6
//	date: 1996-07-25 19:47:07-04;  author: gsl;  state: V4_3_00;  lines: +1 -1
//	NT
//	----------------------------
//	revision 1.5
//	date: 1996-07-25 14:15:23-04;  author: gsl;  state: Exp;  lines: +0 -0
//	Renamed from machine.cc to machine.cpp
//	----------------------------
//	revision 1.4
//	date: 1995-04-25 06:00:03-04;  author: gsl;  state: V3_3_19;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:52:20-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:32:59-05;  author: gsl;  state: V3_3x12;  lines: +75 -22
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:12-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
