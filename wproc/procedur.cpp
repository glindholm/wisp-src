// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : procedur.cpp
// Author : George Soules
// Date   : 13 March 1991

// Specification
#include "procedur.hpp"

// Classes
#include "process.hpp"

// Definitions and subprograms
#include <string.h>
#include "debugaid.hpp"
#include "ext.hpp"
#include "fileinfo.hpp"
#include "memory.hpp"
#include "options.hpp"
#include "product.hpp"
#include "report.hpp"
#include "utility.hpp"


procedure::procedure(char *a_name, arguments *args) {
   trace_begin(object, "procedure");
   the_name = dup_string(a_name);
   the_args = args;
   the_machine = NULL;
   trace_end(object);
}


procedure::~procedure() {
   delete_string(the_name);
   trace(object, "~procedure");
}


void procedure::create_machine() {
   the_pcode_reader = new pcode_reader(the_state->the_pcode);
   the_machine = new machine(the_args,
                             the_pcode_reader,
                             the_state->the_symbol_table,
                             the_state->the_stack);
}


void procedure::destroy_machine() {
   the_status = the_machine->status();
   the_return_code = the_machine->return_code();
   delete the_pcode_reader;
   delete the_machine;
}

#if RUNTIME
#else
// Bodies of compilable_procedure members

compilable_procedure::compilable_procedure(
   reader    *a_source_reader,
   char      *a_name,
   arguments *args) : procedure(a_name, args)
{
   trace_begin(object, "compilable_procedure");
   the_state = new state();
   the_emitter = new pcode_emitter(the_state->the_pcode);
   the_compiler = new compiler
      (a_source_reader, the_emitter, the_state->the_symbol_table, a_name);
   trace_end(object);
}


compilable_procedure::~compilable_procedure() {
   delete the_state;
   delete the_emitter;
   delete the_compiler;
   trace(object, "~compilable_procedure");
}


void compilable_procedure::compile() {
   report_status(product_copyright());
   report_status("");
   report_status(user_options.syntax_check() ?
      "Syntax checking " : "Compiling ", the_name);

   the_compiler->compile_statements();

   int errors   = the_compiler->errors();
   int warnings = the_compiler->warnings();
   char *action = user_options.syntax_check() ? "Syntax check" : "Compilation";

   if (errors || warnings) {
      report_status("");
      report_status(action, " completed");
   }
   else
      report_status(action, " completed with no errors");

   if (errors) {
      char *num = int_32_to_string(errors);
      report_status(num, " error", errors > 1 ? "s" : "");
      delete num;
   }
   if (warnings) {
      char *num = int_32_to_string(warnings);
      report_status(num, " warning", warnings > 1 ? "s" : "");
      delete num;
   }

   if (the_compiler->errors() == 0) {
      if (! user_options.syntax_check()) {
         char *filename = pcode_filename(the_name);

         if (the_state->save_state(filename))
            report_status("Code generated into ", filename);
         else
            report_status("Unable to create ", filename);

         delete filename;
      }
      the_status = status_ok;
      the_return_code = the_compiler->warnings() ? 1 : 0;
   }
   else {
      if (! user_options.syntax_check())
         report_status("No code generated");
      the_status = the_compiler->errors() > 0 ?
         status_runtime_error : status_cancel;
      the_return_code = 2;
   }
}


void compilable_procedure::compile_and_execute() {
   create_machine();

   do {
      the_compiler->compile_statements(1);
      if (the_compiler->errors() == 0)
         the_machine->execute();
      else {
         the_machine->abort_execution();
         break;
      }
   } while (the_machine->status() == status_need_code);

   destroy_machine();
}


void compilable_procedure::interpret() {
   trace_begin(object, "interpreting compilable_procedure");
   if (user_options.compile())
      compile();
   else
      compile_and_execute();
   trace_end(object);
}
#endif

// Bodies of executable_procedure members

executable_procedure::executable_procedure(
   char      *a_name,
   arguments *args) : procedure(a_name, args)
{
   trace_begin(object, "executable_procedure");
   the_state = new state();
   if (the_state->restore_state(a_name))
      the_status = status_ok;
   else
      the_status = status_open_error;
   trace_end(object);
}


executable_procedure::~executable_procedure() {
   delete the_state;
   trace(object, "~executable_procedure");
}


void executable_procedure::interpret() {
   if (the_status == status_ok) {
      create_machine();
      the_machine->execute();
      destroy_machine();
   }
}


