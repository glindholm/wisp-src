//
//	Copyright (c) 1996-1998 NeoMedia Technologies Inc. All rights reserved.
//
//	Project:	WPROC
//	Id:		$Id:$
//	RCS:		$Source:$
//	
//

// Copyright (c) Lexical Software, 1992.  All rights reserved.
//
// Module : e_wang.cpp
// Author : George Soules
// Date   : 5 March 1992

#if WANG

// Specification
#include "machine.hpp"

// Classes
#include "memory.hpp"

// Definitions and subprograms
#include <ctype.h>
#include "debugaid.hpp"
#include "utility.hpp"
#include "subops.hpp"
#include "txt.hpp"
#include "wang_os.hpp"


Boolean machine::exec_display_enter(Boolean is_enter) {
   Boolean ok = true;

   char *s;
   char put_label[8 + 1];
   s = the_pcode_reader->get_string();
   init_string_with_blank_pad(put_label, 8, s);
   delete s;
   char prname[8 + 1];
   s = the_pcode_reader->get_string();
   init_string_with_blank_pad(prname, 8, s);
   delete s;

   // Push pfkey value and keyword/value pairs (or ref label name)
   opcode the_machine_op = execute();
   if (the_machine_op != statement_end_op) {
      // The machine encountered a fatal error
      assert(the_machine_op == halt_op);
      ok = false;
   }

   if (ok) {
      // Keyword count is next value in code stream (-1 means full back ref)
      int_16 keyword_count = the_pcode_reader->get_int_16();
      Boolean fbr = BOOLEAN(keyword_count == -1);
      if (fbr)
         keyword_count = 0;

      // The stack now contains the pfkey followed by a single full back
      // ref (2 items) or a list of keyword/value pairs (pfkey + n*2 items)
      int keylist_size = keyword_count * 2;
      pop_expressions(fbr ? 2 : 1 + keylist_size);

      char ref_label[8 + 1] = "        ";
      char keyword[8 + 1];
      int_32 pfkey = exp[1].integer(0, 32, 59, txt_int_value);
      string_array_data keylist(keylist_size, 0, NULL);

      if (fbr)
         init_string_with_blank_pad(ref_label, 8, exp[2].string());
      else
         for (int i = 0; i < keylist_size;) {
            init_string_with_blank_pad(keyword, 8, exp[2 + i].string());
            keylist[i].set_contents(keyword);
            i++;
            keylist[i].set_contents(exp[2 + i].string()); // value
            i++;
         }

      int_32 rc = wang_os_putparm
         (is_enter, put_label, ref_label, prname, pfkey, keylist);
      if (rc)
         report_fatal_error(21, int_32_to_string(rc));

   }
   return ok;
}


void machine::exec_extract() {
   pop_expressions(2);
   char *keyword = the_pcode_reader->get_string();

   if (strlen(keyword) == 0) {
      delete keyword;
      return;
   }

   assert(strlen(keyword) == 2);
   int_8 size = the_pcode_reader->get_int_8(); // zero means integer keyword

   expression *variable = exp.remove_exp(1);
   expression *value;

   if (size) {
      char *alpha_value = new_string(size + 1);
      init_string_with_blank_pad(alpha_value, size);
      wang_os_extract_alpha(keyword, alpha_value);
      value = new expression(alpha_value, exp[2]);
   }
   else {
      int_32 int_value;
      wang_os_extract_int(keyword, int_value);
      value = new expression(int_value, exp[2]);
   }
   assign(variable, value);
   delete keyword;
}

void machine::exec_extract_records() 
{
	pop_expressions(3);

	expression *variable = exp.remove_exp(1);
	char *file_name = dup_string(exp[3].stripped_string());
	wang_filename name(file_name);
	expression *value;
	int_32 int_value;

	wang_os_readfdr_int(name.filename, name.libname, name.volname, "RC", int_value);

	value = new expression(int_value, exp[2]);

	assign(variable, value);

	delete file_name;
}


void machine::exec_print() {
   opcode       the_machine_op;
   print_opcode the_print_op;
   Boolean      in_error_state = false;
   char        *file_name;
   int_32       form    = -1;
   int_32       copies  = 1;
#ifdef OLD
   char         *fclass = "?";
   char         *status = "S";  // spool
   char         *disp   = "DS"; // save - NOT Wang default
#endif
   char         fclass[] = "?";
   char         status[] = "S";  // spool
   char         disp[]   = "DS"; // save - NOT Wang default
//   int_32       return_code;

   pop_expressions(1);
   file_name = dup_string(exp[1].stripped_string());

   // Process options
   while (! in_error_state) {
      the_machine_op = execute();

      if (the_machine_op == statement_end_op)
         break;

      if (the_machine_op == halt_op) {
         // The machine encountered a fatal error
         in_error_state = true;
         break;
      }
      the_print_op = (print_opcode) (the_pcode_reader->get_usign_8());
      pop_expressions(1);

      switch (the_print_op) {
         case print_class_op : {
            const char *s = exp[1].string();
            fclass[0] = strlen(s) ? toupper(s[0]) : ' ';
            while ((fclass[0] < 'A' || fclass[0] > 'Z') && ! in_error_state) {
               exp[1].request_string(68, txt_option);
               if (exp[1].is_bad())
                  in_error_state = true;
               else {
                  s = exp[1].string();
                  fclass[0] = strlen(s) ? toupper(s[0]) : ' ';
               }
            }
            break;
         }

         case print_status_op : {
            const char *s = exp[1].string();
            int option = 0;
            while (! in_error_state && ! (option = match(s, "spool", "hold"))) {
               exp[1].request_string(69, txt_option);
               if (exp[1].is_bad())
                  in_error_state = true;
               else
                  s = exp[1].string();
            }
            switch (option) {
               case 0 :
                  break;
               case 1 :
                  status[0] = 'S';
                  break;
               case 2 :
                  status[0] = 'H';
                  break;
               default :
                  assert(UNREACHABLE);
            }
            break;
         }

         case print_form_op : {
            form = exp[1].integer(0, 254, 46, txt_int_value);
            in_error_state = exp[1].is_bad();
            break;
         }

         case print_copies_op : {
            copies = exp[1].integer(0, 254, 46, txt_int_value);
            in_error_state = exp[1].is_bad();
            break;
         }

         case print_disp_op : {
            const char *s = exp[1].string();
            int option = 0;
            while (! in_error_state &&
                   ! (option = match(s, "scratch", "requeue", "save")))
            {
               exp[1].request_string(70, txt_option);
               if (exp[1].is_bad())
                  in_error_state = true;
               else
                  s = exp[1].string();
            }
            switch (option) {
               case 0 :
                  break;
               case 1 :
                  disp[0] = 'D';
                  disp[1] = 'X';
                  break;
               case 2 :
                  disp[0] = 'R';
                  disp[1] = 'S';
                  break;
               case 3:
                  disp[0] = 'D';
                  disp[1] = 'S';
                  break;
               default :
                  assert(UNREACHABLE);
            }
            break;
         }
         default :
            assert(UNREACHABLE);
      }
   }
   if (in_error_state)
      enter_cancel_state();
   else {
      wang_filename name(file_name);
      stmt_return_code = wang_os_print(name.filename, name.libname,
         name.volname, fclass, status, form, copies, disp);
   }

   delete file_name;
}


void machine::exec_rename() {
   Boolean is_library = Boolean(the_pcode_reader->get_usign_8() == 1);
   pop_expressions(2);
   char *old_file_name = dup_string(exp[1].stripped_string());
   char *new_file_name = dup_string(exp[2].stripped_string());
   wang_filename old_name(old_file_name);
   wang_filename new_name(new_file_name);
   stmt_return_code =
      wang_os_rename(is_library, old_name.filename, old_name.libname,
      old_name.volname, new_name.filename, new_name.libname);
   delete old_file_name;
   delete new_file_name;
}


void machine::exec_scratch() {
   Boolean is_library = Boolean(the_pcode_reader->get_usign_8() == 1);
   pop_expressions(1);
   char *file_name = dup_string(exp[1].stripped_string());
   wang_filename name(file_name);
   stmt_return_code =
      wang_os_scratch(is_library, name.filename, name.libname, name.volname);
   delete file_name;
}


void machine::exec_set() {
   char *keyword = the_pcode_reader->get_string();
   assert(strlen(keyword) == 2);
   int_8 size = the_pcode_reader->get_int_8(); // zero means integer keyword
   pop_expressions(1);

   if (size) {
      char *alpha_value = new_string(size + 1);
      init_string_with_blank_pad(alpha_value, size, exp[1].string());
      wang_os_set_alpha(keyword, alpha_value);
      delete alpha_value;
   }
   else
      wang_os_set_int(keyword, exp[1].integer(46, txt_int_value));

   delete keyword;
}


void machine::exec_submit() {
   opcode        the_machine_op;
   submit_opcode the_submit_op;
   Boolean       in_error_state = false;
   char         *file_name;
   struct submit_parameters parameters;
   char          proc_id[9]  = "        ";
   Boolean       globals     = false;
   Boolean       environment = false;
   char          jclass[]      = "?";
   char          status[]      = "R";  // run
   char          dump[]        = "P";  // program
   char          action[]      = "C";  // cancel
   int_32        cpulimit    = 0;
   Boolean       requeue     = false;
//   int_32        return_code;
   int_8         arg_count   = the_pcode_reader->get_usign_8();

   // Get file name, proc id and 'using' parameters
   pop_expressions(2 + arg_count);
   file_name = dup_string(exp[1].stripped_string());
   init_string_with_blank_pad(proc_id, 8, exp[2].stripped_string());

   assert(arg_count <= MAX_SUBMIT_PARAMETERS);
   parameters.count = arg_count;
   for (int i = 0; i < parameters.count; i++)
   {
	parameters.string[i] = NULL;
	if (expression::integer_kind == exp[3 + i].kind())
	{
		parameters.integer[i] = exp[3 + i].integer();
	}
	else
	{
		parameters.string[i] = dup_string(exp[3 + i].string());
	}
   }

   // Process options
   while (! in_error_state) {
      the_machine_op = execute();

      if (the_machine_op == statement_end_op)
         break;

      if (the_machine_op == halt_op) {
         // The machine encountered a fatal error
         in_error_state = true;
         break;
      }
      the_submit_op = (submit_opcode) (the_pcode_reader->get_usign_8());

      switch (the_submit_op) {
         case submit_globals_op :
         case submit_environ_op : {
            Boolean opt;
            if (! get_yes_no_option(opt))
               in_error_state = true;
            else {
               if (the_submit_op == submit_globals_op)
                  globals = opt;
               else
                  environment = opt;
            }
            break;
         }

         case submit_class_op : {
            pop_expressions(1);
            const char *s = exp[1].string();
            jclass[0] = strlen(s) ? toupper(s[0]) : ' ';
            while ((jclass[0] < 'A' || jclass[0] > 'Z') && ! in_error_state) {
               exp[1].request_string(68, txt_option);
               if (exp[1].is_bad())
                  in_error_state = true;
               else {
                  s = exp[1].string();
                  jclass[0] = strlen(s) ? toupper(s[0]) : ' ';
               }
            }
            break;
         }

         case submit_status_op : {
            pop_expressions(1);
            const char *s = exp[1].string();
            int option = 0;
            while (! in_error_state && ! (option = match(s, "run", "hold"))) {
               exp[1].request_string(71, txt_option);
               if (exp[1].is_bad())
                  in_error_state = true;
               else
                  s = exp[1].string();
            }
            switch (option) {
               case 0 :
                  break;
               case 1 :
                  status[0] = 'R';
                  break;
               case 2 :
                  status[0] = 'H';
                  break;
               default :
                  assert(UNREACHABLE);
            }
            break;
         }

         case submit_dump_op : {
            pop_expressions(1);
            const char *s = exp[1].string();
            int option = 0;
            while (! in_error_state &&
                   ! (option = match(s, "program", "yes", "no")))
            {
               exp[1].request_string(72, txt_option);
               if (exp[1].is_bad())
                  in_error_state = true;
               else
                  s = exp[1].string();
            }
            switch (option) {
               case 0 :
                  break;
               case 1 :
                  dump[0] = 'P';
                  break;
               case 2 :
                  dump[0] = 'Y';
                  break;
               case 3:
                  dump[0] = 'N';
                  break;
               default :
                  assert(UNREACHABLE);
            }
            break;
         }

         case submit_cpu_op : {
            int_8 args = the_pcode_reader->get_usign_8();
            int_32 ss = 0;
            int_32 mm = 0;
            int_32 hh = 0;
            if (args == 1) {
               pop_expressions(1);
               ss = exp[1].integer(0, 59, 46, txt_int_value);
               in_error_state = exp[1].is_bad();
            }
            else {
               pop_expressions(3);
               assert(args == 3);
               hh = exp[1].integer(0, 99, 46, txt_int_value);
               in_error_state = exp[1].is_bad();
               if (! in_error_state) {
                  mm = exp[2].integer(0, 59, 46, txt_int_value);
                  in_error_state = exp[2].is_bad();
                  if (! in_error_state) {
                     ss = exp[3].integer(0, 59, 46, txt_int_value);
                     in_error_state = exp[3].is_bad();
                  }
               }
            }
            if (! in_error_state)
               cpulimit = hh*3600 + mm*60 + ss;
            break;
         }

         case submit_action_op : {
            pop_expressions(1);
            const char *s = exp[1].string();
            int option = 0;
            while (! in_error_state &&
                   ! (option = match(s, "cancel", "warn", "pause")))
            {
               exp[1].request_string(73, txt_option);
               if (exp[1].is_bad())
                  in_error_state = true;
               else
                  s = exp[1].string();
            }
            switch (option) {
               case 0 :
                  break;
               case 1 :
                  action[0] = 'C';
                  break;
               case 2 :
                  action[0] = 'W';
                  break;
               case 3:
                  action[0] = 'P';
                  break;
               default :
                  assert(UNREACHABLE);
            }
            break;
         }

         case submit_disp_op : {
            pop_expressions(1);
            const char *s = exp[1].string();
            int option = 0;
            while (! in_error_state && ! (option = match(s, "requeue"))) {
               exp[1].request_string(74, txt_option);
               if (exp[1].is_bad())
                  in_error_state = true;
               else
                  s = exp[1].string();
            }
            requeue = BOOLEAN(option != 0);
            break;
         }

         default :
            assert(UNREACHABLE);
      }
   }
   if (in_error_state)
      enter_cancel_state();
   else {
      wang_filename name(file_name);

      // Make this machine available to wang_os_submit() so it can write
      // out the global variables.
      wang_os_access_to_machine(this);

      stmt_return_code = wang_os_submit(name.filename, name.libname,
         name.volname, proc_id, &parameters, globals, environment,
         jclass, status, dump, cpulimit, action, requeue);
   }

   delete file_name;
   for (int j = 0; j < parameters.count; j++)
   {
	if (parameters.string[j]) delete parameters.string[j];
   }
}


void machine::file_lib_vol_exists() {
   pop_expressions(1);
   wang_filename name(exp[1].string());
   the_stack->push(new expression(wang_os_exists(name), exp[1]));
}


void machine::resolve_full_backref() {
   pop_expressions(1);
   the_stack->push
      (new expression(wang_os_backref_full(exp[1].string()), exp[1]));
}


void machine::resolve_partial_backref() {
   pop_expressions(2);
   the_stack->push
      (new expression(wang_os_backref_partial
         (exp[1].string(), exp[2].string()), exp[1], exp[2]));
}

#endif

/*
**	History:
**	$Log: e_wang.cpp,v $
**	Revision 1.9  1998/08/31 19:50:32  gsl
**	drcs update
**	
**	Revision 1.8  1998-08-31 15:13:45-04  gsl
**	drcs update
**
**	Revision 1.7  1996-07-25 14:15:03-04  gsl
**	Renamed from e_wang.cc to e_wang.cpp
**
**	Revision 1.6  1995-10-17 10:31:43-07  gsl
**	Add a call to wang_os_access_to_machine() in exec_submit() so that
**	the wang_os_submit() has access to this machine so it can get at
**	the global symbols tables.
**
**
**
*/

//
//	History:
//	$Log: e_wang.cpp,v $
//	Revision 1.9  1998/08/31 19:50:32  gsl
//	drcs update
//	
//	Revision 1.8  1998-08-31 15:13:45-04  gsl
//	drcs update
//
//

//	
//	RCS file: /disk1/neomedia/RCS/wisp/wproc/e_wang.cpp,v
//	Working file: e_wang.cpp
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
//	date: 1996-07-25 14:15:03-04;  author: gsl;  state: V4_3_00;  lines: +7 -2
//	Renamed from e_wang.cc to e_wang.cpp
//	----------------------------
//	revision 1.6
//	date: 1995-10-17 13:31:43-04;  author: gsl;  state: V3_3_19;  lines: +16 -5
//	Add a call to wang_os_access_to_machine() in exec_submit() so that
//	the wang_os_submit() has access to this machine so it can get at
//	the global symbols tables.
//	----------------------------
//	revision 1.5
//	date: 1995-06-02 10:49:55-04;  author: gsl;  state: V3_3_18;  lines: +2 -2
//	fix warning
//	----------------------------
//	revision 1.4
//	date: 1995-04-25 05:59:53-04;  author: gsl;  state: V3_3_16;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:52:11-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:32:49-05;  author: gsl;  state: V3_3x12;  lines: +60 -26
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:06-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
