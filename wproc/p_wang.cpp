//
//	Copyright (c) Shell Stream Software LLC, All Rights Reserved.
//
//	Project:	WPROC
//	
//// Copyright (c) Lexical Software, 1992.  All rights reserved.
//
// Module : p_wang.cpp
// Author : George Soules
// Date   : 28 February 1992

#if WANG && ! RUNTIME

// Specification
#include "compiler.hpp"

// Classes
#include "tables.hpp"

// Definitions and subprograms
#include <ctype.h>
#include "debugaid.hpp"
#include "memory.hpp"
#include "subops.hpp"
#include "utility.hpp"


#define dont_emit_code_for(stmt)       \
   static int call = 0;                \
   if (++call == 1) {                  \
      the_emitter->pause_emission();   \
      stmt;                            \
      the_emitter->resume_emission();  \
      call = 0;                        \
      return;                          \
   }


class keyword : public table_entry {
   public:
      keyword(char *a_name, char* kind, int_8 size) : table_entry(a_name) {
         assert(kind ? strlen(kind) == 2 : 1);
         the_kind = kind;
         the_size = size;
      }
      int_8    the_size; // size -1 means 32 bit integer
      char    *the_kind;
};


//  delacl_statement
//     -> 'delacl' ('file' | 'print')
//     -> 'delacl' 'file' '=' (filename-exp)

void compiler::delacl_stmt() {
   dont_emit_code_for(delacl_stmt());

   trace_begin(parser, "delacl_stmt");
   semantic_warning(48);
   discard();  // 'delacl'
   if (keyword_is("print"))
      discard(); // 'print'
   else {
      require_syntax("file", 75);
      discard(); // 'file'
      if (lexeme_is("=")) {
         discard(); // '='
         parse_filename();
      }
   }
   trace_end(parser);
}


//  dismount_statement
//     -> 'dismount' 'disk'? volname-exp ('vsid' '=' exp)?
//     -> 'dismount' 'tape' volname-exp

void compiler::dismount_stmt() {
   dont_emit_code_for(dismount_stmt());

   trace_begin(parser, "dismount_stmt");
   Boolean disk = true;
   semantic_warning(48);
   discard();  // 'dismount'
   if (keyword_is("tape")) {
      discard(); // 'tape'
      disk = false;
   }
   else if (keyword_is("disk"))
      discard(); // 'disk'

   if (parse_volname()) {
      if (disk && keyword_is("vsid")) {
         discard(); // 'vsid'
         require_syntax("=", 3);
         discard(); // '='
         require_nonterminal(expression(8));
      }
   }

   trace_end(parser);
}


//  extracl_stmt
//     -> 'extracl' ('file' | 'print') variable_name
//     -> 'extracl' variable_name '=' filename-exp

void compiler::extracl_stmt() {
   dont_emit_code_for(extracl_stmt());

   trace_begin(parser, "extracl_stmt");
   semantic_warning(48);
   discard();  // 'extracl'
   if (kind_is(tk_variable)) {
      discard();  // variable
      require_syntax("=", 3);
      discard(); // '='
      parse_filename();
   }
   else {
      if (! keyword_is("print"))
         require_syntax("file", 75);
      discard(); // 'print' or 'file'
      require_kind(tk_variable, 9);
      discard(); // variable
   }
   trace_end(parser);
}


table *extract_options = NULL;

void init_extract_options() {
   assert(! extract_options);
   extract_options = new table();
//                                   Name         Kind  Size
   extract_options->add(new keyword("clusvol",    NULL,  0));
   extract_options->add(new keyword("curlib",     "CL",  8));
   extract_options->add(new keyword("curvol",     "CV",  6));
   extract_options->add(new keyword("diskio",     NULL,  0));
   extract_options->add(new keyword("fileclas",   NULL,  0));
   extract_options->add(new keyword("form#",      "FN",  0));
   extract_options->add(new keyword("inlib",      "IL",  8));
   extract_options->add(new keyword("invol",      "IV",  6));
   extract_options->add(new keyword("jobclass",   "JC",  1));
   extract_options->add(new keyword("joblimit",   "JL",  0));
   extract_options->add(new keyword("jobqueue",   "JS",  1));
   extract_options->add(new keyword("lines",      "LI",  0));
   extract_options->add(new keyword("otio",       NULL,  0));
   extract_options->add(new keyword("outlib",     "OL",  8));
   extract_options->add(new keyword("outvol",     "OV",  6));
   extract_options->add(new keyword("printer",    "PR",  0));
   extract_options->add(new keyword("printio",    NULL,  0));
   extract_options->add(new keyword("prntmode",   "PM",  1));
   extract_options->add(new keyword("proglib",    "PL",  8));
   extract_options->add(new keyword("progvol",    "PV",  6));
   extract_options->add(new keyword("prtclass",   "PC",  1));
   extract_options->add(new keyword("prtfclas",   NULL,  0));
   extract_options->add(new keyword("runlib",     "RL",  8));
   extract_options->add(new keyword("runvol",     "RV",  6));
   extract_options->add(new keyword("spoollib",   "SL",  8));
   extract_options->add(new keyword("spoolib",    "SL",  8));
   extract_options->add(new keyword("spoolscr",   NULL,  0));
   extract_options->add(new keyword("spoolsys",   NULL,  0));
   extract_options->add(new keyword("spoolvol",   "SV",  6));
   extract_options->add(new keyword("syslib",     NULL,  0));
   extract_options->add(new keyword("sysvol",     NULL,  0));
   extract_options->add(new keyword("syswork",    NULL,  0));
   extract_options->add(new keyword("tapeio",     NULL,  0));
   extract_options->add(new keyword("tape#",      NULL,  0));
   extract_options->add(new keyword("task#",      "T#",  0));
   extract_options->add(new keyword("tasktype",   "TT",  1));
   extract_options->add(new keyword("userid",     "ID",  3));
   extract_options->add(new keyword("username",   "NA",  80));
   extract_options->add(new keyword("version",    NULL,  0));
   extract_options->add(new keyword("worklib",    "WL",  8));
   extract_options->add(new keyword("workvol",    "WV",  6));
   extract_options->add(new keyword("ws",         "W#",  0));
   extract_options->add(new keyword("wsio",       NULL,  0));
}


void compiler::extract_stmt() {
   trace_begin(parser, "extract_stmt");
   discard();  // 'extract'

   if (! extract_options)
      init_extract_options();

//   Boolean  supported;
   keyword *the_keyword;
   symbol   *the_symbol;
   int       args;

   extract_options->set_none_seen();

   while (true) {
      require_kind(tk_variable, 9);
      range the_range(the_token);
      the_symbol = lookup_or_create_symbol(the_token->lexeme());
      args = parse_variable(the_symbol, the_range);
      if (args == -1)
         return;

      require_syntax("=", 3);
      discard(); // '='

      if (! kind_is(tk_identifier))
         improper_syntax(82);

      the_keyword = (keyword *) extract_options->lookup(the_token->lexeme());
      if (the_keyword) {
         require_semantics(the_keyword->seen() == 1, 22);
         emit_string();
         the_emitter->emit_opcode(extract_op);
         if (the_keyword->the_kind != NULL) {
            the_emitter->emit_string(the_keyword->the_kind);
            the_emitter->emit_int_8(the_keyword->the_size);
         }
         else {
            semantic_warning(55);
            the_emitter->emit_string(""); // tells runtime to discard lvalue
         }
         discard(); // keyword
      }
      else if (keyword_is("records")) {
		Boolean bad_syntax  = false;
		emit_string();
		discard(); // 'records'
		if (keyword_is("used")) {
			discard(); // 'used'
			if (keyword_is("by"))
				discard(); // 'by'
			else
				bad_syntax = true;
		}
		else
			bad_syntax = true;

		if (bad_syntax) 
		{
			improper_syntax(80);
		}

		parse_filename();
		the_emitter->emit_opcode(extract_records_op);
      }
      else if (keyword_matches("blocks", "set")) {
         Boolean set_type_of = false;
         Boolean bad_syntax  = false;
         semantic_warning(49);

         if (keyword_is("set")) {
            set_type_of = true;
            discard(); // 'set'
            if (keyword_is("type")) {
               discard(); // 'type'
               if (keyword_is("of"))
                  discard(); // 'of'
               else
                  bad_syntax = true;
            }
            else
               bad_syntax = true;
         }
         else {
            if (keyword_is("blocks")) {
               discard(); // 'blocks'
               if (keyword_is("allocated")) {
                  discard(); // 'allocated'
                  if (keyword_is("for"))
                     discard(); // 'for'
                  else
                     bad_syntax = true;
               }
               else
                  bad_syntax = true;
            }
#if 0
            else if (keyword_is("records")) {
               discard(); // 'records'
               if (keyword_is("used")) {
                  discard(); // 'used'
                  if (keyword_is("by"))
                     discard(); // 'by'
                  else
                     bad_syntax = true;
               }
               else
                  bad_syntax = true;
            }
#endif
         }
         if (bad_syntax) {
            improper_syntax(80);
         }
         else {
            the_emitter->pause_emission();
            if (set_type_of)
               parse_volname();
            else
               parse_filename();
            the_emitter->resume_emission();
         }
	 // Push a dummy value to satisfy exec_extract
         the_emitter->emit_opcode(push_int_8_op);
         the_emitter->emit_int_8(0);
	 
         the_emitter->emit_opcode(extract_op);
         the_emitter->emit_string(""); // tells runtime to discard lvalue
         return;
      }
      else
         improper_syntax(82);

      if (kind_is(tk_comma))
         discard(); // ','
      else
         break;
   }
   trace_end(parser);
}


//  mount_stmt
//     -> 'mount' 'disk'? vol_name-exp ('vsid' '=' exp)? ('on' exp)?
//        ('with'? ('standard' | 'no') ('label' | 'labels')?
//        ('for'? protection 'usage')?
//        (option '=' yes_no-exp) list ','
//     -> 'mount' 'tape' vol_name-exp 'on' exp
//        ('with'? ('ibm' | 'ansi' | 'no') ('label' | 'labels')?
//        ('for'? ('shared' | 'exclusive') 'usage')?
//  protection
//     -> 'shared' | 'exclusive' | 'protected' | 'restricted' 'removal'?
//  option
//     -> 'paging' | 'secure' | 'spool' | 'work'

void compiler::mount_stmt() {
   dont_emit_code_for(mount_stmt());

   trace_begin(parser, "mount_stmt");
   semantic_warning(48);
   discard();  // 'mount'
   if (keyword_is("tape")) {
      discard(); // 'tape'
      if (! parse_volname())
         return;
      require_syntax("on", 80);
      discard(); // 'on'
      require_nonterminal(expression(8));

      if (keyword_matches("with", "ibm", "ansi", "no")) {
         if (keyword_is("with"))
            discard(); // 'with'

         if (! keyword_matches("ibm", "ansi", "no"))
            improper_syntax(80);
         discard(); // keyword

         if (! keyword_matches("label", "labels"))
            improper_syntax(80);
         discard(); // 'label' or 'labels'
      }
      if (keyword_matches("for", "shared", "exclusive")) {
         if (keyword_is("for"))
            discard(); // 'for'
         if (! keyword_matches("shared", "exclusive"))
            improper_syntax(80);
         discard(); // keyword

         if (! (keyword_is("usage")))
            improper_syntax(80);
         discard(); // 'usage'
      }
   }
   else {
      allow_syntax("disk");
      if (! parse_filename())
         return;
      if (keyword_is("vsid")) {
         discard(); // 'vsid'
         require_syntax("=", 3);
         discard(); // '='
         require_nonterminal(expression(8));
      }
      if (keyword_is("on")) {
         discard(); // 'on'
         require_nonterminal(expression(8));
      }
      if (keyword_matches("with", "standard", "no")) {
         if (keyword_is("with"))
            discard(); // 'with'

         if (! keyword_matches("standard", "no"))
            improper_syntax(80);
         discard(); // keyword

         if (! keyword_matches("label", "labels"))
            improper_syntax(80);
         discard(); // 'label' or 'labels'
      }
      if (keyword_matches("for", "shared", "exclusive", "protected", "restricted")) {
         if (keyword_is("for"))
            discard(); // 'for'
         if (! keyword_matches("shared", "exclusive", "protected", "restricted"))
            improper_syntax(80);
         if (keyword_is("restricted")) {
            discard(); // 'restricted'
            allow_syntax("removal");
         }
         else
            discard(); // keyword

         if (! (keyword_is("usage")))
            improper_syntax(80);
         discard(); // 'usage'
      }
      Boolean comma_found = false;
      while (keyword_matches("paging", "secure", "spool", "work")) {
         discard(); // keyword
         require_syntax("=", 3);
         discard(); // '='
         if (! parse_yes_no())
            return;
         comma_found = BOOLEAN(kind_is(tk_comma));
         if (comma_found)
            discard(); // ','
      }
      if (comma_found)
         improper_syntax(65);
   }
   trace_end(parser);
}


// operator_stmt

void compiler::operator_stmt() {
   dont_emit_code_for(operator_stmt());

   trace_begin(parser, "operator_stmt");
   semantic_warning(48);
   discard();  // 'operator'
   require_syntax("message", 77);
   discard(); // 'message'
   require_syntax("=", 3);
   discard(); // '='
   require_nonterminal(expression(8));
   trace_end(parser);
}


// options_stmt

void compiler::options_stmt() {
   dont_emit_code_for(options_stmt());

   trace_begin(parser, "options_stmt");
   semantic_warning(48);
   discard();  // 'options'
   require_syntax("procmsg", 78);
   discard(); // 'message'
   require_syntax("=", 3);
   discard(); // '='
   parse_yes_no();
   trace_end(parser);
}


table *print_options   = NULL;

void init_print_options() {
   assert(! print_options);
   print_options = new table();
//                                     Name          Kind             YesNo
   print_options->add(new option_name("class",       print_class_op,  false));
   print_options->add(new option_name("status",      print_status_op, false));
   print_options->add(new option_name("form#",       print_form_op,   false));
   print_options->add(new option_name("copies",      print_copies_op, false));
   print_options->add(new option_name("disposition", print_disp_op,   false));
   print_options->add(new option_name("disp",        print_disp_op,   false));
}


void compiler::print_stmt() {
   trace_begin(parser, "print_stmt");
   discard();  // 'print'

   Boolean ok;

   if (! parse_filename())
      return;

   the_emitter->emit_opcode(print_op);

   if (! print_options)
      init_print_options();

   option_name *the_option;
   print_options->set_none_seen();

   while (kind_is(tk_comma)) {
      discard(); // ','
      the_option =
         (option_name *) print_options->lookup(the_token->lexeme());
      if (the_option) {
         require_semantics(the_option->seen() == 1, 22);
         discard(); // option
         require_syntax("=", 3);
         discard(); // '='

         if (the_option->kind() == print_form_op ||
             the_option->kind() == print_copies_op ||
             (the_option->kind() == print_class_op && ! kind_is(tk_identifier)))
         {
            require_nonterminal(expression(58));
         }
         else {
            switch (the_option->kind()) {
               case print_class_op :
                  ok = parse_char_option(83, 'A', 'Z');
                  break;
               case print_status_op :
                  ok = parse_keyword_option(84, "spool", "hold");
                  break;
               case print_disp_op :
                  ok = parse_keyword_option(85, "scratch", "requeue", "save");
                  break;
               default :
                  assert(UNREACHABLE);
            }
            if (! ok)
               return;
         }

         the_emitter->emit_opcode(print_subop);
         the_emitter->emit_usign_8(the_option->kind());
      }
      else
         improper_syntax(65);
   }
   the_emitter->emit_opcode(statement_end_op);
   the_emitter->emit_opcode(label_assign_op);
   trace_end(parser);
}


void compiler::protect_stmt() {
   dont_emit_code_for(protect_stmt());

   trace_begin(parser, "protect_stmt");

   Boolean ok;
   semantic_warning(48);
   discard();  // 'protect'
   if (keyword_is("library")) {
      discard(); // 'library'
      ok = parse_libname();
   }
   else
      ok = parse_filename();

   if (ok) {
      require_syntax("to", 24);
      discard(); // 'to'
      Boolean option_required = true;

      while (keyword_matches("owner", "period", "fileclas")) {
         discard(); // keyword
         require_syntax("=", 3);
         discard(); // '='
         if (kind_is(tk_identifier))
            discard(); // identifier
         else
            require_nonterminal(expression(8));
         option_required = BOOLEAN(kind_is(tk_comma));
         if (! option_required)
            break;
         discard(); // ','
      }
      if (option_required)
         improper_syntax(66);
   }
   trace_end(parser);
}


//  rename_statement
//     -> 'rename' 'library'? filename-exp 'to' filename-exp

void compiler::rename_stmt() {
   trace_begin(parser, "rename_stmt");
   discard(); // 'rename'

   Boolean ok;
   Boolean library = false;

   if (keyword_is("library")) {
      discard(); // 'library'
      ok = parse_libname();
      library = true;
   }
   else
      ok = parse_filename();

   if (ok) {
      require_syntax("to", 24);
      discard(); // 'to'
      if (library)
         ok = parse_libname();
      else
         ok = parse_filename();
   }
   if (! ok)
      return;

   the_emitter->emit_opcode(rename_op);
   the_emitter->emit_usign_8(library ? 1 : 0);
   the_emitter->emit_opcode(label_assign_op);
   trace_end(parser);
}


//  scratch_statement
//     -> 'scratch' 'library' filename-exp

void compiler::scratch_stmt() {
   trace_begin(parser, "scratch_stmt");
   discard(); // 'scratch'

   Boolean ok;
   Boolean library = false;

   if (keyword_is("library")) {
      discard(); // 'library'
      ok = parse_libname();
      library = true;
   }
   else
      ok = parse_filename();

   if (! ok)
      return;

   the_emitter->emit_opcode(scratch_op);
   the_emitter->emit_usign_8(library ? 1 : 0);
   the_emitter->emit_opcode(label_assign_op);
   trace_end(parser);
}


table *set_options = NULL;

void init_set_options() {
   assert(! set_options);
   set_options = new table();
//                               Name         Kind  Size
   set_options->add(new keyword("fileclas",   NULL,  0));
   set_options->add(new keyword("form#",      "FN",  0));
   set_options->add(new keyword("inlib",      "IL",  8));
   set_options->add(new keyword("invol",      "IV",  6));
   set_options->add(new keyword("jobclass",   "JC",  1));
   set_options->add(new keyword("joblimit",   "JL",  0));
   set_options->add(new keyword("jobqueue",   "JS",  1));
   set_options->add(new keyword("lines",      "LI",  0));
   set_options->add(new keyword("opermsgs",   NULL,  0));
   set_options->add(new keyword("outlib",     "OL",  8));
   set_options->add(new keyword("outvol",     "OV",  6));
   set_options->add(new keyword("printer",    "PR",  0));
   set_options->add(new keyword("prntmode",   "PM",  1));
   set_options->add(new keyword("proglib",    "PL",  8));
   set_options->add(new keyword("progvol",    "PV",  6));
   set_options->add(new keyword("prtclass",   "PC",  1));
   set_options->add(new keyword("prtfclas",   NULL,  0));
   set_options->add(new keyword("records",    NULL,  0));
   set_options->add(new keyword("runlib",     "RL",  8));
   set_options->add(new keyword("runvol",     "RV",  6));
   set_options->add(new keyword("spoollib",   "SL",  8));
   set_options->add(new keyword("spoolib",    "SL",  8));
   set_options->add(new keyword("spoolscr",   NULL,  0));
   set_options->add(new keyword("spoolsys",   NULL,  0));
   set_options->add(new keyword("spoolsysrc", NULL,  0));
   set_options->add(new keyword("spoolvol",   "SV",  6));
   set_options->add(new keyword("workvol",    "WV",  6));
}


void compiler::set_stmt() {
   trace_begin(parser, "set_stmt");
   discard(); // 'set'

   if (! set_options)
      init_set_options();

   Boolean  supported;
   keyword *the_keyword;

   set_options->set_none_seen();

   while (true) {
      the_keyword = (keyword *) set_options->lookup(the_token->lexeme());
      if (the_keyword) {
         supported = BOOLEAN(the_keyword->the_kind != NULL);
         require_semantics(the_keyword->seen() == 1, 22);
         if (! supported) {
            semantic_warning(55);
            the_emitter->pause_emission();
         }
         discard(); // keyword
         require_syntax("=", 3);
         discard(); // '='

//FIX003
//FIX the SET XXX= , problem
         if (kind_is(tk_comma)) {
            the_emitter->emit_opcode(push_string_op);
            the_emitter->emit_string(" ");
         }
         else if (! parse_identifier())
            expression(63);

         if (! supported)
            the_emitter->resume_emission();
         if (in_error_state)
            return;
         if (supported) {
            the_emitter->emit_opcode(set_op);
            the_emitter->emit_string(the_keyword->the_kind);
            the_emitter->emit_int_8(the_keyword->the_size);
         }
      }
      else
         improper_syntax(82);

      if (kind_is(tk_comma))
         discard(); // ','
      else
         break;
   }
   trace_end(parser);
}


//  setacl_stmt
//     -> 'setacl' ('file' | 'print') (variable_name | permission)
//     -> 'setacl' file '=' (variable_name | permission | filename-exp)
//
//  permission
//     -> (('type' exp) | ('id' '=' exp) | ('level' '=' 'exp')) (',' permission)*

//  Note: the description of the SETACL statement in the Wang Procedure Language
//  Reference (6th Edition, 12/87) is incorrect.  The syntax diagram does not
//  match the general rules or the examples, therefore, the recognition logic
//  below is very loose.  It should recognize legal SETACL statements, but
//  will also recognize some illegal forms.

void compiler::setacl_stmt() {
   dont_emit_code_for(setacl_stmt());

   trace_begin(parser, "setacl_stmt");
   semantic_warning(48);
   discard();  // 'setacl'
   if (keyword_is("print"))
      discard(); // 'print'
   else {
      require_syntax("file", 75);
      discard(); // 'file'
      if (lexeme_is("=")) {
         discard(); // '='
         if (! parse_filename())
            return;
      }
   }
   if (kind_is(tk_variable))
      discard();  // variable
   else if (keyword_is("type")) {
      Boolean comma_found;
      while (keyword_matches("type", "id", "level")) {
         discard(); // keyword
         require_syntax("=", 3);
         discard(); // '='
         if (kind_is(tk_identifier))
            discard(); // identifier
         else
            require_nonterminal(expression(8));
         comma_found = BOOLEAN(kind_is(tk_comma));
         if (! comma_found)
            break;
         discard(); // ','
      }
      if (comma_found)
         improper_syntax(65);
   }
   else
      improper_syntax(76);
   trace_end(parser);
}


// submit_stmt

table *submit_options   = NULL;

void init_submit_options() {
   assert(! submit_options);
   submit_options = new table();
//                                      Name          Kind               YesNo
   submit_options->add(new option_name("globals",     submit_globals_op, true));
   submit_options->add(new option_name("environment", submit_environ_op, true));
   submit_options->add(new option_name("class",       submit_class_op,   false));
   submit_options->add(new option_name("status",      submit_status_op,  false));
   submit_options->add(new option_name("dump",        submit_dump_op,    false));
   submit_options->add(new option_name("cpulimit",    submit_cpu_op,     false));
   submit_options->add(new option_name("action",      submit_action_op,  false));
   submit_options->add(new option_name("disp",        submit_disp_op,    false));
   submit_options->add(new option_name("disposition", submit_disp_op,    false));
}


void compiler::submit_stmt() {
   trace_begin(parser, "submit_stmt");
   discard();  // 'submit'

   Boolean ok;
   int_8   cpu_args;

   if (! parse_filename())
      return;

   if (keyword_is("as")) {
      discard(); // 'as'
      if (kind_is(tk_identifier)) {
         emit_string();
         discard(); // procedure id
      }
      else
         require_nonterminal(expression(8));
   }
   else {
      the_emitter->emit_opcode(push_string_op);
      the_emitter->emit_string("");
   }

   int_8 args = 0;

   if (keyword_is("using")) {
      discard(); // 'using'
      if (kind_is(tk_identifier))
         improper_syntax(54);
      while (true) {
         require_nonterminal(expression(54));
         args += 1;
         if (kind_is(tk_comma)) {
            if (the_scanner->lookahead(1)->kind() == tk_identifier)
               break;
            else
               discard(); // ','
         }
         else
            break;
      }
   }

   the_emitter->emit_opcode(submit_op);
   the_emitter->emit_int_8(args);

   if (! submit_options)
      init_submit_options();

   option_name *the_option;
   submit_options->set_none_seen();

   while (kind_is(tk_comma)) {
      discard(); // ','
      the_option =
         (option_name *) submit_options->lookup(the_token->lexeme());
      if (the_option) {
         require_semantics(the_option->seen() == 1, 22);
         if (the_option->kind() == submit_dump_op ||
             the_option->kind() == submit_cpu_op ||
             the_option->kind() == submit_action_op ||
             the_option->kind() == submit_disp_op)
         {
            semantic_warning(55);
         }
         discard(); // option
         require_syntax("=", 3);
         discard(); // '='

         if (the_option->kind() == submit_class_op && ! kind_is(tk_identifier)) {
            require_nonterminal(expression(58));
         }
         else {
            switch (the_option->kind()) {
               case submit_globals_op :
               case submit_environ_op :
                  ok = parse_yes_no();
                  break;
               case submit_class_op :
                  ok = parse_char_option(83, 'A', 'Z');
                  break;
               case submit_status_op :
                  ok = parse_keyword_option(86, "run", "hold");
                  break;
               case submit_dump_op :
                  ok = parse_keyword_option(87, "program", "yes", "no");
                  break;
               case submit_cpu_op : {
                  require_nonterminal(expression(8));
                  if (kind_is(tk_colon)) {
                     discard(); // ':'
                     require_nonterminal(expression(8));
                     require_kind(tk_colon, 90);
                     discard(); // ':'
                     require_nonterminal(expression(8));
                     cpu_args = 3;
                  }
                  else
                     cpu_args = 1;
                  break;
               }
               case submit_action_op :
                  ok = parse_keyword_option(88, "cancel", "warn", "pause");
                  break;
               case submit_disp_op :
                  ok = parse_keyword_option(89, "requeue");
                  break;
               default :
                  assert(UNREACHABLE);
            }
            if (! ok)
               return;
         }

         the_emitter->emit_opcode(submit_subop);
         the_emitter->emit_usign_8(the_option->kind());
         if (the_option->kind() == submit_cpu_op)
            the_emitter->emit_int_8(cpu_args);
      }
      else
         improper_syntax(65);
   }
   the_emitter->emit_opcode(statement_end_op);
   the_emitter->emit_opcode(label_assign_op);
   trace_end(parser);
}


//  trace_statement
//     -> 'trace' 'end'
//     -> 'trace' ('into' filename-exp)? (',' trace_option)*

//  trace_option
//     -> ('resources' | 'scratch' | 'variables') '=' yes_no-exp
//     -> 'statements' '=' ('run' | 'all')

void compiler::wang_trace_stmt() 
{
	Boolean ok, into, options;
	int_8	v_resources, v_scratch, v_statements, v_variables;

	into = options = false;
	v_resources = v_scratch = v_statements = v_variables = -1;

	if (keyword_is("into")) 
	{
		discard(); // 'into'
		into = true;
		ok = parse_filename();
		if (!ok)
			return;
	}

	while (kind_is(tk_comma)) 
	{
		discard(); // ','
		options = true;

      		if (keyword_matches("resources", "scratch", "statements", "variables")) 
		{
			if (keyword_is("resources"))
			{
				discard(); // keyword
				require_syntax("=", 3);
				discard(); // '='

				if (kind_is(tk_identifier)) 
				{
					if (keyword_is("no"))
						v_resources = 0;
					else if (keyword_is("yes"))
						v_resources = 1;
					else
						improper_syntax(79);

					discard(); // 'yes' or 'no'
				}
				else
					require_nonterminal(expression(79));
			}
			else if (keyword_is("scratch"))
			{
				discard(); // keyword
				require_syntax("=", 3);
				discard(); // '='

				if (kind_is(tk_identifier)) 
				{
					if (keyword_is("no"))
						v_scratch = 0;
					else if (keyword_is("yes"))
						v_scratch = 1;
					else
						improper_syntax(79);

					discard(); // 'yes' or 'no'
				}
				else
					require_nonterminal(expression(79));
			}
			else if (keyword_is("statements"))
			{
				discard(); // keyword
				require_syntax("=", 3);
				discard(); // '='

				if (kind_is(tk_identifier)) 
				{
					if (keyword_is("all"))
						v_statements = 0;
					else if (keyword_is("run"))
						v_statements = 1;
					else
						improper_syntax(79);

					discard(); // 'run' or 'all'
				}
				else
					require_nonterminal(expression(79));
			}
			else if (keyword_is("variables"))
			{
				discard(); // keyword
				require_syntax("=", 3);
				discard(); // '='

				if (kind_is(tk_identifier)) 
				{
					if (keyword_is("no"))
						v_variables = 0;
					else if (keyword_is("yes"))
						v_variables = 1;
					else
						improper_syntax(79);

					discard(); // 'yes' or 'no'
				}
				else
					require_nonterminal(expression(79));
			}
			else 
			{
				improper_syntax(65);
			}
		}
		else
			improper_syntax(65);
	}

	the_emitter->emit_opcode(trace_op);
	the_emitter->emit_usign_8(3);
	the_emitter->emit_usign_8((into)?1:0);
	the_emitter->emit_int_8(v_resources);
	the_emitter->emit_int_8(v_scratch);
	the_emitter->emit_int_8(v_statements);
	the_emitter->emit_int_8(v_variables);
	the_emitter->emit_opcode(label_assign_op);
}
#ifdef OLD
void compiler::wang_trace_stmt() {
   dont_emit_code_for(wang_trace_stmt());

   Boolean statements;
   semantic_warning(49); // 'into' or ',' syntax not supported

   if (keyword_is("into")) {
      discard(); // 'into'
      if (! parse_filename())
         return;
   }
   while (kind_is(tk_comma)) {
      discard(); // ','
      if (keyword_matches("resources", "scratch", "statements", "variables")) {
         statements = BOOLEAN(keyword_is("statements"));
         discard(); // keyword
         require_syntax("=", 3);
         discard(); // '='

         if (statements) {
            if (kind_is(tk_identifier)) {
               if (keyword_matches("run", "all"))
                  discard(); // 'run' or 'all'
               else
                  improper_syntax(79);
            }
            else
               require_nonterminal(expression(79));
         }
         else {
            if (! parse_yes_no())
               return;
         }
      }
      else
         improper_syntax(65);
   }
}
#endif

table *display_enter_table = NULL;

void init_display_enter_table() {
   // Use a table instead of keyword_is() to allow abbreviations
   assert(! display_enter_table);
   display_enter_table = new table();
   display_enter_table->add(new table_entry("display", 4));
   display_enter_table->add(new table_entry("enter", 3));
}


Boolean compiler::parse_display_enter() {
   const int    prname_size = 8;
   char        *label;
   Boolean      done;
   Boolean      is_enter;
   Boolean      require_keyword;
   Boolean      found_keyword;
   Boolean      emitted_pfkey;
   int_16       keyword_count;
   table_entry *the_keyword;

   if (! display_enter_table)
      init_display_enter_table();

   while (true) {
      // Parse either Display/Enter OR a label decl followed by Display/Enter

      the_keyword = NULL;

      if (kind_is(tk_identifier))
         the_keyword = display_enter_table->lookup(the_token->lexeme(), true);
      else if (kind_is(tk_label)) {
         token *tk = the_scanner->lookahead(1);
         if (tk->kind() == tk_identifier)
            the_keyword = display_enter_table->lookup(tk->lexeme(), true);
      }
      if (the_keyword) {
         if (kind_is(tk_label)) {
            // Remove ':' from end of label declaration
            the_token->lexeme()[the_token->lexeme_size() - 1] = '\0';
            if (the_token->lexeme_size() > WANG_LABELNAME_SIZE+1)	// lexeme includes the trailing ":" character
               semantic_warning(51);
            the_token->shift_to_upper_case();
            label = dup_string(the_token->lexeme());
            discard(); // label
         }
         else
            label = dup_string("");
         discard(); // 'display' or 'enter'
      }
      else
         break;

      is_enter = the_keyword->matches("enter");
      the_emitter->emit_opcode(run_subop);
      the_emitter->emit_usign_8(is_enter ? run_enter_op : run_display_op);
      the_emitter->emit_string(label);
      delete label;

      // Parse PRNAME
      if (kind_is(tk_identifier)) {
         if (the_token->lexeme_size() > prname_size)
            semantic_warning(57);
         the_token->shift_to_upper_case();
         the_emitter->emit_string(the_token->lexeme());
         discard(); // prname
      }
      else {
         syntax_error(91);
         return false;
      }

      done = false;
      require_keyword = false;
      keyword_count = 0;

      // Parse the optional pfkey allowed in an Enter clause
      emitted_pfkey = false;
      if (is_enter) {
         if (kind_is_expression() && ! is_full_backward_ref()) {
            expression(92);
            if (in_error_state)
               return false;
            emitted_pfkey = true;
            require_keyword = BOOLEAN(kind_is(tk_comma));
            if (require_keyword)
               discard(); // ','
            else
               done = true;
         }
      }
      if (! emitted_pfkey) {
         the_emitter->emit_opcode(push_int_8_op);
         the_emitter->emit_int_8(0);
      }

      // If full backward reference, parse it and we're done
      if (! done && is_full_backward_ref()) {
         the_scanner->lookahead(1)->shift_to_upper_case(); // the label
         parse_full_backward_ref();
         keyword_count = -1; // means full back ref
         done = true;
      }

      // Parse keyword list
      while (! done) {
         found_keyword = false;
         if (kind_is(tk_identifier)) {
            token *tk = the_scanner->lookahead(1);
            if (tk->kind() == tk_eq || tk->kind() == tk_minus)
               require_keyword = true;
         }
         if (require_keyword)
            found_keyword = parse_cobol_keyword();

         if (! found_keyword) {
            if (require_keyword) {
               syntax_error(93);
               return false;
            }
            else
               break;
         }

         if (! lexeme_is("=")) {
            syntax_error(3);
            return false;
         }
         discard(); // '='

         if (kind_is(tk_identifier))
            the_token->shift_to_upper_case();

         if (kind_is(tk_comma)) {
            the_emitter->emit_opcode(push_string_op);
            the_emitter->emit_string(" ");
         }
         else if (kind_is(tk_parenleft) &&
                  the_scanner->lookahead(1)->kind() == tk_identifier &&
                  the_scanner->lookahead(2)->kind() == tk_dot)
         {
            // Partial backward reference
            char lbl[8 + 1];
            char kwd[8 + 1];
            char pbr[1 + 16 + 1];
            discard(); // '('
            the_token->shift_to_upper_case();
            init_string_with_blank_pad(lbl, 8, the_token->lexeme());
            discard(); // label
            discard(); // '.'
            if (! kind_is(tk_identifier)) {
               syntax_error(94);
               return false;
            }
            the_token->shift_to_upper_case();
            init_string_with_blank_pad(kwd, 8, the_token->lexeme());
            discard(); // keyword
            if (! kind_is(tk_parenright)) {
               syntax_error(72);
               return false;
            }
            discard(); // ')'
            strcpy(pbr, "\x01"); // Means this is a partial back ref
            strcat(pbr, lbl);
            strcat(pbr, kwd);
            the_emitter->emit_opcode(push_string_op);
            the_emitter->emit_string(pbr);
         }
         else if (! parse_identifier()) {
            expression(8);
            if (in_error_state)
               return false;
         }
         keyword_count += 1;

         require_keyword = BOOLEAN(kind_is(tk_comma));
         if (require_keyword)
            discard(); // ','
         else
            done = true;
      }
      the_emitter->emit_opcode(statement_end_op); // end of clause
      the_emitter->emit_int_16(keyword_count);
   }
   return true;
}


void compiler::p_wang_cleanup() {
   if (set_options) {
      delete set_options;
      set_options = NULL;
   }
   if (extract_options) {
      delete extract_options;
      extract_options = NULL;
   }
   if (print_options) {
      delete print_options;
      print_options = NULL;
   }
   if (submit_options) {
      delete submit_options;
      submit_options = NULL;
   }
   if (display_enter_table) {
      delete display_enter_table;
      display_enter_table = NULL;
   }
}

#endif

//
//	History:
//	$Log: p_wang.cpp,v $
//	Revision 1.8  1998/10/13 19:39:44  gsl
//	Fix type mismatch warning
//	
//	Revision 1.7  1998-08-31 15:14:05-04  gsl
//	drcs update
//
//

//	
//	RCS file: /disk1/neomedia/RCS/wisp/wproc/p_wang.cpp,v
//	Working file: p_wang.cpp
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
//	date: 1996-07-25 14:15:55-04;  author: gsl;  state: V4_3_00;  lines: +0 -0
//	Renamed from p_wang.cc to p_wang.cpp
//	----------------------------
//	revision 1.5
//	date: 1995-06-02 10:31:20-04;  author: gsl;  state: V3_3_19;  lines: +1 -1
//	fix warning
//	----------------------------
//	revision 1.4
//	date: 1995-04-25 06:00:13-04;  author: gsl;  state: V3_3_16;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:52:30-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:33:12-05;  author: gsl;  state: V3_3x12;  lines: +195 -33
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:19-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
