//
//	Copyright (c) 1996-1998 NeoMedia Technologies Inc. All rights reserved.
//
//	Project:	WPROC
//	Id:		$Id:$
//	RCS:		$Source:$
//	
//
// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : symbols.cpp
// Author : George Soules
// Date   : 20 March 1991

// Specification
#include "symbols.hpp"

// Classes
#include "process.hpp"
#include "scope.hpp"
#include "tables.hpp"

// Definitions and subprograms
#include <string.h>
#ifdef UNIX
#include <unistd.h>
#endif
#ifdef WIN32
#include <io.h>
#endif

#include "builtin.hpp"
#include "debugaid.hpp"
#include "memory.hpp"
#include "utility.hpp"
#include "report.hpp"

#include "wang_os.hpp"
#include "miscsubs.h"

#ifdef WIN32
#define access(filename, mode)		_access(filename, mode)
#endif

class builtin : public name_kind {
   public:
      builtin(
         char         *a_name,
         builtin_kind  a_kind,
         Boolean       returns_integer,
         int           min_args,
         int           max_args = 0) : name_kind(a_name, a_kind)
      {
         the_min_args   = min_args;
         the_max_args   = max_args ? max_args : min_args;
         integer_result = returns_integer;
      }
      int     min_args()        {return the_min_args;}
      int     max_args()        {return the_max_args;}
      Boolean returns_integer() {return integer_result;}
   private:
      int_8   the_min_args;
      int_8   the_max_args;
      Boolean integer_result;
};

table* builtins = NULL;


void init_builtins() {
   assert(! builtins);
   builtins = new table();
   //                                                                   Returns  Min/Max
   //                         Name                  Kind                integer  args
   builtins->add(new builtin("&abs",                bi_abs,               true , 1));
   builtins->add(new builtin("&alarm",              bi_alarm,             true , 0, 1));
   builtins->add(new builtin("&arg",                bi_arg,               false, 1));
   builtins->add(new builtin("&args",               bi_args,              true , 0));
   builtins->add(new builtin("&byte",               bi_byte,              false, 1));
   builtins->add(new builtin("&center",             bi_center,            false, 1));
   builtins->add(new builtin("&change_dir",         bi_change_dir,        true , 1));
#if DOS
   builtins->add(new builtin("&change_drive",       bi_change_drive,      true , 1));
#endif
   builtins->add(new builtin("&color",              bi_color,             false, 1));
   builtins->add(new builtin("&copy",               bi_copy,              false, 2));
   builtins->add(new builtin("&current_dir",        bi_current_dir,       false, 0));
#if DOS
   builtins->add(new builtin("&current_drive",      bi_current_drive,     false, 0));
#endif
   builtins->add(new builtin("&curcol",             bi_curcol,            true , 0));
   builtins->add(new builtin("&currow",             bi_currow,            true , 0));
   builtins->add(new builtin("&cursor",             bi_cursor,            true , 2));
   builtins->add(new builtin("&date",               bi_date,              false, 0, 2));
   builtins->add(new builtin("&delay",              bi_delay,             true , 1));
   builtins->add(new builtin("&dir_entries",        bi_dir_entries,       true , 1));
   builtins->add(new builtin("&dir_exists",         bi_dir_exists,        true , 1));
   builtins->add(new builtin("&disk_space",         bi_disk_space,        true , 1));
#if DOS
   builtins->add(new builtin("&dos_version",        bi_dos_version,       false, 0));
   builtins->add(new builtin("&drive_ready",        bi_drive_ready,       true , 1));
#endif
   builtins->add(new builtin("&extract",            bi_extract,           false, 1));
   builtins->add(new builtin("&false",              bi_false,             true , 0));
   builtins->add(new builtin("&file_date",          bi_file_date,         false, 1, 2));
   builtins->add(new builtin("&file_dir",           bi_file_dir,          false, 1));
#if DOS
   builtins->add(new builtin("&file_drive",         bi_file_drive,        false, 1));
#endif
   builtins->add(new builtin("&file_exists",        bi_file_exists,       true , 1));
   builtins->add(new builtin("&file_ext",           bi_file_ext,          false, 1));
   builtins->add(new builtin("&file_lines",         bi_file_lines,        true,  1));
   builtins->add(new builtin("&file_name",          bi_file_name,         false, 1));
   builtins->add(new builtin("&file_search",        bi_file_search,       false, 1));
   builtins->add(new builtin("&file_size",          bi_file_size,         true , 1));
   builtins->add(new builtin("&file_time",          bi_file_time,         false, 1, 2));
   builtins->add(new builtin("&get_char",           bi_get_char,          false, 0));
   builtins->add(new builtin("&get_key",            bi_get_key,           true,  0));
   builtins->add(new builtin("&index",              bi_index,             true , 2));
   builtins->add(new builtin("&integer",            bi_integer,           true , 1));
#if DOS
   builtins->add(new builtin("&is_archive",         bi_is_archive,        true , 1));
#endif
   builtins->add(new builtin("&is_alpha",           bi_is_alpha,          true , 1));
   builtins->add(new builtin("&is_alphanum",        bi_is_alphanum,       true , 1));
   builtins->add(new builtin("&is_color",           bi_is_color,          true , 1));
   builtins->add(new builtin("&is_hex",             bi_is_hex,            true , 1));
   builtins->add(new builtin("&is_digits",          bi_is_digits,         true , 1));
#if DOS
   builtins->add(new builtin("&is_dir",             bi_is_dir,            true , 1));
   builtins->add(new builtin("&is_hidden",          bi_is_hidden,         true , 1));
#endif
   builtins->add(new builtin("&is_integer",         bi_is_integer,        true , 1));
   builtins->add(new builtin("&is_lower",           bi_is_lower,          true , 1));
   builtins->add(new builtin("&is_printable",       bi_is_printable,      true , 1));
#if DOS
   builtins->add(new builtin("&is_readonly",        bi_is_readonly,       true , 1));
   builtins->add(new builtin("&is_system",          bi_is_system,         true , 1));
#endif
   builtins->add(new builtin("&is_upper",           bi_is_upper,          true , 1));
   builtins->add(new builtin("&is_yesno",           bi_is_yesno,          true , 1));
   builtins->add(new builtin("&key_ready",          bi_key_ready,         true , 0));
   builtins->add(new builtin("&left",               bi_left,              false, 1));
   builtins->add(new builtin("&length",             bi_length,            true , 1));
#if WANG
   builtins->add(new builtin("&label",              bi_label,             true , 1));
#endif
   builtins->add(new builtin("&lower",              bi_lower,             false, 1));
   builtins->add(new builtin("&make_dir",           bi_make_dir,          true , 1));
   builtins->add(new builtin("&match",              bi_match,             true , 2));
   builtins->add(new builtin("&max",                bi_max,               true , 2));
#if DOS
   builtins->add(new builtin("&memory",             bi_memory,            true , 0));
#endif
   builtins->add(new builtin("&min",                bi_min,               true , 2));
   builtins->add(new builtin("&mod",                bi_mod,               true , 2));
   builtins->add(new builtin("&monochrome",         bi_monochrome,        true , 0));
#if NETWORK
   builtins->add(new builtin("&net_address",           bi_net_address,           false, 0));
   builtins->add(new builtin("&net_allow_messages",    bi_net_allow_messages,    true,  1));
   builtins->add(new builtin("&net_attach",            bi_net_attach,            true,  1));
   builtins->add(new builtin("&net_available",         bi_net_available,         true,  0));
   builtins->add(new builtin("&net_clear_connection",  bi_net_clear_connection,  true,  1));
   builtins->add(new builtin("&net_date",              bi_net_date,              false, 0));
   builtins->add(new builtin("&net_detach",            bi_net_detach,            true,  1));
   builtins->add(new builtin("&net_file_server",       bi_net_file_server,       false, 0));
   builtins->add(new builtin("&net_full_name",         bi_net_full_name,         false, 0, 1));
   builtins->add(new builtin("&net_get_bindery_names", bi_net_get_bindery_names, true , 2));
   builtins->add(new builtin("&net_get_member_names",  bi_net_get_member_names,  true , 2));
   builtins->add(new builtin("&net_get_users",         bi_net_get_users,         true , 1));
   builtins->add(new builtin("&net_logged_in",         bi_net_logged_in,         true,  0));
   builtins->add(new builtin("&net_login",             bi_net_login,             true,  2));
   builtins->add(new builtin("&net_login_name",        bi_net_login_name,        false, 0));
   builtins->add(new builtin("&net_logout",            bi_net_logout,            true,  0, 1));
   builtins->add(new builtin("&net_lpt_print_queue",   bi_net_lpt_print_queue,   false, 1));
   builtins->add(new builtin("&net_map",               bi_net_map,               true,  2, 4));
   builtins->add(new builtin("&net_member_of_group",   bi_net_member_of_group,   true,  1));
   builtins->add(new builtin("&net_password_valid",    bi_net_password_valid,    true,  2));
   builtins->add(new builtin("&net_security_equals",   bi_net_security_equals,   true,  1));
   builtins->add(new builtin("&net_send_message",      bi_net_send_message,      true,  1, 2));
   builtins->add(new builtin("&net_time",              bi_net_time,              false, 0));
#endif
   builtins->add(new builtin("&parse",              bi_parse,             false, 2, 3));
   builtins->add(new builtin("&proc_name",          bi_proc_name,         false, 0));
   builtins->add(new builtin("&random",             bi_random,            true , 0, 1));
   builtins->add(new builtin("&rank",               bi_rank,              true , 1));
   builtins->add(new builtin("&read_dir",           bi_read_dir,          true , 2));
   builtins->add(new builtin("&remove_dir",         bi_remove_dir,        true , 1));
   builtins->add(new builtin("&remainder",          bi_remainder,         true , 2));
   builtins->add(new builtin("&right",              bi_right,             false, 1));
   builtins->add(new builtin("&sort",               bi_sort,              true , 1));
#if DOS
   builtins->add(new builtin("&set_file_date",      bi_set_file_date,     true , 2));
   builtins->add(new builtin("&set_file_time",      bi_set_file_time,     true , 2));
#endif
   builtins->add(new builtin("&strip",              bi_strip,             false, 1));
   builtins->add(new builtin("&time",               bi_time,              false, 0, 2));
   builtins->add(new builtin("&translate",          bi_translate,         false, 3));
   builtins->add(new builtin("&trim",               bi_trim,              false, 1));
   builtins->add(new builtin("&true",               bi_true,              true , 0));
   builtins->add(new builtin("&unique",             bi_unique,            false, 0, 1));
   builtins->add(new builtin("&upper",              bi_upper,             false, 1));
   builtins->add(new builtin("&verify",             bi_verify,            true , 2));
   builtins->add(new builtin("&verify_date",        bi_verify_date,       true , 1));
   builtins->add(new builtin("&verify_time",        bi_verify_time,       true , 1));
   builtins->add(new builtin("&yesno",              bi_yesno,             false, 1));
}


symbol_attributes initial_attributes =
   {
	0, // integer   
	0, // string        
	0, // array         
	0, // global        
	0, // dynamic       
	0, // initialized   
	0, // undeclared    
	0, // allocated     
	0, // builtin       
	0, // label         
	0, // subroutine    
	0, // not_local     
	0, // fetched_arg   
	0, // inactive      
	0      // spare  
   };

// Bodies of symbol members

symbol::symbol() {
   trace_begin(object, "symbol (uninitialized)");
   is        = initial_attributes;
   the_id    = 0;
   the_name  = NULL;
   the_block = outermost_block;
   the_data  = NULL;
#if WANG
   the_fetched_value = NULL;
#endif
   trace_end(object);
}


symbol::symbol(
   symbol_attributes attributes,
   const char       *a_name,
   block             decl_block,
   data             *some_data)
{
   trace_begin(object, "symbol (initialized)");
   is        = attributes;
   the_id    = 0;
   the_name  = dup_string(a_name);
   the_block = decl_block;
   the_data  = some_data;
#if WANG
   the_fetched_value = NULL;
#endif
   trace_end(object);
}


symbol::~symbol() {
   trace_begin(object, "~symbol");
   delete_string(the_name);
#if WANG
   if (is.fetched_arg && the_fetched_value) {
      assert(the_data->raw_data());
//    for (int i = 0; i < FETCHED_ARG_SIZE; i++)
//       the_fetched_value[i] =
//          i < the_data->raw_data_size() ? the_data->raw_data()[i] : '\0';
      memset(the_fetched_value, '\0', FETCHED_ARG_SIZE);
      memcpy(the_fetched_value, the_data->raw_data(), the_data->raw_data_size());
   }
#endif
   if (! is.not_local)
      delete the_data;
   trace_end(object);
}


void symbol::save_state() {
   trace_ss(object, "saving symbol ", the_name);
   save(&is, sizeof(is));
   save(the_name);
   if (is.label) {
      offset the_location = ((label_data *) the_data)->location();
      save(&the_location, sizeof(the_location));
   }
   else if (is.builtin) {
      int_8 byte;
      byte = ((builtin_data *) the_data)->kind();
      save(&byte, sizeof(byte));
      byte = ((builtin_data *) the_data)->min_args();
      save(&byte, sizeof(byte));
      byte = ((builtin_data *) the_data)->max_args();
      save(&byte, sizeof(byte));
   }
}


void symbol::restore_state() {
   restore(&is, sizeof(is));
   restore(the_name);
   if (is.label) {
      offset the_location;
      restore(&the_location, sizeof(the_location));
      the_data = new label_data(the_location);
   }
   else if (is.builtin) {
      int_8 kind;
      int_8 min_args;
      int_8 max_args;
      restore(&kind, sizeof(kind));
      restore(&min_args, sizeof(min_args));
      restore(&max_args, sizeof(max_args));
      the_data = new builtin_data(kind, min_args, max_args);
   }

   trace_ss(object, "restored symbol ", the_name);
}

void symbol::dump_global() {
   if (is.global && !is.inactive && !is.array && !is.dynamic && !is.not_local) {
      trace_ss(symbols, "Dumping global symbol ", the_name);
      save(&is, sizeof(is));
      save(the_name);
      if (is.integer) {
	 int_32 value = ((integer_data *)the_data)->contents();
         save(&value, sizeof(value));
      }
      else if (is.string) {
	 char *value = ((fixed_string_data *)the_data)->contents();
	 save(value);
      }
   }
}


void symbol::restore_global() {
   restore(&is, sizeof(is));
   restore(the_name);
   if (is.integer) {
      int_32 value;
      restore(&value, sizeof(value));
      the_data = new integer_data(value);
   }
   else if (is.string) {
      char *value;
      restore(value);
      the_data = new fixed_string_data(strlen(value), value);
      delete_string(value);
   }
   trace_ss(symbols, "Restored global symbol ", the_name);
}

// Bodies of symbol_table members

/*
Add logic to read in global data at the time the symbol table for a  
linked-to procedure is being created.  The variable global_count is  
used by procedures that dump their symbols--it tells them how many to  
update after a Wang unlink.  The variable ancestoral_globals_count is  
used by linked-to procedures--it tells them how many and which  
symbols they inherited from their ancestor procedures.  Warning: a  
"leftover" globals file restulting from e.g. a system crash, would  
cause ghost symbols to be created in an unsuspecting procedure.  
*/

symbol_table::symbol_table() {
   trace_begin(object, "symbol_table");
   entry_count          = 0;
   dump_before_destroying = false;
   the_table            = NULL;
   last_entry           = NULL;
   if (user_options.compile())
      global_table = NULL;
   else {
      global_table = the_process->global_symbol_table;
      the_process->global_symbol_table = this;
   }

   if (the_process->nesting_level == 1) {
      init_builtins();

      // If a procedure ancestor has created a file of global data,
      // initialize symbol table with its contents and then delete the file.
      const char *globals_file = globaldata();
      if (access(globals_file, 0) == 0) {
	      assert(!global_table);
	      global_table = new symbol_table(globals_file);
	      the_process->global_symbol_table = this;
      }
   }
   trace_end(object);
}

symbol_table::symbol_table(const char *globals_file) {
	trace_ss(symbols, "restoring global symbols from ", globals_file);
	entry_count	= 0;
	dump_before_destroying = true;
	global_table 	= NULL;
	the_table   	= NULL;
   	last_entry      = NULL;

	if (!open_state_file(globals_file,state_object::restore_mode))
            report_fatal_error(32); // Error 632 reading original dump file

	symbol_id entries;
   	restore(&entries, sizeof(entries));
   	trace_si(object, "Global symbols to restore = ", entries);
   	while (entries--) {
      		symbol *the_symbol = new symbol();
      		the_symbol->restore_global();
      		insert(the_symbol);
   	}
	close_state_file();
	delete_globaldata();
}

// Add logic that allows a linked-to procedure, just before it unlinks,  
// to know that is must dump globals it inherited from its ancestors.

symbol_table::~symbol_table() {
   trace_begin(object, "~symbol_table");

   if (the_process->nesting_level == 1 && global_table)
      // This should only occur if global_table was created from a file.
      delete global_table;

   if (dump_before_destroying) {
      if (!open_state_file(globaldata(),state_object::save_mode))
         report_fatal_error(33); // Error 633 creating update dump file

      save(&entry_count, sizeof(entry_count));
   }

   symbol_table_entry *prev_entry;
   symbol_table_entry *this_entry = last_entry;

   while (this_entry) {
      prev_entry = this_entry->prev;
      if (dump_before_destroying)
	     this_entry->the_symbol->dump_global();
      delete this_entry;
      this_entry = prev_entry;
   }

   if (! user_options.compile())
      the_process->global_symbol_table = global_table;
   if (the_process->nesting_level == 1) {
      delete builtins;
      builtins = NULL;
   }

   if (dump_before_destroying)
      close_state_file();

   trace_end(object);
}

void symbol_table::save_state() {
   trace_begin(object, "saving symbol_table");
   trace_si(object, "count = ", entry_count);
   symbol_table_entry *this_entry = last_entry;
   save(&entry_count, sizeof(entry_count));
   while (this_entry) {
      this_entry->the_symbol->save_state();
      this_entry = this_entry->prev;
   }
   trace_end(object);
}


void symbol_table::restore_state() {
   trace_begin(object, "restoring symbol_table");
   symbol *new_symbol;
   the_table  = NULL;
   restore(&entry_count, sizeof(entry_count));
   trace_si(object, "count = ", entry_count);
   symbol_id i = entry_count;
   entry_count = 0;
   while (i--) {
      new_symbol = new symbol();
      new_symbol->restore_state();
      insert(new_symbol);
   }
   trace_end(object);
}


void symbol_table::insert(symbol *a_symbol) {
   trace(symbols, "symbol_table::insert (start)");
   assert(a_symbol);
   a_symbol->the_id = ++entry_count;

   symbol_table_entry *new_entry = new symbol_table_entry(a_symbol);

   // Insert at head of list
   if (the_table) {
      new_entry->next = the_table;
      the_table->prev = new_entry;
      the_table = new_entry;
   }
   else {
      the_table  = new_entry;
      last_entry = new_entry;
      new_entry->next = NULL;
   }
   if (user_options.debug_trace_symbols())
   {
	char message[80];
	sprintf(message,"insert name=\"%s\" id=%d", a_symbol->the_name, a_symbol->the_id);
   	trace(symbols, message);
   }
}


symbol *symbol_table::insert(
   const char       *a_name,
   symbol_attributes attributes,
   block             decl_block,
   data             *some_data)
{
   symbol *new_symbol = new symbol(attributes, a_name, decl_block, some_data);
   insert(new_symbol);
   return new_symbol;
}

// FIX001
void symbol_table::activate(symbol *a_symbol) {
	// Set all symbols with the same name as this symbol to inactive.
	// Set this symbol to active.

	symbol_table_entry *this_entry = the_table;
	const char *a_name = a_symbol->name();

	while (this_entry) 
	{
      		if (same_string(this_entry->the_symbol->the_name, a_name))
		{
			this_entry->the_symbol->is.inactive = true;
		}

		this_entry = this_entry->next;
	}
	a_symbol->is.inactive = false;
}


symbol *symbol_table::lookup(symbol_id an_id) {
   trace_si(symbols, "lookup id ", an_id);

   symbol_table_entry *this_entry = the_table;
   while (this_entry) {
      if (this_entry->the_symbol->the_id == an_id) {
         if (this_entry->the_symbol->is.undeclared ||
             this_entry->the_symbol->is.builtin)
         {
            if (global_table) {
               symbol *global_symbol =
                  global_table->global_lookup(this_entry->the_symbol->the_name);
               if (global_symbol) {
                  trace_si(symbols, "change undeclared/builtin to global : ", an_id);
                  delete this_entry->the_symbol->the_data;
                  this_entry->the_symbol->the_data = global_symbol->the_data;
                  this_entry->the_symbol->is = global_symbol->is;
                  this_entry->the_symbol->is.not_local = true;
               }
            }
         }
	 // FIX001 - If symbol is inactive then return the lookup of it's name.
	 if (this_entry->the_symbol->is.inactive)
	 {
	    return this->lookup(this_entry->the_symbol->name());
	 }
	 else
	 {
	    return this_entry->the_symbol;
         }
      }
      else
         this_entry = this_entry->next;
   }
   assert(UNREACHABLE);
   return NULL;
}


symbol *symbol_table::lookup(const char *a_name) {
   // WARNING: lookup can change the state of the symbol table if a builtin is found.
   trace_ss(symbols, "lookup name ", a_name);
   symbol_table_entry *this_entry = the_table;
   while (this_entry) {
      if (same_string(this_entry->the_symbol->the_name, a_name) 
	  && !this_entry->the_symbol->is.inactive) // FIX001 - only find active symbols
         return this_entry->the_symbol;
      else
         this_entry = this_entry->next;
   }
   // See if it's builtin
   assert(builtins);
   builtin *the_builtin = (builtin *) builtins->lookup(a_name);
   if (the_builtin) {
      symbol_attributes symbol_is = initial_attributes;
      symbol_is.builtin = true;
      symbol_is.integer = the_builtin->returns_integer();
      symbol_is.string  = BOOLEAN(! symbol_is.integer);
      symbol *new_symbol = insert(a_name, symbol_is,
         outermost_block, new builtin_data(the_builtin->kind(),
         the_builtin->min_args(), the_builtin->max_args()));
      return new_symbol;
   }
   return NULL;
}


symbol_table_entry *symbol_table::entry_after(symbol_table_entry *an_entry) {
   return an_entry ? an_entry->next : the_table;
}


symbol_table_entry *symbol_table::entry_before(symbol_table_entry *an_entry) {
   return an_entry ? an_entry->prev : last_entry;
}

/*
Add logic to dump symbols before a link.  In this and update_globals,  
the value of "emissary" is set to true in e_run.cc.  The emissary  
procedure is the one whose mission it is to take care of dumping and  
updating global symbols for itself and for its parents, grandparents  
etc.
*/

void symbol_table::dump_globals(Boolean emissary, int subtotal) {
   trace(symbols, "Dumping global symbols");

   symbol_table_entry *this_entry;

   /*
   **	Calculate global_count to be the number of globals in this
   **	symbol table that will actually be dumped.
   */
   global_count = 0;
   this_entry = last_entry;
   while (this_entry) {
	   if ( this_entry->the_symbol->is.global 	&& 
	       !this_entry->the_symbol->is.inactive 	&& 
	       !this_entry->the_symbol->is.array 	&& 
	       !this_entry->the_symbol->is.dynamic 	&& 
	       !this_entry->the_symbol->is.not_local	   )
	   {
		   global_count+=1;
	   }
	   this_entry = this_entry->prev;
   }   

   if (emissary)
      if (!open_state_file(globaldata(), state_object::save_mode))
         report_fatal_error(31); // Error 631 creating original dump

   if (global_table)
      global_table->dump_globals(false, global_count + subtotal);
   else {
      symbol_id global_total = global_count + subtotal;
      save(&global_total, sizeof(global_total));
   }	 

   this_entry = last_entry;
   while (this_entry) {
      this_entry->the_symbol->dump_global();
      this_entry = this_entry->prev;
   }
   
   if (emissary)
      close_state_file();

   trace(symbols, "Done dumping global symbols");
}

/*
**	ROUTINE:	symbol_table::write_declare_globals()
**
**	FUNCTION:	Write out declare statements for each of the global variables.
**			Used by SUBMIT to create a new proc with GLOBALS=YES clause.
**
**	DESCRIPTION:	Call write_declare_global() for each global variable
**			in each of the symbol tables.
**
**	ARGUMENTS:
**	fh		The open file handle to write to.
**
**	RETURN:		None
**
**	WARNINGS:	None
**
*/

void symbol_table::write_declare_globals(FILE *fh) {
   trace(symbols, "Write declare globals");

   // Write out the declarations for each global variable.
   // Start at the top symbol table and work down.
   if (global_table)
      global_table->write_declare_globals(fh);

   // Loop thru this symbol table
   symbol_table_entry *this_entry;
   this_entry = last_entry;
   while (this_entry) {
      this_entry->the_symbol->write_declare_global(fh);
      this_entry = this_entry->prev;
   }

   trace(symbols, "Done Write declare globals");
}

/*
**	ROUTINE:	symbol::write_declare_global() 
**
**	FUNCTION:	Write a declare statement for this global variable.
**			Used by SUBMIT to create a new proc with GLOBALS=YES clause.
**
**	DESCRIPTION:	If this is a global variable then write out a DECLARE
**			statement for it with an INITIAL clause set to it's
**			current value.
**
**	ARGUMENTS:	
**	fh		The open file handle to write to.
**
**	RETURN:		None
**
**	WARNINGS:	The fh file handle is expected to be pointing 
**			at the beginning of a line.
**
*/
void symbol::write_declare_global(FILE *fh) 
{
	if (is.global && !is.inactive && !is.array && !is.dynamic && !is.not_local)
	{
		trace_ss(symbols, "write_declare_global ", the_name);

		fprintf(fh,"DECLARE %s AS GLOBAL ", the_name);
		
		if (is.integer) {
			fprintf(fh,"INTEGER INITIAL %ld", (long)(((integer_data *)the_data)->contents()));
		}
		else if (is.string) {
			fprintf(fh,"STRING(%d) INITIAL \n",((fixed_string_data *)the_data)->size());
			write_string_literal(fh,
					     ((fixed_string_data *)the_data)->contents(),
					     ((fixed_string_data *)the_data)->size());
		}
		fprintf(fh,"\n");
	}
}

/*
Add logic to update symbols after returing from a link.  The printfs  
are only for debugging.  This code assumes that only one instance of  
a symbol name (in the case of duplicate declarations) is active at  
one time and therefore, use of lookup(name) will find the correct  
symbol.
*/

void symbol_table::update_globals(Boolean emissary) {
   trace(symbols, "Updating global symbols");

   if (emissary){
      if ( 0 != access(globaldata(), 0)) {
  	 // If the updated file is missing then subprocess failed
	 return;
      }
      if (!open_state_file(globaldata(),state_object::restore_mode)){
         report_fatal_error(34); // Error 634 reading updated dump file
      }

      symbol_id tmp_count;
      restore(&tmp_count, sizeof(tmp_count));   
   }
 
   if (global_table)
      global_table->update_globals(false);

   for (int i = 0; i < global_count; i++) {
      symbol *restored_symbol = new symbol();
      restored_symbol->restore_global();

      symbol *the_symbol = lookup(restored_symbol->the_name);
      assert(the_symbol);

      if (the_symbol->is.integer) {
//         int_32 old_value =
//            ((integer_data *)the_symbol->the_data)->contents();
         int_32 new_value =
            ((integer_data *)restored_symbol->the_data)->contents();
         ((integer_data *)the_symbol->the_data)->set_contents(new_value);
      }
      else if (the_symbol->is.string) {
//         char *old_value =
//            ((fixed_string_data *)the_symbol->the_data)->contents();
         char *new_value =
            ((fixed_string_data *)restored_symbol->the_data)->contents();
         ((fixed_string_data *)the_symbol->the_data)->set_contents(new_value);
      }

      delete restored_symbol;
   } 
   
   if (emissary) {
      close_state_file();
      delete_globaldata();
   }
   
   trace(symbols, "Done updating global symbols");
}

symbol *symbol_table::global_lookup(const char *a_name) {
   trace_ss(symbols, "global lookup name ", a_name);
   symbol_table_entry *this_entry = the_table;
   while (this_entry) {
      if (this_entry->the_symbol->is.global &&
          same_string(this_entry->the_symbol->the_name, a_name)
	  && !this_entry->the_symbol->is.inactive) // FIX001 - only find active symbols
      {
         return this_entry->the_symbol;
      }
      this_entry = this_entry->next;
   }
   if (global_table)
      return global_table->global_lookup(a_name);
   else
      return NULL;
}

/*
**	History:
**	$Log: symbols.cpp,v $
**	Revision 1.20  2011/10/29 20:09:14  gsl
**	Fix ISO routine name warnins on WIN32
**	
**	Revision 1.19  2003/02/11 19:12:44  gsl
**	fix duplicate history
**	
**	Revision 1.18  2003/02/11 19:05:26  gsl
**	Remove unneeded #ifdef's for DEBUG
**	
**	Revision 1.17  1998/10/02 19:37:39  gsl
**	Fix a debug trace of symbols flag
**	
**	Revision 1.16  1998-09-08 14:06:13-04  gsl
**	Fixed the debug trace message for insert
**
**	Revision 1.15  1998-08-31 15:50:37-04  gsl
**	drcs update
**
**	Revision 1.14  1998-08-31 15:14:19-04  gsl
**	drcs update
**
**	Revision 1.13  1998-02-02 11:09:03-05  gsl
**	Fix bug0022
**
**	Revision 1.12  1997-04-17 18:07:50-04  gsl
**	Fix the null padding of the fetched arg.
**	ALL THIS FETCHED_ARG STUFF WAS TO FIX A BUG
**	where nexting_level > 1 was treating its args as if they were fetched
**	and the ~symbol() routines was padding all "fetched_args" to there
**	max size which was overriding memory and sometimes crashing.
**
**	Revision 1.11  1997-04-17 17:55:07-04  gsl
**	In symbol::~symbol() removed logic that null padded fetched symbols
**	data areas as this is not required.
**
**	Revision 1.10  1996-07-25 19:48:07-04  gsl
**	NT
**
**	Revision 1.9  1996-07-25 11:16:24-07  gsl
**	Renamed from symbols.cc to symbols.cpp
**
**	Revision 1.8  1995-10-18 01:58:23-07  gsl
**	Add routines to write out DECLARE statements for global variables
**	These are used by SUBMIT to write out a new proc that will setup
**	the globals to handle the GLOBALS=YES option.
**
**
**
*/
//	----------------------------
//	revision 1.7
//	date: 1995-08-29 10:35:28-04;  author: gsl;  state: Exp;  lines: +18 -6
//	Fixed the "unable to read work file" bug that occurs when globals
//	are redeclared at a lower link level.
//	Fixed the logic that calculated global_count so that it counts
//	only the globals that will be written to the dump file.
//	----------------------------
//	revision 1.6
//	date: 1995-06-10 13:55:59-04;  author: gsl;  state: V3_3_18;  lines: +18 -4
//	commented initial_attributes
//	----------------------------
//	revision 1.5
//	date: 1995-06-02 12:13:28-04;  author: gsl;  state: Exp;  lines: +4 -4
//	fix warnings
//	----------------------------
//	revision 1.4
//	date: 1995-04-25 06:00:27-04;  author: gsl;  state: V3_3_16;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:52:42-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:33:27-05;  author: gsl;  state: V3_3x12;  lines: +218 -18
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:29-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
