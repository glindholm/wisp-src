/* 
	Copyright (c) 1995 DevTech Migrations, All rights reserved.
	$Id:$
*/

// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : symbols.hpp
// Author : George Soules
// Date   : 12 March 1991

#ifndef SYMBOLS__HPP
#define SYMBOLS__HPP


// Classes
#include "data.hpp"
#include "pcode.hpp"
#include "stateobj.hpp"

// Definitions and subprograms
#include "environ.hpp"


typedef int_16 symbol_id;


class symbol_attributes {
   public:
      Boolean integer       : 1;
      Boolean string        : 1;
      Boolean array         : 1;
      Boolean global        : 1;
      Boolean dynamic       : 1;
      Boolean initialized   : 1;
      Boolean undeclared    : 1;
      Boolean allocated     : 1;
      Boolean builtin       : 1;
      Boolean label         : 1;
      Boolean subroutine    : 1;
      Boolean not_local     : 1; // passed in parameter
      Boolean fetched_arg   : 1; // passed across a linklevel via a file using the "-p" option
      Boolean inactive      : 1; // FIX001 - add flag to track when symbol inactive
      int     spare         : 2;
};

extern symbol_attributes initial_attributes;

class symbol_table;


class symbol : public state_object {
   public:
      friend symbol_table;

      symbol();

      symbol(
         symbol_attributes attributes,
         const char       *a_name,
         block             decl_block,
         data             *some_data);

      virtual ~symbol();

      virtual void save_state();
      virtual void restore_state();

      void dump_global();
      void restore_global();

      void write_declare_global(FILE *fh);

#if ! RUNTIME
      static void emit_id(symbol_id an_id, pcode_emitter &an_emitter) {
         an_emitter.emit_int_16(an_id);
      }
#endif
      static symbol_id get_id(pcode_reader &a_reader) {
         return a_reader.get_int_16();
      }

      symbol_id   id()                      {return the_id;}
      block       decl_block()              {return the_block;}
      data       &data_ref()                {return *the_data;}
      data       *data_address()            {return the_data;}
      void        set_data(data *some_data) {the_data = some_data;}
#if WANG
      void        set_fetched_value(usign_8 *a_value)
                                            {the_fetched_value = a_value;}
#endif
      void        delete_data()             {delete the_data; the_data = NULL;}
      const char *name()                    {return the_name;}

      symbol_attributes is;

   private:
      symbol_id the_id;
      char     *the_name;
      block     the_block;
      data     *the_data;
#if WANG
      usign_8  *the_fetched_value;
#endif
};


class symbol_table_entry : public object {
   public:
      symbol_table_entry(symbol *a_symbol) {
         the_symbol = a_symbol;
         prev = NULL;
         next = NULL;
      }
      ~symbol_table_entry() {
         delete the_symbol;
      }
      symbol             *the_symbol;
      symbol_table_entry *next;
      symbol_table_entry *prev;
};


class symbol_table : public state_object {
   public:
      symbol_table();
      symbol_table(const char *globals_file);
      virtual ~symbol_table();
      void    save_state();
      void    restore_state();

      symbol *insert(
         const char       *a_name,
         symbol_attributes attributes,
         block             decl_block,
         data             *some_data = NULL);

      void    insert(symbol *a_symbol);
      void    activate(symbol *a_symbol); // FIX001
      symbol *lookup(symbol_id an_id);
      symbol *lookup(const char *a_name);
      symbol_table_entry *entry_after(symbol_table_entry *an_entry);
      symbol_table_entry *entry_before(symbol_table_entry *an_entry);

      void    write_declare_globals(FILE *fh);
      void    dump_globals(Boolean emissary, int subtotal = 0);
      void    update_globals(Boolean emissary);
      symbol *global_lookup(const char *a_name);
      symbol_table *global_symbol_table() {return global_table;}

   private:

      symbol_id           entry_count;
      symbol_id           global_count;
      Boolean             dump_before_destroying;
      symbol_table_entry *the_table;
      symbol_table_entry *last_entry;
      symbol_table       *global_table;
};

#endif

/*
**	History:
**	$Log: symbols.hpp,v $
**	Revision 1.6  1997-04-17 17:57:03-04  gsl
**	Comment the meaning of a fetched symbol
**
**	Revision 1.5  1995-10-18 05:00:57-04  gsl
**	add write_declare_globals() routines
**
**
**
*/
