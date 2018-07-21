//
//	Copyright (c) 1996-1998 NeoMedia Technologies Inc. All rights reserved.
//
//	Project:	WPROC
//	
//// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : tables.hpp
// Author : George Soules
// Date   : 1 July 1991

#ifndef TABLES__HPP
#define TABLES__HPP

// Classes
#include "object.hpp"

// Definitions and subprograms
#include <string.h>
#include "environ.hpp"


class table_entry {
   public:
#if WANG
      table_entry(const char *a_name, int abbrev = 0) {
         the_name   = a_name;
         next_entry = NULL;
         times_seen = 0;
         min_abbrev = abbrev;
      }
#else
      table_entry(const char *a_name) {
         the_name   = a_name;
         next_entry = NULL;
         times_seen = 0;
      }
#endif

      const char  *name()                        {return the_name;}
      void         set_not_seen()                {times_seen = 0;}
      usign_8      seen()                        {return times_seen;}
      table_entry *next()                        {return next_entry;}
      void         append(table_entry *an_entry) {next_entry = an_entry;}
#if WANG
      Boolean      matches(const char *a_name, int abbrev = 0);
      int          min_abbrev;
#else
      Boolean      matches(const char *a_name);
#endif

   private:
      const char  *the_name;
      table_entry *next_entry;
      usign_8      times_seen;
};


class name_kind : public table_entry {
   public:
      name_kind(const char *a_name, int a_kind) : table_entry(a_name) {
         the_kind = a_kind;
      }
      int kind() {return the_kind;}
   private:
      int the_kind;
};


class option_name : public name_kind {
   public:
      option_name(
         char    *a_name,
         int      a_kind,
         Boolean  is_yes_no,
         Boolean  is_list = false) : name_kind(a_name, a_kind)
      {
         yes_no = is_yes_no;
         list   = is_list;
      }
      Boolean is_yes_no() {return yes_no;}
      Boolean is_list()   {return list;}
   private:
      Boolean yes_no;
      Boolean list;
};


class table : public object {
   public:
      table();
      ~table();
      void         add(table_entry *an_entry);
#if WANG
      table_entry *lookup(const char *a_name, Boolean allow_abrev = false);
#else
      table_entry *lookup(const char *a_name);
#endif
      void         set_none_seen();
   private:
      int          the_dimension;
      table_entry *first;
      table_entry *last;
};

#endif


//
//	History:
//	$Log: tables.hpp,v $
//	Revision 1.5  1998/08/31 19:14:22  gsl
//	drcs update
//	
//

//	
//	RCS file: /disk1/neomedia/RCS/wisp/wproc/tables.hpp,v
//	Working file: tables.hpp
//	head: 1.4
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
//	total revisions: 4;	selected revisions: 4
//	description:
//	----------------------------
//	revision 1.4
//	date: 1995-04-25 06:00:29-04;  author: gsl;  state: V4_3_00;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:52:44-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:33:30-05;  author: gsl;  state: V3_3x12;  lines: +9 -9
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:31-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
