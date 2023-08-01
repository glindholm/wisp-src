//
//	Copyright (c) Shell Stream Software LLC, All Rights Reserved.
//
//	Project:	WPROC
//	
//
// Module : tables.cpp
// Author : George Soules
// Date   : 1 July 1991

// Specification
#include "tables.hpp"

// Classes
// (none)

// Definitions and subprograms
#include <ctype.h>
#include <string.h>
#include "debugaid.hpp"
#include "utility.hpp"


// table member functions

table::table() {
   first = NULL;
   last  = NULL;
}


table::~table() {
   while (first) {
      last = first->next();
//    trace_ss(object, "delete table_entry ", first->name());
      delete first;
      first = last;
   }
}


void table::add(table_entry *an_entry) {
// trace_ss(object, "add table_entry ", an_entry->name());
   if (last) {
      last->append(an_entry);
      last = an_entry;
   }
   else {
      first = an_entry;
      last = first;
   }
}

#if WANG
table_entry *table::lookup(const char *a_name, Boolean allow_abrev) {
   table_entry *entry = first;
   while (entry) {
      if (entry->matches(a_name, allow_abrev ? entry->min_abbrev : 0))
         break;
      else
         entry = entry->next();
   }
   return entry;
}
#else

table_entry *table::lookup(const char *a_name) {
   table_entry *entry = first;
   while (entry) {
      if (entry->matches(a_name))
         break;
      else
         entry = entry->next();
   }
   return entry;
}
#endif

#if WANG
Boolean table_entry::matches(const char *a_name, int abbrev) {
   if (abbrev) {
      if ((int)strlen(a_name) < abbrev)
         return false;
      for (int i = 0; i < abbrev; i++) {
         if (tolower(a_name[i]) != the_name[i])
            return false;
      }
   }
   else {
      if (! same_string(a_name, the_name))
         return false;
   }
   times_seen += 1;
   return true;
}
#else
Boolean table_entry::matches(const char *a_name) {
   if (! same_string(a_name, the_name))
      return false;
   times_seen += 1;
   return true;
}
#endif


void table::set_none_seen() {
   table_entry *entry = first;
   while (entry) {
      entry->set_not_seen();
      entry = entry->next();
   }
}




//
//	History:
//	$Log: tables.cpp,v $
//	Revision 1.7  1998/08/31 19:14:21  gsl
//	drcs update
//	
//

//	
//
//	Working file: tables.cpp
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
//	date: 1997-10-01 09:29:39-04;  author: gsl;  state: V4_3_00;  lines: +1 -1
//	Fix warnings
//	----------------------------
//	revision 1.5
//	date: 1996-07-25 14:16:28-04;  author: gsl;  state: V4_1_02;  lines: +0 -0
//	Renamed from tables.cc to tables.cpp
//	----------------------------
//	revision 1.4
//	date: 1995-04-25 06:00:29-04;  author: gsl;  state: V3_3_19;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:52:44-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:33:29-05;  author: gsl;  state: V3_3x12;  lines: +3 -3
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:30-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
