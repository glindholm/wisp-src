// Copyright (c) Lexical Software, 1991.  All rights reserved.
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



