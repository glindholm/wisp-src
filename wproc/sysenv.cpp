// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : sysenv.cpp
// Author : George Soules
// Date   : 27 June 1991

// Specification
#include "sysenv.hpp"

// Classes
// (none)

// Definitions and subprograms
#include <string.h>
#include "debugaid.hpp"
#include "memory.hpp"
#include "utility.hpp"


#if WANG && ! DOS_HOST
extern char **environ;
#endif
char    **os_environ       = environ; // Borland & UNIX global
char    **saved_environ    = os_environ;
const int vector_expansion = 4;


system_environment::system_environment() {
   trace(object, "system_environment");
   the_vector = NULL;
   entry_count = 0;
   entries_allocated = 0;
}


system_environment::~system_environment() {
   trace(object, "~system_environment");
   if (the_vector) {
      trace(object, "deleting environment vector");
      entry_count = 0;
      while (the_vector[entry_count])
         delete_string(the_vector[entry_count++]);
      delete the_vector;
   }
}


void system_environment::create_vector() {
   trace(object, "creating environment vector");

   // Determine size of C++ runtime environment vector (this is
   // really our own vector if the process nesting level > 1).
   entry_count = 0;
   while (os_environ[entry_count])
      entry_count += 1;

   // Allocate vector (add extra elements for expansion)
   entries_allocated = entry_count + vector_expansion;
   the_vector = new char*[entries_allocated + 1]; // +1 for null entry at end

   // Copy C++ environment to ours
   int i = 0;
   while (os_environ[i]) {
      the_vector[i] = dup_string(os_environ[i]);
      i += 1;
   }
   the_vector[i] = '\0';

   // Fool C++ runtime into thinking our vector is theirs so that Spawn
   // and System calls will be able to convert the vector into a DOS
   // environment block suitable for use by a child process.
   os_environ = the_vector;
}


int system_environment::lookup(const char* a_name) {
   Boolean found = false;
   int     index = 0;
#if DOS
   char   *name  = upper_case(dup_string(a_name));
#else
   char   *name  = dup_string(a_name);
#endif
   int     len   = strlen(name);

   while (os_environ[index]) {
      if (strncmp(name, os_environ[index], len) == 0) {
         if (os_environ[index][len] == '=') {
            found = true;
            break;
         }
      }
      index += 1;
   }
   delete_string(name);
   return found ? index : -1;
}


char *system_environment::get(const char* a_name) {
   int index = lookup(a_name);
   return dup_string(index == -1 ? "" : os_environ[index] + strlen(a_name) + 1);
}


int system_environment::set(const char* a_name, const char* a_value) {
   int name_len  = strlen(a_name);
   int value_len = strlen(a_value);

   // Allocate NAME=VALUE string
   char *s = new_string(name_len + value_len + 2);
   strcpy(s, a_name);
#if DOS
   upper_case(s);
#endif
   strcat(s, "=");
   strcat(s, a_value);

   int     index = lookup(a_name);
   Boolean found = BOOLEAN(index >= 0);
   Boolean erase = BOOLEAN(! value_len);

   if (! found && erase) {
      // They specified NAME= but NAME not in environment
      delete s;
      return 1;
   }

   // If this is the first call to Set, create the vector (this is an
   // optimization to burden only procedures that use the Set statement
   // with the overhead of a copy of the environment).
   if (the_vector == NULL)
      create_vector();

   if (found && erase) {
      // Delete an entry (don't use delete_string because it NULL's the pointer)
      delete the_vector[index];
      delete s;

      // Move rest of entries up one position
      while(the_vector[index]) {
         the_vector[index] = the_vector[index + 1];
         index++;
      }
      entry_count -= 1;
   }
   else if (found) {
      // Replace an existing entry
      delete_string(the_vector[index]);
      the_vector[index] = s;
   }
   else {
      // Add a new entry
      entry_count += 1;
      if (entry_count > entries_allocated) {
         trace(object, "enlarging environment vector");
         // Vector is full.  Allocate a larger one and copy old one to it.
         entries_allocated = entry_count + vector_expansion;
         char **new_vector = new char*[entries_allocated + 1];
         int i = 0;
         while(the_vector[i]) {
            new_vector[i] = the_vector[i];
            i += 1;
         }
         delete the_vector;
         the_vector = new_vector;
         os_environ = the_vector;
      }
      the_vector[entry_count - 1] = s;
      the_vector[entry_count] = '\0';
   }
   return 0;
}


void system_environment::restore() {
   os_environ = the_vector ? the_vector : saved_environ;
}
