// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : stateobj.cpp
// Author : George Soules
// Date   : 21 March 1991

// Specification
#include "stateobj.hpp"

// Classes
// (none)

// Definitions and subprograms
#include <string.h>
#include "debugaid.hpp"
#include "memory.hpp"
#include "report.hpp"


// The actual object for the_file (a static member of state_object)
state_file *state_object::the_file = NULL;


state_object::state_object() {
   next_state_object = NULL;
}


Boolean state_object::open_state_file(const char *a_filename, state_mode a_mode) {
   switch (a_mode) {
      case save_mode :
         if (the_file == NULL) {
            trace(object, "opening state file for save");
            the_file = fopen(a_filename, "wb");
         }
         break;
      case restore_mode :
         if (the_file == NULL) {
            trace(object, "opening state file for restore");
            the_file = fopen(a_filename, "rb");
         }
         break;
      default:
         assert(UNREACHABLE);
   }
   return BOOLEAN(the_file != NULL);
}


void state_object::close_state_file() {
   trace(object, "closing state file");
   int status = fclose(the_file);
   assert(status == 0);
   the_file = NULL;
}


void state_object::save(const void *ptr, size_t size) {
   if (fwrite(ptr, size, 1, the_file) != 1)
      report_fatal_error(5);
}


void state_object::save(const char *a_string) {
   int_16 size;
   size = strlen(a_string);
   save(&size, sizeof(size));
   save(a_string, size);
}


void state_object::restore(void *ptr, size_t size) {
   if (fread(ptr, size, 1, the_file) != 1)
      report_fatal_error(4);
}


void state_object::restore(char *&a_string) {
   int_16 size;
   restore(&size, sizeof(size));
   a_string = new_string(size + 1);
   restore(a_string, size);
   a_string[size] = '\0';
}







