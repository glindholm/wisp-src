//
//	Copyright (c) Shell Stream Software LLC, All Rights Reserved.
//
//	Project:	WPROC
//	
//// Copyright (c) Lexical Software, 1991.  All rights reserved.
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

state_object::~state_object() {
   // Virtual destructor
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








//
//	History:
//	$Log: stateobj.cpp,v $
//	Revision 1.7  2010/01/10 18:07:33  gsl
//	fix missing virtual destructor warnings
//	
//	Revision 1.6  1998/08/31 19:14:17  gsl
//	drcs update
//	
//

//	
//
//	Working file: stateobj.cpp
//	head: 1.5
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
//	total revisions: 5;	selected revisions: 5
//	description:
//	----------------------------
//	revision 1.5
//	date: 1996-07-25 14:16:22-04;  author: gsl;  state: V4_3_00;  lines: +0 -0
//	Renamed from stateobj.cc to stateobj.cpp
//	----------------------------
//	revision 1.4
//	date: 1995-04-25 06:00:25-04;  author: gsl;  state: V3_3_19;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:52:41-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:33:25-05;  author: gsl;  state: V3_3x12;  lines: +2 -2
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:28-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
