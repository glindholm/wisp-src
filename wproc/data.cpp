//
//	Copyright (c) 1996-1998 NeoMedia Technologies Inc. All rights reserved.
//
//	Project:	WPROC
//	Id:		$Id:$
//	RCS:		$Source:$
//	
//// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : data.cpp
// Author : George Soules
// Date   : 16 April 1991

// Specification
#include "data.hpp"

// Classes
// (none)

// Definitions and subprograms
#include <string.h>
#include "memory.hpp"
#include "utility.hpp"


const int min_heap = 4 * 1024;


// integer_data member functions

integer_data::integer_data(int_32 an_integer) {
   the_contents = an_integer;
#if WANG
   the_raw_data      = (usign_8*) &the_contents;
   the_raw_data_size = sizeof(an_integer);
#endif
}


void integer_data::set_contents(int_32 an_integer) {
   the_contents = an_integer;
}



// integer_array_data member functions

integer_array_data::integer_array_data
   (array_index a_dimension, int_32 an_integer)
{
   the_dimension = a_dimension;
   if (the_dimension) {
      the_contents = new int_32[the_dimension];
      if (the_contents && memory_left(min_heap)) {
         for (int i = 0; i < the_dimension; i++)
            the_contents[i] = an_integer;
      }
      else {
         if (the_contents) {
            delete the_contents;
            the_contents = NULL;
         }
         the_ok = false;
      }
   }
   else {
      the_contents = NULL;
      the_ok = true;
   }
}


integer_array_data::~integer_array_data() {
   if (the_contents)
      delete the_contents;
}


int compare_int_32(const void *a, const void *b) {
   if (*((int_32 *) a) == *((int_32 *) b))
      return 0;
   else
      return *((int_32 *) a) > *((int_32 *) b) ? 1 : -1;
}


void integer_array_data::sort() {
   qsort((void *) the_contents, the_dimension, sizeof(int_32), compare_int_32);
}


// string_data member functions

string_data::~string_data() {
   if (the_contents)
      delete_string(the_contents);
}


char *string_data::contents() {
   return dup_string(the_contents);
}


char *string_data::substring(string_index start, string_index length) {
   if (length == -1) { // *
      length = last_non_blank(the_contents) - start + 1;
      if (length < 0)
         length = 0;
   }
   char *the_substring = new_string(length + 1);
   char *s = the_contents + start - 1;
   for (int i = 0; i < length; i++)
      the_substring[i] = s[i];
   the_substring[length] = '\0';
   return the_substring;
}


void string_data::set_substring(
   string_index start,
   string_index length,
   const char  *a_string)
{
   char *s = the_contents + start - 1;
   if (length == -1) // *
      length = last_non_blank(s);
   string_index a_string_size = min(length, (int)strlen(a_string));
   int i = 0;
   for (; i < a_string_size; i++)
      s[i] = a_string[i];
   while (i < length)
      s[i++] = ' ';
}


string_index string_data::last_non_blank(const char *a_string) {
// returns 0 if all blanks or a null string;
   string_index index = strlen(a_string);
   while (index && a_string[index - 1] == ' ')
      index -= 1;
   return index;
}


// fixed_string_data member functions

fixed_string_data::fixed_string_data(
   string_index a_size,
   const char  *a_string)
{
   the_size = a_size;
   the_contents = new char[the_size + 1];

   if (the_contents && memory_left(min_heap)) {
      int i = 0;
      if (a_string) {
         string_index a_string_size = min(the_size, (int)strlen(a_string));
         for (; i < a_string_size; i++)
            the_contents[i] = a_string[i];
      }
      while (i < the_size)
         the_contents[i++] = ' ';
      the_contents[the_size] = '\0';
   }
   else {
      if (the_contents) {
         delete the_contents;
         the_contents = NULL;
      }
      the_ok = false;
   }
#if WANG
   the_raw_data      = (usign_8*) the_contents;
   the_raw_data_size = the_size;
#endif
}


void fixed_string_data::set_contents(const char *a_string) {
   string_index a_string_size = min(the_size, (int)strlen(a_string));
   int i = 0;
   for (; i < a_string_size; i++)
      the_contents[i] = a_string[i];
   while (i < the_size)
      the_contents[i++] = ' ';
}


// dynamic_string_data member functions

dynamic_string_data::dynamic_string_data(const char *a_string) {
   the_size = a_string ? strlen(a_string) : 0;
   the_max_size = the_size;
   the_contents = new char[the_size + 1];

   if (the_contents && memory_left(min_heap)) {
      int i = 0;
      if (a_string) {
         string_index a_string_size = min(the_size, (int)strlen(a_string));
         for (; i < a_string_size; i++)
            the_contents[i] = a_string[i];
      }
      while (i < the_size)
         the_contents[i++] = ' ';
      the_contents[the_size] = '\0';
   }
   else {
      if (the_contents) {
         delete the_contents;
         the_contents = NULL;
      }
      the_ok = false;
   }
#if WANG
   the_raw_data      = (usign_8*) the_contents;
   the_raw_data_size = the_size;
#endif
}

void dynamic_string_data::set_contents(const char *a_string) {
   string_index a_string_size = strlen(a_string);
   if (a_string_size > the_max_size) {
      delete_string(the_contents);
      the_contents = new char[a_string_size + 1];
      if (the_contents && memory_left(min_heap))
         the_max_size = a_string_size;
      else {
         if (the_contents) {
            delete the_contents;
            the_contents = NULL;
         }
         the_ok = false;
         return;
      }
   }
   the_size = a_string_size;
   int i = 0;
   for (; i < the_size; i++)
      the_contents[i] = a_string[i];
   the_contents[the_size] = '\0';
#if WANG
   the_raw_data      = (usign_8*) the_contents;
   the_raw_data_size = the_size;
#endif
}


// string_array_data member functions

string_array_data::string_array_data(
   array_index  a_dimension,
   string_index a_size,
   const char  *initial_string)
{
   // Allocate the array
   the_dimension = a_dimension;
   if (the_dimension) {
      the_contents = new string_data*[the_dimension];
      if (the_contents && memory_left(min_heap)) {
         // Allocate the elements
         for (int i = 0; i < the_dimension; i++) {
            if (a_size)
               the_contents[i] = new fixed_string_data(a_size, initial_string);
            else
               the_contents[i] = new dynamic_string_data(initial_string);
            if (! the_contents[i]->ok()) {
               // Couldn't allocate all elements so free everything
               the_dimension = i + 1;
               delete_array_and_elements();
               the_ok = false;
               return;
            }
         }
      }
      else {
         if (the_contents) {
            delete the_contents;
            the_contents = NULL;
         }
         the_ok = false;
      }
   }
   else {
      the_contents = NULL;
      the_ok = true;
   }
}


string_array_data::~string_array_data() {
   delete_array_and_elements();
}


void string_array_data::delete_array_and_elements() {
   if (the_contents) {
      while (the_dimension > 0)
         delete the_contents[--the_dimension];
      delete the_contents;
      the_contents = NULL;
   }
}


int compare_string_data(const void *a, const void *b) {
   return strcmp((*((string_data **)a))->contents_address(),
                 (*((string_data **)b))->contents_address());
}


void string_array_data::sort() {
   qsort((void *) the_contents, the_dimension,
         sizeof(string_data *), compare_string_data);
}


//
//	History:
//	$Log: data.cpp,v $
//	Revision 1.7  1998-08-31 15:13:39-04  gsl
//	drcs update
//
//

//	
//	RCS file: /disk1/neomedia/RCS/wisp/wproc/data.cpp,v
//	Working file: data.cpp
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
//	date: 1997-10-01 09:22:44-04;  author: gsl;  state: V4_3_00;  lines: +4 -4
//	fix warnings
//	----------------------------
//	revision 1.5
//	date: 1996-07-25 14:14:33-04;  author: gsl;  state: V4_1_02;  lines: +0 -0
//	Renamed from data.cc to data.cpp
//	----------------------------
//	revision 1.4
//	date: 1995-04-25 05:59:46-04;  author: gsl;  state: V3_3_19;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:52:02-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:32:42-05;  author: gsl;  state: V3_3x12;  lines: +0 -0
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:01-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
