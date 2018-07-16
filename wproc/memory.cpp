// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : memory.cpp
// Author : George Soules
// Date   : 8 April 1991

// Specification
#include "memory.hpp"

// Classes
#if DEBUG
#if DOS || DOS_HOST
#include <iostream.h>
#endif
#endif
#include "options.hpp"

// Definitions and subprograms
#if DOS || DOS_HOST
#include <alloc.h>
#endif
#include <stdio.h>
#include <string.h>
#include "crt_io.hpp"
#include "debugaid.hpp"


char *new_string(size_t size) {
   char *the_string = (char *) malloc(size);
#if DEBUG
   if (user_options.debug_trace_memory())
      show_memory("alloc string ", the_string, size);
#endif
   if (size && the_string == NULL) {
      puts("\nMemory exhausted allocating a string\n");
      restore_video_state();
      exit(0);
   }
   return the_string;
}


char *dup_string(const char *a_string) {
   char   *the_copy;
   size_t  size;
   size = strlen(a_string) + 1;
   the_copy = new_string(size);
   memcpy(the_copy, a_string, size);
   return the_copy;
}


void delete_string(char *&a_string) {
   if (a_string) {
#if DEBUG
      assert(valid_ptr(a_string));
      if (user_options.debug_trace_memory()) {
         show_memory("free string  ", a_string, strlen(a_string) + 1);
#ifdef EXT_COUT
         cout << a_string << '\n';
#else
	 printf("%s\n",a_string);
#endif
      }
#endif
      free(a_string);
      a_string = NULL;
   }
}


#if DEBUG
void show_memory(char *kind, void* ptr, size_t size) {
   if (user_options.debug_trace_memory()) {
      print_trace_id("memory");
#ifdef EXT_COUT
      cout << kind;
      cout.width(5);
      cout.setf(ios::right);
      cout << size << " bytes at " << ptr << " (";
      cout.width(5);
      cout.setf(ios::right);
#else
	printf("%s %5ld bytes at 0x%lX\n", kind, (long)size, (long)ptr);
#endif
#if DOS || DOS_HOST
      cout << coreleft() << " bytes left)\n";
#endif
   }
}
#endif



