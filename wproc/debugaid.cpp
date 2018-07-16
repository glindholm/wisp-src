// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : debugaid.cpp
// Author : George Soules
// Date   : 24 February 1991

#if DEBUG

// Specification
#include "debugaid.hpp"

// Classes
#include "process.hpp"

// Definitions and subprograms
#if DOS || DOS_HOST
#include <alloc.h>
#endif
#include "crt_io.hpp"
#include <ctype.h>


Boolean valid_ptr(void *p) {
#if WANG  // JUST DURING DEVELOPMENT
   if (user_options.debug_trace_heap())
      return true;
#else
   if (! user_options.debug_trace_heap())
      return true;
#endif

#if DOS || DOS_HOST
   struct heapinfo hi;

   if (heapcheck() == _HEAPCORRUPT) {
      cerr << "\nHEAP IS CORRUPT : " << p << "\n";
      return false;
   }

   if (p == NULL) {
      cerr << "\nATTEMPT TO USE NULL POINTER\n";
      return false;
   }

   hi.ptr = NULL;
   while (heapwalk(&hi) != _HEAPEND)
      if (p == hi.ptr && hi.in_use)
         return true;

   cerr << "\nATTEMPT TO USE BAD POINTER : " << p << " -> ";
   for (int i = 0; i < 16; i++) {
      char c = ((char*) p+i)[0];
      if (! isprint(c))
         c = '.';
      printf("%c",c);
   }
   cerr << '\n';
   return false;
#else
   if (p == p);
   return true;
#endif
}


static trace_level = 0;
static saved_trace_level = 0;

void print_trace_id(const char *trace_kind) {
   use_standard_io();
   printf("%d %8s: ",the_process->nesting_level, trace_kind);
   for (int i = 1; i <= trace_level * 3; i++)
	printf(" ");
}


void debug_trace_begin(Boolean option, const char *msg, const char *kind) {
   if (option) {
      print_trace_id(kind);
      printf("%s\n",msg);
      fflush(stdout);
      trace_level += 1;
   }
}


void debug_trace_end(Boolean option) {
   if (option) {
      trace_level -= 1;
   }
}


void debug_trace(Boolean option, const char *msg, const char *kind) {
   if (option) {
      print_trace_id(kind);
      printf("%s\n",msg);
      fflush(stdout);
   }
}


void debug_trace_i(Boolean option, int_32 number, const char *kind) {
   if (option) {
	print_trace_id(kind);
	printf("%ld\n",number);
	fflush(stdout);
   }
}


void debug_trace_si(Boolean option, const char *msg, int_32 number, const char *kind) {
   if (option) {
      print_trace_id(kind);
      printf("%s%ld\n",msg, number);
      fflush(stdout);
   }
}


void debug_trace_sc(Boolean option, const char *msg, char the_char, const char *kind) {
   if (option) {
      print_trace_id(kind);
      printf("%s\"%c\"\n",msg, the_char);
      fflush(stdout);
   }
}


void debug_trace_ss(Boolean option, const char *msg1, const char *msg2, const char *kind) {
   if (option) {
      print_trace_id(kind);
      printf("%s%s\n", msg1, msg2);
      fflush(stdout);
   }
}


void debug_trace_sq(Boolean option, const char *msg1, const char *msg2, const char *kind) {
   if (option) {
      print_trace_id(kind);
      printf("%s\"%s\"\n", msg1, msg2);
      fflush(stdout);
   }
}


void debug_trace_level_save() {
   saved_trace_level = trace_level;
}


void debug_trace_level_restore() {
   trace_level = saved_trace_level;
}

#endif




