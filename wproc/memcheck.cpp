// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : memcheck.cpp
// Author : George Soules
// Date   : 9 February 1991

#if DEBUG

// Specification
// "memcheck.hpp" (not included to avoid redeclaration of state variables)

// Classes
#if DOS || DOS_HOST
#include <iostream.h>
#endif

// Definitions and subprograms
#if DOS || DOS_HOST
#include <alloc.h>
#endif
#include <ctype.h>
#include "environ.hpp"


void check_and_verify_heap(int caller_id) {
#if DOS || DOS_HOST
   if (heapcheck() == _HEAPCORRUPT)
      cout << "\nWARNING: HEAP IS CORRUPT (" << caller_id << ")\n";
#endif
}


void dump_heap() {
#if DOS || DOS_HOST
   void         *last_in_use_ptr = NULL;
   unsigned int  last_in_use_size = 0;
   char          c;
   struct        heapinfo hi;
   hi.ptr = NULL;
   while (heapwalk(&hi) == _HEAPOK)
      if (hi.in_use) {
         last_in_use_ptr = hi.ptr;
         last_in_use_size = hi.size;
         cout << hi.size << " bytes at " << hi.ptr << "\n";
      }
   cout << "Last heap node in use (" << last_in_use_size
        << " bytes at " << last_in_use_ptr << "):\n";
   for (int i = 0; i < last_in_use_size; i++) {
      c = (((char*) last_in_use_ptr)+i)[0];
      if (! isprint(c))
         c = '.';
      cout << c;
   }
#endif
}


void check_memory_usage(
   unsigned long initial_memory,
   unsigned long current_memory)
{
#if DOS || DOS_HOST
   Boolean leakage = false;
   if (current_memory != initial_memory) {
      cout << "\n";
      cout << "MEMORY USAGE WARNING:\n";
      cout << "Initial = " << initial_memory << "\n";
      cout << "Current = " << current_memory << "\n";
      leakage = true;
   }
   check_and_verify_heap(0);

   if (leakage)
      dump_heap();
#endif
}

#endif

