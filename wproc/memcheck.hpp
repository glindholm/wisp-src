// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : memcheck.hpp
// Author : George Soules
// Date   : 9 February 1991

#ifndef MEMCHECK__HPP
#define MEMCHECK__HPP

// Classes
// (none)

// Definitions and subprograms
// (none)

#if DEBUG
void check_and_verify_heap(int caller_id);
void dump_heap();

void check_memory_usage(
   unsigned long initial_memory,
   unsigned long current_memory);
#endif


#if DEBUG && (DOS || DOS_HOST)
unsigned long im; // initial memory
unsigned long cm; // current memory
#define save_memory_state()  im = farcoreleft()
#define check_memory_state() cm = farcoreleft(); check_memory_usage(im, cm)
#define check_heap(id)       check_and_verify_heap(id)

#else

#define save_memory_state()
#define check_memory_state()
#define check_heap()

#endif
#endif
