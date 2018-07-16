// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : object.cpp
// Author : George Soules
// Date   : 22 February 1991

// Specification
#include "object.hpp"

// Classes
#include "options.hpp"

// Definitions and subprograms
#include "crt_io.hpp"
#include "debugaid.hpp"
#include "environ.hpp"
#include "memory.hpp"
#include "report.hpp"


#if DOS
void* object::operator new(size_t size) {
   void *allocated = ::operator new(size);
   if (allocated == NULL) {
      write_stdout("\nMemory exhausted allocating an object\n");
      restore_video_state();
      exit(0);
   }
#if DEBUG
   if (user_options.debug_trace_memory())
      show_memory("alloc object ", allocated, size);
#endif
   return allocated;
}


#if DEBUG
void object::operator delete(void* p, size_t size) {
#else
void object::operator delete(void* p) {
#endif
#if DEBUG
   assert(valid_ptr(p));
   if (user_options.debug_trace_memory())
      show_memory("free object  ", p, size);
#endif
   delete (void*) p;
}
#endif

