// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : object.hpp
// Author : George Soules
// Date   : 22 February 1991

#ifndef OBJECT__HPP
#define OBJECT__HPP

// Classes
// (none)

// Definitions and subprograms
#include <stdlib.h>


class object {
   public:
#if DOS
      void *operator new(size_t size);
#if DEBUG
      void  operator delete(void* p, size_t size);
#else
      void  operator delete(void* p);
#endif
#endif
};

#endif

