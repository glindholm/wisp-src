// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : assert.hpp
// Author : George Soules
// Date   : 8 October 1991

#ifndef ASSERT__HPP
#define ASSERT__HPP
// Classes
// (none)

// Definitions and subprograms
// (none)

#ifdef NDEBUG
#define assert(p)
#else
void assert_error(char *source_file, int line_number);
#define assert(p) {if (!(p)) assert_error(__FILE__, __LINE__);}
#endif

#endif
