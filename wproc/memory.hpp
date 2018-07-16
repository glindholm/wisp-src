// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : memory.hpp
// Author : George Soules
// Date   : 8 April 1991

#ifndef MEMORY__HPP
#define MEMORY__HPP

// Classes
// (none)

// Definitions and subprograms
#include <stdlib.h>


char *new_string(size_t size);

char *dup_string(const char *a_string);

void delete_string(char *&a_string);

#if DEBUG
void show_memory(char *kind, void* ptr, size_t size);
#endif

#endif

