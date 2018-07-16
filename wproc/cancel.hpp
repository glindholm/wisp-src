// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : cancel.hpp
// Author : George Soules
// Date   : 5 July 1991

#ifndef CANCEL__HPP
#define CANCEL__HPP

// Classes
// (none)

// Definitions and subprograms
#include "environ.hpp"


void set_cancel_handler();

int cancel_handler();

Boolean cancel_requested();

#endif

