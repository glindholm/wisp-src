// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : install.hpp
// Author : George Soules
// Date   : 7 February 1991

#ifndef INSTALL__HPP
#define INSTALL__HPP

// Classes
// (none)

// Definitions and subprograms
#include "environ.hpp"

#if DOS || DOS_HOST
#define MESSAGES_NAME "RUN"
#define MESSAGES_EXT  ".MSG"
#else
#define MESSAGES_NAME "wproc"
#define MESSAGES_EXT  ".msg"
#endif

char *installation_pathname(char *file, char *ext);

Boolean installation_ok();

#if RUNTIME
#if DEMO
Boolean runtime_demo_ok(const char *source_name);
#endif
#endif

#endif

