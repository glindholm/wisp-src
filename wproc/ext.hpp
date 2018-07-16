// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : ext.hpp
// Author : George Soules
// Date   : 4 March 1991

#ifndef EXT__HPP
#define EXT__HPP

// Classes
// (none)

// Definitions and subprograms
// (none)


// This is the only place that file extensions should be explicitly coded

#if DOS
#define PCODE_FILE_EXT  ".RUN"
#define SRC_FILE_EXT    ".SRC"
#else
#if WANG
#define PCODE_FILE_EXT  ".wpr"
#define SRC_FILE_EXT    ".wps"
#endif
#endif
#define BAT_FILE_EXT    ".BAT"
#define COM_FILE_EXT    ".COM"
#define EXE_FILE_EXT    ".EXE"

#endif

