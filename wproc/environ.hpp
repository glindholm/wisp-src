// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : environ.hpp
// Author : George Soules
// Date   : 21 February 1991

#ifndef ENVIRON__HPP
#define ENVIRON__HPP

#define UNREACHABLE 0

#ifndef BOOL_TYPE
#define false	bool_false
#define true	bool_true
enum bool_type {false, true};
#define Boolean bool_type
#else
#define Boolean bool
#endif

#define BOOLEAN(x) ((x)?true:false)

typedef unsigned char  usign_8;

/*
**	CHAR MUST BE SIGNED !!!
**	Some compilers don't support "signed" keyword which is
**	ok as long as it treats them as signed i.e. (-127 to 128)
**
**	typedef   signed char  int_8;
*/
typedef          char  int_8;

#if DOS | DOS_HOST
typedef unsigned int   usign_16;
typedef          int   int_16;
#else
typedef unsigned short usign_16;
typedef          short int_16;
#endif

#if OSF1_ALPHA
typedef unsigned int  usign_32;
typedef          int  int_32;
#else
typedef unsigned long  usign_32;
typedef          long  int_32;
#endif

#define INT_32_STRING_SIZE 12  // 10 digits + sign + '\0'
#define INT_16_STRING_SIZE  7  //  5 digits + sign + '\0'

#define FILENAME_SIZE 13

#if WANG
#define WANG_LABELNAME_SIZE    8
#define WANG_FILENAME_SIZE     8
#define WANG_LIBNAME_SIZE      8
#define WANG_VOLNAME_SIZE      6
#define MAX_SOURCE_WIDTH      71
#define FETCHED_ARG_SIZE    1024
#endif

#if DOS || DOS_HOST
#define SCREEN_HEIGHT 25
#else
#if WANG
#define SCREEN_HEIGHT 24
#endif
#endif
#define SCREEN_WIDTH 80

// Classes
// (none)

// Definitions and subprograms
// (none)

#endif
