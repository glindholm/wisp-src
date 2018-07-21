//
//	Copyright (c) Shell Stream Software LLC, All Rights Reserved.
//
//	Project:	WPROC
//	
//// Copyright (c) Lexical Software, 1991.  All rights reserved.
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

typedef unsigned int  usign_32;
typedef          int  int_32;

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

//
//	History:
//	$Log: environ.hpp,v $
//	Revision 1.11  2003/02/07 21:10:51  gsl
//	fix int32 sizes
//	
//	Revision 1.10  1998/08/31 19:13:46  gsl
//	drcs update
//	
//

//	
//	RCS file: /disk1/neomedia/RCS/wisp/wproc/environ.hpp,v
//	Working file: environ.hpp
//	head: 1.9
//	branch:
//	locks: strict
//	access list:
//		gsl
//		scass
//		ljn
//		jockc
//		jlima
//	symbolic names:
//	keyword substitution: kv
//	total revisions: 9;	selected revisions: 9
//	description:
//	----------------------------
//	revision 1.9
//	date: 1997-06-09 15:48:22-04;  author: scass;  state: V4_3_00;  lines: +6 -0
//	Added typedef for int_32 and usign_32 to be an int
//	on OSF1_ALPHA platform
//	----------------------------
//	revision 1.8
//	date: 1996-08-13 13:56:27-04;  author: gsl;  state: V3_3_93;  lines: +1 -2
//	fix where false and true are defined
//	----------------------------
//	revision 1.7
//	date: 1996-08-13 13:49:32-04;  author: gsl;  state: Exp;  lines: +7 -2
//	Fix the Boolean defines so doesn't use bool, false, or true which are
//	all reserved
//	----------------------------
//	revision 1.6
//	date: 1996-07-25 19:46:00-04;  author: gsl;  state: Exp;  lines: +1 -1
//	NT
//	----------------------------
//	revision 1.5
//	date: 1995-06-02 09:00:53-04;  author: gsl;  state: V3_3_19;  lines: +10 -1
//	removed the signed keyword
//	----------------------------
//	revision 1.4
//	date: 1995-04-25 05:59:54-04;  author: gsl;  state: V3_3_16;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:52:12-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:32:51-05;  author: gsl;  state: V3_3x12;  lines: +6 -1
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:07-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
