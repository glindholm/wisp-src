//
//	Copyright (c) Shell Stream Software LLC, All Rights Reserved.
//
//	Project:	WPROC
//	
//// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : assert.cpp
// Author : George Soules
// Date   : 8 October 1991

// Specification
#include "crt_io.hpp"

// Classes
// (none)

// Definitions and subprograms
#include "fileinfo.hpp"
#include <stdlib.h>
#include "utility.hpp"


void assert_error(char *source_file, int line_number) {
   char num[INT_32_STRING_SIZE];
   int_32_to_ascii(line_number, num);
   use_standard_io();
   write_stdout("\nERROR CODE = ");
   write_stdout(upper_case(file_name(source_file)));
   write_stdout(num);
   write_stdout("\n");
   write_stdout
      ("An unexpected error in WPROC has occurred.  Please report the code above\n");
   write_stdout
      ("to WISP support.  Thank you for your assistance.\n\n");
   abort();
}


//
//	History:
//	$Log: assert.cpp,v $
//	Revision 1.10  2010/01/10 16:12:30  gsl
//	Shell Stream
//	
//	Revision 1.9  2003/02/05 15:40:14  gsl
//	Fix copyright headers
//	
//	Revision 1.8  1998/08/31 19:13:32  gsl
//	drcs update
//	
//

//	
//
//	Working file: assert.cpp
//	head: 1.7
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
//	total revisions: 7;	selected revisions: 7
//	description:
//	----------------------------
//	revision 1.7
//	date: 1996-12-12 13:25:48-05;  author: gsl;  state: V4_3_00;  lines: +1 -1
//	----------------------------
//	revision 1.6
//	date: 1996-07-25 19:43:00-04;  author: gsl;  state: Exp;  lines: +1 -1
//	----------------------------
//	revision 1.5
//	date: 1996-07-25 14:14:18-04;  author: gsl;  state: Exp;  lines: +0 -0
//	Renamed from assert.cc to assert.cpp
//	----------------------------
//	revision 1.4
//	date: 1995-04-25 05:59:38-04;  author: gsl;  state: V3_3_19;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:51:55-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:32:33-05;  author: gsl;  state: V3_3x12;  lines: +2 -2
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:50:56-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
