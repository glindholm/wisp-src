//
//	Copyright (c) Shell Stream Software LLC, All Rights Reserved.
//
//	Project:	WPROC
//	
//// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : report.hpp
// Author : George Soules
// Date   : 12 February 1991

#ifndef REPORT__HPP
#define REPORT__HPP

// Classes
#include "range.hpp"

// Definitions and subprograms
#include <limits.h>
#include "environ.hpp"


class location {
   public:
      location(const char *a_source, range *a_range) {
         the_source = a_source;
         the_range = a_range;
      }
      const char *the_source;
      range      *the_range;
};

void report_syntax_error
   (int error_number, location *a_location, const char *sub = NULL);

void report_semantic_error
   (int error_number, location *a_location, const char *sub = NULL);


const int_32 error_is_fatal = INT_MIN;

void report_correction(
   int_32     &new_value,
   Boolean    &no_correction,
   int         error_number,
   location   *a_location,
   const char *bad_value,
   const char *value_kind,
   const char *first = NULL,
   const char *last = NULL);

void report_correction(
   char      *&new_value,
   Boolean    &no_correction,
   int         error_number,
   location   *a_location,
   const char *bad_value,
   const char *value_kind);

#if WANG
void report_filename_correction(
   char      *&new_value,
   Boolean    &no_correction,
   int         error_number,
   location   *a_location,
   const char *bad_value,
   const char *value_kind);
#endif

void report_general_error(int error_number, const char *sub = NULL);

void report_fatal_error(int error_number, const char *sub = NULL);

void report_status(
   const char *msg1,
   const char *msg2 = NULL,
   const char *msg3 = NULL);

#endif


//
//	History:
//	$Log: report.hpp,v $
//	Revision 1.6  1998/08/31 19:14:12  gsl
//	drcs update
//	
//

//	
//	RCS file: /disk1/neomedia/RCS/wisp/wproc/report.hpp,v
//	Working file: report.hpp
//	head: 1.5
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
//	total revisions: 5;	selected revisions: 5
//	description:
//	----------------------------
//	revision 1.5
//	date: 1997-06-10 12:05:54-04;  author: scass;  state: V4_3_00;  lines: +1 -1
//	Corrected LONG_MIN and LONG_MAX
//	----------------------------
//	revision 1.4
//	date: 1995-04-25 06:00:20-04;  author: gsl;  state: V3_3_93;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:52:36-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:33:19-05;  author: gsl;  state: V3_3x12;  lines: +3 -3
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:24-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
