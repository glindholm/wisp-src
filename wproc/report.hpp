// Copyright (c) Lexical Software, 1991.  All rights reserved.
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

