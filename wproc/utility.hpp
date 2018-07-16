// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : utility.hpp
// Author : George Soules
// Date   : 14 February 1991

#ifndef UTILITY__HPP
#define UTILITY__HPP

// Classes
// (none)

// Definitions and subprograms
#include <string.h>
#include "environ.hpp"


#define max(a,b)  (((a) > (b)) ? (a) : (b))
#define min(a,b)  (((a) < (b)) ? (a) : (b))

char *strip(const char *a_string);

char *trim(const char *a_string);

int str_index(const char *s1, const char *s2);

char *str_translate(const char *s1, const char *s2, const char *s3);

int str_verify(const char *s1, const char *s2);

char *lower_case(char *s);

char *upper_case(char *s);

Boolean same_string(const char *s1, const char *s2);

int match(
   const char *s0,
   const char *s1,
   const char *s2 = NULL,
   const char *s3 = NULL);

Boolean in_int_8_range(const char *a_digit_string);

Boolean in_int_16_range(const char *a_digit_string);

Boolean in_int_32_range(const char *a_digit_string);

Boolean string_to_int_32(const char *a_string, int_32 &an_int_32);

char *int_32_to_string(int_32 an_int_32);

Boolean is_yes_or_no(const char *a_string, Boolean &is_yes);

char *formatted_date(
   int format,
   int year    = -1,
   int month   = -1,
   int day     = -1,
   int weekday = -1);

char *formatted_time(
   int format,
   int hour   = -1,
   int minute = -1,
   int sec    = -1,
   int dec    = -1);

int print(const char* filename, int copies, Boolean eject, Boolean spool);

Boolean memory_left(int min_heap, int min_stack = 256);

void init_string_with_blank_pad(char *s, int size, const char *value = NULL);

void int_32_to_ascii(int_32 an_int_32, char *a_string);

int parse_date(
   const char *a_date,
   int         low_year,
   int         high_year,
   int_32     &year,
   int_32     &month,
   int_32     &day);

int parse_time(
   const char *a_time,
   int_32     &hour,
   int_32     &min,
   int_32     &sec);

#endif

