//
//	Copyright (c) Shell Stream Software LLC, All Rights Reserved.
//
//	Project:	WPROC
//	
//
// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : utility.cpp
// Author : George Soules
// Date   : 14 February 1991

#if DOS || DOS_HOST
// Tell compiler there is inline assembler in this unit
#pragma inline
#endif

// Specification
#include "utility.hpp"

// Classes
#include "process.hpp"
#include "reader.hpp"
#include "screen.hpp"

// Definitions and subprograms
#if DOS || DOS_HOST
#include <alloc.h>
#include <bios.h>
#include <dos.h>
#include <io.h>
#endif
#if UNIX
#include <unistd.h>
#endif
#if UNIX || WIN32
#include <time.h>
#include <sys/types.h>
#endif
#if WIN32
#include <io.h>
#endif
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include "colors.hpp"
#include "crt_io.hpp"
#include "debugaid.hpp"
#include "environ.hpp"
#include "memory.hpp"

#ifdef WIN32
#define access(filename, mode)		_access(filename, mode)
#endif


#if DOS || DOS_HOST
struct bytes_2 {
   usign_8 low;
   usign_8 high;
};

union bits {
   usign_16 word;
   bytes_2  byte;
};

bits reg_ax;
bits reg_dx;

// Spontaneous Assembly routines
extern "C" void console_init(void);
extern "C" void cbrk_disable(void);
extern "C" void cbrk_enable(void);
extern "C" void get_date(void);
extern "C" void get_time(void);

// Spontaneous Assembly global variables
extern unsigned char cbrk_pending;
extern unsigned char screen_cols;
extern unsigned char screen_rows;
#endif


char *strip(const char *a_string) {
   // Return a_string without leading or trailing whitespace
   int first = 0;
   int last  = strlen(a_string) - 1;
   int size;
   while (a_string[first] == ' ')
      first++;
   if (first > last)
      size = 0;
   else {
      while (last >= 0 && a_string[last] == ' ')
         last--;
      size = last - first + 1;
   }
   char *s = new_string(size + 1);
   strncpy(s, a_string + first, size);
   s[size] = '\0';
   return s;
}


char *trim(const char *a_string) {
   // Return a_string without trailing whitespace
   int last  = strlen(a_string) - 1;
   int size;
   if (last < 0)
      size = 0;
   else {
      while (last >= 0 && a_string[last] == ' ')
         last--;
      size = last + 1;
   }
   char *s = new_string(size + 1);
   strncpy(s, a_string, size);
   s[size] = '\0';
   return s;
}


int str_index(const char *s1, const char *s2) {
   // Returns the position of S2 within S1.  Returns 0 if S2 not found.
   // Modified from CUL strindex(), (c) South Mountain Software
   int j;
   int k;
   int index;

   for(j = k = index = 0; ; ++k) {
      if (s1[k] == '\0')
         return 0; // not found
      if (s1[k] == s2[j]) {
         if (j == 0)
            index = k;
         j += 1;
         if (s2[j] == '\0')
            return index + 1; // found */
      }
      else {
         if (j != 0)
            k = index;
         j = 0; // reset to look at check string from the beginning */
      }
   }
}


char *str_translate(const char *s1, const char *s2, const char *s3) {
   // Translate s3 character contained in s1 to s2 characters
   // Modified from CUL strdate(), (c) South Mountain Software

   int i;
   int j;
   char *out = new_string(strlen(s1) + 1);

   for (i = 0; ; i++) {
      if (s1[i] == '\0') {
         out[i] = '\0';
         return out; // done
      }
      for (j = 0; ; j++) {
         if (s1[i] == s3[j]) {
            if(j > (((int)strlen(s2)) - 1)) {
               out[i] = ' '; // blank inserted
               break;
            }
            out[i] = s2[j]; // conversion needed
            break;
         }
         else {
            if(s3[j] == '\0') {
               out[i] = s1[i]; // conversion not needed
               break;
            }
            else
               continue;
         }
      }
   }
}


int str_verify(const char *s1, const char *s2) {
   // Verify that all chars in s1 are contained in s2

   int j;
   int i;
   int s2_len = strlen(s2);
   Boolean found = true;

   for (i = 0; i < (int)strlen(s1); i++) {
      for (j = 0; j < s2_len; j++) {
         if (s1[i] == s2[j])
            break;
      }
      if (j == s2_len) {
         found = false;
         break;
      }
   }
   return found ? 0 : i + 1;
}


char *lower_case(char *s) {
#if DOS
   return strlwr(s);
#else
   int i = 0;
   while (s[i]) {
      s[i] = tolower(s[i]);
      i++;
   }
   return s;
#endif
}


char *upper_case(char *s) {
#if DOS
   return strupr(s);
#else
   int i = 0;
   while (s[i]) {
      s[i] = toupper(s[i]);
      i++;
   }
   return s;
#endif
}


Boolean same_string(const char *s1, const char *s2) {
   assert(s1);
   assert(s2);
#if DOS
   return BOOLEAN(stricmp(s1, s2) == 0);
#else
   int i = 0;
   while (s1[i]) {
      if (toupper(s1[i]) != toupper(s2[i]))
         return false;
      i++;
   }
   return BOOLEAN(s2[i] == '\0');
#endif
}


int match(
   const char *s0,
   const char *s1,
   const char *s2,
   const char *s3)
{
   char *s = strip(s0);
   int which = 0;

   if (same_string(s, s1))
      which = 1;
   else if (s2)
      if (same_string(s, s2))
         which = 2;
      else
         if (s3)
            if (same_string(s, s3))
               which = 3;

   delete s;
   return which;
}


Boolean in_int_8_range(const char *a_digit_string) {
   // The in_int_n_range routines assume that a_digit_string contas2
   // only digits (no sign, or leading/trailing blanks allowed)
   int len = strlen(a_digit_string);
   if (len == 3)
      return BOOLEAN(strcmp(a_digit_string, "127") <= 0);
   return len < 3 ? true : false;
}


Boolean in_int_16_range(const char *a_digit_string) {
   int len = strlen(a_digit_string);
   if (len == 5)
      return BOOLEAN(strcmp(a_digit_string, "32767") <= 0);
   return len < 5 ? true : false;
}


Boolean in_int_32_range(const char *a_digit_string) {
   int len = strlen(a_digit_string);
   if (len == 10)
      return BOOLEAN(strcmp(a_digit_string, "2147483647") <= 0);
   return len < 10 ? true : false;
}


Boolean string_to_int_32(const char *a_string, int_32 &an_int_32) {
   Boolean is_integer = false;

   // Remove any leading and trailing white space
   char *s = strip(a_string);
   if (s[0]) {
      // Try to convert
      char *endptr;
      int_32 number = strtol(s, &endptr, 10);

      // Verify that conversion ended with last char in string
      if (endptr == s + strlen(s)) {

         // Skip past the sign, if any
         int first_digit = 0;
         while (! isdigit(s[first_digit]))
            first_digit++;

         // Verify that the number is in range
         if (in_int_32_range(s + first_digit)) {
            an_int_32 = number;
            is_integer = true;
         }
      }
   }
   delete_string(s);
   return is_integer;
}


char *int_32_to_string(int_32 an_int_32) {
   char *s = new_string(INT_32_STRING_SIZE);
   int_32_to_ascii(an_int_32, s);
   return s;
}


Boolean is_yes_or_no(const char *a_string, Boolean &is_yes) {
   int which = match(a_string, "yes", "no");
   if (which) {
      is_yes = BOOLEAN(which == 1);
      return true;
   }
   else
      return false;
}


char *formatted_date(
   int format,
   int year,
   int month,
   int day,
   int weekday)
{
   // Return the date in a specified format
   // Severely modified from CUL strdate(), (c) South Mountain Software

   const int first_format = 0;
   //  0 - weekday

   //  USA formats
   //  1 - mmddyy
   //  2 - mmddyyyy
   //  3 - mm/dd/yy
   //  4 - mm/dd/yyyy
   //  5 - mm-dd-yy
   //  6 - mm-dd-yyyy
   //  7 - mm.dd.yy
   //  8 - mm.dd.yyyy
   //  9 - mm..m dd, yy
   // 10 - mm..m dd, yyyy
   // 11 - mmm dd, yy
   // 12 - mmm dd, yyyy

   //  European formats
   // 13 - ddmmyy
   // 14 - ddmmyyyy
   // 15 - dd/mm/yyyy
   // 16 - dd/mm/yy
   // 17 - dd-mm-yy
   // 18 - dd-mm-yyyy
   // 19 - dd.mm.yy
   // 20 - dd.mm.yyyy
   // 21 - dd mmm yy
   // 22 - dd mmm yyyy
   // 23 - dd mm..m yy
   // 24 - dd mm..m yyyy

   //  Japanese formats
   // 25 - yymmdd
   // 26 - yyyymmdd
   // 27 - yy/mm/dd
   // 28 - yyyy/mm/dd
   // 29 - yy-mm-dd
   // 30 - yyyy-mm-dd
   // 31 - yy.mm.dd
   // 32 - yyyy.mm.dd
   // 33 - yy mmm dd
   // 34 - yyyy mmm dd
   // 35 - yy mm..m dd
   // 36 - yyyy mm..m dd
   const int last_format = 36;

   Boolean leading_zero = BOOLEAN(format >= 0);
   format = abs(format);

   if (format > last_format || format < first_format)
      return dup_string("");

   if (year == -1) {
      // Get the system date
#if DOS || DOS_HOST
      asm call far ptr get_date
      reg_ax.word = _AX;
      weekday     = _CL;
      year        = _DX;
      day         = reg_ax.byte.low;
      month       = reg_ax.byte.high;
#else
#if WANG
      time_t raw_time = 0;
                struct tm timedate;
                struct tm *ptimedate = &timedate;

      raw_time  = time(&raw_time);
                ptimedate = localtime(&raw_time);

      weekday = ptimedate->tm_wday;
      year    = ptimedate->tm_year + 1900;
      day     = ptimedate->tm_mday;
      month   = ptimedate->tm_mon + 1;
#endif
#endif
   }

   if (format == 0) {
      switch (weekday) {
         case -1 : return dup_string("");
         case  0 : return dup_string("Sunday");
         case  1 : return dup_string("Monday");
         case  2 : return dup_string("Tuesday");
         case  3 : return dup_string("Wednesday");
         case  4 : return dup_string("Thursday");
         case  5 : return dup_string("Friday");
         case  6 : return dup_string("Saturday");
      }
   }

   // Derive format kind and country
   enum format_class {usa, europe, japan};
   format_class country = usa;
   int          kind    = format;
   if (format > 12) {
      kind    = format - (format > 24 ? 24 : 12);
      country = format > 24 ? japan : europe;
   }

   char ds[3];
   char ys[5];
   char ms[10];

   // Format day
   int_32_to_ascii(day, ds);
   if (day <= 9 && (kind <= 2 || leading_zero)) {
      // Add leading zero
      ds[2] = '\0';
      ds[1] = ds[0];
      ds[0] = '0';
   }

   // Format month
   if (kind <= 8) {
      int_32_to_ascii(month, ms);
      if (month <= 9 && (kind <= 2 || leading_zero)) {
         // Add leading zero
         ms[2] = '\0';
         ms[1] = ms[0];
         ms[0] = '0';
      }
   }
   else {
      switch (month) {
         case  1 : strcpy(ms, "January");   break;
         case  2 : strcpy(ms, "February");  break;
         case  3 : strcpy(ms, "March");     break;
         case  4 : strcpy(ms, "April");     break;
         case  5 : strcpy(ms, "May");       break;
         case  6 : strcpy(ms, "June");      break;
         case  7 : strcpy(ms, "July");      break;
         case  8 : strcpy(ms, "August");    break;
         case  9 : strcpy(ms, "September"); break;
         case 10 : strcpy(ms, "October");   break;
         case 11 : strcpy(ms, "November");  break;
         case 12 : strcpy(ms, "December");  break;
      }
   }
   if (kind >= 11)
      ms[3] = '\0';

   // Format year
   int_32_to_ascii(year, ys);
   if (kind % 2 == 1) {
      // 2 digit year (odd number) format
      ys[0] = ys[2];
      ys[1] = ys[3];
      ys[2] = '\0';
   }

   // Catenate day, month, year
   char out[20];
   out[0] = '\0';

   // Part 1
   if (country == usa)
      strcat(out, ms);
   else if (country == europe)
      strcat(out, ds);
   else
      strcat(out, ys);

   if (kind == 3 || kind == 4)
      strcat(out,"/");
   else if (kind == 5 || kind == 6)
      strcat(out,"-");
   else if (kind == 7 || kind == 8)
      strcat(out,".");
   else if (kind >= 9)
      strcat(out," ");

   // Part 2
   strcat(out, country == usa ? ds : ms);

   if (kind == 3 || kind == 4)
      strcat(out,"/");
   else if (kind == 5 || kind == 6)
      strcat(out,"-");
   else if (kind == 7 || kind == 8)
      strcat(out,".");
   else if (kind >= 9)
      strcat(out, country == usa ? ", " : " ");

   // Part 3
   strcat(out, country == japan ? ds : ys);

   return strip(out);
}


char *formatted_time(
   int format,
   int hour,
   int minute,
   int sec,
   int dec)
{
   // Return the date in a specified format
   // Modified from CUL strdate(), (c) South Mountain Software

   const int first_format = 1;
   //  1 - hhmm
   //  2 - hrmm
   //  3 - hhmmss
   //  4 - hrmmss
   //  5 - hhmmssdd
   //  6 - hrmmssdd
   //  7 - hh:mm
   //  8 - hr:mm
   //  9 - hh:mm:ss
   // 10 - hr:mm:ss
   // 11 - hh:mm:ss:dd
   // 12 - hr:mm:ss:dd
   // 13 - hh:mm:ss.dd
   // 14 - hr:mm:ss.dd
   // 15 - hrmm pp
   // 16 - hrmmss pp
   // 17 - hrmmssdd pp
   // 18 - hr:mm pp
   // 19 - hr:mm:ss pp
   // 20 - hr:mm:ss:dd pp
   // 21 - hr:mm:ss.dd pp
   const int last_format = 21;

   Boolean leading_zero = BOOLEAN(format >= 0);
   format = abs(format);

   if (format > last_format || format < first_format)
      return dup_string("");

   if (hour == -1) {
      // Get the system time
#if DOS || DOS_HOST
      asm call far ptr get_time
      reg_ax.word = _AX;
      reg_dx.word = _DX;
      hour        = reg_dx.byte.high;
      minute      = reg_dx.byte.low;
      sec         = reg_ax.byte.high;
      dec         = reg_ax.byte.low;
#else
#if WANG
      time_t raw_time = 0;
                struct tm timedate;
                struct tm *ptimedate = &timedate;

      raw_time  = time(&raw_time);
                ptimedate = localtime(&raw_time);

                hour   = ptimedate->tm_hour;
                minute = ptimedate->tm_min;
                sec    = ptimedate->tm_sec;
                dec    = 0;
#endif
#endif
   }

   int actual_hour = hour;

   char hs[3];
   char ms[3];
   char ss[3];
   char ds[3];

   Boolean colon = BOOLEAN(format >= 7 && format <= 14 || format >= 18 && format <= 21);

   // Format hour
   if ((format >= 14 || format % 2 == 0) && hour > 12)
      hour = hour - 12;

   int_32_to_ascii(hour, hs);
   if (hour <= 9 && (! colon || leading_zero)) {
      // Add leading zero
      hs[2] = '\0';
      hs[1] = hs[0];
      hs[0] = '0';
   }

   // Format minutes
   int_32_to_ascii(minute, ms);
   if (minute <= 9) {
      // Add leading zero
      ms[2] = '\0';
      ms[1] = ms[0];
      ms[0] = '0';
   }

   // Format seconds
   int_32_to_ascii(sec, ss);
   if (sec <= 9) {
      // Add leading zero
      ss[2] = '\0';
      ss[1] = ss[0];
      ss[0] = '0';
   }

   // Format hundredths
   int_32_to_ascii(dec, ds);
   if (dec <= 9) {
      ds[2] = '\0';
      ds[1] = ds[0];
      ds[0] = '0';
   }

   // Catenate the parts
   char out[20];
   out[0] = '\0';
   strcat(out, hs);

   if (colon)
      strcat(out, ":");
   strcat(out, ms);

   if (format != 1 && format != 2  && format != 7 &&
       format != 8 && format != 15 && format != 18)
   {
      if (colon)
         strcat(out, ":");
      strcat(out, ss);
   }

   if (format == 5  || format == 6 || (format >= 11 && format <= 14) ||
       format == 17 || format >= 20)
   {
      if (format == 11 || format == 12 || format == 20)
         strcat(out, ":");
      else if (format == 13 || format == 14 || format == 21 )
         strcat(out, ".");
      strcat(out, ds);
   }
   if (format > 14)
      strcat(out, actual_hour > 11 ? " PM" : " AM");
   return strip(out);
}


#if DOS || DOS_HOST
Boolean printer_assistance() {
   Boolean cursor_on = BOOLEAN(! the_process->cursor_off);
   colors coloring = {color_black, color_white};
   field_attributes attrs = default_field_attributes;
   screen the_screen;
   the_screen.add_field(new field
      (1, "Printer needs attention", attrs, false, coloring));
   the_screen.add_field(new field
      (3, "Enter - Resume printing", attrs, false, coloring));
   the_screen.add_field(new field
      (4, "F10   - Cancel printing", attrs, false, coloring));
   the_screen.disable_all_fkeys();
   the_screen.enable_fkey(0);
   the_screen.enable_fkey(10);
   the_screen.set_restrict(true);
   the_screen.set_screen_colors(coloring);
   the_screen.set_border_colors(coloring);
   the_screen.set_title_colors(coloring);
   the_screen.set_border(7);
   the_screen.set_restore(true);
   the_screen.set_first_row(1);
   the_screen.set_corners(26, 9, 52, 14);
   the_screen.set_alarm(true);
   cursor_visible(false);
   the_screen.display(true);
   if (cursor_on)
      cursor_visible(true);
   return BOOLEAN(! (the_screen.fkey() == 10 || the_screen.cancelled()));
}
#endif

Boolean spooler_installed() {
#if DOS || DOS_HOST
   _AH = 0x01;
   _AL = 0x00;
   geninterrupt(0x2F);
   return BOOLEAN(_AL == 0xFF);
#else
   return false;
#endif
}


int spool_file(const char* filename) {
#if DOS || DOS_HOST
   // This code was isolated in part because int 2F seems to clobber
   // SI and other registers that the C++ code generator assumed were safe.

   struct printer_request {
      usign_8         level;
      const char far *fname;
   };
   printer_request packet;
   packet.level = 0;
   packet.fname = filename;

   asm push ds
   _DS = FP_SEG(&packet);
   _DX = FP_OFF(&packet);
   _AH = 0x01;
   _AL = 0x01;
   geninterrupt(0x2F);
   asm pop  ds
   asm jnc ok
   return _AX;
   ok:
#endif
   return 0;
}


int print(const char* filename, int copies, Boolean eject, Boolean spool) {
   // Return codes:
   //    0=success, 1=file not found, 2=spooler not installed,
   //    3=printing cancelled, 4=other

   if (! copies)
      return 0;
   if (access(filename, 0))
      return 1;

   for (int i = 1; i <= copies; i++) {
      if (spool) {
         if (! spooler_installed())
            return 2;
         int error = spool_file(filename);
         if (error)
            return (error == 2 || error == 3) ? 1 : 4;
      }
      else {
         file_reader the_reader(filename);
         if (! the_reader.ok())
            return 1;
#if DOS || DOS_HOST
         int c = 0;
         int status = biosprint(2, 0, 0);
         while ((c = the_reader.next_char()) != EOF) {
            if (! (status & 0x10)) {
               if (! printer_assistance())
                  return 3;
            }
            if (c == '\n')
               biosprint(0, '\r', 0);
            status = biosprint(0, c, 0);
         }
         if (eject)
            biosprint(0, '\f', 0);
#endif
      }
   }
   return 0;
}


Boolean memory_left(int min_heap, int min_stack) {
#if DOS || DOS_HOST
   return Boolean (coreleft() >= min_heap && _SP >= min_stack);
#else
   return true;
#endif
}


void init_string_with_blank_pad(char *s, int size, const char *value) {
   int value_size = value ? strlen(value) : 0;
   for (int i = 0; i < size; i++)
      s[i] = i < value_size ? value[i] : ' ';
   s[size] = '\0';
}


void int_32_to_ascii(int_32 an_int_32, char *a_string) {
#if DOS
   ltoa(an_int_32, a_string, 10);
#else
   sprintf(a_string, "%ld", (long)an_int_32);
#endif
}


int parse_date(
   const char *a_date,
   int         low_year,
   int         high_year,
   int_32     &year,
   int_32     &month,
   int_32     &day)
{
   // Returns:
   // 0 : good date
   // 2 : bad year
   // 3 : bad month
   // 4 : bad day
   // 5 : bad format string

   if (strlen(a_date) == 8) {
      char s[8 + 1];
      strcpy(s, a_date);
      if (! string_to_int_32(s + 6, day))
         day = -1;
      s[6] = '\0';
      if (! string_to_int_32(s + 4, month))
         month = -1;
      s[4] = '\0';
      if (! string_to_int_32(s, year))
         year = -1;
      if (year < low_year || year > high_year)
         return 2;
      if (month < 1 || month > 12)
         return 3;
      if (month == 2 && day > (abs(year - 1992) & 0x0003 ? 28 : 29))
         return 4; // day failed February/leap year test
      Boolean short_month = BOOLEAN(month == 4 || month == 6 || month == 9 || month == 11);
      if (day < 1 || day > (short_month ? 30 : 31))
         return 4;
      return 0;
   }
   return 5;
}


int parse_time(
   const char *a_time,
   int_32     &hour,
   int_32     &min,
   int_32     &sec)
{
   // Returns:
   // 0 : good time
   // 2 : bad hour
   // 3 : bad minute
   // 4 : bad month
   // 5 : bad format string

   if (strlen(a_time) == 6) {
      char s[6 + 1];
      strcpy(s, a_time);
      if (! string_to_int_32(s + 4, sec))
         sec = -1;
      s[4] = '\0';
      if (! string_to_int_32(s + 2, min))
         min = -1;
      s[2] = '\0';
      if (! string_to_int_32(s, hour))
         hour = -1;
      if (hour < 0 || hour > 23)
         return 2;
      if (min < 0 || min > 59)
         return 3;
      if (sec < 0 || sec > 59)
         return 4;
      return 0;
   }
   return 5;
}

//
//	History:
//	$Log: utility.cpp,v $
//	Revision 1.17  2011/10/29 20:09:14  gsl
//	Fix ISO routine name warnins on WIN32
//	
//	Revision 1.16  2003/02/05 15:23:59  gsl
//	Fix -Wall warnings
//	
//	Revision 1.15  2002/07/08 21:09:31  gsl
//	For int_32 don;'t use %ld in printf (long 64 on Alpha)
//	
//	Revision 1.14  1998/08/31 19:50:39  gsl
//	drcs update
//	
//	Revision 1.13  1998-08-31 15:14:26-04  gsl
//	drcs update
//
//

//	
//	RCS file: /disk1/neomedia/RCS/wisp/wproc/utility.cpp,v
//	Working file: utility.cpp
//	head: 1.12
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
//	total revisions: 12;	selected revisions: 12
//	description:
//	----------------------------
//	revision 1.12
//	date: 1997-10-01 09:31:47-04;  author: gsl;  state: V4_3_00;  lines: +3 -3
//	Fix warnings
//	----------------------------
//	revision 1.11
//	date: 1997-06-09 17:29:27-04;  author: scass;  state: V4_1_02;  lines: +2 -2
//	int4 -> int_32
//	----------------------------
//	revision 1.10
//	date: 1997-06-09 16:51:20-04;  author: scass;  state: Exp;  lines: +2 -2
//	Changed long to int4 for portability.
//	----------------------------
//	revision 1.9
//	date: 1996-07-25 19:48:25-04;  author: gsl;  state: V3_3_93;  lines: +9 -5
//	NT
//	----------------------------
//	revision 1.8
//	date: 1996-07-25 14:16:37-04;  author: gsl;  state: Exp;  lines: +1 -1
//	Renamed from utility.cc to utility.cpp
//	----------------------------
//	revision 1.7
//	date: 1996-04-18 13:00:42-04;  author: jockc;  state: Exp;  lines: +6 -1
//	moved declaration of for loop index from for loop to function
//	 auto decl area
//	----------------------------
//	revision 1.6
//	date: 1995-06-02 12:15:22-04;  author: gsl;  state: V3_3_19;  lines: +1 -1
//	fix warning
//	----------------------------
//	revision 1.5
//	date: 1995-06-02 10:16:57-04;  author: gsl;  state: Exp;  lines: +2 -1
//	Ifdef out a DOS piece that didn't compile on sequent
//	----------------------------
//	revision 1.4
//	date: 1995-04-25 06:00:33-04;  author: gsl;  state: V3_3_16;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:52:47-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:33:33-05;  author: gsl;  state: V3_3x12;  lines: +26 -28
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:33-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
