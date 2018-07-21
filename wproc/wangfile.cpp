//
//	Copyright (c) 1996-1998 NeoMedia Technologies Inc. All rights reserved.
//
//	Project:	WPROC
//	
//
#include <ctype.h>

#include "wangfile.hpp"

#include "utility.hpp"

#if WANG
wang_filename::wang_filename(const char *s) {
   int i;
   init_string_with_blank_pad(filename, WANG_FILENAME_SIZE);
   init_string_with_blank_pad(libname, WANG_LIBNAME_SIZE);
   init_string_with_blank_pad(volname, WANG_VOLNAME_SIZE);

   int index = 0;

   i = 0;
   while (s[index]) {
      if (s[index] == '\x01' || s[index] == '\x02')
         break;
      else {
         if (i < WANG_FILENAME_SIZE)
            filename[i++] = toupper(s[index]);
      }
      index++;
   }

   if (s[index] == '\x01') {
      index++;
      i = 0;
      while (s[index]) {
         if (s[index] == '\x02')
            break;
         else {
            if (i < WANG_LIBNAME_SIZE)
               libname[i++] = toupper(s[index]);
         }
         index++;
      }
   }

   if (s[index] == '\x02') {
      index++;
      i = 0;
      while (s[index] && i < WANG_VOLNAME_SIZE)
         volname[i++] = toupper(s[index++]);
   }
}
#endif



//
//	History:
//	$Log: wangfile.cpp,v $
//	Revision 1.6  1998/08/31 19:50:41  gsl
//	drcs update
//	
//	Revision 1.5  1998-08-31 15:14:28-04  gsl
//	drcs update
//
//

//	
//	RCS file: /disk1/neomedia/RCS/wisp/wproc/wangfile.cpp,v
//	Working file: wangfile.cpp
//	head: 1.4
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
//	total revisions: 4;	selected revisions: 4
//	description:
//	----------------------------
//	revision 1.4
//	date: 1996-07-25 14:16:41-04;  author: gsl;  state: V4_3_00;  lines: +0 -0
//	Renamed from wangfile.cc to wangfile.cpp
//	----------------------------
//	revision 1.3
//	date: 1995-04-25 06:00:35-04;  author: gsl;  state: V3_3_19;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.2
//	date: 1995-04-17 07:52:50-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 18:33:37-05;  author: gsl;  state: V3_3x12;
//	drcs load
//	=============================================================================
