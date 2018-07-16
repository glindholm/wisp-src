//
//	Copyright (c) 1996-1998 NeoMedia Technologies Inc. All rights reserved.
//
//	Project:	WPROC
//	Id:		$Id:$
//	RCS:		$Source:$
//	
//// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : product.cpp
// Author : George Soules
// Date   : 12 February 1991

// Specification
#include "product.hpp"

// Classes
// (none)

// Definitions and subprograms
#include <stdio.h>
#include <string.h>

extern "C" char *wisp_version(void);

char *product_copyright() {
   static char notice[256];

#if WANG
#if RUNTIME
   strcpy(notice, "WPROCRT");
#else
   strcpy(notice, "WPROC");
#endif
#else
#if RUNTIME
   strcpy(notice, "RunRT");
#else
   strcpy(notice, "Run");
#endif
#endif

#if DEMO
   strcat(notice, " DEMO");
#endif
	/* CHANGE-COPYRIGHT-DATE */

#if DOS
   strcat(notice, " v2.0x Copyright (c) 1991-1992 Lexical Software. All Rights Reserved.");
#else
   char wproc_version[80];
   sprintf(wproc_version," version %s    (WL=%s)\n", product_version_str(), wisp_version());
   strcat(notice, wproc_version);
   strcat(notice, "Copyright (c) 1991-1993 Lexical Software. All rights reserved.\n");
   strcat(notice, "Copyright (c) 1994-2002 NeoMedia Technologies, Inc.\nAll Rights Reserved.\n");
#endif
   return notice;
}

usign_16 product_version() {
   return WPROC_VERSION;
}

char *product_version_str(void)
{
	static char version[10];
	static int first = 1;
	
	if (first)
	{
		first = 0;
	
		char buf[10];
		sprintf(buf,"%04d", (int)(WPROC_VERSION));
		sprintf(version,"v%c.%c.%c%c",buf[0],buf[1],buf[2],buf[3]);
	}
	
	return version;
}



char *product_name() {
#if WANG
   return "WPROC";
#else
   return "Run";
#endif
}

#if DEMO
char *demo_notice() {
   return "DEMONSTRATION SOFTWARE -- NOT FOR SALE";
}
#endif

//
//	History:
//	$Log: product.cpp,v $
//	Revision 1.17  2002-04-03 17:00:38-05  gsl
//	2002
//
//	Revision 1.16  2001-09-13 16:20:24-04  gsl
//	Change version to 3 part 4 digits 9.9.99 format
//
//	Revision 1.15  2000-03-16 10:33:18-05  gsl
//	2000
//
//	Revision 1.14  1999-09-23 13:42:02-04  gsl
//	change copyright
//
//	Revision 1.13  1998-09-08 14:49:55-04  gsl
//	Fix copyright
//
//	Revision 1.12  1998-08-31 15:14:09-04  gsl
//	drcs update
//
//

//	
//	RCS file: /disk1/neomedia/RCS/wisp/wproc/product.cpp,v
//	Working file: product.cpp
//	head: 1.11
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
//	total revisions: 11;	selected revisions: 11
//	description:
//	----------------------------
//	revision 1.11
//	date: 1997-03-26 10:55:07-05;  author: gsl;  state: V4_3_00;  lines: +1 -1
//	Change to NeoMedia Technologies
//	----------------------------
//	revision 1.10
//	date: 1996-12-12 13:24:29-05;  author: gsl;  state: V3_9_90;  lines: +1 -1
//	DevTech -> NeoMedia
//	----------------------------
//	revision 1.9
//	date: 1996-07-25 14:16:03-04;  author: gsl;  state: Exp;  lines: +0 -0
//	Renamed from product.cc to product.cpp
//	----------------------------
//	revision 1.8
//	date: 1996-01-08 05:35:00-05;  author: gsl;  state: Exp;  lines: +1 -1
//	change copyright to 1996
//	----------------------------
//	revision 1.7
//	date: 1995-10-18 13:20:22-04;  author: gsl;  state: V3_3_19;  lines: +17 -2
//	add routine product_version_str()
//	----------------------------
//	revision 1.6
//	date: 1995-06-15 06:43:49-04;  author: gsl;  state: V3_3_18;  lines: +7 -5
//	Add reporting of the WISPLIB version number
//	----------------------------
//	revision 1.5
//	date: 1995-05-08 11:51:46-04;  author: gsl;  state: V3_3_16;  lines: +7 -7
//	changed to get the WPROC version number from the define WPROC_VERSION
//	which is set from the makefile wproc.umf
//	----------------------------
//	revision 1.4
//	date: 1995-04-25 06:00:16-04;  author: gsl;  state: V3_3_15;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:52:32-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:33:15-05;  author: gsl;  state: V3_3x12;  lines: +3 -2
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:21-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================