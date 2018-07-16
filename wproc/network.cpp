//
//	Copyright (c) 1996-1998 NeoMedia Technologies Inc. All rights reserved.
//
//	Project:	WPROC
//	Id:		$Id:$
//	RCS:		$Source:$
//	
//// Copyright (c) Lexical Software, 1992.  All rights reserved.
//
// Module : network.cpp
// Author : George Soules
// Date   : 26 July 1992

#if NETWORK

// Specification
#include "network.hpp"

// Classes
// none

// Definitions and subprograms
extern "C" {
#include <nit.h>
}
#include "memory.hpp"
#include "report.hpp"
#include "utility.hpp"


void verify_network_rc(int actual_rc, int expected_rc) {
   if (actual_rc != expected_rc)
      report_fatal_error(22, int_32_to_string(actual_rc));
}


char *nw_network_address() {
   int  rc;
   char address[12]; // 4 + 6 + 2

   assert(nw_network_available());

   rc = GetInternetAddress
      (GetConnectionNumber(), address, address + 4, (WORD *) (address + 10));
   verify_network_rc(rc, 0);

   char     s[26 + 1]; // 00000000:000000000000:0000 = 3 sections: 0, 1, 2
   int      index = 0;
   int      section = 0;
   usign_32 x;

   for (int i = 0; i < 12; i++) {
      // Convert each address byte to a hex representation
      x = (usign_8) (address[i]);
      if (i == 4) {
         section = 1;
         s[8] = ':';
      }
      else if (i == 10) {
         section = 2;
         s[21] = ':';
      }
      index = i*2 + section;
      if (x <= 0x0F)
         s[index++] = '0'; // Insert leading zero
      ultoa(x, s + index, 16);
   }
   s[26] = '\0';
   return upper_case(s);
}


Boolean nw_user_logged_in() {
   char objName[48];
   WORD objType;
   int_32 objID;
   BYTE loginTime[7];

   assert(nw_network_available());

   return BOOLEAN(GetConnectionInformation(GetConnectionNumber(), objName, &objType, &objID, loginTime) == 0);
}


Boolean nw_network_available() {
   BYTE dummy1 = 0;
   BYTE dummy2 = 0;
   BYTE dummy3 = 0;
   return BOOLEAN(GetNetWareShellVersion(&dummy1, &dummy2, &dummy3) == 255);
}


char *nw_user_full_name() {
   int  rc;
   BYTE propertyValue[128];
   BYTE moreSegments;
   BYTE propertyFlags;

   assert(nw_user_logged_in());

   rc = ReadPropertyValue(nw_user_login_name(), OT_USER, "IDENTIFICATION",
      1, propertyValue, &moreSegments, &propertyFlags);
   verify_network_rc(rc, 0);
   return dup_string(propertyValue);
}


char *nw_user_login_name() {
   int  rc;
   char objName[48];
   WORD objType;
   int_32 objID;
   BYTE loginTime[7];

   assert(nw_user_logged_in());

   rc = GetConnectionInformation
      (GetConnectionNumber(), objName, &objType, &objID, loginTime);
   verify_network_rc(rc, 0);
   return dup_string(objName);
}

#endif



//
//	History:
//	$Log: network.cpp,v $
//	Revision 1.8  1998-08-31 15:13:57-04  gsl
//	drcs update
//
//

//	
//	RCS file: /disk1/neomedia/RCS/wisp/wproc/network.cpp,v
//	Working file: network.cpp
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
//	date: 1997-06-09 17:30:47-04;  author: scass;  state: V4_3_00;  lines: +2 -2
//	int4 -> int_32
//	----------------------------
//	revision 1.6
//	date: 1997-06-09 16:49:50-04;  author: scass;  state: Exp;  lines: +2 -2
//	Changed long to int4 for portability.
//	----------------------------
//	revision 1.5
//	date: 1996-07-25 14:15:29-04;  author: gsl;  state: V3_3_93;  lines: +0 -0
//	Renamed from network.cc to network.cpp
//	----------------------------
//	revision 1.4
//	date: 1995-04-25 06:00:06-04;  author: gsl;  state: V3_3_19;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:52:23-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:33:03-05;  author: gsl;  state: V3_3x12;  lines: +4 -6
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:14-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
