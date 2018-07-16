// Copyright (c) Lexical Software, 1992.  All rights reserved.
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


