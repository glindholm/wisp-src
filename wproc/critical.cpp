//
//	Copyright (c) Shell Stream Software LLC, All Rights Reserved.
//
//	Project:	WPROC
//	
//
// Module : critical.cpp
// Author : George Soules
// Date   : 3 July 1992

#if DOS || DOS_HOST
// Tell compiler there is inline assembler in this unit
#pragma inline
#endif

// Specification
#include "critical.hpp"

// Classes
// (none)

// Definitions and subprograms
#if DOS
#include <dos.h>
#endif


#if DOS
Boolean drive_is_ready;


#pragma option -N- // Don't allow stack overflow checks
#pragma argsused
void far critical_error_handler(
   unsigned deverr,
   unsigned errval,
   unsigned far *devhdr)
{
   drive_is_ready = BOOLEAN((deverr & 0x8000) != 0);
   _hardretn(5); // "access denied" (ignored)
}


Boolean drives_mapped(int drive_1, int drive_2) {
   if (_osmajor == 3 && _osminor < 20)
      return false;

   drive_1 += 1;
   drive_2 += 1;

   // See if logical drive 1 mapped to 2 (pg 433 "Advanced MSDOS Programming"
   asm mov ah,44h
   asm mov al,0eh
   asm mov bl,drive_1
   asm int 21h
   asm jc  ioctl_failed

   return BOOLEAN(_AL == drive_2);

ioctl_failed:
   return false;
}


Boolean drive_ready(int drive) {
   const int drive_a = 0;
   const int drive_b = 1;

   if (drive == drive_a) {
      if (drives_mapped(drive_a, drive_b)) {
         return false;
      }
   }

   if (drive == drive_b) {
      if (drives_mapped(drive_b, drive_a)) {
         return false;
      }
   }

   // Save original critical error handler
   const unsigned critical_error_handler_vector = 0x24;
   void interrupt(*old_handler)(...);
   old_handler = _dos_getvect(critical_error_handler_vector);

   // Install our handler
   _harderr(critical_error_handler);

   // Try to get information about the drive.  If the drive is not
   // ready, our handler will be invoked and set drive_is_ready to false.
   dfree free;
   drive_is_ready = true;
   getdfree(drive + 1, &free);
   if (drive_is_ready)
      drive_is_ready = BOOLEAN(free.df_sclus != 0xFFFF);

   // Re-install the original handler
   _dos_setvect(critical_error_handler_vector, old_handler);

   return drive_is_ready;
}

#endif



//
//	History:
//	$Log: critical.cpp,v $
//	Revision 1.6  1998/08/31 19:13:37  gsl
//	drcs update
//	
//

//	
//
//	Working file: critical.cpp
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
//	date: 1996-07-25 14:14:29-04;  author: gsl;  state: V4_3_00;  lines: +0 -0
//	Renamed from critical.cc to critical.cpp
//	----------------------------
//	revision 1.4
//	date: 1995-04-25 05:59:43-04;  author: gsl;  state: V3_3_19;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:52:00-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:32:39-05;  author: gsl;  state: V3_3x12;  lines: +6 -6
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:00-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
