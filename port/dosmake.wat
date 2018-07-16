# /************************************************************************/
# /*                                                                      */
# /*              WISP - Wang Interchange Source Pre-processor            */
# /*                     Copyright (c) 1988, 1989, 1990, 1991, 1992       */
# /*       An unpublished work of International Digital Scientific Inc.   */
# /*                          All rights reserved.                        */
# /*                                                                      */
# /************************************************************************/
#
#
#       File            DOSMAKE.WAT   
#
#       Function        Standard make include file for WISP on MS-DOS
#                       using WATCOM with Rational Systems DOS/4gw
#
#       Description:    This file is included into all WISP makefiles.
#                       It defines all system specific options.
#
#       Input:          $(WISP)         points to WISP base directory
#                       $(CDEBUG)       c flags to debug or optimize
#                       $(ACUDIR)       the acucobol directory
#
#       Output:         $(CFLAGS)       the standard c flags
#                       $(CFLAGS0)      same as $(CFLAGS) used to add to CFLAGS
#                       $(CC)           the c compiler command
#			$(WLIB)		The library command and flags
#			$(BIND4GW)	The command to bind in the 4GW memory manager
#                       $(V)            the location of video header files
#                       $(STDBIN)       standard place to put built bin files
#                       $(STDLIB)       standard place to put build library files
#                       $(STDINC)       standard place to put <include> files
#			$(PORT)		the $(WISP)/src/port directory
#
#       History         06/03/92        Written by GSL
#                       11/03/92        Converted for MS-DOS
#			04/07/94	Modified for Watcom, CBA
#			04/20/94	CBA, modified file so that either
#					wmake or nmake can use it
#			11/10/94	Fixed for Watcom. GSL

STDBIN= $(WISP)\src\bin
STDLIB= $(WISP)\src\lib
STDINC= $(WISP)\src\include
PORT= $(WISP)\src\port

STDCFLAGS= /dMSDOS /dDACU /dINCLUDE_VDISPIDX 
V= $(STDINC)\v

#
#       SYSTEM defines for system dependent options. 
#       Also add any defines needed to override any dedault defines
#       that are incorrect.
#               SYSCFLAGS       System dependent c flags. Do not specify
#                               the -g or -O flags as these are specified
#                               externally.
#
#
#                               /DINCLUDE_VDISPIDX to include the index file
#                                                  display code
#

#==================================================================
#       Watcom's Compile and Link command
#
CC = wcl386
WLIB = wlib -c -q
BIND4GW = $(WISP)\src\port\bindone

#
# Define SYSCFLAGS, system dependant flags
#
SYSCFLAGS = /zq /k64k /dWATCOM /l=dos4g /3s /mf /i=$(STDINC)

#
# For vdispidx() you must define LINKACU to point to the filetbl.obj 
# and run386.lib files from the ACUCOBOL distribution.  vsn_only.obj
# should be in the wisputils dir. uncomment the following for proper
# linking of acucobol stuff
#
SUBOBJ=$(WISP)\src\acu\sub.obj
ACUOBJ=$(WISP)\src\acu\filetbl.obj
ACULIB=$(ACUDIR)\run386.lib

#==================================================================
#
#       Construct CFLAGS from the pieces.  
#               CDEBUG          defined in the makefile as -g or -O
#
CFLAGS = $(SYSCFLAGS) $(STDCFLAGS) $(CDEBUG) 
CFLAGS0 = $(SYSCFLAGS) $(STDCFLAGS) $(CDEBUG) 
