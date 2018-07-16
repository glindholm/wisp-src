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
#       File            DOSMAKE.MAK    
#
#       Function        Standard make include file for WISP on MS-DOS
#			using Intel C Code Builder.
#
#       Description:    This file is included into all WISP makefiles.
#                       It defines all system specific options.
#
#       Input:          $(WISP)         points to WISP base directory
#                       $(CDEBUG)       c flags to debug or optimize
#			$(ACUDIR)	the acucobol directory
#
#       Output:         $(CFLAGS)       the standard c flags
#                       $(CFLAGS0)      same as $(CFLAGS) used to add to CFLAGS
#                       $(CC)           the c compiler command
#                       $(V)            the location of video header files
#                       $(PQFLAG)       Printq c flags
#                       $(LIBSOC)       The socket library flags
#                       $(STDBIN)       standard place to put built bin files
#                       $(STDLIB)       standard place to put build library files
#                       $(STDINC)       standard place to put <include> files
#                       $(LIBCFLAGS)    standard c flags when libraries are used
#
#       History         06/03/92        Written by GSL
#                       11/03/92        Converted for MS-DOS


#
#       STANDARD defines used on all systems
#               STDBIN          standard place to put built bin files
#               STDLIB          standard place to put build library files
#               STDINC          standard place to put <include> files
#               STDCFLAGS       standard c flags
#               STDLIBFLAGS     standard c flags when libraries are used
#               V               location of the video header files
#
STDBIN= $(WISP)\src\bin
STDLIB= $(WISP)\src\lib
STDINC= $(WISP)\src\include

STDCFLAGS= /DMSDOS
LIBCFLAGS= -L$(STDLIB)
V= $(STDINC)\v

#
#       DEFAULT defines for all systems.
#       If the specific system requires different values it will override
#       these later in the file.
#               CC              the c compiler
#               RANLIB          how to change an archive lib into a random lib
#               PQFLAG          Printq c flags:
#                               -DSOCKCODE  to use Berkley sockets (default)
#                               -DMQCODE    to use System V message Queues
#                               -DBSDWAIT   to use BSD wait mechanism (default)
#                               -DSYSVWAIT  to use System V wait 
#                               -DNO_SELECT if select() not supported
#               LIBSOC          The socket library flags
#

PQFLAG= 
LIBSOC=

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
#       MS-DOS 386 Intel C Code Builder
#
CC = icc

#SYSCFLAGS = /DDACU /znoalign /I $(STDINC)
SYSCFLAGS = /DDACU /DINCLUDE_VDISPIDX /znoalign /I $(STDINC)

#
# For vdispidx() you must define LINKACU to point to the filetbl.obj 
# and run386.lib files from the ACUCOBOL distribution.  vsn_only.obj
# should be in the wisputils dir. uncomment the following for proper
# linking of acucobol stuff
#
LINKACU = $(ACUDIR)\filetbl.obj $(ACUDIR)\run386.lib

#==================================================================




#
#       Construct CFLAGS from the pieces.  
#               CDEBUG          defined in the makefile as -g or -O
#
CFLAGS = $(CDEBUG) $(SYSCFLAGS) $(STDCFLAGS)
CFLAGS0 = $(CDEBUG) $(SYSCFLAGS) $(STDCFLAGS)

