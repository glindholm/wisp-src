#/************************************************************************/
#/*                                                                      */
#/*              Copyright (c) 1988, 1989, 1990, 1991, 1992              */
#/*      An unpublished work of International Digital Scientific Inc.    */
#/*                         All rights reserved.                         */
#/*                                                                      */
#/************************************************************************/
#
#
#       File:           wruncblx.wat
#
#                       **** This is an IDSI only version ****
#
#       Function:       Makefile for building the Acucobol runtime
#                       using Watcom
#
#       Description:    This makefile can generate the following versions
#                       of the Acucobol runtime system.
#
#                       wruncbl         The standard runtime for Acucobol
#                                       version 2 and up.
#
#                       wruncble        The EDE version of the runtime.
#
#                       wruncbl1        The Acucobol version 1.5 runtime.
#                                       (Only used if running an old version
#                                       of Acucobol.)
#
#       Instructions:   To create an Acucobol runtime system follow the
#                       instructions in Appendix A of the WISP Manual
#                       "Building an ACUCOBOL runtime system".
#
#                       To build wruncbl:
#                               $ make -f wruncbl.umf
#
#                       To build wruncble:
#                               $ make -f wruncbl.umf wruncble
#
#
#       LIBPATH:        If your libraries reside in a directory other then
#                       /usr/lib then change LIBPATH to there correct
#                       location.
#
#	DATE:		Mods:
#	04/20/94	CBA, Created and set up file so that either
#			wmake or nmake can use it
#			just have to comment/uncomment the appropriate
#			lines; main difference here is that references
#			to environment variables using '%' are for 
#			Watcom's wmake, vs nmake.
#
#       Leave CDEBUG blank - it will be filled in manually on the build
#
CDEBUG= /d2
!include $(WISP)\src\port\dosmake.wat

LIBVIDEO=$(STDLIB)\video.lib
LIBWISP=$(STDLIB)\wisp.lib
LIBEDE=$(STDLIB)\ede.lib

RTS   = wruncbl.exe
RTSE  = wruncble.exe

all:    $(RTS)
	!ECHO $(RTS) is now up to date

both:   $(RTS) $(RTSE)
	!ECHO $(RTS) and $(RTSE) are now up to date

clean:
	del $(RTS)
	del $(RTSE)
	del sub.obj
	del filetbl.obj

$(RTS): sub.obj filetbl.obj $(LIBWISP) $(LIBVIDEO) 
	echo debug all	>make.rsp
	echo option CASEEXACT	>>make.rsp
	echo file sub.obj	>>make.rsp
	echo file filetbl.obj	>>make.rsp
	echo library $(LIBWISP)	>>make.rsp 
	echo library $(LIBVIDEO)	>>make.rsp
	echo file $(ACULIB)	>>make.rsp
	set wcl386=$(CFLAGS0)
	$(CC) /fe=$@ @make.rsp /k128k
	$(BIND4GW) $@

$(RTSE): sub.obj filetbl.obj $(LIBWISP) $(LIBVIDEO) $(LIBEDE)
	echo debug all	>make.rsp
	echo option CASEEXACT	>>make.rsp
	echo file sub.obj	>>make.rsp
	echo file filetbl.obj	>>make.rsp
	echo library $(LIBEDE)	>>make.rsp
	echo library $(LIBWISP)	>>make.rsp 
	echo library $(LIBVIDEO)	>>make.rsp
	echo file $(ACULIB)	>>make.rsp
	set wcl386=$(CFLAGS0)
	$(CC) /fe=$@ @make.rsp /k128k
	$(BIND4GW) $@

sub.obj: sub.c sub85.c 
	set wcl386=$(CFLAGS0)
	$(CC) /i=$(ACUDIR) /d2 sub.c /c
	
sub.c: $(ACUDIR)\sub.c        
	copy $(ACUDIR)\$@
	
filetbl.obj: filetbl.c
	set wcl386=$(CFLAGS0)
	$(CC) /i=$(ACUDIR) /d2 filetbl.c /c

filetbl.c: $(ACUDIR)\filetbl.c        
	copy $(ACUDIR)\$@
