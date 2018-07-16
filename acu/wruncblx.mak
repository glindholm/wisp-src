#/************************************************************************/
#/*									 */
#/*		 Copyright (c) 1988, 1989, 1990, 1991, 1992		 */
#/*	 An unpublished work of International Digital Scientific Inc.	 */
#/*			    All rights reserved.			 */
#/*									 */
#/************************************************************************/
#
#
#	File:		wruncblx.mak
#
#			**** This is an IDSI only version ****
#
#	Function:	Makefile for building the Acucobol runtime
#			using Intel C Code Builder.
#
#	Description:	This makefile can generate the following versions
#			of the Acucobol runtime system.
#
#			wruncbl		The standard runtime for Acucobol
#					version 2 and up.
#
#			wruncble	The EDE version of the runtime.
#
#			wruncbl1	The Acucobol version 1.5 runtime.
#					(Only used if running an old version
#					of Acucobol.)
#
#	Instructions:	To create an Acucobol runtime system follow the
#			instructions in Appendix A of the WISP Manual
#			"Building an ACUCOBOL runtime system".
#
#			To build wruncbl:
#				$ make -f wruncbl.umf
#
#			To build wruncble:
#				$ make -f wruncbl.umf wruncble
#
#
#	LIBPATH:	If your libraries reside in a directory other then
#			/usr/lib then change LIBPATH to there correct
#			location.
#

#
#	Leave CDEBUG blank - it will be filled in manually on the build
#
CDEBUG=

!include $(WISP)\src\port\dosmake.mak

SUBS2=sub.obj filetbl.obj

LIBPATH=$(STDLIB)
LIBPATHS=$(LIBPATH)\wisp.lib $(LIBPATH)\video.lib
LIBPATHSE=$(LIBPATH)\ede.lib $(LIBPATH)\wisp.lib $(LIBPATH)\video.lib
LIBPATHDS=$(LIBPATH)\wispd.lib $(LIBPATH)\videod.lib

LINKFLAGS = $(CFLAGS) /s65535 run386.lib graphics.lib

all:	wruncbl.exe

both:	wruncbl.exe wruncble.exe

wruncbl.exe: $(SUBS2) $(LIBPATHS)
	echo $(SUBS2)		>make.rsp
	echo $(LIBPATHS)	>>make.rsp
	echo $(LINKFLAGS)	>>make.rsp
	$(CC) /e $@ @make.rsp
	copy $@ $(WISP)\SRC\ACU

wruncble.exe: $(SUBS2) $(LIBPATHSE)
	echo $(SUBS2)		>make.rsp
	echo $(LIBPATHSE)	>>make.rsp
	echo $(LINKFLAGS)	>>make.rsp
	$(CC) /e $@ /n @make.rsp
	copy $@ $(WISP)\SRC\ACU

wruncbld.exe: $(SUBS2) $(LIBPATHDS)
	echo $(SUBS2)		>make.rsp
	echo $(LIBPATHDS)	>>make.rsp
	echo $(LINKFLAGS)	>>make.rsp
	$(CC) /g /e $@ @make.rsp

sub.obj: sub.c sub85.c

