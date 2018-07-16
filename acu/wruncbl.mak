#/************************************************************************/
#/*									 */
#/*		 Copyright (c) 1988, 1989, 1990, 1991, 1992		 */
#/*	 An unpublished work of International Digital Scientific Inc.	 */
#/*			    All rights reserved.			 */
#/*									 */
#/************************************************************************/
#
#
#	File:		wruncbl.mak
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
#
#	Instructions:	To create an Acucobol runtime system follow the
#			instructions in Appendix A of the WISP Manual
#			"Building an ACUCOBOL runtime system".
#
#			To build wruncbl:
#				$ make -f wruncbl.mak
#
#			To build wruncble:
#				$ make -f wruncbl.mak wruncble
#
#
#	LIBPATH:	If your libraries reside in a directory other then
#			C:\LIB then change LIBPATH to there correct
#			location.
#

#
#
CFLAGS=-DMSDOS -DDACU /znoalign

SUBS2=sub.obj filetbl.obj

LIBPATH=C:\LIB

LIBPATHS=$(LIBPATH)\wisp.lib $(LIBPATH)\video.lib
LIBPATHSE=$(LIBPATH)\ede.lib $(LIBPATH)\wisp.lib $(LIBPATH)\video.lib

LINKFLAGS = /s65535 run386.lib graphics.lib

all:	wruncbl.exe

both:	wruncbl.exe wruncble.exe

wruncbl.exe: $(SUBS2) $(LIBPATHS)
	echo $(SUBS2)		>make.lrf
	echo $(LIBPATHS)	>>make.lrf
	echo $(LINKFLAGS)	>>make.lrf
	$(CC) /e $@ @make.lrf

wruncble.exe: $(SUBS2) $(LIBPATHSE)
	echo $(SUBS2)		>make.lrf
	echo $(LIBPATHSE)	>>make.lrf
	echo $(LINKFLAGS)	>>make.lrf
	$(CC) /e $@ @make.lrf

sub.obj: sub.c sub85.c

