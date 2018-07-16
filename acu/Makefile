#/************************************************************************/
#/*									 */
#/*		 Copyright (c) 1988, 1989, 1990, 1991, 1992		 */
#/*	 An unpublished work of International Digital Scientific Inc.	 */
#/*			    All rights reserved.			 */
#/*									 */
#/************************************************************************/
#
#
#	File:		wruncbl.umf
#
#	Function:	The UNIX makefile for building the Acucobol runtime
#			systems that include the WISP runtime routines.
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

CFLAGS= -O
LDFLAGS= -s

SUBS1=sub.o
SUBS2=sub.o filetbl.o

LIBPATH=/usr/lib
LIBPATHS=$(LIBPATH)/libwisp.a $(LIBPATH)/libilp.a $(LIBPATH)/libvideo.a
LIBS=-lwisp -lilp -lvideo -lm

all:	wruncbl

wruncbl: $(SUBS2) $(LIBPATHS)
	$(CC) $(CFLAGS) -o $@ $(SUBS2) runcbl.a -L$(LIBPATH) $(LIBS)

wruncble: $(SUBS2) $(LIBPATHS) $(LIBPATH)/libede.a
	$(CC) $(CFLAGS) -o $@ $(SUBS2) runcbl.a -L$(LIBPATH) -lede $(LIBS)

wruncbl1: $(SUBS1) $(LIBPATHS)
	$(CC) $(CFLAGS) -o $@ $(SUBS1) runcbl.a -L$(LIBPATH) $(LIBS)

sub.o: sub.c sub85.c


