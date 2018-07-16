#/************************************************************************/
#/*									 */
#/*		 Copyright (c) 1988, 1989, 1990, 1991, 1992,1993	 */
#/*	 An unpublished work of International Digital Scientific Inc.	 */
#/*			    All rights reserved.			 */
#/*									 */
#/************************************************************************/
#
#
#	File:		wruncblx.umf
#
#			**** This is an IDSI only version ****
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

include $(WISP)/src/port/make.include

SUBS1=sub.o
SUBS2=sub.o filetbl.o

LIBPATH=$(STDLIB)
LIBPATHS=$(LIBPATH)/libwisp.a $(LIBPATH)/libvideo.a
LIBPATHDS=$(LIBPATH)/libwispd.a $(LIBPATH)/libvideod.a
LIBS=-lwisp -lvideo -lm $(LIBCURSES)
LIBDS=-lwispd -lvideod -lm $(LIBCURSES)

all:	setup wruncbl
	@echo ">>> wruncbl  is up to date <<<"

both:	setup wruncbl wruncble
	@echo ">>> wruncbl  is up to date <<<"
	@echo ">>> wruncble is up to date <<<"

setup:
	@echo ">>> BUILDING ACUCOBOL RUNTIME <<<"
	@echo "WISP   = " $(WISP)
	@echo "ACUDIR = " $(ACUDIR)

wruncbl: $(SUBS2) $(LIBPATHS)
	$(CC) -O $(CFLAGS) -o $@ $(SUBS2) $(ACUDIR)/runcbl.a -L$(LIBPATH) $(LIBS)

wruncble: $(SUBS2) $(LIBPATHS) $(LIBPATH)/libede.a
	$(CC) -O $(CFLAGS) -o $@ $(SUBS2) $(ACUDIR)/runcbl.a -L$(LIBPATH) -lede $(LIBS)

wruncbld: $(SUBS2) $(LIBPATHDS)
	$(CC) -g $(CFLAGS) -o $@ $(SUBS2) $(ACUDIR)/runcbl.a -L$(LIBPATH) $(LIBDS)

sub.o: sub.c sub85.c
	$(CC) -g $(CFLAGS) -c -I$(ACUDIR) sub.c

filetbl.o: filetbl.c
	$(CC) $(CFLAGS) -c -I$(ACUDIR) filetbl.c

sub.c: $(ACUDIR)/sub.c
	cp $(ACUDIR)/sub.c .

filetbl.c: $(ACUDIR)/filetbl.c
	cp $(ACUDIR)/filetbl.c .


