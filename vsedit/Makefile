#	/************************************************************************/
#	/*									*/
#	/*	        WISP - Wang Interchange Source Pre-processor		*/
#	/*		 Copyright (c) 1988, 1989, 1990, 1991, 1992		*/
#	/*	 An unpublished work of International Digital Scientific Inc.	*/
#	/*			    All rights reserved.			*/
#	/*									*/
#	/************************************************************************/
#
#
#	File:		vsedit.umf
#
#	Function:	Generic makefile for VSEDIT on all unix platforms.
#
# 	History:	
#			10/02/92        written by JEC
#                       09/14/93        refs to libilp removed
#                                       and $(LIBCURSES) added
#
VSEDIT = vsedit
CFLAGS = -O

include $(WISP)/src/port/make.include

ALL: $(VSEDIT)

LIBS= $(LIBCFLAGS) -lwisp -lvideo $(LIBCURSES)
LIBPATHS= $(STDLIB)/libwisp.a $(STDLIB)/libvideo.a 

OBJECTS=vsemain.o vseglb.o vsegp.o vseinp.o vseutl.o vsedit.o \
	vsestmnu.o vsespmnu.o vsescr.o vsenaf.o vsetxt.o \
	vsedscr.o vsedmod.o vsedins.o vsedel.o  \
	vsedfnd.o vsemov.o vsegps.o vsebasic.o


$(VSEDIT): $(OBJECTS) $(LIBPATHS)
	$(CC) $(CFLAGS) -o $(VSEDIT) $(OBJECTS) $(LIBS) 

vsedel.o: vsedel.c vseglb.h vsegp.h vsescr.h vsedscr.h
vsedel.o: vseglb.h vsescr.h
vsedfnd.o: vsedfnd.c vseglb.h vsegp.h vsescr.h
vsedfnd.o: vsedscr.h vseglb.h vsescr.h
vsedins.o: vsedins.c vseglb.h vsegp.h vsescr.h
vsedins.o: vsedscr.h vseglb.h vsescr.h
vsedit.o: vsedit.c vseglb.h vsegp.h
vsedmod.o: vsedmod.c vseglb.h vsegp.h vsescr.h
vsedmod.o: vsedscr.h vseglb.h vsescr.h
vsedscr.o: vsedscr.c vseglb.h vsegp.h vsescr.h
vsedscr.o: vsedscr.h vseglb.h vsescr.h
vseglb.o: vseglb.c vseglb.h vsegp.h
vsegp.o: vsegp.c vseglb.h vsegp.h
vseinp.o: vseinp.c vseglb.h vsegp.h vsegp.h
vsemain.o: vsemain.c vseglb.h vsegp.h
vsemov.o: vsemov.c vseglb.h vsegp.h vsescr.h vsedscr.h
vsemov.o: vseglb.h vsescr.h
vsenaf.o: vsenaf.c vseglb.h vsegp.h vsescr.h
vsegps.o: vsegps.c vseglb.h vsegp.h vsegp.h
vsescr.o: vsescr.c vseglb.h vsegp.h vsescr.h
vsespmnu.o: vsespmnu.c vseglb.h vsegp.h vsescr.h
vsestmnu.o: vsestmnu.c vseglb.h vsegp.h vsescr.h
vsetxt.o: vsetxt.c vseglb.h vsegp.h
vseutl.o: vseutl.c
vsebasic.o: vsebasic.c vseglb.h

