#/************************************************************************/
#/*									*/
#/*	        WISP - Wang Interchange Source Pre-processor		*/
#/*		 Copyright (c) 1988, 1989, 1990, 1991, 1992, 1993	*/
#/*	 An unpublished work of International Digital Scientific Inc.	*/
#/*			    All rights reserved.			*/
#/*									*/
#/************************************************************************/
#
#
#	File:		vsedit.mak
#
#	Function:	Makefile for VSEDIT on MSDOS with Intel CodeBuilder
#
# 	History:	
#			10/02/92        written by JEC
#			01/08/93	Modified for MSDOS
#
PROGRAM = vsedit.exe
CDEBUG = 

!include $(WISP)\src\port\dosmake.mak

all: $(STDBIN)\$(PROGRAM)

$(STDBIN)\$(PROGRAM):	$(PROGRAM) $(STDBIN)
	copy $(PROGRAM) $@

$(STDBIN):
	mkdir $(STDBIN)

WLIBS= $(STDLIB)\WISP.LIB $(STDLIB)\VIDEO.LIB

OBJS=vsemain.obj vseglb.obj vsegp.obj vseinp.obj vseutl.obj vsedit.obj \
	vsestmnu.obj vsespmnu.obj vsescr.obj vsenaf.obj vsetxt.obj \
	vsedscr.obj vsedmod.obj vsedins.obj vsedel.obj  \
	vsedfnd.obj vsemov.obj vsegps.obj \
	$(WLIBS)

LINKFLAGS = $(CDEBUG) /e $(PROGRAM) /s64000  graphics.lib

!include $(CBPATH)\bin\link.mak


vsedel.obj: vsedel.c vseglb.h vsegp.h vsescr.h vsedscr.h
vsedel.obj: vseglb.h vsescr.h
vsedfnd.obj: vsedfnd.c vseglb.h vsegp.h vsescr.h
vsedfnd.obj: vsedscr.h vseglb.h vsescr.h
vsedins.obj: vsedins.c vseglb.h vsegp.h vsescr.h
vsedins.obj: vsedscr.h vseglb.h vsescr.h
vsedit.obj: vsedit.c vseglb.h vsegp.h
vsedmod.obj: vsedmod.c vseglb.h vsegp.h vsescr.h
vsedmod.obj: vsedscr.h vseglb.h vsescr.h
vsedscr.obj: vsedscr.c vseglb.h vsegp.h vsescr.h
vsedscr.obj: vsedscr.h vseglb.h vsescr.h
vseglb.obj: vseglb.c vseglb.h vsegp.h
vsegp.obj: vsegp.c vseglb.h vsegp.h
vseinp.obj: vseinp.c vseglb.h vsegp.h vsegp.h
vsemain.obj: vsemain.c vseglb.h vsegp.h
vsemov.obj: vsemov.c vseglb.h vsegp.h vsescr.h vsedscr.h
vsemov.obj: vseglb.h vsescr.h
vsenaf.obj: vsenaf.c vseglb.h vsegp.h vsescr.h
vsegps.obj: vsegps.c vseglb.h vsegp.h vsegp.h
vsescr.obj: vsescr.c vseglb.h vsegp.h vsescr.h
vsespmnu.obj: vsespmnu.c vseglb.h vsegp.h vsescr.h
vsestmnu.obj: vsestmnu.c vseglb.h vsegp.h vsescr.h
vsetxt.obj: vsetxt.c vseglb.h vsegp.h
vseutl.obj: vseutl.c

