#/************************************************************************/
#/*									*/
#/*		 Copyright (c) 1988, 1989, 1990, 1991, 1992, 1993	*/
#/*	 An unpublished work of International Digital Scientific Inc.	*/
#/*			    All rights reserved.			*/
#/*									*/
#/************************************************************************/
#
#
#	File:		good.wat
#
#	Function:	Makefile for the GOOD utility on MSDOS using
#			Watcom
#
# 	History:	
#	06/04/92	Changed to use make.include GSL
#	06/09/92	Removed the copy. GSL
#	01/28/93	Modified for MSDOS. GSL
#	04/14/94	Modified for Watcom, CBA
#	04/20/94	CBA, modified file so that either
#			wmake or nmake can use it
#

#CDEBUG =/od
CDEBUG = /d2

GOOD = good.exe

!include $(WISP)\src\port\dosmake.wat

LIBVIDEO=$(STDLIB)\video.lib
LIBEDE=$(STDLIB)\ede.lib

all: 	$(GOOD)

clean: 
	del $(GOOD)

$(GOOD): good.c $(LIBVIDEO) $(LIBEDE)
	echo debug all		>make.rsp
	echo option CASEEXACT	>>make.rsp
	echo library $(LIBEDE)	>>make.rsp
	echo library $(LIBVIDEO)	>>make.rsp
	set wcl386=$(CFLAGS0)
	$(CC) /fe=$@ good.c @make.rsp 
	$(BIND4GW) $@

