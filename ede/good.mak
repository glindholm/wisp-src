#/************************************************************************/
#/*									*/
#/*		 Copyright (c) 1988, 1989, 1990, 1991, 1992, 1993	*/
#/*	 An unpublished work of International Digital Scientific Inc.	*/
#/*			    All rights reserved.			*/
#/*									*/
#/************************************************************************/
#
#
#	File:		good.mak
#
#	Function:	Makefile for the GOOD utility on MSDOS using
#			Intel C CodeBuilder.
#
# 	History:	
#	06/04/92	Changed to use make.include GSL
#	06/09/92	Removed the copy. GSL
#	01/28/93	Modified for MSDOS. GSL
#

CDEBUG =  /g

GOOD = good.exe

!include $(WISP)\src\port\dosmake.mak


all: 	$(GOOD)

OBJS=good.obj
LIBS=$(STDLIB)\video.lib $(STDLIB)\ede.lib

$(GOOD): $(OBJS) $(LIBS)
	echo $(CFLAGS) >good.lrf
	echo $(OBJS) $(LIBS) graphics.lib>>good.lrf 
	$(CC) /e $@ /s32000 /n @good.lrf




