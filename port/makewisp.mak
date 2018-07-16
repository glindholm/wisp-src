#/************************************************************************/
#/*									*/
#/*	        WISP - Wang Interchange Source Pre-processor		*/
#/*		 Copyright (c) 1988, 1989, 1990, 1991, 1992		*/
#/*	 An unpublished work of International Digital Scientific Inc.	*/
#/*			    All rights reserved.			*/
#/*									*/
#/************************************************************************/
#
#
#	File:		makewisp.mak
#
#	Function:	Master makefile for building all of WISP on MSDOS
#			with Code Builder.
#
#	Description:	This routine will start all the make commands used to 
#			do the complete WISP port.
#
#				$ make -f makewisp.mak
#
# 	History:	06/08/92	Written by GSL
#			06/09/92	Change to use Makefile instead of .umf. GSL
#			06/12/92	Added mfsubs. GSL
#			07/07/92	Added wisplib as a dependency of printq. GSL
#			03/23/93	Modified for MSDOS. GSL

!include $(WISP)\src\port\dosmake.mak

all:		ede proctran videolib videotest wisplib wisptran wisputils vsedit 
		@echo ALL is up-to-date.

ede:		edelib good
		@echo EDE is up-to-date.

edelib:	
		(cd $(WISP)\src\ede; 		$(MAKE) -f libede.mak)

good:		videolib edelib
		(cd $(WISP)\src\ede; 		$(MAKE) -f good.mak)

proctran:
		(cd $(WISP)\src\proctran;	$(MAKE))
		@echo PROCTRAN is up-to-date.

vsedit:
		(cd $(WISP)\src\vsedit;		$(MAKE))
		@echo VSEDIT is up-to-date.

videolib:	
		(cd $(WISP)\src\videolib;	$(MAKE))
		@echo VIDEOLIB is up-to-date.

videotest:	videolib
		(cd $(WISP)\src\videotest;	$(MAKE))
		@echo VIDEOTEST is up-to-date.

wisplib:
		(cd $(WISP)\src\wisplib;	$(MAKE))
		@echo WISPLIB is up-to-date.

wisptran:
		(cd $(WISP)\src\wisptran;	$(MAKE))
		@echo WISPTRAN is up-to-date.

wisputils:	wisplib videolib 
		(cd $(WISP)\src\wisputils;	$(MAKE))
		@echo WISPUTILS is up-to-date.




