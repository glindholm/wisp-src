#/************************************************************************/
#/*									*/
#/*	        WISP - Wang Interchange Source Pre-processor		*/
#/*		 Copyright (c) 1988, 1989, 1990, 1991, 1992, 1994	*/
#/*	 An unpublished work of International Digital Scientific Inc.	*/
#/*			    All rights reserved.			*/
#/*									*/
#/************************************************************************/
#
#
#	File:		makewisp.wat
#
#	Function:	Master makefile for building all of WISP on MSDOS
#			using Watcom
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
#			04/14/94	Modified for Watcom, CBA
#			04/20/94	CBA, modified file so that either
#					wmake or nmake can use it

MAKE=NMAKE
!include $(WISP)\src\port\dosmake.wat

all:		video wisp ede
		@echo ALL is up-to-date.

video:		videolib videotest
		@echo VIDEO is up-to-date.

wisp:		video wisplib wisptran wisputils vsedit proctran acu
		@echo WISP is up-to-date.

ede:		edelib good
		@echo EDE is up-to-date.

videolib:	
		cd $(WISP)\src\videolib
		$(MAKE) /f libvideo.wat
		@echo VIDEOLIB is up-to-date.

videotest:	videolib
		cd $(WISP)\src\videotest
		$(MAKE) /f vtest.wat
		@echo VIDEOTEST is up-to-date.

wisplib:
		cd $(WISP)\src\wisplib
		$(MAKE) /f libwisp.wat
		@echo WISPLIB is up-to-date.

wisptran:
		cd $(WISP)\src\wisptran
		$(MAKE) /f wisp.wat
		@echo WISPTRAN is up-to-date.

wisputils:	wisplib videolib 
		cd $(WISP)\src\wisputils
		$(MAKE) /f utils.wat
		@echo WISPUTILS is up-to-date.

proctran:
		cd $(WISP)\src\proctran
		$(MAKE) /f proctran.wat
		@echo PROCTRAN is up-to-date.

vsedit:		wisplib videolib
		cd $(WISP)\src\vsedit
		$(MAKE) /f vsedit.wat
		@echo VSEDIT is up-to-date.

acu:		wisplib ede
		cd $(WISP)\src\acu
		$(MAKE) /f wruncblx.wat both
		@echo ACU is up-to-date.

edelib:	
		cd $(WISP)\src\ede
 		$(MAKE) /f libede.wat
		@echo EDELIB is up-to-date.

good:		videolib edelib
		cd $(WISP)\src\ede 
		$(MAKE) /f good.wat
		@echo GOOD is up-to-date.

clean:
		cd $(WISP)\src\videolib
		$(MAKE) /f libvideo.wat clean
		cd $(WISP)\src\videotest
		$(MAKE) /f vtest.wat clean
		cd $(WISP)\src\wisplib
		$(MAKE) /f libwisp.wat clean
		cd $(WISP)\src\wisptran
		$(MAKE) /f wisp.wat clean
		cd $(WISP)\src\wisputils
		$(MAKE) /f utils.wat clean
		cd $(WISP)\src\proctran
		$(MAKE) /f proctran.wat clean
		cd $(WISP)\src\vsedit
		$(MAKE) /f vsedit.wat clean
		cd $(WISP)\src\acu
		$(MAKE) /f wruncblx.wat clean
		cd $(WISP)\src\ede
 		$(MAKE) /f libede.wat clean
		cd $(WISP)\src\ede 
		$(MAKE) /f good.wat clean
		del $(WISP)\src\lib\*.lib
		@echo ALL is CLEAN
