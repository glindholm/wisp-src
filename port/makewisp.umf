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
#	File:		makewisp.umf
#
#	Function:	Master makefile for building all of WISP on a unix platform.
#
#	Description:	This routine will start all the make commands used to 
#			do the complete WISP port.
#
#				$ make -f makewisp.umf
#
# 	History:	06/08/92	Written by GSL
#			06/09/92	Change to use Makefile instead of .umf. GSL
#			06/12/92	Added mfsubs. GSL
#			07/07/92	Added wisplib as a dependency of printq. GSL
#

include $(WISP)/src/port/make.include

all:		ede printq proctran videolib videotest wisplib wisptran wisputils mfsubs
		@echo ALL is up-to-date.

ede:		edelib good
		@echo EDE is up-to-date.

edelib:	
		(cd $(WISP)/src/ede; 		$(MAKE))

good:		videolib edelib
		(cd $(WISP)/src/ede; 		$(MAKE) -f good.umf)

printq:		videolib wisplib
		(cd $(WISP)/src/printq;		$(MAKE))
		@echo PRINTQ is up-to-date.

ilplib:		printq

proctran:
		(cd $(WISP)/src/proctran;	$(MAKE))
		@echo PROCTRAN is up-to-date.

videolib:	
		(cd $(WISP)/src/videolib;	$(MAKE))
		@echo VIDEOLIB is up-to-date.

videotest:	videolib
		(cd $(WISP)/src/videotest;	$(MAKE))
		@echo VIDEOTEST is up-to-date.

wisplib:
		(cd $(WISP)/src/wisplib;	$(MAKE))
		@echo WISPLIB is up-to-date.

wisptran:
		(cd $(WISP)/src/wisptran;	$(MAKE))
		@echo WISPTRAN is up-to-date.

wisputils:	wisplib videolib ilplib
		(cd $(WISP)/src/wisputils;	$(MAKE))
		@echo WISPUTILS is up-to-date.

mfsubs:		
		(cd $(WISP)/src/mf;		$(MAKE) subsonly)
		@echo MFSUBS is up-to-date.



