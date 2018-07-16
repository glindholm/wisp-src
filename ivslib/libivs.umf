#	/************************************************************************/
#	/*									*/
#	/*		 Copyright (c) 1988, 1989, 1990, 1991, 1992		*/
#	/*	 An unpublished work of International Digital Scientific Inc.	*/
#	/*			    All rights reserved.			*/
#	/*									*/
#	/************************************************************************/
#
#
#	File:		libivs.umf
#
#	Function:	Generic makefile for the IVS library on all unix platforms.
#
# 	History:	11/14/92
#

#CDEBUG = -g
CDEBUG = -O

#LIB = libivsd.a
LIB = libivs.a

include $(WISP)/src/port/make.include

.PRECIOUS:	$(LIB) 

all: $(STDLIB)/$(LIB)

$(STDLIB)/$(LIB): $(LIB)
	cp $(LIB) $(STDLIB)/$(LIB)

$(LIB):	$(LIB)(vchinese.o)
	$(RANLIB) $(LIB)
	@echo lib $(LIB) is now up-to-date

$(LIB)(vchinese.o): $(STDINC)/v/vchinese.h
