#
#	Copyright (c) 1995 DevTech Migrations, All rights reserved.
#	$Id:$
#
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
CDEBUG = 

L_VIDEO = video

include $(WISP)/src/port/make.include

CFLAGS  = $(CFLAGS0) -I$(VIDEO_INC_DIR)

IVS_LIB = libivs.a

.PRECIOUS:	$(IVS_LIB) 

all: $(STD_LIB_DIR)/$(IVS_LIB)

clean:
	rm -f $(IVS_LIB) *.o core *~

$(STD_LIB_DIR)/$(IVS_LIB): $(IVS_LIB)
	cp $(IVS_LIB) $(STD_LIB_DIR)/$(IVS_LIB)

$(IVS_LIB):	$(IVS_LIB)(vchinese.o)
	$(RANLIB) $(IVS_LIB)
	@echo lib $(IVS_LIB) is now up-to-date

$(IVS_LIB)(vchinese.o): $(VIDEO_INC_DIR)/vchinese.h
