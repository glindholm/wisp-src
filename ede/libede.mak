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
#	File:		libede.mak
#
#	Function:	Makefile for the EDE library on MSDOS with
#			Intel C CodeBuilder
#
# 	History:
#	06/04/92	Changed to use make.include GSL
#	01/28/93	Modified for MSDOS. GSL
#

CDEBUG= 

LIB = EDE.LIB

!include $(WISP)\src\port\dosmake.mak

.PRECIOUS:	$(LIB) 

all: $(STDLIB)\$(LIB)

$(STDLIB)\$(LIB): $(LIB)
	copy $(LIB) $@


OBJS=	edehelp.obj \
	edehli.obj \
	edenetc.obj \
	edeoldoc.obj \
	genvec.obj

LIB_FILE = $(LIB)
LIB_OBJS = $(OBJS)
!include $(CBPATH)\BIN\LIB.MAK

$(LIB): $(LIB_FILE)

edehelp.obj: edehelp.c $(V)\video.h $(V)\vlocal.h $(V)\vdata.h
edehelp.obj: $(V)\vmenu.h vwang.h wglobals.h wfiles.h
edehli.obj: edehli.c $(V)\video.h $(V)\vmenu.h $(V)\vlocal.h
edehli.obj: $(V)\vdata.h wglobals.h wfiles.h
edenetc.obj: edenetc.c $(V)\video.h $(V)\vlocal.h $(V)\vdata.h
edenetc.obj: $(V)\vmenu.h vwang.h wglobals.h wfiles.h
genvec.obj: genvec.c
edeoldoc.obj: edeoldoc.c



