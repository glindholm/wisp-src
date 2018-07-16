#/************************************************************************/
#/*									*/
#/*	        WISP - Wang Interchange Source Pre-processor		*/
#/*		 Copyright (c) 1988, 1989, 1990, 1991, 1992, 1997	*/
#/*	 	An unpublished work of NeoMedia Technologies Inc.	*/
#/*			    All rights reserved.			*/
#/*									*/
#/************************************************************************/
#
#
#	File:		proctran.mak
#
#	Function:	Makefile for PROCTRAN on MSDOS with Intel Code Builder.
#
# 	History:	
#			06/04/92	Changed to use make.include GSL
#			06/09/92	Removed copy. GSL
#			01/06/93	Modified for MSDOS. GSL
#

PROCTRAN = proctran.exe

CDEBUG = 

!include $(WISP)\src\port\dosmake.mak

all: 	$(STDBIN)\$(PROCTRAN)

$(STDBIN)\$(PROCTRAN): $(PROCTRAN) $(STDBIN)
	copy $(PROCTRAN) $@

$(STDBIN):
	mkdir $(STDBIN)

OBJS=	ptcheck.obj \
	ptcli.obj \
	ptdecl.obj \
	ptdoit.obj \
	ptequatn.obj \
	ptextrct.obj \
	ptif.obj \
	ptinit.obj \
	ptmain.obj \
	ptusing.obj \
	ptpdiv.obj \
	ptrens.obj \
	ptscreen.obj \
	ptscrdef.obj \
	pttype.obj \
	ptutils.obj \
	ptwrite.obj \
	getopt.obj

LIB_FILE = proctran.lib
LIB_OBJS = $(OBJS)
!include $(CBPATH)\bin\lib.mak

$(PROCTRAN): $(LIB_FILE)
	$(CC) $(CDEBUG) /e $@ /s64000 $(LIB_FILE)

ptcheck.obj: ptcheck.c pgcommon.h pgglobal.h pgcblsrc.h
ptcheck.obj: pgstruct.h
ptcli.obj: ptcli.c pgcommon.h pgglobal.h
ptdecl.obj: ptdecl.c pgcommon.h pgglobal.h pgstruct.h
ptdoit.obj: ptdoit.c pgcommon.h pgglobal.h pgstruct.h
ptdoit.obj: pgcblsrc.h pgkeyw.h pgeqtns.h
ptequatn.obj: ptequatn.c pgcommon.h pgglobal.h pgstruct.h
ptequatn.obj: pgeqtns.h pgdefeqn.h
ptextrct.obj: ptextrct.c pgcommon.h pgglobal.h pgstruct.h
ptextrct.obj: pgextrct.h
ptif.obj: ptif.c pgcommon.h pgstruct.h pgglobal.h
ptinit.obj: ptinit.c pgcommon.h pgstruct.h pgglobal.h
ptmain.obj: ptmain.c pgcommon.h pgstruct.h pgglobal.h
ptmain.obj: pgcblsrc.h pgkeyw.h pgeqtns.h pgextrct.h
ptusing.obj: ptusing.c pgcommon.h pgstruct.h pgglobal.h
ptusing.obj: pgeqtns.h
ptpdiv.obj: ptpdiv.c pgcommon.h pgstruct.h pgglobal.h
ptpdiv.obj: pgcblsrc.h pgkeyw.h
ptrens.obj: ptrens.c pgcommon.h pgglobal.h pgstruct.h
ptscreen.obj: ptscreen.c pgcommon.h pgstruct.h pgglobal.h
ptscreen.obj: pgkeyw.h
ptscrdef.obj: ptscrdef.c pgcommon.h pgglobal.h pgstruct.h
ptscrdef.obj: pgcblsrc.h
pttype.obj: pttype.c pgcommon.h pgglobal.h pgstruct.h
ptutils.obj: ptutils.c pgcommon.h pgglobal.h pgstruct.h
ptutils.obj: pgkeyw.h pgcblsrc.h
ptwrite.obj: ptwrite.c pgcommon.h pgstruct.h pgglobal.h
ptwrite.obj: pgcblsrc.h pgextrct.h


