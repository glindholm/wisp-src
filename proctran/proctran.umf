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
#	File:		proctran.umf
#
#	Function:	Generic makefile for PROCTRAN on all unix platforms.
#
# 	History:	
#			06/04/92	Changed to use make.include GSL
#			06/09/92	Removed copy. GSL
#			01/06/93	Changed names for MSDOS. GSL
#

#PROCTRAN = proctrand
PROCTRAN = proctran

#LIB = proctrand.a
LIB = proctran.a

#CDEBUG = -g
CDEBUG = -O

include $(WISP)/src/port/make.include

all: 	$(PROCTRAN)

$(PROCTRAN): $(LIB)
	$(CC) $(CFLAGS) -o $@ $(LIB)


$(LIB):	$(LIB)(ptcheck.o) \
	$(LIB)(ptcli.o) \
	$(LIB)(ptdecl.o) \
	$(LIB)(ptdoit.o) \
	$(LIB)(ptequatn.o) \
	$(LIB)(ptextrct.o) \
	$(LIB)(ptif.o) \
	$(LIB)(ptinit.o) \
	$(LIB)(ptmain.o) \
	$(LIB)(ptusing.o) \
	$(LIB)(ptpdiv.o) \
	$(LIB)(ptrens.o) \
	$(LIB)(ptscreen.o) \
	$(LIB)(ptscrdef.o) \
	$(LIB)(pttype.o) \
	$(LIB)(ptutils.o) \
	$(LIB)(ptwrite.o)
	$(RANLIB) $(LIB)
	@echo lib $(LIB) is now up-to-date


$(LIB)(ptcheck.o): ptcheck.c pgcommon.h pgglobal.h pgcblsrc.h
$(LIB)(ptcheck.o): pgstruct.h
$(LIB)(ptcli.o): ptcli.c pgcommon.h pgglobal.h
$(LIB)(ptdecl.o): ptdecl.c pgcommon.h pgglobal.h pgstruct.h
$(LIB)(ptdoit.o): ptdoit.c pgcommon.h pgglobal.h pgstruct.h
$(LIB)(ptdoit.o): pgcblsrc.h pgkeyw.h pgeqtns.h
$(LIB)(ptequatn.o): ptequatn.c pgcommon.h pgglobal.h pgstruct.h
$(LIB)(ptequatn.o): pgeqtns.h pgdefeqn.h
$(LIB)(ptextrct.o): ptextrct.c pgcommon.h pgglobal.h pgstruct.h
$(LIB)(ptextrct.o): pgextrct.h
$(LIB)(ptif.o): ptif.c pgcommon.h pgstruct.h pgglobal.h
$(LIB)(ptinit.o): ptinit.c pgcommon.h pgstruct.h pgglobal.h
$(LIB)(ptmain.o): ptmain.c pgcommon.h pgstruct.h pgglobal.h
$(LIB)(ptmain.o): pgcblsrc.h pgkeyw.h pgeqtns.h pgextrct.h
$(LIB)(ptusing.o): ptusing.c pgcommon.h pgstruct.h pgglobal.h
$(LIB)(ptusing.o): pgeqtns.h
$(LIB)(ptpdiv.o): ptpdiv.c pgcommon.h pgstruct.h pgglobal.h
$(LIB)(ptpdiv.o): pgcblsrc.h pgkeyw.h
$(LIB)(ptrens.o): ptrens.c pgcommon.h pgglobal.h pgstruct.h
$(LIB)(ptscreen.o): ptscreen.c pgcommon.h pgstruct.h pgglobal.h
$(LIB)(ptscreen.o): pgkeyw.h
$(LIB)(ptscrdef.o): ptscrdef.c pgcommon.h pgglobal.h pgstruct.h
$(LIB)(ptscrdef.o): pgcblsrc.h
$(LIB)(pttype.o): pttype.c pgcommon.h pgglobal.h pgstruct.h
$(LIB)(ptutils.o): ptutils.c pgcommon.h pgglobal.h pgstruct.h
$(LIB)(ptutils.o): pgkeyw.h pgcblsrc.h
$(LIB)(ptwrite.o): ptwrite.c pgcommon.h pgstruct.h pgglobal.h
$(LIB)(ptwrite.o): pgcblsrc.h pgextrct.h

