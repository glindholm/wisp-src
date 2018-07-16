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
#	File:		proctran.umf
#
#	Function:	Generic makefile for PROCTRAN on all unix platforms.
#
# 	History:	
#			06/04/92	Changed to use make.include GSL
#			06/09/92	Removed copy. GSL
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
	cc $(CFLAGS) -o $@ $(LIB)


$(LIB):	$(LIB)(pg_check.o) \
	$(LIB)(pg_cli.o) \
	$(LIB)(pg_declare.o) \
	$(LIB)(pg_doit.o) \
	$(LIB)(pg_equatn.o) \
	$(LIB)(pg_extract.o) \
	$(LIB)(pg_if.o) \
	$(LIB)(pg_init.o) \
	$(LIB)(pg_main.o) \
	$(LIB)(pg_prgcnst.o) \
	$(LIB)(pg_procdiv.o) \
	$(LIB)(pg_rs.o) \
	$(LIB)(pg_screen.o) \
	$(LIB)(pg_strtscn.o) \
	$(LIB)(pg_type.o) \
	$(LIB)(pg_utils.o) \
	$(LIB)(pg_write.o)
	$(RANLIB) $(LIB)
	@echo lib $(LIB) is now up-to-date


$(LIB)(pg_check.o): pg_check.c pgcommon.h pgglobal.h pgcblsrc.h
$(LIB)(pg_check.o): pgstruct.h
$(LIB)(pg_cli.o): pg_cli.c pgcommon.h pgglobal.h
$(LIB)(pg_declare.o): pg_declare.c pgcommon.h pgglobal.h pgstruct.h
$(LIB)(pg_doit.o): pg_doit.c pgcommon.h pgglobal.h pgstruct.h
$(LIB)(pg_doit.o): pgcblsrc.h pgkeyw.h pgeqtns.h
$(LIB)(pg_equatn.o): pg_equatn.c pgcommon.h pgglobal.h pgstruct.h
$(LIB)(pg_equatn.o): pgeqtns.h pgdefeqn.h
$(LIB)(pg_extract.o): pg_extract.c pgcommon.h pgglobal.h pgstruct.h
$(LIB)(pg_extract.o): pgextrct.h
$(LIB)(pg_if.o): pg_if.c pgcommon.h pgstruct.h pgglobal.h
$(LIB)(pg_init.o): pg_init.c pgcommon.h pgstruct.h pgglobal.h
$(LIB)(pg_main.o): pg_main.c pgcommon.h pgstruct.h pgglobal.h
$(LIB)(pg_main.o): pgcblsrc.h pgkeyw.h pgeqtns.h pgextrct.h
$(LIB)(pg_prgcnst.o): pg_prgcnst.c pgcommon.h pgstruct.h pgglobal.h
$(LIB)(pg_prgcnst.o): pgeqtns.h
$(LIB)(pg_procdiv.o): pg_procdiv.c pgcommon.h pgstruct.h pgglobal.h
$(LIB)(pg_procdiv.o): pgcblsrc.h pgkeyw.h
$(LIB)(pg_rs.o): pg_rs.c pgcommon.h pgglobal.h pgstruct.h
$(LIB)(pg_screen.o): pg_screen.c pgcommon.h pgstruct.h pgglobal.h
$(LIB)(pg_screen.o): pgkeyw.h
$(LIB)(pg_strtscn.o): pg_strtscn.c pgcommon.h pgglobal.h pgstruct.h
$(LIB)(pg_strtscn.o): pgcblsrc.h
$(LIB)(pg_type.o): pg_type.c pgcommon.h pgglobal.h pgstruct.h
$(LIB)(pg_utils.o): pg_utils.c pgcommon.h pgglobal.h pgstruct.h
$(LIB)(pg_utils.o): pgkeyw.h pgcblsrc.h
$(LIB)(pg_write.o): pg_write.c pgcommon.h pgstruct.h pgglobal.h
$(LIB)(pg_write.o): pgcblsrc.h pgextrct.h

