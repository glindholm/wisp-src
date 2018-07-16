# 
#	Copyright (c) 1995 DevTech Migrations, All rights reserved.
#	$Id:$
#
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
#	File:		wisp.mak
#
#	Function:	Makefile for the WISP translator on MSDOS
#			using Intel C Code Builder
#
# 	History:	09/26/89	Written by GSL
#			06/04/92	Changed to use make.include GSL
#			06/09/92	Removed copy to $(STDBIN). GSL
#			12/31/92	Fixed for codebuilder. GSL
#

WISPTRAN = wisp.exe

!include $(WISP)\src\port\dosmake.mak

all: 	$(STDBIN)\$(WISPTRAN)

$(STDBIN)\$(WISPTRAN):	$(WISPTRAN)
	copy $(WISPTRAN) $@

OBJS =	wisp.obj \
	getopt.obj \
	ring.obj \
	wisp_pic.obj \
	wt_acept.obj \
	wt_call.obj \
	wt_cli.obj \
	wt_crtrw.obj \
	wt_datad.obj \
	wt_decl.obj \
	wt_delet.obj \
	wt_disp.obj \
	wt_divs.obj \
	wt_files.obj \
	wt_free.obj \
	wt_ident.obj \
	wt_if.obj \
	wt_input.obj \
	wt_io.obj \
	wt_locks.obj \
	wt_opcls.obj \
	wt_procd.obj \
	wt_read.obj \
	wt_scrn.obj \
	wt_sort.obj \
	wt_start.obj \
	wt_utils.obj \
	wt_write.obj \
	wt_wsdat.obj \
	wt_wsdiv.obj 

LIB_FILE = wisptran.lib
LIB_OBJS = $(OBJS)
!include $(CBPATH)\bin\lib.mak

$(WISPTRAN): $(LIB_FILE)
	$(CC) $(CFLAGS) -e $@ /s64000 $(LIB_FILE)


ring.obj: ring.c
wisp.obj: wisp.c wisp.h wcommon.h
wisp_pic.obj: wisp_pic.c
wt_acept.obj: wt_acept.c wisp.h wcommon.h
wt_call.obj: wt_call.c wisp.h wcommon.h
wt_cli.obj: wt_cli.c wisp.h wcommon.h
wt_crtrw.obj: wt_crtrw.c wisp.h wcommon.h
wt_datad.obj: wt_datad.c wisp.h wcommon.h
wt_decl.obj: wt_decl.c wisp.h wcommon.h
wt_delet.obj: wt_delet.c wisp.h wcommon.h
wt_disp.obj: wt_disp.c wisp.h wcommon.h
wt_divs.obj: wt_divs.c wisp.h wcommon.h
wt_files.obj: wt_files.c wisp.h wcommon.h
wt_free.obj: wt_free.c wisp.h wcommon.h
wt_ident.obj: wt_ident.c wisp.h wcommon.h
wt_if.obj: wt_if.c wisp.h wcommon.h
wt_input.obj: wt_input.c wisp.h wcommon.h
wt_io.obj: wt_io.c wisp.h wcommon.h
wt_locks.obj: wt_locks.c wisp.h wcommon.h
wt_opcls.obj: wt_opcls.c wisp.h wcommon.h
wt_procd.obj: wt_procd.c wisp.h wcommon.h
wt_read.obj: wt_read.c wisp.h wcommon.h
wt_scrn.obj: wt_scrn.c wisp.h wcommon.h
wt_sort.obj: wt_sort.c wisp.h wcommon.h
wt_start.obj: wt_start.c wisp.h wcommon.h
wt_utils.obj: wt_utils.c wisp.h wcommon.h
wt_write.obj: wt_write.c wisp.h wcommon.h
wt_wsdat.obj: wt_wsdat.c wisp.h wcommon.h
wt_wsdiv.obj: wt_wsdiv.c wisp.h wcommon.h

