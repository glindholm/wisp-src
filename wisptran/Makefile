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
#	File:		wisp.umf
#
#	Function:	Generic makefile for the WISP translator on all unix platforms.
#
# 	History:	09/26/89	Written by GSL
#			06/04/92	Changed to use make.include GSL
#			06/09/92	Removed copy to $(STDBIN). GSL
#

#WISPTRAN = wispd
WISPTRAN = wisp

#LIB = wisptrand.a
LIB = wisptran.a

#CDEBUG = -g
CDEBUG = -O

include $(WISP)/src/port/make.include

all: 	$(WISPTRAN)

$(WISPTRAN): $(LIB)
	cc $(CFLAGS) -o $@ $(LIB)


$(LIB):	$(LIB)(wisp.o) \
	$(LIB)(ring.o) \
	$(LIB)(wisp_pic.o) \
	$(LIB)(wt_acept.o) \
	$(LIB)(wt_call.o) \
	$(LIB)(wt_cli.o) \
	$(LIB)(wt_crtrw.o) \
	$(LIB)(wt_datad.o) \
	$(LIB)(wt_decl.o) \
	$(LIB)(wt_delet.o) \
	$(LIB)(wt_disp.o) \
	$(LIB)(wt_divs.o) \
	$(LIB)(wt_files.o) \
	$(LIB)(wt_free.o) \
	$(LIB)(wt_ident.o) \
	$(LIB)(wt_if.o) \
	$(LIB)(wt_input.o) \
	$(LIB)(wt_io.o) \
	$(LIB)(wt_locks.o) \
	$(LIB)(wt_opcls.o) \
	$(LIB)(wt_procd.o) \
	$(LIB)(wt_read.o) \
	$(LIB)(wt_scrn.o) \
	$(LIB)(wt_sort.o) \
	$(LIB)(wt_start.o) \
	$(LIB)(wt_utils.o) \
	$(LIB)(wt_write.o) \
	$(LIB)(wt_wsdat.o) \
	$(LIB)(wt_wsdiv.o) 
	$(RANLIB) $(LIB)
	@echo lib $(LIB) is now up-to-date


$(LIB)(ring.o): ring.c
$(LIB)(wisp.o): wisp.c wisp.h wcommon.h
$(LIB)(wisp_pic.o): wisp_pic.c
$(LIB)(wt_acept.o): wt_acept.c wisp.h wcommon.h
$(LIB)(wt_call.o): wt_call.c wisp.h wcommon.h
$(LIB)(wt_cli.o): wt_cli.c wisp.h wcommon.h
$(LIB)(wt_crtrw.o): wt_crtrw.c wisp.h wcommon.h
$(LIB)(wt_datad.o): wt_datad.c wisp.h wcommon.h
$(LIB)(wt_decl.o): wt_decl.c wisp.h wcommon.h
$(LIB)(wt_delet.o): wt_delet.c wisp.h wcommon.h
$(LIB)(wt_disp.o): wt_disp.c wisp.h wcommon.h
$(LIB)(wt_divs.o): wt_divs.c wisp.h wcommon.h
$(LIB)(wt_files.o): wt_files.c wisp.h wcommon.h
$(LIB)(wt_free.o): wt_free.c wisp.h wcommon.h
$(LIB)(wt_ident.o): wt_ident.c wisp.h wcommon.h
$(LIB)(wt_if.o): wt_if.c wisp.h wcommon.h
$(LIB)(wt_input.o): wt_input.c wisp.h wcommon.h
$(LIB)(wt_io.o): wt_io.c wisp.h wcommon.h
$(LIB)(wt_locks.o): wt_locks.c wisp.h wcommon.h
$(LIB)(wt_opcls.o): wt_opcls.c wisp.h wcommon.h
$(LIB)(wt_procd.o): wt_procd.c wisp.h wcommon.h
$(LIB)(wt_read.o): wt_read.c wisp.h wcommon.h
$(LIB)(wt_scrn.o): wt_scrn.c wisp.h wcommon.h
$(LIB)(wt_sort.o): wt_sort.c wisp.h wcommon.h
$(LIB)(wt_start.o): wt_start.c wisp.h wcommon.h
$(LIB)(wt_utils.o): wt_utils.c wisp.h wcommon.h
$(LIB)(wt_write.o): wt_write.c wisp.h wcommon.h
$(LIB)(wt_wsdat.o): wt_wsdat.c wisp.h wcommon.h
$(LIB)(wt_wsdiv.o): wt_wsdiv.c wisp.h wcommon.h
