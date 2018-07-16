# 
#	Copyright (c) 1995 DevTech Migrations, All rights reserved.
#	$Id:$
#
#/************************************************************************/
#/*                                                                     */
#/*             WISP - Wang Interchange Source Pre-processor            */
#/*              Copyright (c) 1988, 1989, 1990, 1991, 1992             */
#/*      An unpublished work of International Digital Scientific Inc.   */
#/*                         All rights reserved.                        */
#/*                                                                     */
#/************************************************************************/
#
#
#       File:           wisp.wat
#
#       Function:       Makefile for the WISP translator on MSDOS
#                       using Watcom v9.5
#
#       History:        09/26/89        Written by GSL
#                       06/04/92        Changed to use make.include GSL
#                       06/09/92        Removed copy to $(STDBIN). GSL
#                       12/31/92        Fixed for codebuilder. GSL
#                       07/14/93        Change for MFC. GSL
#                       12/08/93        Updated for WISP 3.3C Release KMT
#			04/14/94	Changed for Watcom, CBA
#			04/20/94	CBA, modified file so that either
#					wmake or nmake can use it
#

#CDEBUG=/od
CDEBUG=/d2
WISPTRAN = wisp.exe

!include $(WISP)\src\port\dosmake.wat

all:    $(WISPTRAN)

.PRECIOUS:	THE_LIB

THE_LIB=wisptran.lib

OBJS =  wisp.obj     \
	dataconv.obj \
	getopt.obj   \
	input.obj    \
	keywords.obj \
	node.obj     \
	output.obj   \
	reduce.obj   \
	ring.obj     \
	statment.obj \
	stats.obj    \
	tokenize.obj \
	untabstr.obj \
	wisp_pic.obj \
	wmalloc.obj  \
	wt_acept.obj \
	wt_call.obj  \
	wt_cli.obj   \
	wt_crtrw.obj \
	wt_datad.obj \
	wt_debug.obj \
	wt_decl.obj  \
	wt_delet.obj \
	wt_disp.obj  \
	wt_divs.obj  \
	wt_files.obj \
	wt_free.obj  \
	wt_ident.obj \
	wt_if.obj    \
	wt_input.obj \
	wt_io.obj    \
	wt_locks.obj \
	wt_opcls.obj \
	wt_procd.obj \
	wt_read.obj  \
	wt_scrn.obj  \
	wt_sort.obj  \
	wt_start.obj \
	wt_utils.obj \
	wt_write.obj \
	wt_wsdat.obj \
	wt_wsdiv.obj 

$(WISPTRAN): $(THE_LIB)
	echo debug all	> make.rsp
	echo option CASEEXACT	>>make.rsp
	echo option STACK=128k	>>make.rsp
	echo file $(THE_LIB)	>>make.rsp
	set wcl386=$(CFLAGS0)
	$(CC) /fe=$@ @make.rsp /k128k
	$(BIND4GW) $@
	@echo $(WISPTRAN) is now up-to-date

$(THE_LIB): $(OBJS)
	!$(WLIB) $(THE_LIB) -+$?
	@echo lib $(THE_LIB) is now up-to-date

clean:
	del $(WISPTRAN)
	del $(THE_LIB)
	del *.obj

dataconv.obj: dataconv.c wisp.h token.h node.h
input.obj: input.c wisp.h directiv.h wispfile.h token.h node.h wmalloc.h lines.h
keywords.obj: keywords.c wmalloc.h
node.obj: node.c wmalloc.h token.h node.h
output.obj: output.c wisp.h wispfile.h token.h node.h
reduce.obj: reduce.c token.h node.h
ring.obj: ring.c
statment.obj: statment.c token.h node.h lines.h
stats.obj: stats.c
tokenize.obj: tokenize.c wmalloc.h token.h lines.h
untabstr.obj: untabstr.c wmalloc.h
wisp.obj: wisp.c wisp.h wcommon.h node.h token.h directiv.h wmalloc.h 
wisp_pic.obj: wisp_pic.c idsistd.h intdef.h
wmalloc.obj: wmalloc.c wmalloc.h
wt_acept.obj: wt_acept.c wisp.h wcommon.h
wt_call.obj: wt_call.c wisp.h wcommon.h wmalloc.h
wt_cli.obj: wt_cli.c wisp.h wcommon.h wispfile.h
wt_crtrw.obj: wt_crtrw.c wisp.h wcommon.h
wt_datad.obj: wt_datad.c wisp.h wcommon.h
wt_debug.obj: wt_debug.c token.h node.h wispfile.h
wt_decl.obj: wt_decl.c wisp.h wcommon.h
wt_delet.obj: wt_delet.c wisp.h wcommon.h
wt_disp.obj: wt_disp.c wisp.h wcommon.h scrn.h node.h token.h crt.h
wt_divs.obj: wt_divs.c wisp.h wcommon.h
wt_files.obj: wt_files.c wisp.h wcommon.h
wt_free.obj: wt_free.c wisp.h wcommon.h
wt_ident.obj: wt_ident.c wisp.h wcommon.h
wt_if.obj: wt_if.c wisp.h wcommon.h
wt_input.obj: wt_input.c wisp.h wcommon.h directiv.h node.h token.h wmalloc.h
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

