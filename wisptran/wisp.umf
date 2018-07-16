# 
#	Copyright (c) 1995 DevTech Migrations, All rights reserved.
#	$Id:$
#
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

CDEBUG = 

WC=../wispcommon

include $(WISP)/src/port/make.include

CFLAGS  = $(CFLAGS0) -I $(WC)

WISP_EXE = wisp

THE_LIB = wisptran.a

.PRECIOUS:	$(THE_LIB) 

all: 	$(WISP_EXE)

debug:
	$(MAKE) WISP_EXE=wispd THE_LIB=wisptrand.a CDEBUG=-g

$(WISP_EXE): $(THE_LIB)
	$(CC) $(CFLAGS) -o $@ $(THE_LIB)

clean:
	rm -f $(WISP_EXE) wispd $(THE_LIB) wisptrand.a *~ core *.o

$(THE_LIB):	$(THE_LIB)(wisp.o) \
	$(THE_LIB)(dataconv.o) \
	$(THE_LIB)(input.o) \
	$(THE_LIB)(keywords.o) \
	$(THE_LIB)(node.o) \
	$(THE_LIB)(output.o) \
	$(THE_LIB)(reduce.o) \
	$(THE_LIB)(ring.o) \
	$(THE_LIB)(statment.o) \
	$(THE_LIB)(stats.o) \
	$(THE_LIB)(tokenize.o) \
	$(THE_LIB)(untabstr.o) \
	$(THE_LIB)(wisp_pic.o) \
	$(THE_LIB)(wmalloc.o) \
	$(THE_LIB)(wt_acept.o) \
	$(THE_LIB)(wt_call.o) \
	$(THE_LIB)(wt_cli.o) \
	$(THE_LIB)(wt_crtrw.o) \
	$(THE_LIB)(wt_datad.o) \
	$(THE_LIB)(wt_debug.o) \
	$(THE_LIB)(wt_decl.o) \
	$(THE_LIB)(wt_delet.o) \
	$(THE_LIB)(wt_disp.o) \
	$(THE_LIB)(wt_divs.o) \
	$(THE_LIB)(wt_files.o) \
	$(THE_LIB)(wt_free.o) \
	$(THE_LIB)(wt_ident.o) \
	$(THE_LIB)(wt_if.o) \
	$(THE_LIB)(wt_input.o) \
	$(THE_LIB)(wt_io.o) \
	$(THE_LIB)(wt_locks.o) \
	$(THE_LIB)(wt_opcls.o) \
	$(THE_LIB)(wt_procd.o) \
	$(THE_LIB)(wt_read.o) \
	$(THE_LIB)(wt_scrn.o) \
	$(THE_LIB)(wt_sort.o) \
	$(THE_LIB)(wt_start.o) \
	$(THE_LIB)(wt_utils.o) \
	$(THE_LIB)(wt_write.o) \
	$(THE_LIB)(wt_wsdat.o) \
	$(THE_LIB)(wt_wsdiv.o)
	$(RANLIB) $(THE_LIB)
	@echo THE_LIB $(THE_LIB) is now up-to-date

$(THE_LIB)(dataconv.o): dataconv.c wisp.h proto.h
$(THE_LIB)(dataconv.o): $(WC)/idsistd.h $(WC)/intdef.h
$(THE_LIB)(dataconv.o): token.h tokenize.h node.h wispfile.h wmalloc.h
$(THE_LIB)(dataconv.o): output.h keywords.h cobfiles.h statment.h
$(THE_LIB)(dataconv.o): wt_datad.h
$(THE_LIB)(input.o): input.c wisp.h proto.h $(WC)/idsistd.h
$(THE_LIB)(input.o): $(WC)/intdef.h token.h tokenize.h node.h
$(THE_LIB)(input.o): wispfile.h wmalloc.h output.h keywords.h
$(THE_LIB)(input.o): directiv.h lines.h input.h
$(THE_LIB)(keywords.o): keywords.c wmalloc.h keywords.h proto.h
$(THE_LIB)(keywords.o): $(WC)/idsistd.h $(WC)/intdef.h
$(THE_LIB)(keywords.o): token.h tokenize.h node.h wispfile.h
$(THE_LIB)(node.o): node.c proto.h $(WC)/idsistd.h
$(THE_LIB)(node.o): $(WC)/intdef.h token.h tokenize.h node.h
$(THE_LIB)(node.o): wispfile.h wmalloc.h
$(THE_LIB)(output.o): output.c wisp.h proto.h $(WC)/idsistd.h
$(THE_LIB)(output.o): $(WC)/intdef.h token.h tokenize.h node.h
$(THE_LIB)(output.o): wispfile.h wmalloc.h output.h keywords.h
$(THE_LIB)(output.o): statment.h
$(THE_LIB)(reduce.o): reduce.c token.h tokenize.h node.h reduce.h
$(THE_LIB)(statment.o): statment.c token.h tokenize.h node.h lines.h
$(THE_LIB)(statment.o): statment.h proto.h $(WC)/idsistd.h
$(THE_LIB)(statment.o): $(WC)/intdef.h wispfile.h wmalloc.h
$(THE_LIB)(statment.o): $(WC)/ring.h
$(THE_LIB)(stats.o): stats.c
$(THE_LIB)(tokenize.o): tokenize.c wmalloc.h token.h tokenize.h lines.h
$(THE_LIB)(tokenize.o): $(WC)/ring.h input.h wispfile.h
$(THE_LIB)(tokenize.o): output.h node.h keywords.h proto.h
$(THE_LIB)(tokenize.o): $(WC)/idsistd.h $(WC)/intdef.h
$(THE_LIB)(untabstr.o): untabstr.c wmalloc.h
$(THE_LIB)(wisp.o): wisp.c wisp.h proto.h $(WC)/idsistd.h
$(THE_LIB)(wisp.o): $(WC)/intdef.h token.h tokenize.h node.h
$(THE_LIB)(wisp.o): wispfile.h wmalloc.h output.h keywords.h scrn.h
$(THE_LIB)(wisp.o): crt.h cobfiles.h keylist.h directiv.h input.h
$(THE_LIB)(wisp.o): $(WC)/ring.h statment.h wt_procd.h
$(THE_LIB)(wisp.o): $(WC)/wcommon.h $(WC)/wispcfg.h
$(THE_LIB)(wmalloc.o): wmalloc.c wmalloc.h output.h token.h tokenize.h
$(THE_LIB)(wmalloc.o): node.h wispfile.h proto.h
$(THE_LIB)(wmalloc.o): $(WC)/idsistd.h $(WC)/intdef.h
$(THE_LIB)(wt_acept.o): wt_acept.c wisp.h proto.h
$(THE_LIB)(wt_acept.o): $(WC)/idsistd.h $(WC)/intdef.h
$(THE_LIB)(wt_acept.o): token.h tokenize.h node.h wispfile.h wmalloc.h
$(THE_LIB)(wt_acept.o): output.h keywords.h
$(THE_LIB)(wt_call.o): wt_call.c wisp.h proto.h $(WC)/idsistd.h
$(THE_LIB)(wt_call.o): $(WC)/intdef.h token.h tokenize.h node.h
$(THE_LIB)(wt_call.o): wispfile.h wmalloc.h output.h keywords.h
$(THE_LIB)(wt_call.o): cobfiles.h
$(THE_LIB)(wt_cli.o): wt_cli.c wisp.h proto.h $(WC)/idsistd.h
$(THE_LIB)(wt_cli.o): $(WC)/intdef.h token.h tokenize.h node.h
$(THE_LIB)(wt_cli.o): wispfile.h wmalloc.h output.h keywords.h
$(THE_LIB)(wt_cli.o): $(WC)/wcommon.h $(WC)/getopt.h
$(THE_LIB)(wt_crtrw.o): wt_crtrw.c wisp.h proto.h
$(THE_LIB)(wt_crtrw.o): $(WC)/idsistd.h $(WC)/intdef.h
$(THE_LIB)(wt_crtrw.o): token.h tokenize.h node.h wispfile.h wmalloc.h
$(THE_LIB)(wt_crtrw.o): output.h keywords.h crt.h
$(THE_LIB)(wt_datad.o): wt_datad.c wisp.h proto.h
$(THE_LIB)(wt_datad.o): $(WC)/idsistd.h $(WC)/intdef.h
$(THE_LIB)(wt_datad.o): token.h tokenize.h node.h wispfile.h wmalloc.h
$(THE_LIB)(wt_datad.o): output.h keywords.h crt.h cobfiles.h keylist.h
$(THE_LIB)(wt_datad.o): statment.h
$(THE_LIB)(wt_debug.o): wt_debug.c token.h tokenize.h node.h wispfile.h
$(THE_LIB)(wt_debug.o): wisp.h proto.h $(WC)/idsistd.h
$(THE_LIB)(wt_debug.o): $(WC)/intdef.h wmalloc.h output.h
$(THE_LIB)(wt_debug.o): keywords.h
$(THE_LIB)(wt_decl.o): wt_decl.c wisp.h proto.h $(WC)/idsistd.h
$(THE_LIB)(wt_decl.o): $(WC)/intdef.h token.h tokenize.h node.h
$(THE_LIB)(wt_decl.o): wispfile.h wmalloc.h output.h keywords.h crt.h
$(THE_LIB)(wt_decl.o): cobfiles.h
$(THE_LIB)(wt_delet.o): wt_delet.c wisp.h proto.h
$(THE_LIB)(wt_delet.o): $(WC)/idsistd.h $(WC)/intdef.h
$(THE_LIB)(wt_delet.o): token.h tokenize.h node.h wispfile.h wmalloc.h
$(THE_LIB)(wt_delet.o): output.h keywords.h cobfiles.h
$(THE_LIB)(wt_disp.o): wt_disp.c wisp.h proto.h $(WC)/idsistd.h
$(THE_LIB)(wt_disp.o): $(WC)/intdef.h token.h tokenize.h node.h
$(THE_LIB)(wt_disp.o): wispfile.h wmalloc.h output.h keywords.h scrn.h
$(THE_LIB)(wt_disp.o): crt.h statment.h wt_procd.h wt_disp.h reduce.h
$(THE_LIB)(wt_divs.o): wt_divs.c wisp.h proto.h $(WC)/idsistd.h
$(THE_LIB)(wt_divs.o): $(WC)/intdef.h token.h tokenize.h node.h
$(THE_LIB)(wt_divs.o): wispfile.h wmalloc.h output.h keywords.h
$(THE_LIB)(wt_divs.o): cobfiles.h directiv.h
$(THE_LIB)(wt_files.o): wt_files.c wisp.h proto.h
$(THE_LIB)(wt_files.o): $(WC)/idsistd.h $(WC)/intdef.h
$(THE_LIB)(wt_files.o): token.h tokenize.h node.h wispfile.h wmalloc.h
$(THE_LIB)(wt_files.o): output.h keywords.h scrn.h cobfiles.h keylist.h
$(THE_LIB)(wt_files.o): input.h $(WC)/ring.h
$(THE_LIB)(wt_free.o): wt_free.c wisp.h proto.h $(WC)/idsistd.h
$(THE_LIB)(wt_free.o): $(WC)/intdef.h token.h tokenize.h node.h
$(THE_LIB)(wt_free.o): wispfile.h wmalloc.h output.h keywords.h
$(THE_LIB)(wt_ident.o): wt_ident.c wisp.h proto.h
$(THE_LIB)(wt_ident.o): $(WC)/idsistd.h $(WC)/intdef.h
$(THE_LIB)(wt_ident.o): token.h tokenize.h node.h wispfile.h wmalloc.h
$(THE_LIB)(wt_ident.o): output.h keywords.h directiv.h statment.h
$(THE_LIB)(wt_ident.o): $(WC)/ring.h
$(THE_LIB)(wt_if.o): wt_if.c wisp.h proto.h $(WC)/idsistd.h
$(THE_LIB)(wt_if.o): $(WC)/intdef.h token.h tokenize.h node.h
$(THE_LIB)(wt_if.o): wispfile.h wmalloc.h output.h keywords.h
$(THE_LIB)(wt_if.o): statment.h reduce.h
$(THE_LIB)(wt_input.o): wt_input.c wisp.h proto.h
$(THE_LIB)(wt_input.o): $(WC)/idsistd.h $(WC)/intdef.h
$(THE_LIB)(wt_input.o): token.h tokenize.h node.h wispfile.h wmalloc.h
$(THE_LIB)(wt_input.o): output.h keywords.h crt.h cobfiles.h keylist.h
$(THE_LIB)(wt_input.o): directiv.h statment.h
$(THE_LIB)(wt_io.o): wt_io.c wisp.h proto.h $(WC)/idsistd.h
$(THE_LIB)(wt_io.o): $(WC)/intdef.h token.h tokenize.h node.h
$(THE_LIB)(wt_io.o): wispfile.h wmalloc.h output.h keywords.h lines.h
$(THE_LIB)(wt_io.o): input.h
$(THE_LIB)(wt_locks.o): wt_locks.c wisp.h proto.h
$(THE_LIB)(wt_locks.o): $(WC)/idsistd.h $(WC)/intdef.h
$(THE_LIB)(wt_locks.o): token.h tokenize.h node.h wispfile.h wmalloc.h
$(THE_LIB)(wt_locks.o): output.h keywords.h cobfiles.h
$(THE_LIB)(wt_opcls.o): wt_opcls.c wisp.h proto.h
$(THE_LIB)(wt_opcls.o): $(WC)/idsistd.h $(WC)/intdef.h
$(THE_LIB)(wt_opcls.o): token.h tokenize.h node.h wispfile.h wmalloc.h
$(THE_LIB)(wt_opcls.o): output.h keywords.h crt.h cobfiles.h directiv.h
$(THE_LIB)(wt_opcls.o): $(WC)/wcommon.h
$(THE_LIB)(wt_procd.o): wt_procd.c wisp.h proto.h
$(THE_LIB)(wt_procd.o): $(WC)/idsistd.h $(WC)/intdef.h
$(THE_LIB)(wt_procd.o): token.h tokenize.h node.h wispfile.h wmalloc.h
$(THE_LIB)(wt_procd.o): output.h keywords.h scrn.h crt.h cobfiles.h
$(THE_LIB)(wt_procd.o): statment.h wt_disp.h wt_procd.h reduce.h
$(THE_LIB)(wt_read.o): wt_read.c wisp.h proto.h $(WC)/idsistd.h
$(THE_LIB)(wt_read.o): $(WC)/intdef.h token.h tokenize.h node.h
$(THE_LIB)(wt_read.o): wispfile.h wmalloc.h output.h keywords.h crt.h
$(THE_LIB)(wt_read.o): cobfiles.h
$(THE_LIB)(wt_scrn.o): wt_scrn.c wisp.h proto.h $(WC)/idsistd.h
$(THE_LIB)(wt_scrn.o): $(WC)/intdef.h token.h tokenize.h node.h
$(THE_LIB)(wt_scrn.o): wispfile.h wmalloc.h output.h keywords.h scrn.h
$(THE_LIB)(wt_scrn.o): crt.h directiv.h $(WC)/wcommon.h
$(THE_LIB)(wt_scrn.o): reduce.h $(WC)/ring.h statment.h
$(THE_LIB)(wt_sort.o): wt_sort.c wisp.h proto.h $(WC)/idsistd.h
$(THE_LIB)(wt_sort.o): $(WC)/intdef.h token.h tokenize.h node.h
$(THE_LIB)(wt_sort.o): wispfile.h wmalloc.h output.h keywords.h
$(THE_LIB)(wt_sort.o): cobfiles.h
$(THE_LIB)(wt_start.o): wt_start.c wisp.h proto.h
$(THE_LIB)(wt_start.o): $(WC)/idsistd.h $(WC)/intdef.h
$(THE_LIB)(wt_start.o): token.h tokenize.h node.h wispfile.h wmalloc.h
$(THE_LIB)(wt_start.o): output.h keywords.h cobfiles.h reduce.h
$(THE_LIB)(wt_start.o): statment.h
$(THE_LIB)(wt_utils.o): wt_utils.c wisp.h proto.h
$(THE_LIB)(wt_utils.o): $(WC)/idsistd.h $(WC)/intdef.h
$(THE_LIB)(wt_utils.o): token.h tokenize.h node.h wispfile.h wmalloc.h
$(THE_LIB)(wt_utils.o): output.h keywords.h
$(THE_LIB)(wt_write.o): wt_write.c wisp.h proto.h
$(THE_LIB)(wt_write.o): $(WC)/idsistd.h $(WC)/intdef.h
$(THE_LIB)(wt_write.o): token.h tokenize.h node.h wispfile.h wmalloc.h
$(THE_LIB)(wt_write.o): output.h keywords.h cobfiles.h
$(THE_LIB)(wt_wsdat.o): wt_wsdat.c wisp.h proto.h
$(THE_LIB)(wt_wsdat.o): $(WC)/idsistd.h $(WC)/intdef.h
$(THE_LIB)(wt_wsdat.o): token.h tokenize.h node.h wispfile.h wmalloc.h
$(THE_LIB)(wt_wsdat.o): output.h keywords.h crt.h cobfiles.h
$(THE_LIB)(wt_wsdat.o): $(WC)/wcommon.h
$(THE_LIB)(wt_wsdiv.o): wt_wsdiv.c wisp.h proto.h
$(THE_LIB)(wt_wsdiv.o): $(WC)/idsistd.h $(WC)/intdef.h
$(THE_LIB)(wt_wsdiv.o): token.h tokenize.h node.h wispfile.h wmalloc.h
$(THE_LIB)(wt_wsdiv.o): output.h keywords.h keylist.h statment.h

#
#	WISPCOMMON
#

$(THE_LIB)(getopt.o): $(WC)/getopt.c
	$(CC) -c $(CFLAGS) $(WC)/$(%:.o=.c)
	ar rv $@ $%
	rm -f $%

$(THE_LIB)(ring.o): $(WC)/ring.c $(WC)/ring.h
	$(CC) -c $(CFLAGS) $(WC)/$(%:.o=.c)
	ar rv $@ $%
	rm -f $%

$(THE_LIB)(sleepdos.o): $(WC)/sleepdos.c
	$(CC) -c $(CFLAGS) $(WC)/$(%:.o=.c)
	ar rv $@ $%
	rm -f $%

$(THE_LIB)(wisp_pic.o): $(WC)/wisp_pic.c $(WC)/idsistd.h $(WC)/intdef.h $(WC)/wisp_pic.h
	$(CC) -c $(CFLAGS) $(WC)/$(%:.o=.c)
	ar rv $@ $%
	rm -f $%

testacu:
	@for testwcb in test0???.wcb ; do \
		echo Testing file $$testwcb ; \
		grep 'FUNCTION:' $$testwcb ; \
		noext=`echo $$testwcb | cut -f 1 -d .` ; \
		NOEXT=`echo $$noext | tr '[a-z]' '[A-Z]'` ; \
		$(WISP_EXE) $$testwcb ; \
		ccbl -Zd -Da4 -o $$NOEXT $$noext.cob ; \
	done ;

