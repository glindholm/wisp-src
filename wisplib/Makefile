#
#	Copyright (c) 1995-1996 DevTech Migrations, All rights reserved.
#	$Id:$
#
#
#	File:		libwisp.umf
#
#	Function:	Generic makefile for the wisp library on all unix platforms.
#
#

MAKEFILE = libwisp.umf
CDEBUG	=

L_WISP	= wisp
L_VIDEO = video
THE_LIB	= lib$(L_WISP).a

WC=../wispcommon
VC=../videolib

TARGET=all

include $(WISP)/src/port/make.include

CFLAGS  = $(CFLAGS0) -I $(WC) -I$(VC)

.PRECIOUS:	$(THE_LIB) 

all:	$(THE_LIB)

debug:
	$(MAKE) -f $(MAKEFILE) CDEBUG="-g -DDEBUG" L_WISP=wispd $(TARGET)

clean:
	rm -f libwisp*.a *.o core *~

install: $(STDLIB)/$(THE_LIB)

$(STDLIB)/$(THE_LIB): $(THE_LIB)
	cp $(THE_LIB) $@

$(THE_LIB): $(THE_LIB)(acpmap.o) \
	$(THE_LIB)(acustubs.o) \
	$(THE_LIB)(backgrnd.o) \
	$(THE_LIB)(bell.o) \
	$(THE_LIB)(bit_x.o) \
	$(THE_LIB)(bits.o) \
	$(THE_LIB)(breakacp.o) \
	$(THE_LIB)(btransl.o) \
	$(THE_LIB)(cancel.o) \
	$(THE_LIB)(cexit.o) \
	$(THE_LIB)(cgetneg.o) \
	$(THE_LIB)(checkacp.o) \
	$(THE_LIB)(closeacp.o) \
	$(THE_LIB)(coblink.o) \
	$(THE_LIB)(cobpic.o) \
	$(THE_LIB)(costar.o) \
	$(THE_LIB)(customvw.o) \
	$(THE_LIB)(date.o) \
	$(THE_LIB)(dateset.o) \
	$(THE_LIB)(day.o) \
	$(THE_LIB)(dbfile.o) \
	$(THE_LIB)(delete.o) \
	$(THE_LIB)(dosprint.o) \
	$(THE_LIB)(edestubs.o) \
	$(THE_LIB)(errgparm.o) \
	$(THE_LIB)(extract.o) \
	$(THE_LIB)(fcopy.o) \
	$(THE_LIB)(fexists.o) \
	$(THE_LIB)(filecopy.o) \
	$(THE_LIB)(filesize.o) \
	$(THE_LIB)(filgparm.o) \
	$(THE_LIB)(find.o) \
	$(THE_LIB)(findcase.o) \
	$(THE_LIB)(findexts.o) \
	$(THE_LIB)(fixerr.o) \
	$(THE_LIB)(fxzone.o) \
	$(THE_LIB)(getopt.o) \
	$(THE_LIB)(getparm.o) \
	$(THE_LIB)(gparmbld.o) \
	$(THE_LIB)(greclen.o) \
	$(THE_LIB)(hexunpk.o) \
	$(THE_LIB)(idsisubs.o) \
	$(THE_LIB)(initglbs.o) \
	$(THE_LIB)(initscrn.o) \
	$(THE_LIB)(initwisp.o) \
	$(THE_LIB)(isexec.o) \
	$(THE_LIB)(juster.o) \
	$(THE_LIB)(level.o) \
	$(THE_LIB)(licwisp.o) \
	$(THE_LIB)(link.o) \
	$(THE_LIB)(linkproc.o) \
	$(THE_LIB)(linksubs.o) \
	$(THE_LIB)(linkvect.o) \
	$(THE_LIB)(logoff.o) \
	$(THE_LIB)(longarg.o) \
	$(THE_LIB)(machid.o) \
	$(THE_LIB)(makepath.o) \
	$(THE_LIB)(menu_go.o) \
	$(THE_LIB)(menu_key.o) \
	$(THE_LIB)(menuread.o) \
	$(THE_LIB)(menuscan.o) \
	$(THE_LIB)(message.o) \
	$(THE_LIB)(mngfile.o) \
	$(THE_LIB)(mount.o) \
	$(THE_LIB)(msdosfns.o) \
	$(THE_LIB)(msname.o) \
	$(THE_LIB)(mstime.o) \
	$(THE_LIB)(msvalue.o) \
	$(THE_LIB)(mvspaces.o) \
	$(THE_LIB)(mwconv.o) \
	$(THE_LIB)(nextfile.o) \
	$(THE_LIB)(noconfig.o) \
	$(THE_LIB)(onhelp.o) \
	$(THE_LIB)(openacp.o) \
	$(THE_LIB)(operator.o) \
	$(THE_LIB)(packer.o) \
	$(THE_LIB)(paths.o) \
	$(THE_LIB)(platsubs.o) \
	$(THE_LIB)(print.o) \
	$(THE_LIB)(protect.o) \
	$(THE_LIB)(putparm.o) \
	$(THE_LIB)(pwdname.o) \
	$(THE_LIB)(que_jobs.o) \
	$(THE_LIB)(quemgmt.o) \
	$(THE_LIB)(readacp.o) \
	$(THE_LIB)(readfdr.o) \
	$(THE_LIB)(readvtoc.o) \
	$(THE_LIB)(regexpr.o) \
	$(THE_LIB)(rename.o) \
	$(THE_LIB)(retcode.o) \
	$(THE_LIB)(ring.o) \
	$(THE_LIB)(rtransl.o) \
	$(THE_LIB)(runtype.o) \
	$(THE_LIB)(rvmap.o) \
	$(THE_LIB)(scratch.o) \
	$(THE_LIB)(screen.o) \
	$(THE_LIB)(search.o) \
	$(THE_LIB)(set.o) \
	$(THE_LIB)(setenvst.o) \
	$(THE_LIB)(setfile.o) \
	$(THE_LIB)(setprgid.o) \
	$(THE_LIB)(setrcode.o) \
	$(THE_LIB)(setsymb.o) \
	$(THE_LIB)(settrigp.o) \
	$(THE_LIB)(setupqm.o) \
	$(THE_LIB)(sharemem.o) \
	$(THE_LIB)(shellvar.o) \
	$(THE_LIB)(shutexit.o) \
	$(THE_LIB)(sleepdos.o) \
	$(THE_LIB)(sort.o) \
	$(THE_LIB)(sortcall.o) \
	$(THE_LIB)(sortlink.o) \
	$(THE_LIB)(sortseqf.o) \
	$(THE_LIB)(sortx.o) \
	$(THE_LIB)(spawn.o) \
	$(THE_LIB)(string.o) \
	$(THE_LIB)(submit.o) \
	$(THE_LIB)(sysdev.o) \
	$(THE_LIB)(sysinf.o) \
	$(THE_LIB)(untabify.o) \
	$(THE_LIB)(updatfdr.o) \
	$(THE_LIB)(upper.o) \
	$(THE_LIB)(vdispidx.o) \
	$(THE_LIB)(vdisplay.o) \
	$(THE_LIB)(vmspargs.o) \
	$(THE_LIB)(vmssort.o) \
	$(THE_LIB)(vssort.o) \
	$(THE_LIB)(vwang.o) \
	$(THE_LIB)(waccept.o) \
	$(THE_LIB)(wangmenu.o) \
	$(THE_LIB)(wanguid.o) \
	$(THE_LIB)(wassert.o) \
	$(THE_LIB)(wauthsub.o) \
	$(THE_LIB)(wchain.o) \
	$(THE_LIB)(wcmatch.o) \
	$(THE_LIB)(wdelfile.o) \
	$(THE_LIB)(wdellock.o) \
	$(THE_LIB)(wdfinish.o) \
	$(THE_LIB)(wdinit.o) \
	$(THE_LIB)(wdisplay.o) \
	$(THE_LIB)(werrlog.o) \
	$(THE_LIB)(werrpath.o) \
	$(THE_LIB)(werrvre.o) \
	$(THE_LIB)(wexit.o) \
	$(THE_LIB)(wexith.o) \
	$(THE_LIB)(wfaccess.o) \
	$(THE_LIB)(wfcisam.o) \
	$(THE_LIB)(wfclose.o) \
	$(THE_LIB)(wfilechk.o) \
	$(THE_LIB)(wfiledis.o) \
	$(THE_LIB)(wfileext.o) \
	$(THE_LIB)(wfname.o) \
	$(THE_LIB)(wfopen.o) \
	$(THE_LIB)(wfstatus.o) \
	$(THE_LIB)(wftok.o) \
	$(THE_LIB)(wfvision.o) \
	$(THE_LIB)(wfwait.o) \
	$(THE_LIB)(wgetpgrp.o) \
	$(THE_LIB)(wglobals.o) \
	$(THE_LIB)(wisp_pic.o) \
	$(THE_LIB)(wispcfg.o) \
	$(THE_LIB)(wispexit.o) \
	$(THE_LIB)(wispsort.o) \
	$(THE_LIB)(wispsync.o) \
	$(THE_LIB)(wispvers.o) \
	$(THE_LIB)(wlickey.o) \
	$(THE_LIB)(wmalloc.o) \
	$(THE_LIB)(wpause.o) \
	$(THE_LIB)(wperson.o) \
	$(THE_LIB)(wprint.o) \
	$(THE_LIB)(writeacp.o) \
	$(THE_LIB)(wrunconf.o) \
	$(THE_LIB)(wsb.o) \
	$(THE_LIB)(wsclose.o) \
	$(THE_LIB)(wscreen.o) \
	$(THE_LIB)(wsfnm.o) \
	$(THE_LIB)(wsfns.o) \
	$(THE_LIB)(wshelp.o) \
	$(THE_LIB)(wsmove.o) \
	$(THE_LIB)(wspawn.o) \
	$(THE_LIB)(wswap.o) \
	$(THE_LIB)(wsxio.o) \
	$(THE_LIB)(wsystem.o) \
	$(THE_LIB)(wtransl.o) \
	$(THE_LIB)(wvaset.o) \
	$(THE_LIB)(wwaitpid.o)
	$(RANLIB) $(THE_LIB)
	@echo lib $(THE_LIB) is now up-to-date

$(THE_LIB)(acpmap.o): acpmap.c $(WC)/idsistd.h
$(THE_LIB)(acpmap.o): $(WC)/intdef.h acp.h
$(THE_LIB)(acpmap.o): $(WC)/werrlog.h
$(THE_LIB)(acustubs.o): acustubs.c $(WC)/idsistd.h
$(THE_LIB)(acustubs.o): $(WC)/intdef.h $(WC)/werrlog.h
$(THE_LIB)(backgrnd.o): backgrnd.c $(WC)/idsistd.h
$(THE_LIB)(backgrnd.o): $(WC)/intdef.h $(WC)/wperson.h
$(THE_LIB)(backgrnd.o): $(WC)/werrlog.h
$(THE_LIB)(backgrnd.o): $(WC)/wdefines.h
$(THE_LIB)(bell.o): bell.c $(WC)/idsistd.h
$(THE_LIB)(bell.o): $(WC)/intdef.h movebin.h
$(THE_LIB)(bell.o): $(WC)/werrlog.h $(WC)/wisplib.h
$(THE_LIB)(bell.o): $(WC)/setenvst.h $(WC)/vwang.h
$(THE_LIB)(bit_x.o): bit_x.c $(WC)/idsistd.h
$(THE_LIB)(bit_x.o): $(WC)/intdef.h cobrun.h
$(THE_LIB)(bit_x.o): $(WC)/wisplib.h $(WC)/setenvst.h
$(THE_LIB)(bits.o): bits.c $(WC)/idsistd.h
$(THE_LIB)(bits.o): $(WC)/intdef.h $(WC)/werrlog.h
$(THE_LIB)(bits.o): $(WC)/wisplib.h $(WC)/setenvst.h
$(THE_LIB)(breakacp.o): breakacp.c $(WC)/idsistd.h
$(THE_LIB)(breakacp.o): $(WC)/intdef.h $(WC)/werrlog.h
$(THE_LIB)(btransl.o): btransl.c $(WC)/idsistd.h
$(THE_LIB)(btransl.o): $(WC)/intdef.h $(WC)/werrlog.h
$(THE_LIB)(cancel.o): cancel.c $(WC)/idsistd.h
$(THE_LIB)(cancel.o): $(WC)/intdef.h $(WC)/werrlog.h
$(THE_LIB)(cexit.o): cexit.c $(WC)/idsistd.h
$(THE_LIB)(cexit.o): $(WC)/intdef.h $(WC)/werrlog.h
$(THE_LIB)(cgetneg.o): cgetneg.c $(WC)/idsistd.h
$(THE_LIB)(cgetneg.o): $(WC)/intdef.h
$(THE_LIB)(checkacp.o): checkacp.c
$(THE_LIB)(closeacp.o): closeacp.c $(WC)/idsistd.h
$(THE_LIB)(closeacp.o): $(WC)/intdef.h acp.h
$(THE_LIB)(closeacp.o): $(WC)/werrlog.h
$(THE_LIB)(coblink.o): coblink.c $(WC)/idsistd.h
$(THE_LIB)(coblink.o): $(WC)/intdef.h cobrun.h
$(THE_LIB)(coblink.o): $(WC)/wisplib.h $(WC)/setenvst.h
$(THE_LIB)(cobpic.o): cobpic.c $(WC)/idsistd.h
$(THE_LIB)(cobpic.o): $(WC)/intdef.h cobrun.h cobpic.h
$(THE_LIB)(cobpic.o): $(WC)/wisp_pic.h
$(THE_LIB)(costar.o): costar.c costar.h $(VC)/video.h
$(THE_LIB)(costar.o): $(WC)/vwang.h $(WC)/idsistd.h
$(THE_LIB)(costar.o): $(WC)/intdef.h $(WC)/wperson.h
$(THE_LIB)(costar.o): $(WC)/werrlog.h $(WC)/idsisubs.h
$(THE_LIB)(costar.o): $(WC)/wmalloc.h $(VC)/vlocal.h
$(THE_LIB)(costar.o): $(VC)/vdata.h $(VC)/vcap.h
$(THE_LIB)(customvw.o): customvw.c
$(THE_LIB)(date.o): date.c $(WC)/idsistd.h
$(THE_LIB)(date.o): $(WC)/intdef.h movebin.h
$(THE_LIB)(date.o): $(WC)/werrlog.h $(WC)/wisplib.h
$(THE_LIB)(date.o): $(WC)/setenvst.h
$(THE_LIB)(dateset.o): dateset.c $(WC)/idsistd.h
$(THE_LIB)(dateset.o): $(WC)/intdef.h
$(THE_LIB)(day.o): day.c $(WC)/idsistd.h $(WC)/intdef.h
$(THE_LIB)(day.o): movebin.h $(WC)/werrlog.h
$(THE_LIB)(day.o): $(WC)/wisplib.h $(WC)/setenvst.h
$(THE_LIB)(dbfile.o): dbfile.c $(WC)/idsistd.h
$(THE_LIB)(dbfile.o): $(WC)/intdef.h $(WC)/wcommon.h
$(THE_LIB)(delete.o): delete.c $(WC)/idsistd.h
$(THE_LIB)(delete.o): $(WC)/intdef.h
$(THE_LIB)(dosprint.o): dosprint.c
$(THE_LIB)(edestubs.o): edestubs.c $(WC)/idsistd.h
$(THE_LIB)(edestubs.o): $(WC)/intdef.h $(WC)/vwang.h
$(THE_LIB)(edestubs.o): $(WC)/werrlog.h $(WC)/wexit.h
$(THE_LIB)(errgparm.o): errgparm.c $(WC)/idsistd.h
$(THE_LIB)(errgparm.o): $(WC)/intdef.h $(WC)/wangkeys.h
$(THE_LIB)(errgparm.o): $(WC)/wperson.h movebin.h
$(THE_LIB)(errgparm.o): $(WC)/werrlog.h
$(THE_LIB)(errgparm.o): $(WC)/wdefines.h
$(THE_LIB)(errgparm.o): $(WC)/wisplib.h
$(THE_LIB)(errgparm.o): $(WC)/setenvst.h
$(THE_LIB)(extract.o): extract.c $(WC)/wsysconf.h
$(THE_LIB)(extract.o): $(WC)/idsistd.h $(WC)/intdef.h
$(THE_LIB)(extract.o): $(WC)/wdefines.h $(WC)/wperson.h
$(THE_LIB)(extract.o): movebin.h $(WC)/werrlog.h
$(THE_LIB)(extract.o): $(WC)/wglobals.h $(WC)/wfiles.h
$(THE_LIB)(extract.o): $(WC)/wanguid.h $(WC)/wisplib.h
$(THE_LIB)(extract.o): $(WC)/setenvst.h
$(THE_LIB)(extract.o): $(WC)/idsisubs.h
$(THE_LIB)(extract.o): $(WC)/wrunconf.h $(WC)/wispcfg.h
$(THE_LIB)(fcopy.o): fcopy.c $(WC)/idsistd.h
$(THE_LIB)(fcopy.o): $(WC)/intdef.h $(WC)/wisplib.h
$(THE_LIB)(fcopy.o): $(WC)/setenvst.h fcopy.h
$(THE_LIB)(fexists.o): fexists.c $(WC)/idsistd.h
$(THE_LIB)(fexists.o): $(WC)/intdef.h
$(THE_LIB)(filecopy.o): filecopy.c $(WC)/idsistd.h
$(THE_LIB)(filecopy.o): $(WC)/intdef.h fcopy.h
$(THE_LIB)(filecopy.o): $(WC)/wfiles.h $(WC)/wcommon.h
$(THE_LIB)(filecopy.o): movebin.h $(WC)/wperson.h
$(THE_LIB)(filecopy.o): $(WC)/werrlog.h cobrun.h
$(THE_LIB)(filecopy.o): $(WC)/filext.h $(WC)/paths.h
$(THE_LIB)(filecopy.o): $(WC)/wfname.h $(WC)/idsisubs.h
$(THE_LIB)(filecopy.o): $(WC)/wisplib.h
$(THE_LIB)(filecopy.o): $(WC)/setenvst.h
$(THE_LIB)(filesize.o): filesize.c $(WC)/idsistd.h
$(THE_LIB)(filesize.o): $(WC)/intdef.h $(WC)/werrlog.h
$(THE_LIB)(filgparm.o): filgparm.c $(WC)/idsistd.h
$(THE_LIB)(filgparm.o): $(WC)/intdef.h $(WC)/wcommon.h
$(THE_LIB)(filgparm.o): $(WC)/wangkeys.h
$(THE_LIB)(filgparm.o): $(WC)/wperson.h movebin.h
$(THE_LIB)(filgparm.o): $(WC)/werrlog.h
$(THE_LIB)(filgparm.o): $(WC)/wdefines.h
$(THE_LIB)(filgparm.o): $(WC)/wisplib.h
$(THE_LIB)(filgparm.o): $(WC)/setenvst.h
$(THE_LIB)(filgparm.o): $(WC)/idsisubs.h
$(THE_LIB)(find.o): find.c $(WC)/idsistd.h
$(THE_LIB)(find.o): $(WC)/intdef.h $(WC)/wfiles.h
$(THE_LIB)(find.o): movebin.h $(WC)/werrlog.h
$(THE_LIB)(find.o): $(WC)/wdefines.h $(WC)/wfname.h
$(THE_LIB)(find.o): $(WC)/idsisubs.h $(WC)/wexit.h
$(THE_LIB)(find.o): $(WC)/wisplib.h $(WC)/setenvst.h
$(THE_LIB)(find.o): $(WC)/filext.h $(WC)/wperson.h
$(THE_LIB)(find.o): $(WC)/paths.h
$(THE_LIB)(findcase.o): findcase.c $(WC)/idsistd.h
$(THE_LIB)(findcase.o): $(WC)/intdef.h $(WC)/wisplib.h
$(THE_LIB)(findcase.o): $(WC)/setenvst.h
$(THE_LIB)(findexts.o): findexts.c $(WC)/idsistd.h
$(THE_LIB)(findexts.o): $(WC)/intdef.h $(WC)/wdefines.h
$(THE_LIB)(findexts.o): $(WC)/idsisubs.h
$(THE_LIB)(findexts.o): $(WC)/wisplib.h
$(THE_LIB)(findexts.o): $(WC)/setenvst.h
$(THE_LIB)(findexts.o): $(WC)/wcommon.h
$(THE_LIB)(fixerr.o): fixerr.c $(WC)/idsistd.h
$(THE_LIB)(fixerr.o): $(WC)/intdef.h
$(THE_LIB)(fxzone.o): fxzone.c $(WC)/idsistd.h
$(THE_LIB)(fxzone.o): $(WC)/intdef.h
$(THE_LIB)(getparm.o): getparm.c $(WC)/idsistd.h
$(THE_LIB)(getparm.o): $(WC)/intdef.h movebin.h
$(THE_LIB)(getparm.o): $(WC)/werrlog.h $(WC)/wangkeys.h
$(THE_LIB)(getparm.o): $(WC)/wcommon.h $(WC)/scnfacs.h
$(THE_LIB)(getparm.o): $(WC)/sharemem.h $(WC)/putparm.h
$(THE_LIB)(getparm.o): $(WC)/wglobals.h $(WC)/wfiles.h
$(THE_LIB)(getparm.o): $(WC)/wispvers.h $(WC)/wisplib.h
$(THE_LIB)(getparm.o): $(WC)/setenvst.h $(WC)/wexit.h
$(THE_LIB)(getparm.o): $(WC)/idsisubs.h $(WC)/vwang.h
$(THE_LIB)(getparm.o): $(WC)/wmalloc.h
$(THE_LIB)(gparmbld.o): gparmbld.c $(WC)/idsistd.h
$(THE_LIB)(gparmbld.o): $(WC)/intdef.h $(WC)/werrlog.h
$(THE_LIB)(gparmbld.o): $(WC)/wdefines.h
$(THE_LIB)(gparmbld.o): $(WC)/wisplib.h
$(THE_LIB)(gparmbld.o): $(WC)/setenvst.h
$(THE_LIB)(greclen.o): greclen.c $(WC)/idsistd.h
$(THE_LIB)(greclen.o): $(WC)/intdef.h
$(THE_LIB)(hexunpk.o): hexunpk.c $(WC)/idsistd.h
$(THE_LIB)(hexunpk.o): $(WC)/intdef.h movebin.h
$(THE_LIB)(hexunpk.o): $(WC)/werrlog.h $(WC)/wisplib.h
$(THE_LIB)(hexunpk.o): $(WC)/setenvst.h $(WC)/hexunpk.h
$(THE_LIB)(idsisubs.o): idsisubs.c $(WC)/idsisubs.h
$(THE_LIB)(idsisubs.o): $(WC)/idsistd.h $(WC)/intdef.h
$(THE_LIB)(idsisubs.o): $(WC)/paths.h
$(THE_LIB)(initglbs.o): initglbs.c $(WC)/idsistd.h
$(THE_LIB)(initglbs.o): $(WC)/intdef.h $(WC)/wglobals.h
$(THE_LIB)(initglbs.o): $(WC)/wfiles.h $(WC)/werrlog.h
$(THE_LIB)(initglbs.o): $(WC)/filext.h cobrun.h
$(THE_LIB)(initglbs.o): $(WC)/wexit.h $(WC)/wisplib.h
$(THE_LIB)(initglbs.o): $(WC)/setenvst.h
$(THE_LIB)(initglbs.o): $(WC)/wperson.h $(WC)/level.h
$(THE_LIB)(initglbs.o): $(WC)/vwang.h
$(THE_LIB)(initscrn.o): initscrn.c
$(THE_LIB)(initwisp.o): initwisp.c $(WC)/idsistd.h
$(THE_LIB)(initwisp.o): $(WC)/intdef.h $(WC)/werrlog.h
$(THE_LIB)(initwisp.o): $(WC)/wdefines.h
$(THE_LIB)(initwisp.o): $(WC)/wcommon.h $(WC)/vwang.h
$(THE_LIB)(initwisp.o): cobrun.h $(WC)/wglobals.h
$(THE_LIB)(initwisp.o): $(WC)/wfiles.h $(WC)/filext.h
$(THE_LIB)(initwisp.o): $(WC)/wperson.h $(WC)/wisplib.h
$(THE_LIB)(initwisp.o): $(WC)/setenvst.h $(WC)/wexit.h
$(THE_LIB)(initwisp.o): $(WC)/level.h $(WC)/machid.h
$(THE_LIB)(initwisp.o): $(WC)/wlicense.h
$(THE_LIB)(isexec.o): isexec.c $(WC)/idsistd.h
$(THE_LIB)(isexec.o): $(WC)/intdef.h $(WC)/wdefines.h
$(THE_LIB)(juster.o): juster.c $(WC)/idsistd.h
$(THE_LIB)(juster.o): $(WC)/intdef.h $(WC)/werrlog.h
$(THE_LIB)(level.o): level.c $(WC)/idsistd.h
$(THE_LIB)(level.o): $(WC)/intdef.h $(WC)/level.h
$(THE_LIB)(level.o): $(WC)/wisplib.h $(WC)/setenvst.h
$(THE_LIB)(licwisp.o): licwisp.c $(WC)/idsisubs.h
$(THE_LIB)(licwisp.o): $(WC)/idsistd.h $(WC)/intdef.h
$(THE_LIB)(licwisp.o): $(WC)/wlicense.h $(WC)/wispcfg.h
$(THE_LIB)(link.o): link.c $(WC)/idsistd.h
$(THE_LIB)(link.o): $(WC)/intdef.h $(WC)/link.h
$(THE_LIB)(link.o): $(WC)/wglobals.h $(WC)/wfiles.h
$(THE_LIB)(link.o): $(WC)/wdefines.h movebin.h
$(THE_LIB)(link.o): $(WC)/wperson.h $(WC)/wcommon.h
$(THE_LIB)(link.o): cobrun.h runtype.h $(WC)/wrunconf.h
$(THE_LIB)(link.o): $(WC)/idsisubs.h $(WC)/paths.h
$(THE_LIB)(link.o): $(WC)/wfname.h $(WC)/wisplib.h
$(THE_LIB)(link.o): $(WC)/setenvst.h $(WC)/wexit.h
$(THE_LIB)(link.o): linkvect.h $(WC)/filext.h
$(THE_LIB)(link.o): $(WC)/level.h $(WC)/vwang.h
$(THE_LIB)(link.o): $(WC)/sharemem.h $(WC)/wmalloc.h
$(THE_LIB)(link.o): $(WC)/wispcfg.h $(WC)/werrlog.h
$(THE_LIB)(linkproc.o): linkproc.c $(WC)/idsistd.h
$(THE_LIB)(linkproc.o): $(WC)/intdef.h $(WC)/vwang.h
$(THE_LIB)(linkproc.o): $(WC)/wdefines.h movebin.h
$(THE_LIB)(linkproc.o): $(WC)/werrlog.h $(WC)/wperson.h
$(THE_LIB)(linkproc.o): $(WC)/wfiles.h $(WC)/wcommon.h
$(THE_LIB)(linkproc.o): runtype.h $(WC)/wfname.h
$(THE_LIB)(linkproc.o): $(WC)/wisplib.h
$(THE_LIB)(linkproc.o): $(WC)/setenvst.h $(WC)/level.h
$(THE_LIB)(linkproc.o): $(WC)/sharemem.h $(WC)/wexit.h
$(THE_LIB)(linkproc.o): $(WC)/wispcfg.h
$(THE_LIB)(linksubs.o): linksubs.c $(WC)/idsistd.h
$(THE_LIB)(linksubs.o): $(WC)/intdef.h $(WC)/idsisubs.h
$(THE_LIB)(linksubs.o): $(WC)/link.h $(WC)/wglobals.h
$(THE_LIB)(linksubs.o): $(WC)/wfiles.h $(WC)/wdefines.h
$(THE_LIB)(linksubs.o): movebin.h $(WC)/wisplib.h
$(THE_LIB)(linksubs.o): $(WC)/setenvst.h osddefs.h
$(THE_LIB)(linksubs.o): $(WC)/paths.h $(WC)/wexit.h
$(THE_LIB)(linksubs.o): $(WC)/wperson.h $(WC)/werrlog.h
$(THE_LIB)(linkvect.o): linkvect.c $(WC)/idsistd.h
$(THE_LIB)(linkvect.o): $(WC)/intdef.h linkvect.h
$(THE_LIB)(linkvect.o): $(WC)/link.h $(WC)/wperson.h
$(THE_LIB)(linkvect.o): $(WC)/wisplib.h
$(THE_LIB)(linkvect.o): $(WC)/setenvst.h
$(THE_LIB)(logoff.o): logoff.c $(WC)/idsistd.h
$(THE_LIB)(logoff.o): $(WC)/intdef.h $(WC)/wexit.h
$(THE_LIB)(logoff.o): $(WC)/wdefines.h
$(THE_LIB)(longarg.o): longarg.c $(WC)/idsistd.h
$(THE_LIB)(longarg.o): $(WC)/intdef.h $(WC)/wglobals.h
$(THE_LIB)(longarg.o): $(WC)/wfiles.h $(WC)/wisplib.h
$(THE_LIB)(longarg.o): $(WC)/setenvst.h
$(THE_LIB)(machid.o): machid.c $(WC)/intdef.h
$(THE_LIB)(machid.o): $(WC)/wlicense.h $(WC)/wispnt.h assert.h
$(THE_LIB)(machid.o): $(WC)/machid.h $(WC)/wmalloc.h
$(THE_LIB)(makepath.o): makepath.c $(WC)/idsistd.h
$(THE_LIB)(makepath.o): $(WC)/intdef.h $(WC)/paths.h
$(THE_LIB)(makepath.o): $(WC)/wisplib.h
$(THE_LIB)(makepath.o): $(WC)/setenvst.h
$(THE_LIB)(menu_go.o): menu_go.c $(WC)/idsistd.h
$(THE_LIB)(menu_go.o): $(WC)/intdef.h menu.h
$(THE_LIB)(menu_go.o): $(WC)/wexit.h $(WC)/vwang.h
$(THE_LIB)(menu_go.o): $(WC)/wisplib.h $(WC)/setenvst.h
$(THE_LIB)(menu_go.o): $(VC)/video.h $(VC)/vlocal.h
$(THE_LIB)(menu_key.o): menu_key.c $(WC)/idsistd.h
$(THE_LIB)(menu_key.o): $(WC)/intdef.h $(WC)/werrlog.h
$(THE_LIB)(menu_key.o): menu.h
$(THE_LIB)(menuread.o): menuread.c $(WC)/idsistd.h
$(THE_LIB)(menuread.o): $(WC)/intdef.h menu.h
$(THE_LIB)(menuread.o): $(WC)/paths.h $(WC)/wispcfg.h
$(THE_LIB)(menuscan.o): menuscan.c $(WC)/idsistd.h
$(THE_LIB)(menuscan.o): $(WC)/intdef.h menu.h
$(THE_LIB)(menuscan.o): $(WC)/wperson.h $(WC)/wisplib.h
$(THE_LIB)(menuscan.o): $(WC)/setenvst.h $(WC)/vwang.h
$(THE_LIB)(menuscan.o): $(VC)/video.h $(VC)/vlocal.h
$(THE_LIB)(menuscan.o): $(VC)/vdata.h
$(THE_LIB)(message.o): message.c $(WC)/idsistd.h
$(THE_LIB)(message.o): $(WC)/intdef.h $(WC)/werrlog.h
$(THE_LIB)(message.o): $(WC)/wdefines.h $(WC)/vwang.h
$(THE_LIB)(message.o): $(WC)/wmalloc.h $(WC)/wperson.h
$(THE_LIB)(message.o): movebin.h $(WC)/wisplib.h
$(THE_LIB)(message.o): $(WC)/setenvst.h
$(THE_LIB)(mngfile.o): mngfile.c $(WC)/idsistd.h
$(THE_LIB)(mngfile.o): $(WC)/intdef.h runtype.h
$(THE_LIB)(mngfile.o): $(WC)/wcommon.h $(WC)/vwang.h
$(THE_LIB)(mngfile.o): $(WC)/scnfacs.h $(WC)/wperson.h
$(THE_LIB)(mngfile.o): $(WC)/wdefines.h $(WC)/werrlog.h
$(THE_LIB)(mngfile.o): $(WC)/wfname.h $(WC)/ring.h
$(THE_LIB)(mngfile.o): $(WC)/wisplib.h $(WC)/setenvst.h
$(THE_LIB)(mngfile.o): $(WC)/idsisubs.h $(WC)/paths.h
$(THE_LIB)(mngfile.o): $(WC)/wispcfg.h
$(THE_LIB)(mount.o): mount.c $(WC)/werrlog.h
$(THE_LIB)(mount.o): $(WC)/idsistd.h $(WC)/intdef.h
$(THE_LIB)(msdosfns.o): msdosfns.c
$(THE_LIB)(msname.o): msname.c $(WC)/idsistd.h
$(THE_LIB)(msname.o): $(WC)/intdef.h $(WC)/werrlog.h
$(THE_LIB)(mstime.o): mstime.c $(WC)/idsistd.h
$(THE_LIB)(mstime.o): $(WC)/intdef.h
$(THE_LIB)(msvalue.o): msvalue.c $(WC)/idsistd.h
$(THE_LIB)(msvalue.o): $(WC)/intdef.h
$(THE_LIB)(mvspaces.o): mvspaces.c $(WC)/idsistd.h
$(THE_LIB)(mvspaces.o): $(WC)/intdef.h
$(THE_LIB)(mwconv.o): mwconv.c $(WC)/idsistd.h
$(THE_LIB)(mwconv.o): $(WC)/intdef.h cobrun.h
$(THE_LIB)(nextfile.o): nextfile.c $(WC)/idsistd.h
$(THE_LIB)(nextfile.o): $(WC)/intdef.h $(WC)/wdefines.h
$(THE_LIB)(noconfig.o): noconfig.c
$(THE_LIB)(onhelp.o): onhelp.c $(WC)/idsistd.h
$(THE_LIB)(onhelp.o): $(WC)/intdef.h $(WC)/wperson.h
$(THE_LIB)(openacp.o): openacp.c $(WC)/idsistd.h
$(THE_LIB)(openacp.o): $(WC)/intdef.h $(WC)/wispcfg.h
$(THE_LIB)(openacp.o): $(WC)/paths.h acp.h
$(THE_LIB)(openacp.o): $(WC)/wmalloc.h
$(THE_LIB)(operator.o): operator.c
$(THE_LIB)(packer.o): packer.c $(WC)/idsistd.h
$(THE_LIB)(packer.o): $(WC)/intdef.h
$(THE_LIB)(paths.o): paths.c $(WC)/idsisubs.h
$(THE_LIB)(paths.o): $(WC)/idsistd.h $(WC)/intdef.h
$(THE_LIB)(paths.o): $(WC)/paths.h $(WC)/wispcfg.h
$(THE_LIB)(paths.o): $(WC)/wmalloc.h
$(THE_LIB)(platsubs.o): platsubs.c $(WC)/idsistd.h
$(THE_LIB)(platsubs.o): $(WC)/intdef.h wplatdef.h
$(THE_LIB)(platsubs.o): $(WC)/platsubs.h
$(THE_LIB)(print.o): print.c $(WC)/idsistd.h
$(THE_LIB)(print.o): $(WC)/intdef.h $(WC)/wcommon.h
$(THE_LIB)(print.o): $(WC)/wperson.h movebin.h
$(THE_LIB)(print.o): $(WC)/werrlog.h $(WC)/wfname.h
$(THE_LIB)(print.o): $(WC)/wisplib.h $(WC)/setenvst.h
$(THE_LIB)(print.o): $(WC)/filext.h
$(THE_LIB)(protect.o): protect.c $(WC)/idsistd.h
$(THE_LIB)(protect.o): $(WC)/intdef.h
$(THE_LIB)(putparm.o): putparm.c $(WC)/idsistd.h
$(THE_LIB)(putparm.o): $(WC)/intdef.h movebin.h
$(THE_LIB)(putparm.o): $(WC)/werrlog.h $(WC)/sharemem.h
$(THE_LIB)(putparm.o): $(WC)/putparm.h $(WC)/wdefines.h
$(THE_LIB)(putparm.o): $(WC)/wglobals.h $(WC)/wfiles.h
$(THE_LIB)(putparm.o): $(WC)/wisplib.h $(WC)/setenvst.h
$(THE_LIB)(putparm.o): $(WC)/idsisubs.h $(WC)/wmalloc.h
$(THE_LIB)(pwdname.o): pwdname.c $(WC)/idsistd.h
$(THE_LIB)(pwdname.o): $(WC)/intdef.h $(WC)/wisplib.h
$(THE_LIB)(pwdname.o): $(WC)/setenvst.h
$(THE_LIB)(que_jobs.o): que_jobs.c $(WC)/idsistd.h
$(THE_LIB)(que_jobs.o): $(WC)/intdef.h $(WC)/werrlog.h
$(THE_LIB)(quemgmt.o): quemgmt.c $(WC)/werrlog.h
$(THE_LIB)(quemgmt.o): $(WC)/idsistd.h $(WC)/intdef.h
$(THE_LIB)(readacp.o): readacp.c $(WC)/idsistd.h
$(THE_LIB)(readacp.o): $(WC)/intdef.h acp.h movebin.h
$(THE_LIB)(readfdr.o): readfdr.c $(WC)/idsistd.h
$(THE_LIB)(readfdr.o): $(WC)/intdef.h $(WC)/wcommon.h
$(THE_LIB)(readfdr.o): movebin.h cobrun.h $(WC)/werrlog.h
$(THE_LIB)(readfdr.o): $(WC)/wisplib.h $(WC)/setenvst.h
$(THE_LIB)(readfdr.o): $(WC)/wfname.h $(WC)/idsisubs.h
$(THE_LIB)(readfdr.o): $(WC)/filext.h
$(THE_LIB)(readvtoc.o): readvtoc.c $(WC)/idsistd.h
$(THE_LIB)(readvtoc.o): $(WC)/intdef.h $(WC)/werrlog.h
$(THE_LIB)(readvtoc.o): movebin.h $(WC)/wfname.h
$(THE_LIB)(readvtoc.o): $(WC)/wisplib.h
$(THE_LIB)(readvtoc.o): $(WC)/setenvst.h
$(THE_LIB)(regexpr.o): regexpr.c $(WC)/idsistd.h
$(THE_LIB)(regexpr.o): $(WC)/intdef.h regexpr.h
$(THE_LIB)(rename.o): rename.c $(WC)/idsistd.h
$(THE_LIB)(rename.o): $(WC)/intdef.h $(WC)/wfname.h
$(THE_LIB)(rename.o): $(WC)/wfiles.h $(WC)/wcommon.h
$(THE_LIB)(rename.o): movebin.h $(WC)/wperson.h
$(THE_LIB)(rename.o): $(WC)/werrlog.h cobrun.h
$(THE_LIB)(rename.o): $(WC)/idsisubs.h $(WC)/wisplib.h
$(THE_LIB)(rename.o): $(WC)/setenvst.h $(WC)/filext.h
$(THE_LIB)(retcode.o): retcode.c $(WC)/idsistd.h
$(THE_LIB)(retcode.o): $(WC)/intdef.h $(WC)/idsisubs.h
$(THE_LIB)(retcode.o): $(WC)/werrlog.h $(WC)/wdefines.h
$(THE_LIB)(retcode.o): $(WC)/wisplib.h $(WC)/setenvst.h
$(THE_LIB)(retcode.o): $(WC)/paths.h $(WC)/filext.h
$(THE_LIB)(retcode.o): $(WC)/wperson.h $(WC)/wmalloc.h
$(THE_LIB)(rtransl.o): rtransl.c $(WC)/idsistd.h
$(THE_LIB)(rtransl.o): $(WC)/intdef.h
$(THE_LIB)(runtype.o): runtype.c $(WC)/idsistd.h
$(THE_LIB)(runtype.o): $(WC)/intdef.h runtype.h
$(THE_LIB)(runtype.o): $(WC)/wdefines.h
$(THE_LIB)(runtype.o): $(WC)/idsisubs.h $(WC)/wisplib.h
$(THE_LIB)(runtype.o): $(WC)/setenvst.h
$(THE_LIB)(rvmap.o): rvmap.c $(WC)/idsistd.h
$(THE_LIB)(rvmap.o): $(WC)/intdef.h assert.h
$(THE_LIB)(rvmap.o): $(WC)/wmalloc.h rvmap.h
$(THE_LIB)(rvmap.o): $(WC)/wdefines.h $(WC)/werrlog.h
$(THE_LIB)(rvmap.o): $(WC)/wperson.h
$(THE_LIB)(scratch.o): scratch.c $(WC)/idsistd.h
$(THE_LIB)(scratch.o): $(WC)/intdef.h $(WC)/wcommon.h
$(THE_LIB)(scratch.o): $(WC)/wdefines.h movebin.h cobrun.h
$(THE_LIB)(scratch.o): $(WC)/idsisubs.h $(WC)/wisplib.h
$(THE_LIB)(scratch.o): $(WC)/setenvst.h $(WC)/wfname.h
$(THE_LIB)(scratch.o): $(WC)/filext.h $(WC)/wispcfg.h
$(THE_LIB)(screen.o): screen.c $(WC)/idsistd.h
$(THE_LIB)(screen.o): $(WC)/intdef.h $(WC)/vwang.h
$(THE_LIB)(screen.o): $(WC)/wcommon.h $(WC)/werrlog.h
$(THE_LIB)(screen.o): $(WC)/wperson.h $(WC)/wdefines.h
$(THE_LIB)(screen.o): movebin.h $(WC)/wanguid.h
$(THE_LIB)(screen.o): $(WC)/wfname.h $(WC)/wisplib.h
$(THE_LIB)(screen.o): $(WC)/setenvst.h $(WC)/wmalloc.h
$(THE_LIB)(search.o): search.c $(WC)/idsistd.h
$(THE_LIB)(search.o): $(WC)/intdef.h movebin.h
$(THE_LIB)(search.o): $(WC)/werrlog.h $(WC)/wdefines.h
$(THE_LIB)(search.o): $(WC)/wisplib.h $(WC)/setenvst.h
$(THE_LIB)(set.o): set.c $(WC)/idsistd.h $(WC)/intdef.h
$(THE_LIB)(set.o): $(WC)/wperson.h $(WC)/werrlog.h
$(THE_LIB)(set.o): $(WC)/wglobals.h $(WC)/wfiles.h
$(THE_LIB)(set.o): $(WC)/wisplib.h $(WC)/setenvst.h
$(THE_LIB)(setenvst.o): setenvst.c $(WC)/setenvst.h
$(THE_LIB)(setenvst.o): $(WC)/wmalloc.h
$(THE_LIB)(setfile.o): setfile.c $(WC)/idsistd.h
$(THE_LIB)(setfile.o): $(WC)/intdef.h $(WC)/wfiles.h
$(THE_LIB)(setfile.o): $(WC)/wisplib.h $(WC)/setenvst.h
$(THE_LIB)(setfile.o): $(WC)/werrlog.h $(WC)/wfname.h
$(THE_LIB)(setfile.o): $(WC)/idsisubs.h
$(THE_LIB)(setprgid.o): setprgid.c $(WC)/idsistd.h
$(THE_LIB)(setprgid.o): $(WC)/intdef.h $(WC)/wglobals.h
$(THE_LIB)(setprgid.o): $(WC)/wfiles.h
$(THE_LIB)(setrcode.o): setrcode.c
$(THE_LIB)(setsymb.o): setsymb.c $(WC)/idsistd.h
$(THE_LIB)(setsymb.o): $(WC)/intdef.h
$(THE_LIB)(settrigp.o): settrigp.c $(WC)/idsistd.h
$(THE_LIB)(settrigp.o): $(WC)/intdef.h $(WC)/wfiles.h
$(THE_LIB)(settrigp.o): $(WC)/wcommon.h $(WC)/werrlog.h
$(THE_LIB)(settrigp.o): $(WC)/wfname.h $(WC)/filext.h
$(THE_LIB)(setupqm.o): setupqm.c
$(THE_LIB)(sharemem.o): sharemem.c $(WC)/idsistd.h
$(THE_LIB)(sharemem.o): $(WC)/intdef.h $(WC)/werrlog.h
$(THE_LIB)(sharemem.o): $(WC)/sharemem.h
$(THE_LIB)(sharemem.o): $(WC)/putparm.h
$(THE_LIB)(sharemem.o): $(WC)/wdefines.h
$(THE_LIB)(sharemem.o): $(WC)/wanguid.h $(WC)/wisplib.h
$(THE_LIB)(sharemem.o): $(WC)/setenvst.h
$(THE_LIB)(sharemem.o): $(WC)/idsisubs.h $(WC)/level.h
$(THE_LIB)(sharemem.o): $(WC)/wexit.h $(WC)/wperson.h
$(THE_LIB)(sharemem.o): osddefs.h $(WC)/wmalloc.h
$(THE_LIB)(shellvar.o): shellvar.c
$(THE_LIB)(shutexit.o): shutexit.c $(WC)/idsistd.h
$(THE_LIB)(shutexit.o): $(WC)/intdef.h
$(THE_LIB)(sort.o): sort.c $(WC)/idsistd.h
$(THE_LIB)(sort.o): $(WC)/intdef.h $(WC)/werrlog.h
$(THE_LIB)(sort.o): $(WC)/wisplib.h $(WC)/setenvst.h
$(THE_LIB)(sortcall.o): sortcall.c $(WC)/idsistd.h
$(THE_LIB)(sortcall.o): $(WC)/intdef.h $(WC)/idsisubs.h
$(THE_LIB)(sortcall.o): $(WC)/wcommon.h $(WC)/werrlog.h
$(THE_LIB)(sortcall.o): $(WC)/wisplib.h
$(THE_LIB)(sortcall.o): $(WC)/setenvst.h
$(THE_LIB)(sortlink.o): sortlink.c $(WC)/idsistd.h
$(THE_LIB)(sortlink.o): $(WC)/intdef.h movebin.h
$(THE_LIB)(sortlink.o): $(WC)/werrlog.h cobrun.h
$(THE_LIB)(sortlink.o): $(WC)/wisplib.h
$(THE_LIB)(sortlink.o): $(WC)/setenvst.h
$(THE_LIB)(sortseqf.o): sortseqf.c $(WC)/idsistd.h
$(THE_LIB)(sortseqf.o): $(WC)/intdef.h $(WC)/sortseqf.h
$(THE_LIB)(sortseqf.o): $(WC)/idsisubs.h
$(THE_LIB)(sortseqf.o): $(WC)/wisplib.h
$(THE_LIB)(sortseqf.o): $(WC)/setenvst.h
$(THE_LIB)(sortseqf.o): $(WC)/wispcfg.h
$(THE_LIB)(sortx.o): sortx.c
$(THE_LIB)(spawn.o): spawn.c $(WC)/idsistd.h
$(THE_LIB)(spawn.o): $(WC)/intdef.h
$(THE_LIB)(string.o): string.c $(WC)/idsistd.h
$(THE_LIB)(string.o): $(WC)/intdef.h movebin.h
$(THE_LIB)(string.o): $(WC)/werrlog.h $(WC)/wisplib.h
$(THE_LIB)(string.o): $(WC)/setenvst.h
$(THE_LIB)(submit.o): submit.c $(WC)/idsistd.h
$(THE_LIB)(submit.o): $(WC)/intdef.h que_jobs.h
$(THE_LIB)(submit.o): $(WC)/wcommon.h $(WC)/wperson.h
$(THE_LIB)(submit.o): movebin.h $(WC)/werrlog.h
$(THE_LIB)(submit.o): $(WC)/wdefines.h $(WC)/wglobals.h
$(THE_LIB)(submit.o): $(WC)/wfiles.h $(WC)/filext.h
$(THE_LIB)(submit.o): $(WC)/wisplib.h $(WC)/setenvst.h
$(THE_LIB)(submit.o): $(WC)/wfname.h $(WC)/wispcfg.h
$(THE_LIB)(submit.o): $(WC)/level.h $(WC)/wexit.h
$(THE_LIB)(submit.o): $(WC)/wrunconf.h runtype.h
$(THE_LIB)(sysdev.o): sysdev.c
$(THE_LIB)(sysinf.o): sysinf.c $(WC)/idsistd.h
$(THE_LIB)(sysinf.o): $(WC)/intdef.h $(WC)/wglobals.h
$(THE_LIB)(sysinf.o): $(WC)/wfiles.h $(WC)/werrlog.h
$(THE_LIB)(untabify.o): untabify.c $(WC)/idsistd.h
$(THE_LIB)(untabify.o): $(WC)/intdef.h
$(THE_LIB)(updatfdr.o): updatfdr.c $(WC)/idsistd.h
$(THE_LIB)(updatfdr.o): $(WC)/intdef.h $(WC)/werrlog.h
$(THE_LIB)(upper.o): upper.c $(WC)/idsistd.h
$(THE_LIB)(upper.o): $(WC)/intdef.h movebin.h
$(THE_LIB)(upper.o): $(WC)/wisplib.h $(WC)/setenvst.h
$(THE_LIB)(vdispidx.o): vdispidx.c
$(THE_LIB)(vdisplay.o): vdisplay.c $(VC)/video.h
$(THE_LIB)(vdisplay.o): $(VC)/vlocal.h $(VC)/vdata.h
$(THE_LIB)(vdisplay.o): $(VC)/vcap.h $(WC)/idsistd.h
$(THE_LIB)(vdisplay.o): $(WC)/intdef.h $(WC)/werrlog.h
$(THE_LIB)(vdisplay.o): $(WC)/wperson.h $(WC)/scnfacs.h
$(THE_LIB)(vdisplay.o): $(WC)/wanguid.h
$(THE_LIB)(vdisplay.o): $(WC)/idsisubs.h costar.h
$(THE_LIB)(vdisplay.o): $(WC)/vwang.h osddefs.h
$(THE_LIB)(vdisplay.o): $(WC)/wexit.h $(WC)/wisplib.h
$(THE_LIB)(vdisplay.o): $(WC)/setenvst.h $(WC)/filext.h
$(THE_LIB)(vdisplay.o): $(WC)/wispcfg.h
$(THE_LIB)(vmspargs.o): vmspargs.c $(WC)/idsistd.h
$(THE_LIB)(vmspargs.o): $(WC)/intdef.h $(WC)/wglobals.h
$(THE_LIB)(vmspargs.o): $(WC)/wfiles.h $(WC)/wdefines.h
$(THE_LIB)(vmspargs.o): $(WC)/werrlog.h $(WC)/link.h
$(THE_LIB)(vmspargs.o): $(WC)/wexit.h
$(THE_LIB)(vmssort.o): vmssort.c
$(THE_LIB)(vssort.o): vssort.c $(WC)/idsistd.h
$(THE_LIB)(vssort.o): $(WC)/intdef.h $(WC)/idsisubs.h
$(THE_LIB)(vssort.o): $(WC)/vssort.h vmssort.h
$(THE_LIB)(vssort.o): $(WC)/sortseqf.h $(WC)/wfname.h
$(THE_LIB)(vssort.o): $(WC)/wisplib.h $(WC)/setenvst.h
$(THE_LIB)(vwang.o): vwang.c $(VC)/video.h $(VC)/vlocal.h
$(THE_LIB)(vwang.o): $(VC)/vdata.h $(VC)/vcap.h
$(THE_LIB)(vwang.o): $(VC)/vchinese.h $(WC)/idsistd.h
$(THE_LIB)(vwang.o): $(WC)/intdef.h cobrun.h
$(THE_LIB)(vwang.o): $(WC)/wperson.h $(WC)/vwang.h
$(THE_LIB)(vwang.o): $(WC)/wglobals.h $(WC)/wfiles.h
$(THE_LIB)(vwang.o): $(WC)/scnfacs.h $(WC)/wisplib.h
$(THE_LIB)(vwang.o): $(WC)/setenvst.h $(WC)/wexit.h
$(THE_LIB)(vwang.o): assert.h wsfns.h menu.h $(WC)/wmalloc.h
$(THE_LIB)(vwang.o): costar.h $(WC)/wispcfg.h
$(THE_LIB)(vwang.o): $(WC)/werrlog.h
$(THE_LIB)(waccept.o): waccept.c $(WC)/idsistd.h
$(THE_LIB)(waccept.o): $(WC)/intdef.h $(WC)/wisplib.h
$(THE_LIB)(waccept.o): $(WC)/setenvst.h
$(THE_LIB)(wangmenu.o): wangmenu.c $(WC)/idsistd.h
$(THE_LIB)(wangmenu.o): $(WC)/intdef.h menu.h
$(THE_LIB)(wanguid.o): wanguid.c $(WC)/idsistd.h
$(THE_LIB)(wanguid.o): $(WC)/intdef.h $(WC)/wdefines.h
$(THE_LIB)(wanguid.o): $(WC)/wanguid.h $(WC)/wglobals.h
$(THE_LIB)(wanguid.o): $(WC)/wfiles.h $(WC)/idsisubs.h
$(THE_LIB)(wanguid.o): $(WC)/wisplib.h $(WC)/setenvst.h
$(THE_LIB)(wassert.o): wassert.c assert.h $(WC)/werrlog.h
$(THE_LIB)(wassert.o): $(WC)/idsistd.h $(WC)/intdef.h
$(THE_LIB)(wassert.o): $(WC)/wexit.h
$(THE_LIB)(wauthsub.o): wauthsub.c $(WC)/idsistd.h
$(THE_LIB)(wauthsub.o): $(WC)/intdef.h $(WC)/wlicense.h
$(THE_LIB)(wauthsub.o): $(WC)/platsubs.h
$(THE_LIB)(wauthsub.o): $(WC)/idsisubs.h $(WC)/machid.h
$(THE_LIB)(wauthsub.o): $(WC)/wispcfg.h
$(THE_LIB)(wchain.o): wchain.c $(WC)/idsistd.h
$(THE_LIB)(wchain.o): $(WC)/intdef.h $(WC)/werrlog.h
$(THE_LIB)(wchain.o): $(WC)/wcommon.h $(WC)/wdefines.h
$(THE_LIB)(wchain.o): $(WC)/wfname.h $(WC)/filext.h
$(THE_LIB)(wchain.o): $(WC)/wrunconf.h
$(THE_LIB)(wcmatch.o): wcmatch.c $(WC)/idsistd.h
$(THE_LIB)(wcmatch.o): $(WC)/intdef.h $(WC)/wdefines.h
$(THE_LIB)(wdelfile.o): wdelfile.c $(WC)/idsistd.h
$(THE_LIB)(wdelfile.o): $(WC)/intdef.h
$(THE_LIB)(wdellock.o): wdellock.c
$(THE_LIB)(wdfinish.o): wdfinish.c $(VC)/video.h
$(THE_LIB)(wdfinish.o): $(WC)/idsistd.h $(WC)/intdef.h
$(THE_LIB)(wdfinish.o): $(WC)/vwang.h
$(THE_LIB)(wdinit.o): wdinit.c $(VC)/video.h
$(THE_LIB)(wdinit.o): $(WC)/idsistd.h $(WC)/intdef.h
$(THE_LIB)(wdinit.o): $(WC)/vwang.h
$(THE_LIB)(wdisplay.o): wdisplay.c $(WC)/idsistd.h
$(THE_LIB)(wdisplay.o): $(WC)/intdef.h $(WC)/vwang.h
$(THE_LIB)(werrlog.o): werrlog.c $(WC)/idsistd.h
$(THE_LIB)(werrlog.o): $(WC)/intdef.h $(WC)/wperson.h
$(THE_LIB)(werrlog.o): $(WC)/wdefines.h
$(THE_LIB)(werrlog.o): $(WC)/wglobals.h $(WC)/wfiles.h
$(THE_LIB)(werrlog.o): osddefs.h $(WC)/wisplib.h
$(THE_LIB)(werrlog.o): $(WC)/setenvst.h
$(THE_LIB)(werrlog.o): $(WC)/idsisubs.h $(WC)/wispcfg.h
$(THE_LIB)(werrlog.o): $(WC)/werrlog.h
$(THE_LIB)(werrpath.o): werrpath.c $(WC)/idsistd.h
$(THE_LIB)(werrpath.o): $(WC)/intdef.h $(WC)/wdefines.h
$(THE_LIB)(werrpath.o): $(WC)/wglobals.h $(WC)/wfiles.h
$(THE_LIB)(werrpath.o): $(WC)/idsisubs.h
$(THE_LIB)(werrpath.o): $(WC)/wperson.h $(WC)/wmalloc.h
$(THE_LIB)(werrpath.o): $(WC)/wispcfg.h
$(THE_LIB)(werrvre.o): werrvre.c $(WC)/idsistd.h
$(THE_LIB)(werrvre.o): $(WC)/intdef.h $(WC)/vwang.h
$(THE_LIB)(werrvre.o): $(WC)/werrlog.h
$(THE_LIB)(wexit.o): wexit.c $(WC)/idsistd.h
$(THE_LIB)(wexit.o): $(WC)/intdef.h $(WC)/werrlog.h
$(THE_LIB)(wexit.o): $(WC)/wglobals.h $(WC)/wfiles.h
$(THE_LIB)(wexit.o): $(WC)/wexit.h cobrun.h
$(THE_LIB)(wexit.o): $(WC)/vwang.h $(WC)/wisplib.h
$(THE_LIB)(wexit.o): $(WC)/setenvst.h
$(THE_LIB)(wexith.o): wexith.c $(WC)/idsistd.h
$(THE_LIB)(wexith.o): $(WC)/intdef.h que_jobs.h
$(THE_LIB)(wexith.o): $(WC)/wperson.h $(WC)/wfiles.h
$(THE_LIB)(wexith.o): $(WC)/wglobals.h $(WC)/filext.h
$(THE_LIB)(wexith.o): $(WC)/level.h $(WC)/vwang.h
$(THE_LIB)(wexith.o): $(WC)/sharemem.h $(WC)/wisplib.h
$(THE_LIB)(wexith.o): $(WC)/setenvst.h
$(THE_LIB)(wfaccess.o): wfaccess.c $(WC)/idsistd.h
$(THE_LIB)(wfaccess.o): $(WC)/intdef.h $(WC)/wdefines.h
$(THE_LIB)(wfaccess.o): $(WC)/wcommon.h wfaccess.h cobrun.h
$(THE_LIB)(wfaccess.o): $(WC)/idsisubs.h
$(THE_LIB)(wfaccess.o): $(WC)/wisplib.h
$(THE_LIB)(wfaccess.o): $(WC)/setenvst.h
$(THE_LIB)(wfcisam.o): wfcisam.c $(WC)/idsistd.h
$(THE_LIB)(wfcisam.o): $(WC)/intdef.h $(WC)/wisplib.h
$(THE_LIB)(wfcisam.o): $(WC)/setenvst.h osddefs.h
$(THE_LIB)(wfclose.o): wfclose.c $(WC)/idsistd.h
$(THE_LIB)(wfclose.o): $(WC)/intdef.h $(WC)/wperson.h
$(THE_LIB)(wfclose.o): $(WC)/wfiles.h $(WC)/wisplib.h
$(THE_LIB)(wfclose.o): $(WC)/setenvst.h
$(THE_LIB)(wfclose.o): $(WC)/idsisubs.h
$(THE_LIB)(wfilechk.o): wfilechk.c $(WC)/idsistd.h
$(THE_LIB)(wfilechk.o): $(WC)/intdef.h $(WC)/werrlog.h
$(THE_LIB)(wfilechk.o): $(WC)/wcommon.h cobrun.h
$(THE_LIB)(wfilechk.o): $(WC)/wdefines.h
$(THE_LIB)(wfilechk.o): $(WC)/wglobals.h $(WC)/wfiles.h
$(THE_LIB)(wfilechk.o): $(WC)/wisplib.h
$(THE_LIB)(wfilechk.o): $(WC)/setenvst.h
$(THE_LIB)(wfilechk.o): $(WC)/idsisubs.h $(WC)/wexit.h
$(THE_LIB)(wfiledis.o): wfiledis.c $(WC)/idsistd.h
$(THE_LIB)(wfiledis.o): $(WC)/intdef.h $(WC)/wperson.h
$(THE_LIB)(wfiledis.o): $(WC)/wfiles.h $(WC)/wcommon.h
$(THE_LIB)(wfiledis.o): $(WC)/wangkeys.h $(WC)/wfname.h
$(THE_LIB)(wfiledis.o): $(WC)/werrlog.h $(WC)/level.h
$(THE_LIB)(wfiledis.o): $(WC)/wisplib.h
$(THE_LIB)(wfiledis.o): $(WC)/setenvst.h
$(THE_LIB)(wfiledis.o): $(WC)/sharemem.h $(WC)/vwang.h
$(THE_LIB)(wfiledis.o): $(WC)/filext.h
$(THE_LIB)(wfileext.o): wfileext.c $(WC)/idsistd.h
$(THE_LIB)(wfileext.o): $(WC)/intdef.h $(WC)/filext.h
$(THE_LIB)(wfname.o): wfname.c $(WC)/idsistd.h
$(THE_LIB)(wfname.o): $(WC)/intdef.h $(WC)/wperson.h
$(THE_LIB)(wfname.o): $(WC)/wfiles.h $(WC)/wcommon.h
$(THE_LIB)(wfname.o): $(WC)/werrlog.h cobrun.h
$(THE_LIB)(wfname.o): $(WC)/wfname.h $(WC)/wdefines.h
$(THE_LIB)(wfname.o): $(WC)/wisplib.h $(WC)/setenvst.h
$(THE_LIB)(wfname.o): $(WC)/idsisubs.h $(WC)/paths.h
$(THE_LIB)(wfname.o): $(WC)/filext.h $(WC)/wispcfg.h
$(THE_LIB)(wfopen.o): wfopen.c $(WC)/idsistd.h
$(THE_LIB)(wfopen.o): $(WC)/intdef.h $(WC)/wperson.h
$(THE_LIB)(wfopen.o): $(WC)/wangkeys.h $(WC)/wcommon.h
$(THE_LIB)(wfopen.o): wfaccess.h cobrun.h $(WC)/wglobals.h
$(THE_LIB)(wfopen.o): $(WC)/wfiles.h $(WC)/filext.h
$(THE_LIB)(wfopen.o): $(WC)/wmalloc.h assert.h
$(THE_LIB)(wfopen.o): $(WC)/wdefines.h rvmap.h
$(THE_LIB)(wfopen.o): $(WC)/wisplib.h $(WC)/setenvst.h
$(THE_LIB)(wfopen.o): $(WC)/wexit.h $(WC)/idsisubs.h
$(THE_LIB)(wfopen.o): $(WC)/wfname.h $(WC)/werrlog.h
$(THE_LIB)(wfstatus.o): wfstatus.c $(WC)/idsistd.h
$(THE_LIB)(wfstatus.o): $(WC)/intdef.h
$(THE_LIB)(wftok.o): wftok.c $(WC)/idsistd.h
$(THE_LIB)(wftok.o): $(WC)/intdef.h
$(THE_LIB)(wfvision.o): wfvision.c $(WC)/idsistd.h
$(THE_LIB)(wfvision.o): $(WC)/intdef.h $(WC)/wisplib.h
$(THE_LIB)(wfvision.o): $(WC)/setenvst.h visint.h visn2.h
$(THE_LIB)(wfwait.o): wfwait.c $(WC)/idsistd.h
$(THE_LIB)(wfwait.o): $(WC)/intdef.h $(WC)/wglobals.h
$(THE_LIB)(wfwait.o): $(WC)/wfiles.h $(WC)/wisplib.h
$(THE_LIB)(wfwait.o): $(WC)/setenvst.h
$(THE_LIB)(wgetpgrp.o): wgetpgrp.c $(WC)/idsistd.h
$(THE_LIB)(wgetpgrp.o): $(WC)/intdef.h $(WC)/wdefines.h
$(THE_LIB)(wgetpgrp.o): $(WC)/werrlog.h $(WC)/wexit.h
$(THE_LIB)(wgetpgrp.o): $(WC)/wisplib.h
$(THE_LIB)(wgetpgrp.o): $(WC)/setenvst.h
$(THE_LIB)(wglobals.o): wglobals.c $(WC)/idsistd.h
$(THE_LIB)(wglobals.o): $(WC)/intdef.h $(WC)/wglobals.h
$(THE_LIB)(wglobals.o): $(WC)/wfiles.h cobrun.h
$(THE_LIB)(win32err.o): win32err.c
$(THE_LIB)(win32msg.o): win32msg.c
$(THE_LIB)(win32prt.o): win32prt.c
$(THE_LIB)(win32spn.o): win32spn.c
$(THE_LIB)(winnt.o): winnt.c $(WC)/wispnt.h
$(THE_LIB)(wispcfg.o): wispcfg.c $(WC)/wispcfg.h
$(THE_LIB)(wispcfg.o): $(WC)/wmalloc.h $(WC)/idsisubs.h
$(THE_LIB)(wispcfg.o): $(WC)/idsistd.h $(WC)/intdef.h
$(THE_LIB)(wispcfg.o): $(WC)/setenvst.h assert.h $(WC)/wispnt.h
$(THE_LIB)(wispcfg.o): $(WC)/wisplib.h
$(THE_LIB)(wispexit.o): wispexit.c $(WC)/wisplib.h
$(THE_LIB)(wispexit.o): $(WC)/intdef.h $(WC)/setenvst.h
$(THE_LIB)(wispsort.o): wispsort.c $(WC)/idsistd.h
$(THE_LIB)(wispsort.o): $(WC)/intdef.h $(WC)/wcommon.h
$(THE_LIB)(wispsort.o): $(WC)/sortseqf.h $(WC)/wfname.h
$(THE_LIB)(wispsort.o): movebin.h osddefs.h $(WC)/wisplib.h
$(THE_LIB)(wispsort.o): $(WC)/setenvst.h
$(THE_LIB)(wispsort.o): $(WC)/wmalloc.h $(WC)/vwang.h
$(THE_LIB)(wispsort.o): $(WC)/wispcfg.h $(WC)/werrlog.h
$(THE_LIB)(wispsync.o): wispsync.c $(WC)/idsistd.h
$(THE_LIB)(wispsync.o): $(WC)/intdef.h $(WC)/vwang.h
$(THE_LIB)(wispvers.o): wispvers.c $(WC)/wcommon.h
$(THE_LIB)(wispvers.o): $(WC)/wispvers.h
$(THE_LIB)(wlickey.o): wlickey.c $(WC)/idsistd.h
$(THE_LIB)(wlickey.o): $(WC)/intdef.h $(WC)/wlicense.h
$(THE_LIB)(wmalloc.o): wmalloc.c $(WC)/werrlog.h
$(THE_LIB)(wmalloc.o): $(WC)/idsistd.h $(WC)/intdef.h
$(THE_LIB)(wmalloc.o): $(WC)/wexit.h $(WC)/wmalloc.h
$(THE_LIB)(wpause.o): wpause.c $(WC)/idsistd.h
$(THE_LIB)(wpause.o): $(WC)/intdef.h movebin.h
$(THE_LIB)(wpause.o): $(WC)/werrlog.h $(WC)/wisplib.h
$(THE_LIB)(wpause.o): $(WC)/setenvst.h $(WC)/wispnt.h
$(THE_LIB)(wperson.o): wperson.c $(WC)/idsistd.h
$(THE_LIB)(wperson.o): $(WC)/intdef.h $(WC)/wperson.h
$(THE_LIB)(wperson.o): $(WC)/werrlog.h $(WC)/wdefines.h
$(THE_LIB)(wperson.o): $(WC)/wglobals.h $(WC)/wfiles.h
$(THE_LIB)(wperson.o): $(WC)/wanguid.h $(WC)/vwang.h
$(THE_LIB)(wperson.o): osddefs.h $(WC)/wexit.h
$(THE_LIB)(wperson.o): $(WC)/idsisubs.h $(WC)/wisplib.h
$(THE_LIB)(wperson.o): $(WC)/setenvst.h
$(THE_LIB)(wperson.o): $(WC)/wrunconf.h $(WC)/wmalloc.h
$(THE_LIB)(wperson.o): $(WC)/paths.h $(WC)/wispnt.h
$(THE_LIB)(wperson.o): $(WC)/link.h $(WC)/wispcfg.h
$(THE_LIB)(wprint.o): wprint.c $(WC)/idsistd.h
$(THE_LIB)(wprint.o): $(WC)/intdef.h que_jobs.h
$(THE_LIB)(wprint.o): $(WC)/wperson.h $(WC)/werrlog.h
$(THE_LIB)(wprint.o): $(WC)/wglobals.h $(WC)/wfiles.h
$(THE_LIB)(writeacp.o): writeacp.c $(WC)/idsistd.h
$(THE_LIB)(writeacp.o): $(WC)/intdef.h acp.h
$(THE_LIB)(wrunconf.o): wrunconf.c $(WC)/idsistd.h
$(THE_LIB)(wrunconf.o): $(WC)/intdef.h $(WC)/wrunconf.h
$(THE_LIB)(wrunconf.o): $(WC)/wdefines.h
$(THE_LIB)(wrunconf.o): $(WC)/wisplib.h
$(THE_LIB)(wrunconf.o): $(WC)/setenvst.h
$(THE_LIB)(wrunconf.o): $(WC)/idsisubs.h
$(THE_LIB)(wrunconf.o): $(WC)/wmalloc.h $(WC)/wispcfg.h
$(THE_LIB)(wsb.o): wsb.c wsb.h $(WC)/vwang.h
$(THE_LIB)(wsclose.o): wsclose.c $(WC)/idsistd.h
$(THE_LIB)(wsclose.o): $(WC)/intdef.h $(WC)/vwang.h
$(THE_LIB)(wscreen.o): wscreen.c $(WC)/idsistd.h
$(THE_LIB)(wscreen.o): $(WC)/intdef.h $(WC)/wcommon.h
$(THE_LIB)(wscreen.o): movebin.h cobrun.h $(WC)/werrlog.h
$(THE_LIB)(wscreen.o): $(WC)/wexit.h $(WC)/vwang.h
$(THE_LIB)(wscreen.o): cobpic.h $(WC)/wisplib.h
$(THE_LIB)(wscreen.o): $(WC)/setenvst.h
$(THE_LIB)(wsfnm.o): wsfnm.c $(WC)/idsistd.h
$(THE_LIB)(wsfnm.o): $(WC)/intdef.h $(WC)/vwang.h
$(THE_LIB)(wsfnm.o): $(WC)/werrlog.h $(WC)/wglobals.h
$(THE_LIB)(wsfnm.o): $(WC)/wfiles.h $(WC)/wisplib.h
$(THE_LIB)(wsfnm.o): $(WC)/setenvst.h wsfns.h
$(THE_LIB)(wsfns.o): wsfns.c $(WC)/idsistd.h
$(THE_LIB)(wsfns.o): $(WC)/intdef.h $(WC)/vwang.h
$(THE_LIB)(wsfns.o): $(WC)/werrlog.h $(WC)/wglobals.h
$(THE_LIB)(wsfns.o): $(WC)/wfiles.h wsfns.h
$(THE_LIB)(wsfns.o): $(WC)/wisplib.h $(WC)/setenvst.h
$(THE_LIB)(wsfns.o): $(WC)/wexit.h $(WC)/level.h
$(THE_LIB)(wshelp.o): wshelp.c osddefs.h $(VC)/video.h
$(THE_LIB)(wshelp.o): $(VC)/vlocal.h $(VC)/vcap.h
$(THE_LIB)(wshelp.o): $(VC)/vchinese.h $(WC)/idsistd.h
$(THE_LIB)(wshelp.o): $(WC)/intdef.h $(WC)/wcommon.h
$(THE_LIB)(wshelp.o): $(WC)/wperson.h $(WC)/vwang.h
$(THE_LIB)(wshelp.o): $(WC)/scnfacs.h $(WC)/wglobals.h
$(THE_LIB)(wshelp.o): $(WC)/wfiles.h $(WC)/werrlog.h
$(THE_LIB)(wshelp.o): $(WC)/wdefines.h cobrun.h
$(THE_LIB)(wshelp.o): $(WC)/wanguid.h $(WC)/wispvers.h
$(THE_LIB)(wshelp.o): $(WC)/wisplib.h $(WC)/setenvst.h
$(THE_LIB)(wshelp.o): $(WC)/level.h $(WC)/idsisubs.h
$(THE_LIB)(wshelp.o): $(WC)/wexit.h $(WC)/sharemem.h
$(THE_LIB)(wshelp.o): $(WC)/wispnt.h $(WC)/platsubs.h
$(THE_LIB)(wshelp.o): $(WC)/wispcfg.h $(WC)/wlicense.h
$(THE_LIB)(wshelp.o): $(WC)/machid.h $(WC)/wrunconf.h
$(THE_LIB)(wsmove.o): wsmove.c
$(THE_LIB)(wspawn.o): wspawn.c $(WC)/idsistd.h
$(THE_LIB)(wspawn.o): $(WC)/intdef.h $(WC)/wisplib.h
$(THE_LIB)(wspawn.o): $(WC)/setenvst.h
$(THE_LIB)(wswap.o): wswap.c $(WC)/idsistd.h
$(THE_LIB)(wswap.o): $(WC)/intdef.h cobrun.h
$(THE_LIB)(wswap.o): $(WC)/wglobals.h $(WC)/wfiles.h
$(THE_LIB)(wswap.o): $(WC)/wisplib.h $(WC)/setenvst.h
$(THE_LIB)(wswap.o): $(WC)/wexit.h
$(THE_LIB)(wsxio.o): wsxio.c $(WC)/idsistd.h
$(THE_LIB)(wsxio.o): $(WC)/intdef.h $(WC)/vwang.h
$(THE_LIB)(wsxio.o): $(WC)/werrlog.h $(WC)/wisplib.h
$(THE_LIB)(wsxio.o): $(WC)/setenvst.h
$(THE_LIB)(wsystem.o): wsystem.c $(WC)/wisplib.h
$(THE_LIB)(wsystem.o): $(WC)/intdef.h $(WC)/setenvst.h
$(THE_LIB)(wtransl.o): wtransl.c
$(THE_LIB)(wvaset.o): wvaset.c $(WC)/idsistd.h
$(THE_LIB)(wvaset.o): $(WC)/intdef.h
$(THE_LIB)(wwaitpid.o): wwaitpid.c $(WC)/idsistd.h
$(THE_LIB)(wwaitpid.o): $(WC)/intdef.h

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



selftest: validation

VALLIST= wispcfg machid

validation:	$(VALLIST)
	@echo
	@echo Running validation routines
	@for i in $(VALLIST) ; \
	do \
		(./$$i || exit 0); \
	done
	@echo
	@echo Validation completed
	@echo

$(VALLIST): $(THE_LIB)
	$(CC) $(CFLAGS) -g -DDEBUG -DMAIN -o $@ $@.c $(THE_LIB) $(VIDEO_LIB_PATH) $(CURSES_LINK)

#	History:
#	$Log: libwisp.umf,v $
#	Revision 1.33  1997-12-04 18:09:12-05  gsl
#	Rename winnt.h to wispnt.h
#
#	Revision 1.32  1997-12-04 13:58:13-05  gsl
#	move winnt.h to wispcommon
#
#	Revision 1.31  1997-10-29 11:58:23-05  gsl
#	Add wsb.c
#
#	Revision 1.30  1997-03-25 13:43:53-05  gsl
#	Update to use wispcommon
#
#	Revision 1.29  1997-03-06 17:02:17-05  gsl
#	don't install by default
#	specify the makefile name
#
#	Revision 1.28  1997-03-06 14:56:16-05  gsl
#	Add machid to validation list
#
#	Revision 1.27  1996-10-14 14:27:33-04  gsl
#	Add validation and selftest
#
#	Revision 1.26  1996-10-09 14:50:47-07  gsl
#	Add wispcfg.o
#
#	Revision 1.25  1996-07-17 15:44:17-07  gsl
#	Updated dependencies
#
#	Revision 1.24  1996-07-15 09:08:19-07  gsl
#	Add the video include directory to the cflags
#
#	Revision 1.23  1996-07-10 17:28:32-07  gsl
#	Change wassert.h to assert.h
#
#	Revision 1.22  1996-04-10 05:41:04-07  gsl
#	Add target "install"
#
# Revision 1.21  1996/01/04  11:41:09  gsl
# update
#
# Revision 1.20  1996/01/03  11:41:56  gsl
# add rvmap.[ch] wassert.[ch] wmalloc.[ch]
#
#
# 			03/18/90	Written by GSL
#			04/14/92	Moved system specific variables to $(PORT)/include.cfl, devo
#			06/04/92	Changed to use make.include GSL
#			01/26/93	Added level.c. GSL
#			04/21/93	Added isdebug.c  GSL
#			04/27/93	Added wftok.c GSL
#			07/20/93	Changed $(LIB) to $(THE_LIB) for MSDOS compatiblity. GSL
#			07/05/95	Added customvw.c  SMC

