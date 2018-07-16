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
#	File:		libwisp.umf
#
#	Function:	Generic makefile for the wisp library on all unix platforms.
#
# 	History:	03/18/90	Written by GSL
#			04/14/92	Moved system specific variables to $(PORT)/include.cfl, devo
#			06/04/92	Changed to use make.include GSL
#

#CDEBUG = -g
CDEBUG = -O

#LIB = libwispd.a
LIB = libwisp.a

include $(WISP)/src/port/make.include

.PRECIOUS:	$(LIB) 

all:	$(STDLIB)/$(LIB)

$(STDLIB)/$(LIB): $(LIB)
	cp $(LIB) $@

$(LIB): $(LIB)(acpmap.o) \
	$(LIB)(backgrnd.o) \
	$(LIB)(bell.o) \
	$(LIB)(bit_x.o) \
	$(LIB)(bits.o) \
	$(LIB)(breakacp.o) \
	$(LIB)(btransl.o) \
	$(LIB)(cancel.o) \
	$(LIB)(cexit.o) \
	$(LIB)(cgetneg.o) \
	$(LIB)(checkacp.o) \
	$(LIB)(closeacp.o) \
	$(LIB)(coblink.o) \
	$(LIB)(cobpic.o) \
	$(LIB)(date.o) \
	$(LIB)(dateset.o) \
	$(LIB)(day.o) \
	$(LIB)(delete.o) \
	$(LIB)(dummy.o) \
	$(LIB)(edestubs.o) \
	$(LIB)(errgparm.o) \
	$(LIB)(extract.o) \
	$(LIB)(filecopy.o) \
	$(LIB)(filesize.o) \
	$(LIB)(filgparm.o) \
	$(LIB)(find.o) \
	$(LIB)(findcase.o) \
	$(LIB)(findexts.o) \
	$(LIB)(fixerr.o) \
	$(LIB)(getparm.o) \
	$(LIB)(gparmbld.o) \
	$(LIB)(greclen.o) \
	$(LIB)(hexunpk.o) \
	$(LIB)(idsisubs.o) \
	$(LIB)(initglbs.o) \
	$(LIB)(initscrn.o) \
	$(LIB)(initwisp.o) \
	$(LIB)(isexec.o) \
	$(LIB)(juster.o) \
	$(LIB)(link.o) \
	$(LIB)(linkproc.o) \
	$(LIB)(linksubs.o) \
	$(LIB)(logoff.o) \
	$(LIB)(longarg.o) \
	$(LIB)(makepath.o) \
	$(LIB)(menu_go.o) \
	$(LIB)(menu_key.o) \
	$(LIB)(menuread.o) \
	$(LIB)(menuscan.o) \
	$(LIB)(message.o) \
	$(LIB)(mngfile.o) \
	$(LIB)(mount.o) \
	$(LIB)(msname.o) \
	$(LIB)(mstime.o) \
	$(LIB)(msvalue.o) \
	$(LIB)(mvspaces.o) \
	$(LIB)(mwconv.o) \
	$(LIB)(nextfile.o) \
	$(LIB)(noconfig.o) \
	$(LIB)(onhelp.o) \
	$(LIB)(openacp.o) \
	$(LIB)(packer.o) \
	$(LIB)(pbset.o) \
	$(LIB)(platsubs.o) \
	$(LIB)(print.o) \
	$(LIB)(protect.o) \
	$(LIB)(putparm.o) \
	$(LIB)(pwdname.o) \
	$(LIB)(que_jobs.o) \
	$(LIB)(quemgmt.o) \
	$(LIB)(readacp.o) \
	$(LIB)(readfdr.o) \
	$(LIB)(readvtoc.o) \
	$(LIB)(regexpr.o) \
	$(LIB)(rename.o) \
	$(LIB)(retcode.o) \
	$(LIB)(ring.o) \
	$(LIB)(rtransl.o) \
	$(LIB)(scratch.o) \
	$(LIB)(screen.o) \
	$(LIB)(search.o) \
	$(LIB)(set.o) \
	$(LIB)(setenvst.o) \
	$(LIB)(setfile.o) \
	$(LIB)(setprgid.o) \
	$(LIB)(setrcode.o) \
	$(LIB)(setsymb.o) \
	$(LIB)(settrigp.o) \
	$(LIB)(setupqm.o) \
	$(LIB)(sharemem.o) \
	$(LIB)(shellvar.o) \
	$(LIB)(shutexit.o) \
	$(LIB)(sort.o) \
	$(LIB)(sortcall.o) \
	$(LIB)(sortlink.o) \
	$(LIB)(sortseqf.o) \
	$(LIB)(spawn.o) \
	$(LIB)(string.o) \
	$(LIB)(submit.o) \
	$(LIB)(sysinf.o) \
	$(LIB)(updatfdr.o) \
	$(LIB)(upper.o) \
	$(LIB)(vdisplay.o) \
	$(LIB)(vmspargs.o) \
	$(LIB)(vwang.o) \
	$(LIB)(waccept.o) \
	$(LIB)(wangmenu.o) \
	$(LIB)(wanguid.o) \
	$(LIB)(wauthsub.o) \
	$(LIB)(wchain.o) \
	$(LIB)(wchkstrt.o) \
	$(LIB)(wcmatch.o) \
	$(LIB)(wdelfile.o) \
	$(LIB)(wdellock.o) \
	$(LIB)(wdfinish.o) \
	$(LIB)(wdinit.o) \
	$(LIB)(wdisplay.o) \
	$(LIB)(werrlog.o) \
	$(LIB)(werrpath.o) \
	$(LIB)(werrvre.o) \
	$(LIB)(wexit.o) \
	$(LIB)(wexith.o) \
	$(LIB)(wfaccess.o) \
	$(LIB)(wfcisam.o) \
	$(LIB)(wfclose.o) \
	$(LIB)(wfilechk.o) \
	$(LIB)(wfiledis.o) \
	$(LIB)(wfileext.o) \
	$(LIB)(wfname.o) \
	$(LIB)(wfopen.o) \
	$(LIB)(wfstatus.o) \
	$(LIB)(wfvision.o) \
	$(LIB)(wfwait.o) \
	$(LIB)(wgetpgrp.o) \
	$(LIB)(wglobals.o) \
	$(LIB)(wisp_pic.o) \
	$(LIB)(wispexit.o) \
	$(LIB)(wispsort.o) \
	$(LIB)(wispsync.o) \
	$(LIB)(wlickey.o) \
	$(LIB)(wpause.o) \
	$(LIB)(wperson.o) \
	$(LIB)(wprint.o) \
	$(LIB)(writeacp.o) \
	$(LIB)(wrunconf.o) \
	$(LIB)(wsclose.o) \
	$(LIB)(wscreen.o) \
	$(LIB)(wscset.o) \
	$(LIB)(wsfnm.o) \
	$(LIB)(wsfns.o) \
	$(LIB)(wshelp.o) \
	$(LIB)(wsmode.o) \
	$(LIB)(wsmove.o) \
	$(LIB)(wspawn.o) \
	$(LIB)(wswap.o) \
	$(LIB)(wsxio.o) \
	$(LIB)(wsystem.o) \
	$(LIB)(wtransl.o) \
	$(LIB)(wvaset.o) \
	$(LIB)(wwaitpid.o)
	$(RANLIB) $(LIB)
	@echo lib $(LIB) is now up-to-date


$(LIB)(acpmap.o): acpmap.c acp.h werrlog.h
$(LIB)(backgrnd.o): backgrnd.c wperson.h werrlog.h wdefines.h
$(LIB)(bell.o): bell.c movebin.h werrlog.h
$(LIB)(bit_x.o): bit_x.c cobrun.h
$(LIB)(bits.o): bits.c werrlog.h
$(LIB)(breakacp.o): breakacp.c
$(LIB)(btransl.o): btransl.c
$(LIB)(cancel.o): cancel.c werrlog.h
$(LIB)(cexit.o): cexit.c werrlog.h
$(LIB)(cgetneg.o): cgetneg.c
$(LIB)(checkacp.o): checkacp.c
$(LIB)(closeacp.o): closeacp.c acp.h werrlog.h
$(LIB)(coblink.o): coblink.c cobrun.h
$(LIB)(cobpic.o): cobpic.c cobrun.h
$(LIB)(date.o): date.c movebin.h werrlog.h
$(LIB)(dateset.o): dateset.c
$(LIB)(day.o): day.c movebin.h werrlog.h
$(LIB)(delete.o): delete.c
$(LIB)(dummy.o): dummy.c
$(LIB)(edestubs.o): edestubs.c
$(LIB)(errgparm.o): errgparm.c wangkeys.h wperson.h movebin.h werrlog.h
$(LIB)(errgparm.o): wdefines.h
$(LIB)(extract.o): extract.c wsysconf.h wdefines.h wperson.h movebin.h
$(LIB)(extract.o): werrlog.h wglobals.h wfiles.h
$(LIB)(filecopy.o): filecopy.c wfiles.h wcommon.h movebin.h wperson.h
$(LIB)(filecopy.o): werrlog.h cobrun.h
$(LIB)(filesize.o): filesize.c werrlog.h
$(LIB)(filgparm.o): filgparm.c wcommon.h wangkeys.h wperson.h movebin.h
$(LIB)(filgparm.o): werrlog.h wdefines.h
$(LIB)(find.o): find.c wfiles.h movebin.h werrlog.h wdefines.h
$(LIB)(find.o): wperson.h wdefines.h
$(LIB)(findcase.o): findcase.c
$(LIB)(findexts.o): findexts.c wdefines.h
$(LIB)(fixerr.o): fixerr.c
$(LIB)(getparm.o): getparm.c movebin.h werrlog.h wangkeys.h wcommon.h
$(LIB)(getparm.o): scnfacs.h wshmem.h wglobals.h wfiles.h $(V)/video.h
$(LIB)(getparm.o): $(V)/vlocal.h $(V)/vdata.h
$(LIB)(gparmbld.o): gparmbld.c werrlog.h wdefines.h
$(LIB)(greclen.o): greclen.c
$(LIB)(hexunpk.o): hexunpk.c movebin.h werrlog.h
$(LIB)(idsisubs.o): idsisubs.c wdefines.h
$(LIB)(initglbs.o): initglbs.c wglobals.h wfiles.h filext.h cobrun.h
$(LIB)(initscrn.o): initscrn.c $(V)/video.h $(V)/vlocal.h $(V)/vdata.h
$(LIB)(initscrn.o): wperson.h
$(LIB)(initwisp.o): initwisp.c werrlog.h wdefines.h wcommon.h cobrun.h
$(LIB)(initwisp.o): wglobals.h wfiles.h filext.h wperson.h wlicense.h
$(LIB)(isexec.o): isexec.c wdefines.h
$(LIB)(juster.o): juster.c werrlog.h
$(LIB)(link.o): link.c wrunconf.h wglobals.h wfiles.h wdefines.h
$(LIB)(link.o): movebin.h werrlog.h wperson.h wfiles.h wcommon.h
$(LIB)(link.o): cobrun.h $(V)/video.h
$(LIB)(linkproc.o): linkproc.c $(V)/video.h wdefines.h movebin.h
$(LIB)(linkproc.o): werrlog.h wperson.h wfiles.h wcommon.h
$(LIB)(linksubs.o): linksubs.c wglobals.h wfiles.h wdefines.h werrlog.h
$(LIB)(linksubs.o): movebin.h
$(LIB)(logoff.o): logoff.c wdefines.h
$(LIB)(longarg.o): longarg.c wglobals.h wfiles.h
$(LIB)(makepath.o): makepath.c
$(LIB)(menu_go.o): menu_go.c menu.h $(V)/video.h $(V)/vlocal.h
$(LIB)(menu_key.o): menu_key.c
$(LIB)(menuread.o): menuread.c menu.h
$(LIB)(menuscan.o): menuscan.c menu.h wperson.h $(V)/video.h
$(LIB)(menuscan.o): $(V)/vlocal.h $(V)/vdata.h
$(LIB)(message.o): message.c werrlog.h wdefines.h movebin.h
$(LIB)(mngfile.o): mngfile.c wcommon.h vwang.h scnfacs.h wperson.h
$(LIB)(mngfile.o): wdefines.h werrlog.h
$(LIB)(mount.o): mount.c
$(LIB)(msname.o): msname.c
$(LIB)(mstime.o): mstime.c
$(LIB)(msvalue.o): msvalue.c
$(LIB)(mvspaces.o): mvspaces.c
$(LIB)(mwconv.o): mwconv.c cobrun.h
$(LIB)(nextfile.o): nextfile.c wdefines.h
$(LIB)(noconfig.o): noconfig.c
$(LIB)(onhelp.o): onhelp.c wperson.h
$(LIB)(openacp.o): openacp.c acp.h
$(LIB)(packer.o): packer.c
$(LIB)(pbset.o): pbset.c
$(LIB)(platsubs.o): platsubs.c wplatdef.h
$(LIB)(print.o): print.c wcommon.h wperson.h movebin.h werrlog.h
$(LIB)(protect.o): protect.c
$(LIB)(putparm.o): putparm.c movebin.h werrlog.h wshmem.h wdefines.h
$(LIB)(putparm.o): wglobals.h wfiles.h
$(LIB)(pwdname.o): pwdname.c
$(LIB)(que_jobs.o): que_jobs.c werrlog.h
$(LIB)(quemgmt.o): quemgmt.c werrlog.h
$(LIB)(readacp.o): readacp.c acp.h movebin.h
$(LIB)(readfdr.o): readfdr.c wcommon.h movebin.h cobrun.h werrlog.h
$(LIB)(readvtoc.o): readvtoc.c werrlog.h
$(LIB)(regexpr.o): regexpr.c regexpr.h
$(LIB)(rename.o): rename.c wfiles.h wcommon.h movebin.h wperson.h
$(LIB)(rename.o): werrlog.h cobrun.h
$(LIB)(retcode.o): retcode.c werrlog.h wdefines.h
$(LIB)(ring.o): ring.c
$(LIB)(rtransl.o): rtransl.c
$(LIB)(scratch.o): scratch.c $(V)/video.h wcommon.h wdefines.h
$(LIB)(scratch.o): movebin.h cobrun.h
$(LIB)(screen.o): screen.c vwang.h wcommon.h vwang.h werrlog.h
$(LIB)(screen.o): wperson.h wdefines.h movebin.h
$(LIB)(search.o): search.c movebin.h werrlog.h wdefines.h
$(LIB)(set.o): set.c wperson.h werrlog.h wglobals.h wfiles.h
$(LIB)(setenvst.o): setenvst.c
$(LIB)(setfile.o): setfile.c wfiles.h
$(LIB)(setprgid.o): setprgid.c wglobals.h wfiles.h
$(LIB)(setrcode.o): setrcode.c werrlog.h wdefines.h
$(LIB)(setsymb.o): setsymb.c
$(LIB)(settrigp.o): settrigp.c wfiles.h wcommon.h werrlog.h
$(LIB)(setupqm.o): setupqm.c
$(LIB)(sharemem.o): sharemem.c werrlog.h wshmem.h wdefines.h
$(LIB)(shellvar.o): shellvar.c
$(LIB)(shutexit.o): shutexit.c
$(LIB)(sort.o): sort.c werrlog.h
$(LIB)(sortcall.o): sortcall.c werrlog.h
$(LIB)(sortlink.o): sortlink.c movebin.h werrlog.h cobrun.h
$(LIB)(sortseqf.o): sortseqf.c sortseqf.h
$(LIB)(spawn.o): spawn.c
$(LIB)(string.o): string.c movebin.h werrlog.h
$(LIB)(submit.o): submit.c que_jobs.h wcommon.h wperson.h movebin.h
$(LIB)(submit.o): werrlog.h wdefines.h wrunconf.h
$(LIB)(sysinf.o): sysinf.c wglobals.h wfiles.h werrlog.h
$(LIB)(updatfdr.o): updatfdr.c werrlog.h
$(LIB)(upper.o): upper.c
$(LIB)(vdisplay.o): vdisplay.c $(V)/video.h $(V)/vlocal.h $(V)/vdata.h
$(LIB)(vdisplay.o): $(V)/vcap.h werrlog.h wperson.h scnfacs.h
$(LIB)(vmspargs.o): vmspargs.c wglobals.h wfiles.h wdefines.h werrlog.h
$(LIB)(vwang.o): vwang.c $(V)/video.h $(V)/vlocal.h $(V)/vdata.h
$(LIB)(vwang.o): cobrun.h sub_char.h wperson.h wscrn.h werrlog.h
$(LIB)(vwang.o): vwang.h wglobals.h wfiles.h scnfacs.h
$(LIB)(waccept.o): waccept.c
$(LIB)(wangmenu.o): wangmenu.c
$(LIB)(wanguid.o): wanguid.c wdefines.h
$(LIB)(wauthsub.o): wauthsub.c wlicense.h
$(LIB)(wchain.o): wchain.c $(V)/video.h werrlog.h wcommon.h wrunconf.h
$(LIB)(wchkstrt.o): wchkstrt.c $(V)/video.h werrlog.h
$(LIB)(wcmatch.o): wcmatch.c wdefines.h
$(LIB)(wdelfile.o): wdelfile.c
$(LIB)(wdellock.o): wdellock.c
$(LIB)(wdfinish.o): wdfinish.c $(V)/video.h
$(LIB)(wdinit.o): wdinit.c $(V)/video.h
$(LIB)(wdisplay.o): wdisplay.c $(V)/video.h vwang.h
$(LIB)(werrlog.o): werrlog.c wperson.h werrlog.h wdefines.h wglobals.h
$(LIB)(werrlog.o): wfiles.h
$(LIB)(werrpath.o): werrpath.c wdefines.h wglobals.h wfiles.h
$(LIB)(werrvre.o): werrvre.c
$(LIB)(wexit.o): wexit.c werrlog.h wglobals.h wfiles.h
$(LIB)(wexith.o): wexith.c que_jobs.h wperson.h wfiles.h wglobals.h
$(LIB)(wexith.o): wfiles.h filext.h $(V)/video.h
$(LIB)(wfaccess.o): wfaccess.c wdefines.h wcommon.h wfaccess.h cobrun.h
$(LIB)(wfcisam.o): wfcisam.c
$(LIB)(wfclose.o): wfclose.c wperson.h wfiles.h
$(LIB)(wfilechk.o): wfilechk.c werrlog.h wcommon.h cobrun.h wdefines.h
$(LIB)(wfilechk.o): wglobals.h wfiles.h
$(LIB)(wfiledis.o): wfiledis.c wperson.h $(V)/video.h wfiles.h
$(LIB)(wfiledis.o): wcommon.h wangkeys.h wfaccess.h werrlog.h
$(LIB)(wfileext.o): wfileext.c filext.h
$(LIB)(wfname.o): wfname.c wperson.h wfiles.h wcommon.h werrlog.h
$(LIB)(wfname.o): cobrun.h
$(LIB)(wfopen.o): wfopen.c wperson.h wangkeys.h wcommon.h wfaccess.h
$(LIB)(wfopen.o): cobrun.h wglobals.h wfiles.h filext.h
$(LIB)(wfstatus.o): wfstatus.c
$(LIB)(wfvision.o): wfvision.c visint.h visn2.h
$(LIB)(wfwait.o): wfwait.c wglobals.h wfiles.h
$(LIB)(wgetpgrp.o): wgetpgrp.c wdefines.h
$(LIB)(wglobals.o): wglobals.c wglobals.h wfiles.h cobrun.h
$(LIB)(wisp_pic.o): wisp_pic.c
$(LIB)(wispexit.o): wispexit.c cobrun.h
$(LIB)(wispsort.o): wispsort.c wcommon.h sortseqf.h movebin.h werrlog.h
$(LIB)(wispsync.o): wispsync.c
$(LIB)(wlickey.o): wlickey.c wlicense.h
$(LIB)(wpause.o): wpause.c movebin.h werrlog.h
$(LIB)(wperson.o): wperson.c $(V)/video.h wperson.h werrlog.h
$(LIB)(wperson.o): wdefines.h wglobals.h wfiles.h
$(LIB)(wprint.o): wprint.c que_jobs.h wperson.h werrlog.h wglobals.h
$(LIB)(wprint.o): wfiles.h
$(LIB)(writeacp.o): writeacp.c acp.h
$(LIB)(wrunconf.o): wrunconf.c wrunconf.h wdefines.h
$(LIB)(wsclose.o): wsclose.c
$(LIB)(wscreen.o): wscreen.c $(V)/video.h wcommon.h movebin.h cobrun.h
$(LIB)(wscreen.o): werrlog.h
$(LIB)(wscset.o): wscset.c $(V)/video.h $(V)/vlocal.h $(V)/vdata.h
$(LIB)(wscset.o): $(V)/vcap.h
$(LIB)(wsfnm.o): wsfnm.c $(V)/video.h vwang.h werrlog.h wglobals.h
$(LIB)(wsfnm.o): wfiles.h
$(LIB)(wsfns.o): wsfns.c $(V)/video.h vwang.h werrlog.h wglobals.h
$(LIB)(wsfns.o): wfiles.h
$(LIB)(wshelp.o): wshelp.c $(V)/video.h $(V)/vlocal.h $(V)/vcap.h
$(LIB)(wshelp.o): wcommon.h wperson.h vwang.h wscrn.h scnfacs.h
$(LIB)(wshelp.o): wglobals.h wfiles.h werrlog.h wdefines.h cobrun.h
$(LIB)(wshelp.o): license.h
$(LIB)(wsmode.o): wsmode.c $(V)/video.h $(V)/vlocal.h $(V)/vdata.h
$(LIB)(wsmode.o): $(V)/vcap.h
$(LIB)(wsmove.o): wsmove.c $(V)/video.h $(V)/vlocal.h $(V)/vdata.h
$(LIB)(wsmove.o): $(V)/vcap.h
$(LIB)(wspawn.o): wspawn.c
$(LIB)(wswap.o): wswap.c cobrun.h wglobals.h wfiles.h
$(LIB)(wsxio.o): wsxio.c $(V)/video.h $(V)/vlocal.h $(V)/vdata.h
$(LIB)(wsxio.o): vwang.h
$(LIB)(wsystem.o): wsystem.c
$(LIB)(wtransl.o): wtransl.c
$(LIB)(wvaset.o): wvaset.c
$(LIB)(wwaitpid.o): wwaitpid.c


