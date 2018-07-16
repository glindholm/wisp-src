#\************************************************************************\
#\*                                                                      *\
#\*              WISP - Wang Interchange Source Pre-processor            *\
#\*               Copyright (c) 1988, 1989, 1990, 1991, 1992             *\
#\*       An unpublished work of International Digital Scientific Inc.   *\
#\*                          All rights reserved.                        *\
#\*                                                                      *\
#\************************************************************************\
#
#
#       File:           libwisp.mak
#
#       Function:       Makefile for the wisp library on MS-DOS using
#			Intel C Code Builder
#
#       History:        03\18\90        Written by GSL
#                       04\14\92        Moved system specific variables to $(PORT)\include.cfl, devo
#                       06\04\92        Changed to use make.include GSL
#                       11\03\92        Modified for MS-DOS
#			11\19\92	Mod for Intel Code Builder

CDEBUG = /g
LIB = WISP.LIB

!include $(WISP)\src\port\dosmake.mak

.PRECIOUS:      $(LIB) 

all:    $(STDLIB)\$(LIB)

$(STDLIB)\$(LIB): $(LIB)
	copy $(LIB) $@

OBJS=   acpmap.obj \
	acustubs.obj \
	backgrnd.obj \
	bell.obj \
	bit_x.obj \
	bits.obj \
	breakacp.obj \
	btransl.obj \
	cancel.obj \
	cexit.obj \
	cgetneg.obj \
	checkacp.obj \
	closeacp.obj \
	coblink.obj \
	cobpic.obj \
	date.obj \
	dateset.obj \
	day.obj \
	delete.obj \
	dosprint.obj \
	dummy.obj \
	edestubs.obj \
	errgparm.obj \
	extract.obj \
	fcopy.obj \
	fexists.obj \
	filecopy.obj \
	filesize.obj \
	filgparm.obj \
	find.obj \
	findcase.obj \
	findexts.obj \
	fixerr.obj \
	fxzone.obj \
	getopt.obj \
	getparm.obj \
	gparmbld.obj \
	greclen.obj \
	hexunpk.obj \
	idsisubs.obj \
	initglbs.obj \
	initscrn.obj \
	initwisp.obj \
	isdebug.obj \
	isexec.obj \
	juster.obj \
	level.obj \
	link.obj \
	linkproc.obj \
	linksubs.obj \
	logoff.obj \
	longarg.obj \
	machid.obj \
	makepath.obj \
	menu_go.obj \
	menu_key.obj \
	menuread.obj \
	menuscan.obj \
	message.obj \
	mngfile.obj \
	mount.obj \
	msdosfns.obj \
	msname.obj \
	mstime.obj \
	msvalue.obj \
	mvspaces.obj \
	mwconv.obj \
	nextfile.obj \
	noconfig.obj \
	onhelp.obj \
	openacp.obj \
	packer.obj \
	pbset.obj \
	platsubs.obj \
	print.obj \
	protect.obj \
	putparm.obj \
	pwdname.obj \
	que_jobs.obj \
	quemgmt.obj \
	readacp.obj \
	readfdr.obj \
	readvtoc.obj \
	regexpr.obj \
	rename.obj \
	retcode.obj \
	ring.obj \
	rtransl.obj \
	runtype.obj \
	scratch.obj \
	screen.obj \
	search.obj \
	set.obj \
	setenvst.obj \
	setfile.obj \
	setprgid.obj \
	setrcode.obj \
	setsymb.obj \
	settrigp.obj \
	setupqm.obj \
	sharemem.obj \
	shellvar.obj \
	shutexit.obj \
	sleepdos.obj \
	sort.obj \
	sortcall.obj \
	sortlink.obj \
	sortseqf.obj \
	sortx.obj \
	spawn.obj \
	string.obj \
	submit.obj \
	sysinf.obj \
	untabify.obj \
	updatfdr.obj \
	upper.obj \
	vdispidx.obj \
	vdisplay.obj \
	vmspargs.obj \
	vwang.obj \
	waccept.obj \
	wangmenu.obj \
	wanguid.obj \
	wauthsub.obj \
	wchain.obj \
	wchkstrt.obj \
	wcmatch.obj \
	wdelfile.obj \
	wdellock.obj \
	wdfinish.obj \
	wdinit.obj \
	wdisplay.obj \
	werrlog.obj \
	werrpath.obj \
	werrvre.obj \
	wexit.obj \
	wexith.obj \
	wfaccess.obj \
	wfcisam.obj \
	wfclose.obj \
	wfilechk.obj \
	wfiledis.obj \
	wfileext.obj \
	wfname.obj \
	wfopen.obj \
	wfstatus.obj \
	wftok.obj \
	wfvision.obj \
	wfwait.obj \
	wgetpgrp.obj \
	wglobals.obj \
	wisp_pic.obj \
	wispexit.obj \
	wispsort.obj \
	wispsync.obj \
	wlickey.obj \
	wpause.obj \
	wperson.obj \
	wprint.obj \
	writeacp.obj \
	wrunconf.obj \
	wsclose.obj \
	wscreen.obj \
	wscset.obj \
	wsfnm.obj \
	wsfns.obj \
	wshelp.obj \
	wsmode.obj \
	wsmove.obj \
	wspawn.obj \
	wswap.obj \
	wsxio.obj \
	wsystem.obj \
	wtransl.obj \
	wvaset.obj \
	wwaitpid.obj 

LIB_FILE = $(LIB)
LIB_OBJS = $(OBJS)
!include $(CBPATH)\BIN\LIB.MAK

$(LIB): $(LIB_FILE)

acpmap.obj: acpmap.c idsistd.h intdef.h acp.h werrlog.h
acustubs.obj: acustubs.c idsistd.h intdef.h
backgrnd.obj: backgrnd.c idsistd.h intdef.h wperson.h werrlog.h
backgrnd.obj: wdefines.h
bell.obj: bell.c idsistd.h intdef.h movebin.h werrlog.h
bit_x.obj: bit_x.c idsistd.h intdef.h cobrun.h
bits.obj: bits.c idsistd.h intdef.h werrlog.h
breakacp.obj: breakacp.c
btransl.obj: btransl.c idsistd.h intdef.h
cancel.obj: cancel.c idsistd.h intdef.h werrlog.h
cexit.obj: cexit.c idsistd.h intdef.h werrlog.h
cgetneg.obj: cgetneg.c idsistd.h intdef.h
checkacp.obj: checkacp.c
closeacp.obj: closeacp.c idsistd.h intdef.h acp.h werrlog.h
coblink.obj: coblink.c idsistd.h intdef.h cobrun.h
cobpic.obj: cobpic.c idsistd.h intdef.h cobrun.h
date.obj: date.c idsistd.h intdef.h movebin.h werrlog.h
dateset.obj: dateset.c idsistd.h intdef.h
day.obj: day.c idsistd.h intdef.h movebin.h werrlog.h
delete.obj: delete.c idsistd.h intdef.h
dosprint.obj: dosprint.c
dummy.obj: dummy.c idsistd.h intdef.h
edestubs.obj: edestubs.c idsistd.h intdef.h
errgparm.obj: errgparm.c idsistd.h intdef.h wangkeys.h wperson.h
errgparm.obj: movebin.h werrlog.h wdefines.h
extract.obj: extract.c idsistd.h intdef.h wsysconf.h wdefines.h
extract.obj: wperson.h movebin.h werrlog.h wglobals.h wfiles.h
extract.obj: wanguid.h
fcopy.obj: fcopy.c idsistd.h intdef.h fcopy.h
fexists.obj: fexists.c idsistd.h intdef.h
filecopy.obj: filecopy.c idsistd.h intdef.h fcopy.h wfiles.h
filecopy.obj: wcommon.h movebin.h wperson.h werrlog.h cobrun.h
filecopy.obj: filext.h
filesize.obj: filesize.c idsistd.h intdef.h werrlog.h
filgparm.obj: filgparm.c idsistd.h intdef.h wcommon.h wangkeys.h
filgparm.obj: wperson.h movebin.h werrlog.h wdefines.h
find.obj: find.c idsistd.h intdef.h wfiles.h movebin.h werrlog.h
find.obj: wdefines.h wperson.h
findcase.obj: findcase.c idsistd.h intdef.h
findexts.obj: findexts.c idsistd.h intdef.h wdefines.h idsisubs.h
fixerr.obj: fixerr.c idsistd.h intdef.h
fxzone.obj: fxzone.c idsistd.h intdef.h
getopt.obj: getopt.c
getparm.obj: getparm.c idsistd.h intdef.h movebin.h werrlog.h
getparm.obj: wangkeys.h wcommon.h scnfacs.h wshmem.h putparm.h
getparm.obj: wglobals.h wfiles.h $(V)\video.h
getparm.obj: $(V)\vlocal.h
getparm.obj: $(V)\vdata.h
gparmbld.obj: gparmbld.c idsistd.h intdef.h werrlog.h wdefines.h
greclen.obj: greclen.c idsistd.h intdef.h
hexunpk.obj: hexunpk.c idsistd.h intdef.h movebin.h werrlog.h
idsisubs.obj: idsisubs.c idsistd.h intdef.h
initglbs.obj: initglbs.c idsistd.h intdef.h wglobals.h wfiles.h
initglbs.obj: filext.h cobrun.h
initscrn.obj: initscrn.c $(V)\video.h
initscrn.obj: $(V)\vlocal.h
initscrn.obj: $(V)\vdata.h idsistd.h intdef.h
initscrn.obj: wperson.h
initwisp.obj: initwisp.c idsistd.h intdef.h werrlog.h wdefines.h
initwisp.obj: wcommon.h cobrun.h wglobals.h wfiles.h filext.h
initwisp.obj: wperson.h wlicense.h
isdebug.obj: isdebug.c idsistd.h intdef.h cobrun.h
isexec.obj: isexec.c idsistd.h intdef.h wdefines.h
juster.obj: juster.c idsistd.h intdef.h werrlog.h
level.obj: level.c idsistd.h intdef.h
link.obj: link.c idsistd.h intdef.h link.h wglobals.h wfiles.h
link.obj: wdefines.h movebin.h wperson.h wcommon.h cobrun.h
link.obj: $(V)\video.h runtype.h wrunconf.h
link.obj: idsisubs.h werrlog.h
linkproc.obj: linkproc.c $(V)\video.h
linkproc.obj: idsistd.h intdef.h wdefines.h movebin.h werrlog.h
linkproc.obj: wperson.h wfiles.h wcommon.h runtype.h
linksubs.obj: linksubs.c idsistd.h intdef.h link.h wglobals.h
linksubs.obj: wfiles.h wdefines.h movebin.h werrlog.h
logoff.obj: logoff.c idsistd.h intdef.h wdefines.h
longarg.obj: longarg.c idsistd.h intdef.h wglobals.h wfiles.h
machid.obj: machid.c
makepath.obj: makepath.c idsistd.h intdef.h
menu_go.obj: menu_go.c idsistd.h intdef.h menu.h
menu_go.obj: $(V)\video.h
menu_go.obj: $(V)\vlocal.h
menu_key.obj: menu_key.c idsistd.h intdef.h
menuread.obj: menuread.c idsistd.h intdef.h menu.h
menuscan.obj: menuscan.c idsistd.h intdef.h menu.h wperson.h
menuscan.obj: $(V)\video.h
menuscan.obj: $(V)\vlocal.h
menuscan.obj: $(V)\vdata.h
message.obj: message.c idsistd.h intdef.h werrlog.h wdefines.h
message.obj: vwang.h movebin.h
mngfile.obj: mngfile.c idsistd.h intdef.h runtype.h wcommon.h
mngfile.obj: vwang.h scnfacs.h wperson.h wdefines.h werrlog.h
mount.obj: mount.c
msdosfns.obj: msdosfns.c idsistd.h intdef.h
msname.obj: msname.c idsistd.h intdef.h
mstime.obj: mstime.c idsistd.h intdef.h
msvalue.obj: msvalue.c idsistd.h intdef.h
mvspaces.obj: mvspaces.c idsistd.h intdef.h
mwconv.obj: mwconv.c idsistd.h intdef.h cobrun.h
nextfile.obj: nextfile.c idsistd.h intdef.h wdefines.h
noconfig.obj: noconfig.c idsistd.h intdef.h wdefines.h
onhelp.obj: onhelp.c idsistd.h intdef.h wperson.h
openacp.obj: openacp.c idsistd.h intdef.h acp.h
packer.obj: packer.c idsistd.h intdef.h
pbset.obj: pbset.c idsistd.h intdef.h
platsubs.obj: platsubs.c idsistd.h intdef.h wplatdef.h
print.obj: print.c idsistd.h intdef.h wcommon.h wperson.h
print.obj: movebin.h werrlog.h
protect.obj: protect.c idsistd.h intdef.h
putparm.obj: putparm.c idsistd.h intdef.h movebin.h werrlog.h
putparm.obj: wshmem.h putparm.h wdefines.h wglobals.h wfiles.h
pwdname.obj: pwdname.c idsistd.h intdef.h
que_jobs.obj: que_jobs.c idsistd.h intdef.h werrlog.h
quemgmt.obj: quemgmt.c werrlog.h idsistd.h intdef.h
readacp.obj: readacp.c idsistd.h intdef.h acp.h movebin.h
readfdr.obj: readfdr.c idsistd.h intdef.h wcommon.h movebin.h
readfdr.obj: cobrun.h werrlog.h
readvtoc.obj: readvtoc.c idsistd.h intdef.h werrlog.h
regexpr.obj: regexpr.c idsistd.h intdef.h regexpr.h
rename.obj: rename.c idsistd.h intdef.h wfiles.h wcommon.h
rename.obj: movebin.h wperson.h werrlog.h cobrun.h idsisubs.h
retcode.obj: retcode.c idsistd.h intdef.h werrlog.h wdefines.h
ring.obj: ring.c
rtransl.obj: rtransl.c idsistd.h intdef.h
runtype.obj: runtype.c idsistd.h intdef.h runtype.h wdefines.h
runtype.obj: idsisubs.h
scratch.obj: scratch.c $(V)\video.h idsistd.h
scratch.obj: intdef.h wcommon.h wdefines.h movebin.h cobrun.h
scratch.obj: idsisubs.h
screen.obj: screen.c idsistd.h intdef.h vwang.h wcommon.h
screen.obj: werrlog.h wperson.h wdefines.h movebin.h wanguid.h
search.obj: search.c idsistd.h intdef.h movebin.h werrlog.h
search.obj: wdefines.h
set.obj: set.c idsistd.h intdef.h wperson.h werrlog.h wglobals.h
set.obj: wfiles.h
setenvst.obj: setenvst.c idsistd.h intdef.h
setfile.obj: setfile.c idsistd.h intdef.h wfiles.h
setprgid.obj: setprgid.c idsistd.h intdef.h wglobals.h wfiles.h
setrcode.obj: setrcode.c idsistd.h intdef.h werrlog.h wdefines.h
setsymb.obj: setsymb.c idsistd.h intdef.h
settrigp.obj: settrigp.c idsistd.h intdef.h wfiles.h wcommon.h
settrigp.obj: werrlog.h
setupqm.obj: setupqm.c
sharemem.obj: sharemem.c idsistd.h intdef.h werrlog.h wshmem.h
sharemem.obj: putparm.h wdefines.h wanguid.h
shellvar.obj: shellvar.c idsistd.h intdef.h
shutexit.obj: shutexit.c idsistd.h intdef.h
sleepdos.obj: sleepdos.c
sort.obj: sort.c idsistd.h intdef.h werrlog.h
sortcall.obj: sortcall.c idsistd.h intdef.h werrlog.h
sortlink.obj: sortlink.c idsistd.h intdef.h movebin.h werrlog.h
sortlink.obj: cobrun.h
sortseqf.obj: sortseqf.c idsistd.h intdef.h sortseqf.h
sortx.obj: sortx.c
spawn.obj: spawn.c
string.obj: string.c idsistd.h intdef.h movebin.h werrlog.h
submit.obj: submit.c idsistd.h intdef.h que_jobs.h wcommon.h
submit.obj: wperson.h movebin.h werrlog.h wdefines.h filext.h
submit.obj: wrunconf.h runtype.h
sysinf.obj: sysinf.c idsistd.h intdef.h wglobals.h wfiles.h
sysinf.obj: werrlog.h
untabify.obj: untabify.c idsistd.h intdef.h
updatfdr.obj: updatfdr.c idsistd.h intdef.h werrlog.h
upper.obj: upper.c idsistd.h intdef.h
vdispidx.obj: vdispidx.c
vdisplay.obj: vdisplay.c $(V)\video.h
vdisplay.obj: $(V)\vlocal.h
vdisplay.obj: $(V)\vdata.h
vdisplay.obj: $(V)\vcap.h idsistd.h intdef.h
vdisplay.obj: werrlog.h wperson.h scnfacs.h wanguid.h
vmspargs.obj: vmspargs.c idsistd.h intdef.h wglobals.h wfiles.h
vmspargs.obj: wdefines.h werrlog.h link.h
vwang.obj: vwang.c $(V)\video.h
vwang.obj: $(V)\vlocal.h
vwang.obj: $(V)\vdata.h idsistd.h intdef.h
vwang.obj: cobrun.h sub_char.h wperson.h werrlog.h vwang.h
vwang.obj: wglobals.h wfiles.h scnfacs.h
vwang.obj: $(V)\vchinese.h
waccept.obj: waccept.c idsistd.h intdef.h
wangmenu.obj: wangmenu.c idsistd.h intdef.h
wanguid.obj: wanguid.c idsistd.h intdef.h wdefines.h wanguid.h
wauthsub.obj: wauthsub.c idsistd.h intdef.h wlicense.h
wchain.obj: wchain.c $(V)\video.h idsistd.h
wchain.obj: intdef.h werrlog.h wcommon.h wdefines.h wrunconf.h
wchkstrt.obj: wchkstrt.c $(V)\video.h
wchkstrt.obj: idsistd.h intdef.h werrlog.h
wcmatch.obj: wcmatch.c idsistd.h intdef.h wdefines.h
wdelfile.obj: wdelfile.c idsistd.h intdef.h
wdellock.obj: wdellock.c
wdfinish.obj: wdfinish.c $(V)\video.h
wdfinish.obj: idsistd.h intdef.h
wdinit.obj: wdinit.c $(V)\video.h idsistd.h
wdinit.obj: intdef.h
wdisplay.obj: wdisplay.c $(V)\video.h
wdisplay.obj: idsistd.h intdef.h vwang.h
werrlog.obj: werrlog.c idsistd.h intdef.h wperson.h werrlog.h
werrlog.obj: wdefines.h wglobals.h wfiles.h
werrpath.obj: werrpath.c idsistd.h intdef.h wdefines.h wglobals.h
werrpath.obj: wfiles.h
werrvre.obj: werrvre.c idsistd.h intdef.h
wexit.obj: wexit.c idsistd.h intdef.h werrlog.h wglobals.h
wexit.obj: wfiles.h
wexith.obj: wexith.c idsistd.h intdef.h que_jobs.h wperson.h
wexith.obj: wfiles.h wglobals.h filext.h
wexith.obj: $(V)\video.h
wfaccess.obj: wfaccess.c idsistd.h intdef.h wdefines.h wcommon.h
wfaccess.obj: wfaccess.h cobrun.h
wfcisam.obj: wfcisam.c idsistd.h intdef.h
wfclose.obj: wfclose.c idsistd.h intdef.h wperson.h wfiles.h
wfilechk.obj: wfilechk.c idsistd.h intdef.h werrlog.h wcommon.h
wfilechk.obj: cobrun.h wdefines.h wglobals.h wfiles.h
wfiledis.obj: wfiledis.c idsistd.h intdef.h wperson.h
wfiledis.obj: $(V)\video.h wfiles.h wcommon.h
wfiledis.obj: wangkeys.h werrlog.h
wfileext.obj: wfileext.c idsistd.h intdef.h filext.h
wfname.obj: wfname.c idsistd.h intdef.h wperson.h wfiles.h
wfname.obj: wcommon.h werrlog.h cobrun.h
wfopen.obj: wfopen.c idsistd.h intdef.h wperson.h wangkeys.h
wfopen.obj: wcommon.h wfaccess.h cobrun.h wglobals.h wfiles.h
wfopen.obj: filext.h werrlog.h
wfstatus.obj: wfstatus.c idsistd.h intdef.h
wftok.obj: wftok.c idsistd.h intdef.h
wfvision.obj: wfvision.c idsistd.h intdef.h visint.h visn2.h
wfwait.obj: wfwait.c idsistd.h intdef.h wglobals.h wfiles.h
wgetpgrp.obj: wgetpgrp.c idsistd.h intdef.h wdefines.h
wglobals.obj: wglobals.c idsistd.h intdef.h wglobals.h wfiles.h
wglobals.obj: cobrun.h
wisp_pic.obj: wisp_pic.c idsistd.h intdef.h
wispexit.obj: wispexit.c idsistd.h intdef.h cobrun.h
wispsort.obj: wispsort.c idsistd.h intdef.h wcommon.h sortseqf.h
wispsort.obj: movebin.h werrlog.h
wispsync.obj: wispsync.c idsistd.h intdef.h
wlickey.obj: wlickey.c idsistd.h intdef.h wlicense.h
wpause.obj: wpause.c idsistd.h intdef.h movebin.h werrlog.h
wperson.obj: wperson.c $(V)\video.h idsistd.h
wperson.obj: intdef.h wperson.h werrlog.h wdefines.h wglobals.h
wperson.obj: wfiles.h wanguid.h
wprint.obj: wprint.c idsistd.h intdef.h que_jobs.h wperson.h
wprint.obj: werrlog.h wglobals.h wfiles.h
writeacp.obj: writeacp.c idsistd.h intdef.h acp.h
wrunconf.obj: wrunconf.c idsistd.h intdef.h wrunconf.h wdefines.h
wsclose.obj: wsclose.c idsistd.h intdef.h
wscreen.obj: wscreen.c $(V)\video.h idsistd.h
wscreen.obj: intdef.h wcommon.h movebin.h cobrun.h werrlog.h
wscset.obj: wscset.c $(V)\video.h
wscset.obj: $(V)\vlocal.h
wscset.obj: $(V)\vdata.h
wscset.obj: $(V)\vcap.h idsistd.h intdef.h
wsfnm.obj: wsfnm.c $(V)\video.h idsistd.h
wsfnm.obj: intdef.h vwang.h werrlog.h wglobals.h wfiles.h
wsfns.obj: wsfns.c $(V)\video.h idsistd.h
wsfns.obj: intdef.h vwang.h werrlog.h wglobals.h wfiles.h
wshelp.obj: wshelp.c $(V)\video.h
wshelp.obj: $(V)\vlocal.h
wshelp.obj: $(V)\vcap.h
wshelp.obj: $(V)\vchinese.h idsistd.h
wshelp.obj: intdef.h wcommon.h wperson.h vwang.h scnfacs.h
wshelp.obj: wglobals.h wfiles.h werrlog.h wdefines.h cobrun.h
wshelp.obj: wanguid.h license.h
wsmode.obj: wsmode.c $(V)\video.h
wsmode.obj: $(V)\vlocal.h
wsmode.obj: $(V)\vdata.h
wsmode.obj: $(V)\vcap.h idsistd.h intdef.h
wsmove.obj: wsmove.c $(V)\video.h
wsmove.obj: $(V)\vlocal.h
wsmove.obj: $(V)\vdata.h
wsmove.obj: $(V)\vcap.h idsistd.h intdef.h
wspawn.obj: wspawn.c idsistd.h intdef.h
wswap.obj: wswap.c idsistd.h intdef.h cobrun.h wglobals.h
wswap.obj: wfiles.h
wsxio.obj: wsxio.c $(V)\video.h
wsxio.obj: $(V)\vlocal.h
wsxio.obj: $(V)\vdata.h idsistd.h intdef.h
wsxio.obj: vwang.h werrlog.h
wsystem.obj: wsystem.c idsistd.h intdef.h
wtransl.obj: wtransl.c
wvaset.obj: wvaset.c idsistd.h intdef.h
wwaitpid.obj: wwaitpid.c idsistd.h intdef.h




