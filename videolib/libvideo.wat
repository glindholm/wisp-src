#/***********************************************************************/
#/*                                                                     */
#/*              Copyright (c) 1988, 1989, 1990, 1991, 1992             */
#/*      An unpublished work of International Digital Scientific Inc.   */
#/*                         All rights reserved.                        */
#/*                                                                     */
#/***********************************************************************/
#
#
#       File:           LIBVIDEO.wat
#
#       Function:       Makefile for the video library on MS-DOS using
#                       Watcom compiler
#
#       History:        03/18/90        Written by GSL
#                       04/14/92        Moved system specific variables to $(PORT)/include.cfl, devo
#			06/04/92        Changed to use make.include GSL
#			11/04/92        Converted for MS-DOS.
#			12/08/93        Reviewed and updated to WISP 3.3C
#					release by KMT.  Deleted:
#					vfinishf.c, vinitfor.c & vseterro.c
#					and Added: visebug.c, vkeymap.c,
#					vloadfor.c, vplus.c & vprintfo.c
#					to makefile.
#			04/08/94	CBA, reset this makefile for the
#					Watcom compiler, using the MSVC's
#					nmake utility.
#			04/20/94	CBA, modified file so that either
#					wmake or nmake can use it

#CDEBUG = /od
CDEBUG = /d2

LIB = VIDEO.LIB

!include $(WISP)\src\port\dosmake.wat

.PRECIOUS:      $(LIB) 

all: $(STDLIB)\$(LIB)

$(STDLIB)\$(LIB): $(LIB)
	copy $(LIB) $@

OBJS=   gcal2.obj    \
	gcalc.obj    \
	gcalend.obj  \
	gclock.obj   \
	gnotepad.obj \
	gpuzzle.obj  \
	gzones.obj   \
	sleepdos.obj \
	strpos.obj   \
	valert.obj   \
	vbarup.obj   \
	vbell.obj    \
	vcap.obj     \
	vcharset.obj \
	vchstubs.obj \
	vclosefo.obj \
	vclosete.obj \
	vcommand.obj \
	vcontrol.obj \
	vcut.obj     \
	vdefer.obj   \
	vedge.obj    \
	verase.obj   \
	verrmsg.obj  \
	vexit.obj    \
	vfielded.obj \
	vfnkey.obj   \
	vform.obj    \
	vgetbuff.obj \
	vgetfiel.obj \
	vgetnext.obj \
	vgets0.obj   \
	vgrid.obj    \
	vinput.obj   \
	visdebug.obj \
	vkeymap.obj  \
	vline.obj    \
	vlist.obj    \
	vloadch.obj  \
	vloadfor.obj \
	vmacro.obj   \
	vmap.obj     \
	vmenu.obj    \
	vmode.obj    \
	vmove.obj    \
	vnewline.obj \
	vonexit.obj  \
	vop.obj      \
	vopenf.obj   \
	vopenfor.obj \
	vopenter.obj \
	vpaste.obj   \
	vplus.obj    \
	vpopscr.obj  \
	vprint.obj   \
	vprintfo.obj \
	vpushscr.obj \
	vputbuff.obj \
	vputc.obj    \
	vputwind.obj \
	vrawdos.obj  \
	vreadfie.obj \
	vrefresh.obj \
	vrelease.obj \
	vroll.obj    \
	vscreen.obj  \
	vsection.obj \
	vset.obj     \
	vsetkeyl.obj \
	vshowfor.obj \
	vsize.obj    \
	vslew.obj    \
	vstate.obj   \
	vsynch.obj   \
	vsystem.obj  \
	vtext.obj    \
	vtrace.obj   \
	vtrigger.obj \
	vtrim.obj    \
	vuserex.obj  \
	vutil.obj    \
	vwait.obj

$(LIB): $(OBJS)
	!$(WLIB) $(LIB) -+$?
	@echo lib $(LIB) is now up-to-date

clean:
	del $(LIB)
	del *.obj

gcal2.obj: gcal2.c video.h vlocal.h vdata.h vmenu.h
gcalc.obj: gcalc.c video.h vlocal.h vdata.h vmenu.h
gcalend.obj: gcalend.c video.h vlocal.h vdata.h vmenu.h
gclock.obj: gclock.c video.h vlocal.h vdata.h
gnotepad.obj: gnotepad.c video.h vlocal.h vdata.h vmenu.h
gpuzzle.obj: gpuzzle.c video.h vlocal.h vdata.h vmenu.h
gzones.obj: gzones.c
sleepdos.obj: sleepdos.c
strpos.obj: strpos.c
valert.obj: valert.c video.h vlocal.h vdata.h
vbarup.obj: vbarup.c video.h vmenu.h vlocal.h vdata.h
vbell.obj: vbell.c video.h vlocal.h vdata.h
vcap.obj: vcap.c video.h vcap.h
vcharset.obj: vcharset.c video.h vlocal.h vdata.h vcap.h
vchstubs.obj: vchstubs.c vchinese.h
vclosefo.obj: vclosefo.c video.h vform.h vintdef.h vplus.h
vclosete.obj: vclosete.c video.h vform.h vintdef.h vplus.h
vcommand.obj: vcommand.c
vcontrol.obj: vcontrol.c video.h vcap.h
vcut.obj: vcut.c video.h
vdefer.obj: vdefer.c video.h vlocal.h
vedge.obj: vedge.c video.h
verase.obj: verase.c video.h vlocal.h vdata.h vcap.h vmenu.h
verrmsg.obj: verrmsg.c video.h vform.h
vexit.obj: vexit.c video.h vlocal.h vcap.h vdata.h
vfielded.obj: vfielded.c video.h vform.h vintdef.h vplus.h
vfnkey.obj: vfnkey.c video.h vlocal.h vdata.h
vform.obj: vform.c video.h vlocal.h vdata.h vform.h
vgetbuff.obj: vgetbuff.c video.h vform.h vintdef.h vplus.h
vgetfiel.obj: vgetfiel.c video.h vform.h
vgetnext.obj: vgetnext.c video.h vintdef.h vplus.h vform.h
vgets0.obj: vgets0.c video.h
vgrid.obj: vgrid.c video.h vlocal.h vdata.h vcap.h
vinput.obj: vinput.c video.h vlocal.h vcap.h vdata.h
visdebug.obj: visdebug.c video.h vlocal.h vcap.h vdata.h
vkeymap.obj: vkeymap.c
vline.obj: vline.c video.h vlocal.h vdata.h vcap.h
vlist.obj: vlist.c video.h vlist.h vlocal.h vdata.h
vloadch.obj: vloadch.c video.h vlocal.h
vloadfor.obj: vloadfor.c video.h vlocal.h
vmacro.obj: vmacro.c video.h vlocal.h vdata.h
vmap.obj: vmap.c video.h vlocal.h
vmenu.obj: vmenu.c video.h vlocal.h vdata.h vcap.h vmenu.h
vmode.obj: vmode.c video.h vlocal.h vdata.h vcap.h
vmove.obj: vmove.c video.h vlocal.h vdata.h vcap.h
vnewline.obj: vnewline.c video.h vlocal.h vdata.h vcap.h
vonexit.obj: vonexit.c video.h vlocal.h
vop.obj: vop.c video.h
vopenf.obj: vopenf.c video.h vlocal.h vdata.h
vopenfor.obj: vopenfor.c video.h vintdef.h vplus.h vform.h
vopenter.obj: vopenter.c video.h vintdef.h vplus.h
vpaste.obj: vpaste.c video.h vlocal.h vdata.h
vplus.obj: vplus.c video.h vform.h vintdef.h vplus.h vlocal.h vdata.h
vpopscr.obj: vpopscr.c video.h vlocal.h
vprint.obj: vprint.c video.h vlocal.h
vprintfo.obj: vprintfo.c video.h vlocal.h
vpushscr.obj: vpushscr.c video.h vlocal.h
vputbuff.obj: vputbuff.c video.h vform.h vintdef.h vplus.h
vputc.obj: vputc.c video.h
vputwind.obj: vputwind.c video.h vform.h vintdef.h vplus.h
vrawdos.obj: vrawdos.c
vrawunix.obj: vrawunix.c video.h vlocal.h vrawunix.h vchinese.h
vrawvms1.obj: vrawvms1.c
vrawvms2.obj: vrawvms2.c
vreadfie.obj: vreadfie.c video.h vform.h vintdef.h vplus.h
vrefresh.obj: vrefresh.c video.h vlocal.h vdata.h vcap.h
vrelease.obj: vrelease.c video.h
vroll.obj: vroll.c video.h vlocal.h vdata.h vcap.h
vscreen.obj: vscreen.c video.h vlocal.h vdata.h vcap.h
vsection.obj: vsection.c video.h
vset.obj: vset.c video.h vlocal.h vdata.h vcap.h
vsetkeyl.obj: vsetkeyl.c video.h vlocal.h vdata.h vform.h
vsetkeyl.obj: vintdef.h vplus.h
vshowfor.obj: vshowfor.c video.h vform.h vintdef.h vplus.h
vsize.obj: vsize.c video.h vlocal.h vdata.h
vslew.obj: vslew.c video.h
vstate.obj: vstate.c video.h vlocal.h vdata.h vcap.h
vsynch.obj: vsynch.c video.h vlocal.h
vsystem.obj: vsystem.c video.h
vtext.obj: vtext.c video.h vlocal.h
vtrace.obj: vtrace.c
vtrigger.obj: vtrigger.c video.h
vtrim.obj: vtrim.c video.h
vuserex.obj: vuserex.c video.h vlocal.h
vutil.obj: vutil.c video.h vlocal.h vdata.h
vwait.obj: vwait.c video.h

