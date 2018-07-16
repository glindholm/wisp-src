# 
#	Copyright (c) 1995 DevTech Migrations, All rights reserved.
#	$Id:$
#
#	/************************************************************************/
#	/*									*/
#	/*		 Copyright (c) 1988, 1989, 1990, 1991, 1992		*/
#	/*	 An unpublished work of International Digital Scientific Inc.	*/
#	/*			    All rights reserved.			*/
#	/*									*/
#	/************************************************************************/
#
#
#	File:		libvideo.umf
#
#	Function:	Generic makefile for the video library on all unix platforms.
#
# 	History:	03/18/90	Written by GSL
#			04/14/92	Moved system specific variables to $(PORT)/include.cfl, devo
#			06/04/92	Changed to use make.include GSL
#

CDEBUG_D = -g
CDEBUG   = -O

L_VIDEO=video

THE_LIB   = lib$(L_VIDEO).a

include $(WISP)/src/port/make.include

.PRECIOUS:	$(THE_LIB) 

all: $(THE_LIB)

debug:
	$(MAKE) $(MAKEFLAGS) L_VIDEO=videod CDEBUG=$(CDEBUG_D)

clean:
	rm -f libvideo*.a *.o core *~

install: $(STDLIB)/$(THE_LIB)

$(STDLIB)/$(THE_LIB): $(THE_LIB)
	cp $(THE_LIB) $@

$(THE_LIB):	$(THE_LIB)(gcal2.o) \
	$(THE_LIB)(gcalc.o) \
	$(THE_LIB)(gcalend.o) \
	$(THE_LIB)(gclock.o) \
	$(THE_LIB)(gnotepad.o) \
	$(THE_LIB)(gpuzzle.o) \
	$(THE_LIB)(gzones.o) \
	$(THE_LIB)(strpos.o) \
	$(THE_LIB)(valert.o) \
	$(THE_LIB)(vbell.o) \
	$(THE_LIB)(vcap.o) \
	$(THE_LIB)(vcharset.o) \
	$(THE_LIB)(vchstubs.o) \
	$(THE_LIB)(vcommand.o) \
	$(THE_LIB)(vcontrol.o) \
	$(THE_LIB)(vcut.o) \
	$(THE_LIB)(vdefer.o) \
	$(THE_LIB)(vedge.o) \
	$(THE_LIB)(verase.o) \
	$(THE_LIB)(vexit.o) \
	$(THE_LIB)(vfnkey.o) \
	$(THE_LIB)(vgets0.o) \
	$(THE_LIB)(vgrid.o) \
	$(THE_LIB)(vinput.o) \
	$(THE_LIB)(visdebug.o) \
	$(THE_LIB)(vkeymap.o) \
	$(THE_LIB)(vline.o) \
	$(THE_LIB)(vlist.o) \
	$(THE_LIB)(vloadch.o) \
	$(THE_LIB)(vmacro.o) \
	$(THE_LIB)(vmap.o) \
	$(THE_LIB)(vmenu.o) \
	$(THE_LIB)(vmode.o) \
	$(THE_LIB)(vmove.o) \
	$(THE_LIB)(vnewline.o) \
	$(THE_LIB)(vonexit.o) \
	$(THE_LIB)(vop.o) \
	$(THE_LIB)(vopenf.o) \
	$(THE_LIB)(vpaste.o) \
	$(THE_LIB)(vpopscr.o) \
	$(THE_LIB)(vprint.o) \
	$(THE_LIB)(vpushscr.o) \
	$(THE_LIB)(vputc.o) \
	$(THE_LIB)(vrawunix.o) \
	$(THE_LIB)(vrefresh.o) \
	$(THE_LIB)(vrelease.o) \
	$(THE_LIB)(vroll.o) \
	$(THE_LIB)(vscreen.o) \
	$(THE_LIB)(vsection.o) \
	$(THE_LIB)(vset.o) \
	$(THE_LIB)(vsize.o) \
	$(THE_LIB)(vslew.o) \
	$(THE_LIB)(vstate.o) \
	$(THE_LIB)(vsynch.o) \
	$(THE_LIB)(vsystem.o) \
	$(THE_LIB)(vtext.o) \
	$(THE_LIB)(vtrace.o) \
	$(THE_LIB)(vtrigger.o) \
	$(THE_LIB)(vtrim.o) \
	$(THE_LIB)(vuserex.o) \
	$(THE_LIB)(vutil.o) \
	$(THE_LIB)(vwait.o) \
	$(THE_LIB)(version.o)
	$(RANLIB) $(THE_LIB)
	@echo lib $(THE_LIB) is now up-to-date


$(THE_LIB)(gcal2.o): gcal2.c video.h vlocal.h vdata.h vintdef.h vmenu.h
$(THE_LIB)(gcal2.o): vmodules.h
$(THE_LIB)(gcalc.o): gcalc.c video.h vlocal.h vdata.h vintdef.h vmenu.h
$(THE_LIB)(gcalc.o): vmodules.h
$(THE_LIB)(gcalend.o): gcalend.c video.h vlocal.h vdata.h vintdef.h
$(THE_LIB)(gcalend.o): vmenu.h vmodules.h
$(THE_LIB)(gclock.o): gclock.c video.h vlocal.h vdata.h vintdef.h
$(THE_LIB)(gclock.o): vmodules.h
$(THE_LIB)(gnotepad.o): gnotepad.c video.h vlocal.h vdata.h vintdef.h
$(THE_LIB)(gnotepad.o): vmenu.h vmodules.h
$(THE_LIB)(gpuzzle.o): gpuzzle.c video.h vlocal.h vdata.h vintdef.h
$(THE_LIB)(gpuzzle.o): vmenu.h vmodules.h
$(THE_LIB)(gzones.o): gzones.c
$(THE_LIB)(sleepdos.o): sleepdos.c
$(THE_LIB)(strpos.o): strpos.c
$(THE_LIB)(valert.o): valert.c video.h vlocal.h vintdef.h vdata.h
$(THE_LIB)(valert.o): vmodules.h
$(THE_LIB)(vbell.o): vbell.c video.h vlocal.h vintdef.h vdata.h
$(THE_LIB)(vbell.o): vmodules.h
$(THE_LIB)(vcap.o): vcap.c video.h vlocal.h vcap.h vintdef.h vmodules.h
$(THE_LIB)(vcharset.o): vcharset.c video.h vlocal.h vdata.h vcap.h
$(THE_LIB)(vcharset.o): vmodules.h vintdef.h
$(THE_LIB)(vchstubs.o): vchstubs.c vchinese.h
$(THE_LIB)(vcommand.o): vcommand.c
$(THE_LIB)(vcontrol.o): vcontrol.c video.h vcap.h vintdef.h vmodules.h
$(THE_LIB)(vcut.o): vcut.c video.h
$(THE_LIB)(vdefer.o): vdefer.c video.h vlocal.h vintdef.h vdata.h
$(THE_LIB)(vdefer.o): vmodules.h
$(THE_LIB)(vedge.o): vedge.c video.h
$(THE_LIB)(verase.o): verase.c video.h vlocal.h vdata.h vcap.h
$(THE_LIB)(verase.o): vintdef.h vmenu.h vmodules.h
$(THE_LIB)(vexit.o): vexit.c video.h vlocal.h vcap.h vintdef.h vdata.h
$(THE_LIB)(vexit.o): vmodules.h
$(THE_LIB)(vfnkey.o): vfnkey.c video.h vlocal.h vdata.h
$(THE_LIB)(vgets0.o): vgets0.c video.h vmodules.h vintdef.h
$(THE_LIB)(vgrid.o): vgrid.c video.h vlocal.h vdata.h vcap.h vmodules.h
$(THE_LIB)(vgrid.o): vintdef.h
$(THE_LIB)(vinput.o): vinput.c video.h vlocal.h vcap.h vdata.h
$(THE_LIB)(vinput.o): vmodules.h vintdef.h
$(THE_LIB)(visdebug.o): visdebug.c video.h
$(THE_LIB)(vkeymap.o): vkeymap.c vkeymap.h vintdef.h vmodules.h
$(THE_LIB)(vline.o): vline.c video.h vlocal.h vmodules.h vintdef.h
$(THE_LIB)(vline.o): vdata.h vcap.h
$(THE_LIB)(vlist.o): vlist.c vlist.h video.h vlocal.h vdata.h
$(THE_LIB)(vlist.o): vmodules.h vintdef.h
$(THE_LIB)(vloadch.o): vloadch.c video.h vlocal.h
$(THE_LIB)(vmacro.o): vmacro.c video.h vlocal.h vdata.h vmodules.h
$(THE_LIB)(vmacro.o): vintdef.h
$(THE_LIB)(vmap.o): vmap.c video.h vlocal.h vmodules.h vintdef.h
$(THE_LIB)(vmenu.o): vmenu.c video.h vlocal.h vdata.h vcap.h vintdef.h
$(THE_LIB)(vmenu.o): vmenu.h vmodules.h
$(THE_LIB)(vmode.o): vmode.c video.h vlocal.h vmodules.h vintdef.h
$(THE_LIB)(vmode.o): vdata.h vcap.h
$(THE_LIB)(vmove.o): vmove.c video.h vlocal.h vdata.h vcap.h vmodules.h
$(THE_LIB)(vmove.o): vintdef.h
$(THE_LIB)(vnewline.o): vnewline.c video.h vlocal.h vdata.h vcap.h
$(THE_LIB)(vnewline.o): vmodules.h vintdef.h
$(THE_LIB)(vonexit.o): vonexit.c video.h vlocal.h vmodules.h vintdef.h
$(THE_LIB)(vop.o): vop.c video.h vmodules.h vintdef.h
$(THE_LIB)(vopenf.o): vopenf.c video.h vlocal.h vdata.h vmodules.h
$(THE_LIB)(vopenf.o): vintdef.h
$(THE_LIB)(vpaste.o): vpaste.c video.h vlocal.h vdata.h
$(THE_LIB)(vpopscr.o): vpopscr.c video.h vlocal.h vmodules.h vintdef.h
$(THE_LIB)(vprint.o): vprint.c video.h vlocal.h vmodules.h vintdef.h
$(THE_LIB)(vpushscr.o): vpushscr.c video.h vlocal.h vintdef.h
$(THE_LIB)(vpushscr.o): vmodules.h
$(THE_LIB)(vputc.o): vputc.c video.h vmodules.h vintdef.h
$(THE_LIB)(vrawdos.o): vrawdos.c
$(THE_LIB)(vrawunix.o): vrawunix.c
$(THE_LIB)(vrawvms1.o): vrawvms1.c
$(THE_LIB)(vrawvms2.o): vrawvms2.c
$(THE_LIB)(vrefresh.o): vrefresh.c video.h vlocal.h vdata.h vcap.h
$(THE_LIB)(vrefresh.o): vmodules.h vintdef.h
$(THE_LIB)(vrelease.o): vrelease.c video.h vmodules.h vintdef.h
$(THE_LIB)(vroll.o): vroll.c video.h vlocal.h vdata.h vcap.h vmodules.h
$(THE_LIB)(vroll.o): vintdef.h
$(THE_LIB)(vscreen.o): vscreen.c video.h vlocal.h vdata.h vcap.h
$(THE_LIB)(vscreen.o): vmodules.h vintdef.h
$(THE_LIB)(vsection.o): vsection.c video.h vmodules.h vintdef.h
$(THE_LIB)(vset.o): vset.c video.h vlocal.h vdata.h vcap.h vmodules.h
$(THE_LIB)(vset.o): vintdef.h
$(THE_LIB)(vsize.o): vsize.c video.h vlocal.h vdata.h vmodules.h
$(THE_LIB)(vsize.o): vintdef.h
$(THE_LIB)(vslew.o): vslew.c video.h vmodules.h vintdef.h
$(THE_LIB)(vstate.o): vstate.c video.h vlocal.h vdata.h vcap.h
$(THE_LIB)(vstate.o): vmodules.h vintdef.h
$(THE_LIB)(vsynch.o): vsynch.c video.h vlocal.h vmodules.h vintdef.h
$(THE_LIB)(vsystem.o): vsystem.c video.h
$(THE_LIB)(vtext.o): vtext.c video.h vlocal.h vmodules.h vintdef.h
$(THE_LIB)(vtrace.o): vtrace.c vmodules.h vintdef.h
$(THE_LIB)(vtrigger.o): vtrigger.c video.h
$(THE_LIB)(vtrim.o): vtrim.c video.h vmodules.h vintdef.h
$(THE_LIB)(vuserex.o): vuserex.c video.h vlocal.h
$(THE_LIB)(vutil.o): vutil.c video.h vlocal.h vdata.h vmodules.h
$(THE_LIB)(vutil.o): vintdef.h
$(THE_LIB)(vwait.o): vwait.c video.h
$(THE_LIB)(version.o): version.c
