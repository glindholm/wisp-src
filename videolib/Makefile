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

#CDEBUG = -g
CDEBUG = -O

#LIB = libvideod.a
LIB = libvideo.a

include $(WISP)/src/port/make.include

.PRECIOUS:	$(LIB) 

all: $(STDLIB)/$(LIB)

$(STDLIB)/$(LIB): $(LIB)
	cp $(LIB) $@

$(LIB):	$(LIB)(gcal2.o) \
	$(LIB)(gcalc.o) \
	$(LIB)(gcalend.o) \
	$(LIB)(gclock.o) \
	$(LIB)(gnotepad.o) \
	$(LIB)(gpuzzle.o) \
	$(LIB)(gzones.o) \
	$(LIB)(strpos.o) \
	$(LIB)(vbarup.o) \
	$(LIB)(vbell.o) \
	$(LIB)(vcap.o) \
	$(LIB)(vcharset.o) \
	$(LIB)(vcommand.o) \
	$(LIB)(vcontrol.o) \
	$(LIB)(vcut.o) \
	$(LIB)(vdefer.o) \
	$(LIB)(vedge.o) \
	$(LIB)(verase.o) \
	$(LIB)(vexit.o) \
	$(LIB)(vfnkey.o) \
	$(LIB)(vgets0.o) \
	$(LIB)(vgrid.o) \
	$(LIB)(vinput.o) \
	$(LIB)(vline.o) \
	$(LIB)(vlist.o) \
	$(LIB)(vloadch.o) \
	$(LIB)(vmacro.o) \
	$(LIB)(vmap.o) \
	$(LIB)(vmenu.o) \
	$(LIB)(vmode.o) \
	$(LIB)(vmove.o) \
	$(LIB)(vnewline.o) \
	$(LIB)(vonexit.o) \
	$(LIB)(vop.o) \
	$(LIB)(vopenf.o) \
	$(LIB)(vpaste.o) \
	$(LIB)(vpopscr.o) \
	$(LIB)(vprint.o) \
	$(LIB)(vpushscr.o) \
	$(LIB)(vputc.o) \
	$(LIB)(vrawunix.o) \
	$(LIB)(vrefresh.o) \
	$(LIB)(vrelease.o) \
	$(LIB)(vroll.o) \
	$(LIB)(vscreen.o) \
	$(LIB)(vsection.o) \
	$(LIB)(vset.o) \
	$(LIB)(vsize.o) \
	$(LIB)(vslew.o) \
	$(LIB)(vstate.o) \
	$(LIB)(vsynch.o) \
	$(LIB)(vsystem.o) \
	$(LIB)(vtext.o) \
	$(LIB)(vtrace.o) \
	$(LIB)(vtrigger.o) \
	$(LIB)(vtrim.o) \
	$(LIB)(vuserex.o) \
	$(LIB)(vutil.o) \
	$(LIB)(vwait.o)
	$(RANLIB) $(LIB)
	@echo lib $(LIB) is now up-to-date




$(LIB)(gcal2.o): gcal2.c video.h vlocal.h vdata.h vmenu.h
$(LIB)(gcalc.o): gcalc.c video.h vlocal.h vdata.h vmenu.h
$(LIB)(gcalend.o): gcalend.c video.h vlocal.h vdata.h vmenu.h
$(LIB)(gclock.o): gclock.c video.h vlocal.h vdata.h
$(LIB)(gnotepad.o): gnotepad.c video.h vlocal.h vdata.h vmenu.h
$(LIB)(gpuzzle.o): gpuzzle.c video.h vlocal.h vdata.h vmenu.h
$(LIB)(gzones.o): gzones.c
$(LIB)(strpos.o): strpos.c
$(LIB)(vbarup.o): vbarup.c video.h vmenu.h vlocal.h vdata.h
$(LIB)(vbell.o): vbell.c video.h vlocal.h vdata.h
$(LIB)(vcap.o): vcap.c video.h vcap.h
$(LIB)(vcharset.o): vcharset.c video.h vlocal.h vdata.h vcap.h
$(LIB)(vcommand.o): vcommand.c
$(LIB)(vcontrol.o): vcontrol.c video.h vcap.h
$(LIB)(vcut.o): vcut.c video.h
$(LIB)(vdefer.o): vdefer.c video.h vlocal.h
$(LIB)(vedge.o): vedge.c video.h
$(LIB)(verase.o): verase.c video.h vlocal.h vdata.h vcap.h vmenu.h
$(LIB)(vexit.o): vexit.c video.h vlocal.h vcap.h vdata.h
$(LIB)(vfnkey.o): vfnkey.c video.h vlocal.h vdata.h
$(LIB)(vgets0.o): vgets0.c video.h
$(LIB)(vgrid.o): vgrid.c video.h vlocal.h vdata.h vcap.h
$(LIB)(vinput.o): vinput.c video.h vlocal.h vcap.h vdata.h
$(LIB)(vline.o): vline.c video.h vlocal.h vdata.h vcap.h
$(LIB)(vlist.o): vlist.c video.h vlist.h vlocal.h vdata.h
$(LIB)(vloadch.o): vloadch.c video.h vlocal.h
$(LIB)(vmacro.o): vmacro.c video.h vlocal.h vdata.h
$(LIB)(vmap.o): vmap.c video.h vlocal.h
$(LIB)(vmenu.o): vmenu.c video.h vlocal.h vdata.h vcap.h vmenu.h
$(LIB)(vmode.o): vmode.c video.h vlocal.h vdata.h vcap.h
$(LIB)(vmove.o): vmove.c video.h vlocal.h vdata.h vcap.h
$(LIB)(vnewline.o): vnewline.c video.h vlocal.h vdata.h vcap.h
$(LIB)(vonexit.o): vonexit.c video.h vlocal.h
$(LIB)(vop.o): vop.c video.h
$(LIB)(vopenf.o): vopenf.c video.h vlocal.h vdata.h
$(LIB)(vpaste.o): vpaste.c video.h vlocal.h vdata.h
$(LIB)(vpopscr.o): vpopscr.c video.h vlocal.h
$(LIB)(vprint.o): vprint.c video.h vlocal.h
$(LIB)(vpushscr.o): vpushscr.c video.h vlocal.h
$(LIB)(vputc.o): vputc.c video.h
$(LIB)(vrawunix.o): vrawunix.c video.h vlocal.h vrawunix.h
$(LIB)(vrefresh.o): vrefresh.c video.h vlocal.h vdata.h vcap.h
$(LIB)(vrelease.o): vrelease.c video.h
$(LIB)(vroll.o): vroll.c video.h vlocal.h vdata.h vcap.h
$(LIB)(vscreen.o): vscreen.c video.h vlocal.h vdata.h vcap.h
$(LIB)(vsection.o): vsection.c video.h
$(LIB)(vset.o): vset.c video.h vlocal.h vdata.h vcap.h
$(LIB)(vsize.o): vsize.c video.h vlocal.h vdata.h
$(LIB)(vslew.o): vslew.c video.h
$(LIB)(vstate.o): vstate.c video.h vlocal.h vdata.h vcap.h
$(LIB)(vsynch.o): vsynch.c video.h vlocal.h
$(LIB)(vsystem.o): vsystem.c video.h
$(LIB)(vtext.o): vtext.c video.h vlocal.h
$(LIB)(vtrace.o): vtrace.c
$(LIB)(vtrigger.o): vtrigger.c video.h
$(LIB)(vtrim.o): vtrim.c video.h
$(LIB)(vuserex.o): vuserex.c video.h vlocal.h
$(LIB)(vutil.o): vutil.c video.h vlocal.h vdata.h
$(LIB)(vwait.o): vwait.c video.h
