#	/************************************************************************/
#	/*									*/
#	/*		 Copyright (c) 1988, 1989, 1990, 1991, 1992		*/
#	/*	 An unpublished work of International Digital Scientific Inc.	*/
#	/*			    All rights reserved.			*/
#	/*									*/
#	/************************************************************************/
#
#
#	File:		vtest.umf
#
#	Function:	Generic makefile for VTEST on all unix platforms.
#
# 	History:
#			06/04/92	Changed to use make.include GSL
#			06/09/92	Removed copy. GSL
#

CDEBUG = -g
#CDEBUG = -O

include $(WISP)/src/port/make.include


OBJS = 	vtest0.o vtesta.o vtestb.o vtestc.o vtestd.o vteste.o \
	vtestf.o vtestg.o vtesth.o vtesti.o vtestj.o vtestk.o vtestl.o   \
	vtestm.o vtestn.o vtesto.o vtestp.o vtestq.o vtestr.o vtests.o   \
	vtestt.o vtestu.o vtestv.o vtestw.o vtestx.o vtesty.o vtestz.o

LIBPATHS= $(STDLIB)/libvideo.a
LIBS= $(LIBCFLAGS) -lvideo

vtest : $(OBJS) $(LIBPATHS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LIBS)

dump_map.o: dump_map.c  $(V)/video.h
vtest0.o:   vtest0.c    $(V)/video.h
vtesta.o:   vtesta.c    $(V)/video.h
vtestb.o:   vtestb.c    $(V)/video.h
vtestc.o:   vtestc.c    $(V)/video.h
vtestd.o:   vtestd.c    $(V)/video.h
vteste.o:   vteste.c    $(V)/video.h
vtestf.o:   vtestf.c    $(V)/video.h
vtestg.o:   vtestg.c    $(V)/video.h
vtesth.o:   vtesth.c    $(V)/video.h
vtesti.o:   vtesti.c    $(V)/video.h
vtestj.o:   vtestj.c    $(V)/video.h
vtestk.o:   vtestk.c    $(V)/video.h
vtestl.o:   vtestl.c    $(V)/video.h
vtestm.o:   vtestm.c    $(V)/video.h
vtestn.o:   vtestn.c    $(V)/video.h
vtesto.o:   vtesto.c    $(V)/video.h   $(V)/vlocal.h
vtestp.o:   vtestp.c    $(V)/video.h
vtestq.o:   vtestq.c    $(V)/video.h
vtestr.o:   vtestr.c    $(V)/video.h
vtests.o:   vtests.c    $(V)/video.h
vtestt.o:   vtestt.c    $(V)/video.h
vtestu.o:   vtestu.c    $(V)/video.h
vtestv.o:   vtestv.c    $(V)/video.h
vtestw.o:   vtestw.c    $(V)/video.h
vtestx.o:   vtestx.c    $(V)/video.h
vtesty.o:   vtesty.c    $(V)/video.h
vtestz.o:   vtestz.c    $(V)/video.h

