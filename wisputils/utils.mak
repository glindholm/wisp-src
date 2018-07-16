#/************************************************************************/
#/*                                                                     */
#/*              Copyright (c) 1988, 1989, 1990, 1991, 1992             */
#/*      An unpublished work of International Digital Scientific Inc.   */
#/*                         All rights reserved.                        */
#/*                                                                     */
#/************************************************************************/
#
#
#       File:           utils.mak
#
#       Function:       Makefile for the WISP utilities on MS-DOS using
#                       the Intel C Code Builder.
#
#       History:        
#                       06/04/92        Changed to use make.include GSL
#                       06/09/92        Removed copy stmts. GSL
#                       06/18/92        Added target for bldmf
#                       11/24/92        modified for code builder. GSL

CDEBUG = 

!include $(WISP)\src\port\dosmake.mak

LIBPATHS= $(STDLIB)\wisp.lib $(STDLIB)\video.lib
LINKLIBS= $(STDLIB)\wisp.lib $(STDLIB)\video.lib graphics.lib

IDXDISPLAY=vsn_only.obj

all:    display.exe \
	makemsg.exe \
	wcopy.exe \
	wputparm.exe \
	wrun.exe \
	wsort.exe \
	wlicense.exe \
	wusage.exe 

$(STDBIN):
	mkdir $(STDBIN)

display.exe: display.obj $(LIBPATHS) $(STDBIN) $(IDXDISPLAY)
	echo $(LINKLIBS) $(IDXDISPLAY) $(LINKACU) >make.rsp
	$(CC) $(CDEBUG) /e $@ /s32000 display.obj @make.rsp
	copy $@ $(STDBIN)

makemsg.exe: makemsg.obj $(STDBIN)
	$(CC) $(CDEBUG) /e $@ makemsg.obj
	copy $@ $(STDBIN)

wputparm.exe: wputparm.obj $(LIBPATHS) $(STDBIN)
	$(CC) $(CDEBUG) /e $@ /s32000 wputparm.obj $(LINKLIBS)
	copy $@ $(STDBIN)

wrun.exe: wrun.obj $(LIBPATHS) $(STDBIN)
	$(CC) $(CDEBUG) /e $@ wrun.obj $(LINKLIBS)
	copy $@ $(STDBIN)

wsort.exe: wsort.obj $(LIBPATHS) $(STDBIN) $(IDXDISPLAY)
	echo $(LINKLIBS) $(IDXDISPLAY) $(LINKACU) >make.rsp
	$(CC) $(CDEBUG) /e $@ /s32000 wsort.obj $(LINKLIBS) @make.rsp
	copy $@ $(STDBIN)

wcopy.exe: wcopy.obj $(LIBPATHS) $(STDBIN) $(IDXDISPLAY)
	echo $(LINKLIBS) $(IDXDISPLAY) $(LINKACU) >make.rsp
	$(CC) $(CDEBUG) /e $@ /s32000 wcopy.obj @MAKE.RSP
	copy $@ $(STDBIN)

wlicense.exe: wlicense.obj prompt.obj $(LIBPATHS) $(STDBIN)
	$(CC) $(CDEBUG) /e $@ wlicense.obj prompt.obj $(LINKLIBS)
	copy $@ $(STDBIN)

wusage.exe: wusage.obj $(LIBPATHS) $(STDBIN) $(IDXDISPLAY)
	echo $(LINKLIBS) $(IDXDISPLAY) $(LINKACU) >make.rsp
	$(CC) $(CDEBUG) /e $@ /s32000 wusage.obj @make.rsp
	copy $@ $(STDBIN)

hexed.exe: hexed.obj $(LIBPATHS) $(STDBIN)
	$(CC) $(CDEBUG) /e $@ /s32000 hexed.obj $(LINKLIBS)
	copy $@ $(STDBIN)

vsn_only.obj: vsn_only.c
	$(CC) $(CFLAGS) -c vsn_only.c

#############################################################
rest:   selectpg wdelwrk wexists  \
	wrename wscratch wsysconf wsysinit  \
	viewkey vcapkeys wdiag

selectpg: selectpg.c
	$(CC) $(CFLAGS) /e $@ selectpg.c

wcopy: wcopy.c $(LIBPATHS)
	$(CC) $(CFLAGS) -o $@ wcopy.c $(LIBS)

wdelwrk: wdelwrk.c $(LIBPATHS)
	$(CC) $(CFLAGS) -o $@ wdelwrk.c $(LIBS)

wdiag:  wdiag.c
	$(CC) $(CFLAGS) -o $@ wdiag.c

wexists: wexists.obj $(LIBPATHS)
	$(CC) /e $@ wexists.obj $(LINKLIBS)

wfind: wfind.c $(LIBPATHS)
	$(CC) $(CFLAGS) -o $@ wfind.c $(LIBS)

wrename: wrename.c $(LIBPATHS)
	$(CC) $(CFLAGS) -o $@ wrename.c $(LIBS)

wretcode: wretcode.c $(LIBPATHS)
	$(CC) $(CFLAGS) -o $@ wretcode.c $(LIBS)

wscratch: wscratch.c $(LIBPATHS)
	$(CC) $(CFLAGS) -o $@ wscratch.c $(LIBS)

wsubmit: wsubmit.c $(LIBPATHS)
	$(CC) $(CFLAGS) -o $@ wsubmit.c $(LIBS)

wsysconf: wsysconf.obj $(LIBPATHS)
	$(CC) /e $@ wsysconf.obj $(LINKLIBS)

wsysinit: wsysinit.c $(LIBPATHS)
	$(CC) $(CFLAGS) -o $@ wsysinit.c $(LIBS)

viewkey: viewkey.c 
	$(CC) $(CFLAGS) -o $@ viewkey.c

vsx: vsx.c vsx.h
	$(CC) $(CFLAGS) -o $@ vsx.c

wpfkey: wpfkey.c $(LIBPATHS)
	$(CC) $(CFLAGS) -o $@ wpfkey.c $(LIBS)

vcapkeys: vcapkeys.c 
	$(CC) $(CFLAGS) -o $@ vcapkeys.c

wlicense: wlicense.c prompt.o
	$(CC) $(CFLAGS) -o $@ wlicense.c prompt.o $(LIBS)


