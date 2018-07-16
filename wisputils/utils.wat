#/************************************************************************/
#/*									*/
#/*		 Copyright (c) 1988, 1989, 1990, 1991, 1992		*/
#/*	 An unpublished work of International Digital Scientific Inc.	*/
#/*			    All rights reserved.			*/
#/*									*/
#/************************************************************************/
#
#
#	File:		utils.wat
#
#	Function:	Makefile for the WISP utilities on MS-DOS using
#			the MSC.
#
# 	History:	
#			06/04/92	Changed to use make.include GSL
#			06/09/92	Removed copy stmts. GSL
#                       06/18/92        Added target for bldmf
#			11/24/92	modified for code builder. GSL
#			04/15/94	modified for Watcom. CBA
#			04/20/94	CBA, modified file so that either
#					wmake or nmake can use it
#
#	NOTES ON WATCOM CONFIG.
#	Have to find a way to use descriptor files because exceeding
#	the command line limit of 127 characters

#CDEBUG = /od
CDEBUG = /d2

!include $(WISP)\src\port\dosmake.wat

LIBPATHS=$(STDLIB)\wisp.lib $(STDLIB)\video.lib
LIBVIDEO=$(STDLIB)\video.lib
LIBWISP=$(STDLIB)\wisp.lib
LIBEDE=$(STDLIB)\ede.lib

IDXDISPLAY=vsn_only.obj

EXES=	display.exe makemsg.exe wcopy.exe wlicense.exe wputparm.exe wrun.exe \
	wsort.exe wusage.exe  wdiag.exe

all:	$(EXES)

clean:
	del *.obj
	del *.exe

display.exe: display.obj $(LIBPATHS) $(IDXDISPLAY) 
	echo debug all			>make.rsp
	echo option CASEEXACT		>>make.rsp
	echo file display.obj		>>make.rsp
	echo file $(SUBOBJ)		>>make.rsp
	echo file $(IDXDISPLAY) 	>>make.rsp
	echo file $(ACUOBJ)		>>make.rsp
	echo library $(ACULIB)		>>make.rsp
	echo library $(LIBWISP)		>>make.rsp
	echo library $(LIBVIDEO)	>>make.rsp
	set wcl386=$(CFLAGS0)
	$(CC) /fe=$@ @make.rsp
	$(BIND4GW) $@

display.obj:
	set wcl386=$(CFLAGS)
	$(CC) /c display.c

makemsg.exe: makemsg.c
	echo debug all			>make.rsp
	echo option CASEEXACT		>>make.rsp
	set wcl386=$(CFLAGS0)
	$(CC) makemsg.c /fe=$@ @make.rsp
	$(BIND4GW) $@

wputparm.exe: wputparm.obj $(LIBPATHS)
	echo debug all			>make.rsp
	echo option CASEEXACT		>>make.rsp
	echo file wputparm.obj		>>make.rsp
	echo library $(LIBWISP)		>>make.rsp
	echo library $(LIBVIDEO)	>>make.rsp
	set wcl386=$(CFLAGS0)
	$(CC) /fe=$@ @make.rsp 
	$(BIND4GW) $@

wputparm.obj:
	set wcl386=$(CFLAGS)
	$(CC) /c wputparm.c

wrun.exe: wrun.obj $(LIBPATHS)
	echo debug all			>make.rsp
	echo option CASEEXACT		>>make.rsp
	echo file wrun.obj		>>make.rsp
	echo library $(LIBWISP)		>>make.rsp
	echo library $(LIBVIDEO)	>>make.rsp
	set wcl386=$(CFLAGS0)
	$(CC) /fe=$@ @make.rsp
	$(BIND4GW) $@

wrun.obj:
	set wcl386=$(CFLAGS)
	$(CC) /c wrun.c

wsort.exe: wsort.obj $(LIBPATHS) $(IDXDISPLAY)
	echo debug all			>make.rsp
	echo option CASEEXACT		>>make.rsp
	echo file wsort.obj		>>make.rsp
	echo file $(SUBOBJ)		>>make.rsp
	echo file $(IDXDISPLAY) 	>>make.rsp
	echo file $(ACUOBJ)		>>make.rsp
	echo library $(LIBWISP)		>>make.rsp
	echo library $(LIBVIDEO)	>>make.rsp
	echo library $(ACULIB) 		>>make.rsp
	set wcl386=$(CFLAGS0)
	$(CC) /fe=$@ @make.rsp
	$(BIND4GW) $@

wsort.obj:
	set wcl386=$(CFLAGS)
	$(CC) /c wsort.c

wcopy.exe: wcopy.obj $(LIBPATHS) $(IDXDISPLAY)
	echo debug all			>make.rsp
	echo option CASEEXACT		>>make.rsp
	echo file wcopy.obj		>>make.rsp
	echo file $(SUBOBJ)		>>make.rsp
	echo file $(IDXDISPLAY)		>>make.rsp
	echo file $(ACUOBJ)		>>make.rsp
	echo library $(ACULIB)		>>make.rsp
	echo library $(LIBWISP)		>>make.rsp
	echo library $(LIBVIDEO)	>>make.rsp
	set wcl386=$(CFLAGS0)
	$(CC) /fe=$@ @make.rsp
	$(BIND4GW) $@

wcopy.obj:
	set wcl386=$(CFLAGS)
	$(CC) /c wcopy.c

wlicense.exe: wlicense.obj prompt.obj $(LIBPATHS)
	echo debug all	>make.rsp
	echo option CASEEXACT		>>make.rsp
	echo file wlicense.obj		>>make.rsp
	echo file prompt.obj		>>make.rsp
	echo library $(LIBWISP)		>>make.rsp
	echo library $(LIBVIDEO)	>>make.rsp
	set wcl386=$(CFLAGS0)
	$(CC) /fe=$@ @make.rsp
	$(BIND4GW) $@

wlicense.obj:
	set wcl386=$(CFLAGS)
	$(CC) /c wlicense.c

wusage.exe: wusage.obj $(LIBPATHS) $(IDXDISPLAY)
	echo debug all			>make.rsp
	echo option CASEEXACT		>>make.rsp
	echo file wusage.obj		>>make.rsp
	echo file $(SUBOBJ)		>>make.rsp
	echo file $(IDXDISPLAY) 	>>make.rsp
	echo file $(ACUOBJ)		>>make.rsp
	echo library $(ACULIB)		>>make.rsp
	echo library $(LIBWISP)		>>make.rsp
	echo library $(LIBVIDEO)	>>make.rsp
	set wcl386=$(CFLAGS0)
	$(CC) /fe=$@ @make.rsp
	$(BIND4GW) $@

wusage.obj:
	set wcl386=$(CFLAGS)

wdiag.exe: wdiag.obj $(LIBPATHS)
	echo debug all			>make.rsp
	echo option CASEEXACT		>>make.rsp
	echo file wdiag.obj		>>make.rsp
	echo library $(LIBWISP)		>>make.rsp
	set wcl386=$(CFLAGS0)
	$(CC) /fe=$@ @make.rsp 
	$(BIND4GW) $@

wdiag.obj:
	set wcl386=$(CFLAGS)
	$(CC) /c wdiag.c

hexed.exe: hexed.obj $(LIBPATHS)
	echo debug all			>make.rsp
	echo option CASEEXACT		>>make.rsp
	echo file hexed.obj		>>make.rsp
	echo library $(LIBWISP)		>>make.rsp
	echo library $(LIBVIDEO)	>>make.rsp
	set wcl386=$(CFLAGS0)
	$(CC) /fe=$@ @make.rsp
	$(BIND4GW) $@

hexed.obj:
	set wcl386=$(CFLAGS)
	$(CC) /c hexed.c

vsn_only.obj: vsn_only.c
	set wcl386=$(CFLAGS)
	$(CC) /c vsn_only.c

prompt.obj: prompt.c
	set wcl386=$(CFLAGS)
	$(CC) /c prompt.c

#############################################################
rest: 	wdelwrk wexists \
	wrename wscratch wsysconf wsysinit  \
	viewkey vcapkeys wdiag

#selectpg: selectpg.c getopts.obj
selectpg: selectpg.c
	set wcl386=$(CFLAGS0)
	$(CC) /fe=$@ selectpg.c 

wcopy: wcopy.c $(LIBPATHS)
	echo file $(LIBWISP)	>make.rsp
	echo file $(LIBVIDEO)	>>make.rsp
	$(CC) /fe=$@ $(CFLAGS0) wcopy.c @make.rsp

wdelwrk: wdelwrk.c $(LIBPATHS)
	echo file $(LIBWISP)	>make.rsp
	echo file $(LIBVIDEO)	>>make.rsp
	$(CC) /fe=$@ $(CFLAGS0) wdelwrk.c @make.rsp

wdiag: 	wdiag.c
	$(CC) /fe=$@ $(CFLAGS0) wdiag.c 

wexists: wexists.obj $(LIBPATHS)
	echo file $(LIBWISP)	>make.rsp
	echo file $(LIBVIDEO)	>>make.rsp
	$(CC) /fe=$@ $(CFLAGS0) wexists.obj @make.rsp

wfind: wfind.c $(LIBPATHS)
	echo file $(LIBWISP)	>make.rsp
	echo file $(LIBVIDEO)	>>make.rsp
	$(CC) /fe=$@ $(CFLAGS0) wfind.c @make.rsp

wrename: wrename.c $(LIBPATHS)
	echo file $(LIBWISP)	>make.rsp
	echo file $(LIBVIDEO)	>>make.rsp
	$(CC) /fe=$@ $(CFLAGS0) wrename.c @make.rsp

wretcode: wretcode.c $(LIBPATHS)
	echo file $(LIBWISP)	>make.rsp
	echo file $(LIBVIDEO)	>>make.rsp
	$(CC) /fe=$@ $(CFLAGS0) wretcode.c @make.rsp

wscratch: wscratch.c $(LIBPATHS)
	echo file $(LIBWISP)	>make.rsp
	echo file $(LIBVIDEO)	>>make.rsp
	$(CC) /fe=$@ $(CFLAGS0) wscratch.c @make.rsp

wsubmit: wsubmit.c $(LIBPATHS)
	echo file $(LIBWISP)	>make.rsp
	echo file $(LIBVIDEO)	>>make.rsp
	$(CC) /fe=$@ $(CFLAGS0) wsubmit.c @make.rsp

wsysconf: wsysconf.obj $(LIBPATHS)
	echo file $(LIBWISP)	>make.rsp
	echo file $(LIBVIDEO)	>>make.rsp
	$(CC) /fe=$@ $(CFLAGS0) wsysconf.obj @make.rsp

wsysinit: wsysinit.c $(LIBPATHS)
	echo file $(LIBWISP)	>make.rsp
	echo file $(LIBVIDEO)	>>make.rsp
	$(CC) /fe=$@ $(CFLAGS0) wsysinit.c @make.rsp

viewkey: viewkey.c 
	$(CC) /fe=$@ $(CFLAGS0) viewkey.c

vsx: vsx.c vsx.h
	$(CC) /fe=$@ $(CFLAGS0) vsx.c

wpfkey: wpfkey.c $(LIBPATHS)
	echo file $(LIBWISP)	>make.rsp
	echo file $(LIBVIDEO)	>>make.rsp
	$(CC) /fe=$@ $(CFLAGS0) wpfkey.c @make.rsp

vcapkeys: vcapkeys.c 
	$(CC) /fe=$@ $(CFLAGS0) vcapkeys.c

#getopts.obj:
#	$(CC) $(CFLAGS) getopts.c /c
