#	/************************************************************************/
#	/*									*/
#	/*		 Copyright (c) 1988, 1989, 1990, 1991, 1992		*/
#	/*	 An unpublished work of International Digital Scientific Inc.	*/
#	/*			    All rights reserved.			*/
#	/*									*/
#	/************************************************************************/
#
#
#	File:		utils.umf
#
#	Function:	Generic makefile for the WISP utilities on all unix platforms.
#
# 	History:	
#			06/04/92	Changed to use make.include GSL
#			06/09/92	Removed copy stmts. GSL
#                       06/18/92        Added target for bldmf

#CDEBUG = -g
CDEBUG = -O

include $(WISP)/src/port/make.include

LIBS= $(LIBCFLAGS) -lwisp -lilp -lvideo -lm
LIBPATHS= $(STDLIB)/libwisp.a $(STDLIB)/libilp.a $(STDLIB)/libvideo.a


all: 	bldmf display makemsg selectpg wcopy wdelay wdelwrk wexists wfind \
	wputparm wrename wretcode wrun wscratch wsubmit wsysconf wsysinit wusage \
	viewkey vsx wsort vcapkeys wlicense wdiag

bldmf: bldmf.c bldmf.h bldmf_l.l $(LIBPATHS)
	rm -f bldmf_l.c
	lex -t bldmf_l.l >bldmf_l.c 	
	$(CC) $(CFLAGS) -c bldmf_l.c
	$(CC) $(CFLAGS) -o $@ bldmf.c bldmf_l.o $(LIBS) -ll

display: display.c $(LIBPATHS)
	$(CC) $(CFLAGS) -o $@ display.c $(LIBS)

makemsg: makemsg.c
	$(CC) $(CFLAGS) -o $@ makemsg.c

selectpg: selectpg.c
	$(CC) $(CFLAGS) -o $@ selectpg.c

wcopy: wcopy.c $(LIBPATHS)
	$(CC) $(CFLAGS) -o $@ wcopy.c $(LIBS)

wdelay: wdelay.c
	$(CC) $(CFLAGS) -o $@ wdelay.c

wdelwrk: wdelwrk.c $(LIBPATHS)
	$(CC) $(CFLAGS) -o $@ wdelwrk.c $(LIBS)

wdiag: 	wdiag.c
	$(CC) $(CFLAGS) -o $@ wdiag.c

wexists: wexists.c $(LIBPATHS)
	$(CC) $(CFLAGS) -o $@ wexists.c $(LIBS)

wfind: wfind.c $(LIBPATHS)
	$(CC) $(CFLAGS) -o $@ wfind.c $(LIBS)

wputparm: wputparm.c $(LIBPATHS) 
	$(CC) $(CFLAGS) -o $@ wputparm.c $(LIBS)

wrename: wrename.c $(LIBPATHS)
	$(CC) $(CFLAGS) -o $@ wrename.c $(LIBS)

wretcode: wretcode.c $(LIBPATHS)
	$(CC) $(CFLAGS) -o $@ wretcode.c $(LIBS)

wrun: wrun.c $(LIBPATHS)
	$(CC) $(CFLAGS) -o $@ wrun.c $(LIBS)

wscratch: wscratch.c $(LIBPATHS)
	$(CC) $(CFLAGS) -o $@ wscratch.c $(LIBS)

wsubmit: wsubmit.c $(LIBPATHS)
	$(CC) $(CFLAGS) -o $@ wsubmit.c $(LIBS)

wsysconf: wsysconf.c $(LIBPATHS)
	$(CC) $(CFLAGS) -o $@ wsysconf.c $(LIBS)

wsysinit: wsysinit.c $(LIBPATHS)
	$(CC) $(CFLAGS) -o $@ wsysinit.c $(LIBS)

wusage: wusage.c $(LIBPATHS)
	$(CC) $(CFLAGS) -o $@ wusage.c $(LIBS)

viewkey: viewkey.c 
	$(CC) $(CFLAGS) -o $@ viewkey.c

vsx: vsx.c vsx.h
	$(CC) $(CFLAGS) -o $@ vsx.c

wsort: wsort.c $(LIBPATHS)
	$(CC) $(CFLAGS) -o $@ wsort.c $(LIBS)

wpfkey: wpfkey.c $(LIBPATHS)
	$(CC) $(CFLAGS) -o $@ wpfkey.c $(LIBS)

vcapkeys: vcapkeys.c 
	$(CC) $(CFLAGS) -o $@ vcapkeys.c

wlicense: wlicense.c prompt.o
	$(CC) $(CFLAGS) -o $@ wlicense.c prompt.o $(LIBS)


