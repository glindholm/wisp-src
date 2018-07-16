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
#	06/04/92	Changed to use make.include GSL
#	06/09/92	Removed copy stmts. GSL
#       06/18/92        Added target for bldmf
#	09/13/93	Added ulicense and uauthorize, removed ilp stuff. GSL
#

#CDEBUG = -g
CDEBUG = -O

include $(WISP)/src/port/make.include

LIBS= $(LIBCFLAGS) -lwisp -lvideo -lm $(LIBCURSES)
LIBPATHS= $(STDLIB)/libwisp.a $(STDLIB)/libvideo.a

#VSNIDX=vsn_only.o

all: 	bldmf display makemsg selectpg wcopy wdelay wdelwrk wexists wfind \
	wputparm wrename wretcode wrun wscratch wsubmit wsysconf wsysinit wusage \
	viewkey vsx wsort vcapkeys wlicense wdiag

bldmf: bldmf.c bldmf.h bldmf_l.o $(LIBPATHS)
	$(CC) $(CFLAGS) -o $@ bldmf.c bldmf_l.o $(LIBS) -ll

bldmf_l.c: bldmf_l.l bldmf.h
	lex -t bldmf_l.l >bldmf_l.c 	

#bldmf: bldmf.c bldmf.h bldmf_l.l $(LIBPATHS)
#	rm -f bldmf_l.c
#	lex -t bldmf_l.l >bldmf_l.c 	
#	$(CC) $(CFLAGS) -c bldmf_l.c
#	$(CC) $(CFLAGS) -o $@ bldmf.c bldmf_l.o $(LIBS) -ll

display: display.c $(LIBPATHS) $(VSNIDX)
	$(CC) $(CFLAGS) -o $@ display.c $(LIBS) $(LIBACU) 

makemsg: makemsg.c
	$(CC) $(CFLAGS) -o $@ makemsg.c

selectpg: selectpg.c
	$(CC) $(CFLAGS) -o $@ selectpg.c

wcopy: wcopy.c $(LIBPATHS) $(VSNIDX)
	$(CC) $(CFLAGS) -o $@ wcopy.c $(LIBS) $(LIBACU) 

wdelay: wdelay.c
	$(CC) $(CFLAGS) -o $@ wdelay.c

wdelwrk: wdelwrk.c $(LIBPATHS)
	$(CC) $(CFLAGS) -o $@ wdelwrk.c $(LIBS)

wdiag: 	wdiag.c
	$(CC) $(CFLAGS) -o $@ wdiag.c $(LIBS)

wexists: wexists.c $(LIBPATHS)
	$(CC) $(CFLAGS) -o $@ wexists.c $(LIBS)

wfind: wfind.c $(LIBPATHS)
	$(CC) $(CFLAGS) -o $@ wfind.c $(LIBS) 

wputparm: wputparm.c $(LIBPATHS) 
	$(CC) $(CFLAGS) -o $@ wputparm.c $(LIBS)

# SEQUENT needs these special flags
#
#wputparm: wputparm.c $(LIBPATHS) 
#	$(CC) $(CFLAGS) -W0,-Nt2000 -o $@ wputparm.c $(LIBS)

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

wusage: wusage.c $(LIBPATHS) $(VSNIDX)
	$(CC) $(CFLAGS) -o $@ wusage.c $(LIBS) $(LIBACU) 

viewkey: viewkey.c 
	$(CC) $(CFLAGS) -o $@ viewkey.c

vsx: vsx.c vsx.h
	$(CC) $(CFLAGS) -o $@ vsx.c

wsort: wsort.c $(LIBPATHS) $(VSNIDX)
	$(CC) $(CFLAGS) -o $@ wsort.c $(LIBS) $(LIBACU) 

wpfkey: wpfkey.c $(LIBPATHS)
	$(CC) $(CFLAGS) -o $@ wpfkey.c $(LIBS)

vcapkeys: vcapkeys.c 
	$(CC) $(CFLAGS) -o $@ vcapkeys.c $(LIBSOC)

wlicense: wlicense.c prompt.o
	$(CC) $(CFLAGS) -o $@ wlicense.c prompt.o $(LIBS)

ulicense: wlicense.c prompt.o licunique.o
	$(CC) $(CFLAGS) -o $@ wlicense.c prompt.o licunique.o $(LIBS)

wauthorize: wauthorize.c prompt.o
	$(CC) $(CFLAGS) -o $@ wauthorize.c prompt.o $(LIBS)

uauthorize: wauthorize.c prompt.o licunique.o
	$(CC) $(CFLAGS) -o $@ wauthorize.c prompt.o licunique.o $(LIBS)

hexed: hexed.c $(LIBPATHS)
	$(CC) $(CFLAGS) -o $@ hexed.c $(LIBS)
