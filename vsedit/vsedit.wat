#/************************************************************************/
#/*                                                                     */
#/*             WISP - Wang Interchange Source Pre-processor            */
#/*              Copyright (c) 1988, 1989, 1990, 1991, 1992, 1993       */
#/*      An unpublished work of International Digital Scientific Inc.   */
#/*                         All rights reserved.                        */
#/*                                                                     */
#/************************************************************************/
#
#
#       File:           vsedit.wat
#
#       Function:       Makefile for VSEDIT on MSDOS using Watcom
#
#       History:        
#                       10/02/92        written by JEC
#                       01/08/93        Modified for MSDOS
#                       12/03/93        Updated for WISP Release 3.3C
#                                       (Added vsebasic.obj)
#			04/14/94	converted for watcom, CBA
#			04/20/94	CBA, modified file so that either
#					wmake or nmake can use it
#

#CDEBUG = /od
CDEBUG = /d2

!include $(WISP)\src\port\dosmake.wat

VSEDIT = vsedit.exe
LIBWISP = $(STDLIB)\wisp.lib
LIBVIDEO = $(STDLIB)\video.lib

all:	$(VSEDIT)

clean:
	del $(VSEDIT)
	del *.obj

$(STDBIN)\$(VSEDIT):	$(VSEDIT)
	copy $(VSEDIT) $@

$(STDBIN):
	mkdir $(STDBIN)

OBJS = vsemain.obj  \
       vseglb.obj   \
       vsegp.obj    \
       vseinp.obj   \
       vseutl.obj   \
       vsedit.obj   \
       vsestmnu.obj \
       vsespmnu.obj \
       vsescr.obj   \
       vsenaf.obj   \
       vsetxt.obj   \
       vsedscr.obj  \
       vsedmod.obj  \
       vsedins.obj  \
       vsedel.obj   \
       vsedfnd.obj  \
       vsemov.obj   \
       vsegps.obj   \
       vsebasic.obj 

$(VSEDIT): $(OBJS) $(LIBWISP) $(LIBVIDEO)
	echo debug all	>make.rsp
	echo option CASEEXACT 	>>make.rsp
	echo file $(SUBOBJ)	>>make.rsp
	echo file $(ACUOBJ)	>>make.rsp
	echo library $(ACULIB)	>>make.rsp
	echo library $(LIBWISP)	>>make.rsp
	echo library $(LIBVIDEO)	>>make.rsp
	set wcl386=$(CFLAGS0)
	$(CC) *.objs /fe=$@ @make.rsp
	$(BIND4GW) $@

vsedel.obj: vsedel.c vseglb.h vsegp.h vsescr.h vsedscr.h
vsedel.obj: vseglb.h vsescr.h
vsedfnd.obj: vsedfnd.c vseglb.h vsegp.h vsescr.h
vsedfnd.obj: vsedscr.h vseglb.h vsescr.h
vsedins.obj: vsedins.c vseglb.h vsegp.h vsescr.h
vsedins.obj: vsedscr.h vseglb.h vsescr.h
vsedit.obj: vsedit.c vseglb.h vsegp.h
vsedmod.obj: vsedmod.c vseglb.h vsegp.h vsescr.h
vsedmod.obj: vsedscr.h vseglb.h vsescr.h
vsedscr.obj: vsedscr.c vseglb.h vsegp.h vsescr.h
vsedscr.obj: vsedscr.h vseglb.h vsescr.h
vseglb.obj: vseglb.c vseglb.h vsegp.h
vsegp.obj: vsegp.c vseglb.h vsegp.h
vseinp.obj: vseinp.c vseglb.h vsegp.h vsegp.h
vsemain.obj: vsemain.c vseglb.h vsegp.h
vsemov.obj: vsemov.c vseglb.h vsegp.h vsescr.h vsedscr.h
vsemov.obj: vseglb.h vsescr.h
vsenaf.obj: vsenaf.c vseglb.h vsegp.h vsescr.h
vsegps.obj: vsegps.c vseglb.h vsegp.h vsegp.h
vsescr.obj: vsescr.c vseglb.h vsegp.h vsescr.h
vsespmnu.obj: vsespmnu.c vseglb.h vsegp.h vsescr.h
vsestmnu.obj: vsestmnu.c vseglb.h vsegp.h vsescr.h
vsetxt.obj: vsetxt.c vseglb.h vsegp.h
vseutl.obj: vseutl.c
vsebasic.obj: vsebasic.c vseglb.h
