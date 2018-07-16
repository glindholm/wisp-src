#
#	Copyright (c) 1988-1996 NeoMedia Migrations, All rights reserved.
#	$Id:$
#
#
#
#	File:		sampleacu.mak
#
#	Project:	WISP/SAMPLE
#
#	RCS:		$Source:$
#
#	Function:	The makefile for SAMPLE/ACUCOBOL
#
#	Desciption:	This makefile will build SAMPLE for ACUCOBOL.
#
#			nmake -f sampleacu.mak
#			
#

default: all


WISPDIR   = ..
WISPTRAN  = $(WISPDIR)\bin\wisp.exe
WISPLANG  = ACU
WISPFLAGS = -V $(WISPLANG) -I..\wisputils -M

COBOL    = c:\acucorp\acucbl520\acugt\bin\ccbl32.exe
COBFLAGS = -Gd -Zi -Za -Da4 -Te 800

CFLAGS = -DWIN32 -DMSFS

COPY=copy

.wcb.cob:
	$(WISPTRAN) $(WISPFLAGS) $*.wcb

.wcb.acu:
	$(WISPTRAN) $(WISPFLAGS) $*.wcb
	$(COBOL) $(COBFLAGS) -o $*.acu $*.cob

.cob.acu:
	$(COBOL) $(COBFLAGS) -o $*.acu $*.cob

.wcb:
	$(WISPTRAN) $(WISPFLAGS) $*.wcb
	$(COBOL) $(COBFLAGS) -o $* $*.cob

.cob:
	$(COBOL) $(COBFLAGS) -o $* $*.cob

.SUFFIXES: .acu .cob .wcb

WISP_COBOL_OBJS= \
	ACULINK.acu \
	ACUUSING.acu 

SAMPLE_COBOL_OBJS= \
	DISPFILE.acu \
	LESWAP.acu \
	QABCKGRD.acu \
	QAFILEIO.acu \
	QAFILE2.acu \
	QANETCAP.acu \
	QAPRINT.acu \
	QASCREEN.acu \
	QASCRN2.acu \
	QASUBS.acu \
	QASY000M.acu \
	QASYS99.acu \
	QAWSFNM.acu \
	QAWSFNS.acu \
	QAWSUBS.acu \
	QADPCOMA.acu \
	QAWSXIO.acu \
	SAMPLE.acu \
	SUB1.acu \
	SUB3.acu \
	TRIGGER.acu \
	WL0000.acu \
	WL0010.acu \
	WL0011.acu \
	WL0012.acu \
	WL0013.acu \
	WL0014.acu \
	WL0015.acu \
	WL0016.acu \
	WL0017.acu \
	WL0018.acu \
	WL0018A.acu \
	WL0018B.acu \
	WL0019.acu \
	WL0020.acu \
	WL0021.acu \
	WL0022.acu \
	WL0023.acu \
	WL0024.acu \
	WL0025.acu \
	WL0026.acu \
	WL0027.acu \
	WL0028.acu \
	WL0029.acu \
	WL0030.acu \
	WL0031.acu \
	WL0032.acu \
	XLINK.acu \
	XLINK1.acu

ALL_COBOL_OBJS= $(WISP_COBOL_OBJS) $(SAMPLE_COBOL_OBJS)
ALL_COBOL_COBS= $(ALL_COBOL_OBJS:.acu=.cob)

TEST_DIRS= volrun volin volout volspl volwrk

all:	header $(ALL_COBOL_OBJS) testdirs prtargs.exe
	@echo " "
	@echo "SAMPLE is up-to-date"
	@echo " "

header:
	@echo " "
	@echo "Building SAMPLE programs"
	@echo " "
	@echo "WISPTRAN  =" $(WISPTRAN)
	@echo "WISPFLAGS =" $(WISPFLAGS)
	@echo "COBOL     =" $(COBOL)
	@echo "COBFLAGS  =" $(COBFLAGS)
	@echo "PWD       ="
	@cd


clean:
	-del /Q $(ALL_COBOL_OBJS)
	-del /Q $(TEST_DIRS)
	-del /Q *.cob *.acu *.cpy *.obj

# Recreate the .cob files if $(WISPTRAN) is newer
$(ALL_COBOL_COBS): $(WISPTRAN)

#
#==============================================================
#

SAMPLE.cob: SAMPLE.wcb
	$(WISPTRAN)  $(LANG) $(WISPFLAGS) -O samopt.opt SAMPLE

ACULINK.wcb: ..\acu\ACULINK.wcb
	$(COPY) "..\acu\ACULINK.wcb" .

ACUUSING.cob: ..\acu\ACUUSING.cob
	$(COPY) "..\acu\ACUUSING.cob" .

#
#==============================================================
#

testdirs: $(TEST_DIRS)

volrun:
	mkdir volrun
	mkdir volrun\librun
	mkdir volrun\libexe
	mkdir volrun\onpath

volin:
	mkdir volin
	mkdir volin\libin

volout:
	mkdir volout

volspl:
	mkdir volspl

volwrk:
	mkdir volwrk

#
#==============================================================
#

WC=config
VC=$(WC)\videocap

WISPCONFIGFILES= $(WC)\ACUCONFIG \
	$(WC)\CHARMAP \
	$(WC)\FORMS \
	$(WC)\LGMAP \
	$(WC)\LPMAP \
	$(WC)\OPTIONS \
	$(WC)\PRMAP \
	$(WC)\W4WMAP \
	$(WC)\wispmsg.dat \
	$(WC)\wproc.msg \
	$(WC)\wrun.cfg \
	$(WC)\wsysconf.cfg

VIDEOCAPFILES= $(VC)\wincon.vcap $(VC)\ansi.vcap $(VC)\xterm.vcap

wispconfigsetup: $(WC) $(WISPCONFIGFILES) $(VC) $(VIDEOCAPFILES)

$(WC) $(VC):
	mkdir $@

$(WC)\ACUCONFIG:		.\ACUCONFIG.NT
	$(COPY) $** $@

$(WC)\CHARMAP:			$(WISPDIR)\config\$(@F)
	$(COPY) $** $@

$(WC)\FORMS:			$(WISPDIR)\config\$(@F)
	$(COPY) $** $@

$(WC)\LGMAP:			.\LGMAP.NT
	$(COPY) $** $@

$(WC)\LPMAP:			.\LPMAP.NT
	$(COPY) $** $@

$(WC)\OPTIONS:			$(WISPDIR)\config\$(@F)
	$(COPY) $** $@

$(WC)\PRMAP:			$(WISPDIR)\config\$(@F)
	$(COPY) $** $@

$(WC)\W4WMAP:			$(WISPDIR)\config\$(@F)
	$(COPY) $** $@

$(WC)\wispmsg.dat:		$(WISPDIR)\config\$(@F)
	$(COPY) $** $@

$(WC)\wproc.msg:		$(WISPDIR)\config\$(@F)
	$(COPY) $** $@

$(WC)\wrun.cfg:			.\wrun_acu.cfg
	$(COPY) $** $@

$(WC)\wsysconf.cfg:		$(WISPDIR)\config\$(@F)
	$(COPY) $** $@

$(VIDEOCAPFILES):		$(WISPDIR)\config\videocap\$(@F)
	$(COPY) $** $@

#
#	History:
#	$Log: sampleacu.mak,v $
#	Revision 1.13.2.3  2003/02/10 14:59:02  gsl
#	Add autotests from $HEAD
#	
#	Revision 1.13.2.2  2002/11/12 20:21:35  gsl
#	Use -del
#	
#	Revision 1.13.2.1  2002/10/08 19:54:09  gsl
#	Updated from HEAD
#	
#	Revision 1.17  2002/07/31 01:31:29  gsl
#	update
#	
#	Revision 1.16  2002/07/24 00:29:58  gsl
#	update
#	
#	Revision 1.15  2002/07/23 20:50:16  gsl
#	building sample
#	
#	Revision 1.14  2002/07/10 01:36:56  gsl
#	use .acu
#	
#	Revision 1.13  2002/05/15 18:35:32  gsl
#	update
#	
#	Revision 1.12  2001-11-26 11:24:49-05  gsl
#	Add target for testdirs
#
#	Revision 1.11  2001-11-13 11:01:36-05  gsl
#	Use ACUCONFIG.NT
#
#	Revision 1.10  2001-11-12 18:07:44-05  gsl
#	update
#
#	Revision 1.9  2000-04-24 20:59:17-04  gsl
#	wisp 4.3.06
#
#	Revision 1.8  1999-09-27 09:58:04-04  gsl
#	Update the auto build of WISPCONFIG to use a custom wrun.cfg file
#
#	Revision 1.7  1999-09-13 15:53:08-04  gsl
#	Add wispconfigsetup to do the WIN32 setup of the config dir.
#
#	Revision 1.6  1998-06-26 11:22:00-04  gsl
#	Add WL0027 to build
#
#	Revision 1.5  1998-06-15 15:07:52-04  gsl
#	Add manual locking flag
#
#	Revision 1.4  1998-01-16 15:22:01-05  gsl
#	Add CFLAGS to prtargs gets built correctly
#
#	Revision 1.3  1998-01-13 09:23:25-05  gsl
#	Fix makefile
#
#	Revision 1.2  1997-05-20 16:21:33-04  gsl
#	Add WL0018A and WL0018B for extra MESSAGE testing
#
#	Revision 1.1  1997-05-20 15:54:21-04  gsl
#	Initial revision
#
#	Revision 1.9  1997-05-01 09:24:21-04  gsl
#	Add all the WL00xx automatic tests
#
#	Revision 1.8  1996-12-13 14:47:37-05  gsl
#	Update so will work on NT as well as unix
#
#
#
#

