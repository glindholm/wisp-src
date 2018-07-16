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
WISPDIR=C:\WISP
WISPTRAN=$(WISPDIR)\bin\wisp
WISPFLAGS= -I..\wisputils
COBOL=ccbl32
COBFLAGS=-Zd -Da4
LANG=-VACU

OS=win32

.wcb.cob:
	$(WISPTRAN)  $(LANG) $(WISPFLAGS) $*

.wcb.cbx:
	$(WISPTRAN)  $(LANG) $(WISPFLAGS) $*
	$(COBOL) $(COBFLAGS) -o $* $*.cob

.cob.cbx:
	$(COBOL) $(COBFLAGS) -o $* $*.cob

.SUFFIXES: .cob .wcb

ALL= 	SAMPLE QAFILEIO QAFILE2 QANETCAP QAPRINT QASCREEN QASCRN2 QASUBS \
	QASY000M QASYS99 QAWSFNM QAWSFNS QAWSUBS QABCKGRD SUB1 SUB3 \
	TRIGGER XLINK XLINK1 QADPCOMA QAWSXIO ACULINK ACUUSING $(OS)_stuff

AUTOQA=	WL0000.cbx WL0010.cbx WL0011.cbx WL0012.cbx WL0013.cbx \
	WL0014.cbx WL0015.cbx WL0016.cbx WL0017.cbx \
	WL0018.cbx WL0018A.cbx WL0018B.cbx WL0019.cbx WL0020.cbx WL0021.cbx \
	WL0022.cbx WL0023.cbx WL0024.cbx WL0025.cbx WL0026.cbx

TEST_DIRS= volrun volin

all:	$(ALL) $(TEST_DIRS) $(AUTOQA)
	@echo
	@echo SAMPLE is up-to-date
	@echo

clean:
	rm -f $(ALL)
	rm -rf $(TEST_DIRS)
	rm -f *.cob

SAMPLE: sample.cob
	$(COBOL) $(COBFLAGS) -o $@ sample.cob

QAFILEIO: qafileio.cob
	$(COBOL) $(COBFLAGS) -o $@ qafileio.cob

QAFILE2: qafile2.cob
	$(COBOL) $(COBFLAGS) -o $@ qafile2.cob

QANETCAP: qanetcap.cob
	$(COBOL) $(COBFLAGS) -o $@ qanetcap.cob

QAPRINT: qaprint.cob
	$(COBOL) $(COBFLAGS) -o $@ qaprint.cob

QASCREEN: qascreen.cob
	$(COBOL) $(COBFLAGS) -o $@ qascreen.cob

QASCRN2: qascrn2.cob
	$(COBOL) $(COBFLAGS) -o $@ qascrn2.cob

QASUBS: qasubs.cob
	$(COBOL) $(COBFLAGS) -o $@ qasubs.cob

QASY000M: qasy000m.cob
	$(COBOL) $(COBFLAGS) -o $@ qasy000m.cob

QASYS99: qasys99.cob
	$(COBOL) $(COBFLAGS) -o $@ qasys99.cob

QAWSFNM: qawsfnm.cob
	$(COBOL) $(COBFLAGS) -o $@ qawsfnm.cob

QAWSFNS: qawsfns.cob
	$(COBOL) $(COBFLAGS) -o $@ qawsfns.cob

QAWSUBS: qawsubs.cob
	$(COBOL) $(COBFLAGS) -o $@ qawsubs.cob

QABCKGRD: qabckgrd.cob
	$(COBOL) $(COBFLAGS) -o $@ qabckgrd.cob

TRIGGER: trigger.cob
	$(COBOL) $(COBFLAGS) -o $@ trigger.cob

SUB1: sub1.cob
	$(COBOL) $(COBFLAGS) -o $@ sub1.cob

SUB3: sub3.cob
	$(COBOL) $(COBFLAGS) -o $@ sub3.cob

XLINK: xlink.cob
	$(COBOL) $(COBFLAGS) -o $@ xlink.cob

XLINK1: xlink1.cob
	$(COBOL) $(COBFLAGS) -o $@ xlink1.cob

QADPCOMA: qadpcoma.cob
	$(COBOL) $(COBFLAGS) -o $@ qadpcoma.cob

QAWSXIO: qawsxio.cob
	$(COBOL) $(COBFLAGS) -o $@ qawsxio.cob

ACULINK: aculink.cob
	$(COBOL) $(COBFLAGS) -o $@ aculink.cob

ACUUSING: acuusing.cob
	$(COBOL) $(COBFLAGS) -o $@ acuusing.cob
#
#==============================================================
#

sample.cob: sample.wcb
	$(WISPTRAN)  $(LANG) $(WISPFLAGS) -O samopt.opt sample


acuusing.cob: ..\acu\acuusing.cob
	cp "..\acu\acuusing.cob" .


volrun:
	mkdir volrun
	mkdir volrun\librun
	mkdir volrun\libexe
volin:
	mkdir volin
	mkdir volin\libin

unix_stuff:	prtargs

win32_stuff:	prtargs.exe

#
#	History:
#	$Log: sampleacu.mak,v $
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

