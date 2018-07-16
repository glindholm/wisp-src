#
#	Copyright (c) 1988-1996 NeoMedia Migrations, All rights reserved.
#	$Id:$
#
#
#
#	File:		sampleacu.umf
#
#	Project:	WISP/SAMPLE
#
#	RCS:		$Source:$
#
#	Function:	The UNIX makefile for SAMPLE/ACUCOBOL
#
#	Desciption:	This makefile will build SAMPLE for ACUCOBOL.
#
#			make -f sampleacu.umf
#			
#

WISPTRAN=wisp
WISPFLAGS= -I../wisputils
COBOL=ccbl
COBFLAGS=-Zd -Da4
LANG=-VACU

OS=unix

.SUFFIXES: .cob .wcb

.wcb.cob:
	$(WISPTRAN)  $(LANG) $(WISPFLAGS) $*

.wcb:
	$(WISPTRAN)  $(LANG) $(WISPFLAGS) $*
	$(COBOL) $(COBFLAGS) -o `echo $*|tr '[a-z]' '[A-Z]'` $*.cob

.cob:
	$(COBOL) $(COBFLAGS) -o `echo $*|tr '[a-z]' '[A-Z]'` $*.cob

ALL= 	SAMPLE QAFILEIO QAFILE2 QANETCAP QAPRINT QASCREEN QASCRN2 QASUBS \
	QASY000M QASYS99 QAWSFNM QAWSFNS QAWSUBS QABCKGRD SUB1 SUB3 \
	TRIGGER XLINK XLINK1 QADPCOMA QAWSXIO ACULINK ACUUSING $(OS)_stuff

AUTOQA=	WL0000 WL0010 WL0011 WL0012 WL0013 WL0014 WL0015 WL0016 WL0017 \
	WL0018 WL0019 WL0020 WL0021 WL0022 WL0023 WL0024 WL0025 WL0026 \
	WL0018A WL0018B

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
	$(WISPTRAN) $(WISPFLAGS) -O samopt.opt sample

acuusing.cob: ../acu/acuusing.cob
	cp "../acu/acuusing.cob" .


WL0000: wl0000
WL0010: wl0010
WL0011: wl0011
WL0012: wl0012
WL0013: wl0013
WL0014: wl0014
WL0015: wl0015
WL0016: wl0016
WL0017: wl0017
WL0018: wl0018
WL0018A: wl0018a
WL0018B: wl0018b
WL0019: wl0019
WL0020: wl0020
WL0021: wl0021
WL0022: wl0022
WL0023: wl0023
WL0024: wl0024
WL0025: wl0025
WL0026: wl0026
WL0027: wl0027

volrun:
	mkdir volrun
	mkdir volrun/librun
	mkdir volrun/libexe
volin:
	mkdir volin
	mkdir volin/libin

unix_stuff:	prtargs

win32_stuff:	prtargs.exe

#
#	History:
#	$Log: sampleacu.umf,v $
#	Revision 1.12  1997-08-28 14:37:49-04  gsl
#	Fix and simplify so qabckgrd builds properly
#
#	Revision 1.11  1997-07-23 10:14:56-04  scass
#	Moved .SUFFIXES to be above rules so make
#	works properly on DEC Alpha
#
#	Revision 1.10  1997-05-20 16:23:15-04  gsl
#	Add WL0018A and WL0018B for extra MESSAGE tests
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

