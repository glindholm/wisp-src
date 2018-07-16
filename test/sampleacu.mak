#
#	Copyright (c) 1988-2003 NeoMedia Migrations, All rights reserved.
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


WISPDIR   = ..\kit\wisp
WISPTRAN  = $(WISPDIR)\bin\wisp.exe
WISPLANG  = ACU
WISPFLAGS = -V $(WISPLANG) -I..\wisputils -M -u FORCEGENWISPCPY

COBOL    = c:\acucorp\acucbl600\acugt\bin\ccbl32.exe
COBFLAGS = -Da4 -Gd -Za -Te 800

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
	ACUUSING.acu \
	DISPRINT.acu

SAMPLE_COBOL_WCBS= \
	DISPFILE.wcb \
	LESWAP.wcb \
	QABCKGRD.wcb \
	QAFILEIO.wcb \
	QAFILE2.acu \
	QAMESSAG.wcb \
	QANETCAP.wcb \
	QAPRINT.wcb \
	QASCREEN.wcb \
	QASCRN2.wcb \
	QASUBS.wcb \
	QASY000M.wcb \
	QASYS99.wcb \
	QAWSFNM.wcb \
	QAWSFNS.wcb \
	QAWSUBS.wcb \
	QADPCOMA.wcb \
	QAWSXIO.wcb \
	SAMPLE.wcb \
	SUB1.wcb \
	SUB3.wcb \
	TRIGGER.wcb \
	WC0000.wcb \
	WC0001.wcb \
	WC0002.wcb \
	WC0002A.wcb \
	WC0002B.wcb \
	WC0002E.wcb \
	WC0003.wcb \
	WC0003B.wcb \
	WC0004.wcb \
	WL0000.wcb \
	WL0010.wcb \
	WL0011.wcb \
	WL0012.wcb \
	WL0013.wcb \
	WL0014.wcb \
	WL0015.wcb \
	WL0016.wcb \
	WL0017.wcb \
	WL0018.wcb \
	WL0018A.wcb \
	WL0018B.wcb \
	WL0019.wcb \
	WL0020.wcb \
	WL0021.wcb \
	WL0022.wcb \
	WL0023.wcb \
	WL0024.wcb \
	WL0025.wcb \
	WL0026.wcb \
	WL0027.wcb \
	WL0028.wcb \
	WL0029.wcb \
	WL0030.wcb \
	WL0031.wcb \
	WL0032.wcb \
	WL0033.wcb \
	WL0034.wcb \
	WL0034C.wcb \
	WL0034D.wcb \
	WL0035.wcb \
	WT0000.wcb \
	WT0010.wcb \
	WT0011.wcb \
	WT0012.wcb \
	XLINK.wcb \
	XLINK1.wcb

SAMPLE_COBOL_OBJS= $(SAMPLE_COBOL_WCBS:.wcb=.acu)

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
	-del /Q /F $(ALL_COBOL_OBJS)
	-del /Q /F $(TEST_DIRS)
	-del /Q /F DISPRINT.wcb
	-del /Q /F *.cob *.acu *.cpy *.obj

# Recreate the .cob files if $(WISPTRAN) is newer
$(ALL_COBOL_COBS): $(WISPTRAN)

#
#==============================================================
#

SAMPLE.cob: SAMPLE.wcb
	$(WISPTRAN)  $(LANG) $(WISPFLAGS) -O samopt.opt SAMPLE

ACULINK.acu: $(WISPDIR)\acu\ACULINK.acu
	$(COPY) "$(WISPDIR)\acu\ACULINK.acu" .

ACUUSING.acu: $(WISPDIR)\acu\ACUUSING.acu
	$(COPY) "$(WISPDIR)\acu\ACUUSING.acu" .


DISPRINT.acu: DISPRINT.wcb

DISPRINT.wcb: $(WISPDIR)\etc\DISPRINT.wcb
	$(COPY) $(WISPDIR)\etc\$@ $@ 


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
#	Revision 1.38  2003/07/07 20:38:09  gsl
#	WL0035 READVTOC tests
#	
#	Revision 1.37  2003/07/07 19:39:54  gsl
#	WC0004 ##TEMP file tests
#	
#	Revision 1.36  2003/07/07 17:58:27  gsl
#	WL0034 LINK tests
#	
#	Revision 1.35  2003/07/07 14:29:44  gsl
#	WC0003 ACCEPT VERB tests
#	
#	Revision 1.34  2003/07/03 18:53:55  gsl
#	RETURN-CODE tests
#	
#	Revision 1.33  2003/07/02 21:18:30  gsl
#	Add WL0033 FIND auto tests
#	
#	Revision 1.32  2003/07/01 19:28:52  gsl
#	QA
#	
#	Revision 1.31  2003/06/30 20:23:39  gsl
#	fix disprint
#	
#	Revision 1.30  2003/06/26 16:18:35  gsl
#	version numbers
#	
#	Revision 1.29  2003/03/07 20:05:08  gsl
#	Add #FORCEGENWISPCPY option
#	
#	Revision 1.28  2003/02/28 18:19:00  gsl
#	update
#	
#	Revision 1.27  2003/02/27 19:06:55  gsl
#	Move the QA MESSAGE logic out of QASUBS into new file QAMESSAG
#	
#	Revision 1.26  2003/01/30 19:43:45  gsl
#	Add WL0032 to autotest RENAME
#	
#	Revision 1.25  2003/01/20 20:14:18  gsl
#	Add WL0031 auto tests for SETFILE
#	
#	Revision 1.24  2003/01/20 17:02:28  gsl
#	Add WL0030 auto test for SUBMIT
#	
#	Revision 1.23  2003/01/17 15:31:40  gsl
#	Added WL0029 SCRATCH autotest
#	
#	Revision 1.22  2003/01/16 17:25:39  gsl
#	Add FILECOPY tests
#	
#	Revision 1.21  2002/12/11 14:08:43  gsl
#	Removed wispmsg.dat/txt and makemsg
#	
#	Revision 1.20  2002/11/26 21:52:02  gsl
#	Add WC0001.wcb to test Horz Repeating fields
#	
#	Revision 1.19  2002/11/22 18:56:21  gsl
#	Fix and standardize the Acucoobl COBFLAGS
#	
#	Revision 1.18  2002/10/11 18:44:11  gsl
#	Change DEL to -DEL
#	Remove clean from default target
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

