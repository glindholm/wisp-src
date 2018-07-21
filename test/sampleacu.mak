#
#	Copyright (c) Shell Stream Software LLC, All Rights Reserved.
#
#
#
#	File:		sampleacu.mak
#
#	Project:	WISP/SAMPLE
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

#ACUDIR=C:\data\Acucorp\ACUCBL722\ACUGT
#ACUDIR=C:\data\Acucorp\ACUCBL810\ACUGT
#ACUDIR=C:\data\Acucorp\ACUCBL900\ACUGT
#ACUDIR=C:\data\Acucorp\ACUCBL910\ACUGT

COBOL=$(ACUDIR)\bin\ccbl32.exe
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
	WC0005.wcb \
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
	@echo "ACUDIR    =" $(ACUDIR)
	@echo "WISPDIR   =" $(WISPDIR)
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

ACULINK.acu: $(WISPDIR)\acu\$(@F)
	$(COPY) $** $@

ACUUSING.acu: $(WISPDIR)\acu\$(@F)
	$(COPY) $** $@


DISPRINT.acu: DISPRINT.wcb

DISPRINT.wcb: $(WISPDIR)\etc\$(@F)
	$(COPY) $** $@


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

$(WC)\ACUCONFIG:		.\config_nt\$(@F)
	$(COPY) $** $@

$(WC)\CHARMAP:			.\config_nt\$(@F)
	$(COPY) $** $@

$(WC)\FORMS:			.\config_nt\$(@F)
	$(COPY) $** $@

$(WC)\LGMAP:			.\config_nt\$(@F)
	$(COPY) $** $@

$(WC)\LPMAP:			.\config_nt\$(@F)
	$(COPY) $** $@

$(WC)\OPTIONS:			.\config_nt\$(@F)
	$(COPY) $** $@

$(WC)\PRMAP:			.\config_nt\$(@F)
	$(COPY) $** $@

$(WC)\W4WMAP:			.\config_nt\$(@F)
	$(COPY) $** $@

$(WC)\wproc.msg:		.\config_nt\$(@F)
	$(COPY) $** $@

$(WC)\wrun.cfg:			.\config_nt\$(@F)
	$(COPY) $** $@

$(WC)\wsysconf.cfg:		.\config_nt\$(@F)
	$(COPY) $** $@

$(VIDEOCAPFILES):		.\config_nt\videocap\$(@F)
	$(COPY) $** $@
