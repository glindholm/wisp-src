#
#	Copyright (c) 1988-1996 DevTech Migrations, All rights reserved.
#	$Id:$
#
#
#	File:		makewisp.umf
#
#	Function:	Master makefile for building all of WISP on a unix platform.
#
#	Description:	This routine will start all the make commands used to 
#			do the complete WISP port.
#
#				$ make -f makewisp.umf
#



include $(WISP)/src/port/make.include

all:		wisp ede kcsi amu
		@echo ALL is up-to-date.

#
#	Products
#

wisp:		video wisplib wisptran wisputils proctran mfsubs vsedit ivslib wproc rts
		@echo WISP is up-to-date.

ede:		edelib good
		@echo EDE is up-to-date.

kcsi:		kcsi_acu kcsi_mf
		@echo KCSI is up-to-date.

amu:		amu_all
		@echo AMU is up-to-date.

#
#	Components
#

video:		videolib videotest
		@echo VIDEO is up-to-date.

wproc:		wisplib videolib
		(cd $(WISP)/src/wproc; 		$(MAKE) -f wproc.umf kit)

kcsi_acu:	crid_acu create_acu
		@echo KCSI for ACUCOBOL is up-to-date.

kcsi_mf:	crid_mf create_mf
		@echo KCSI for Micro Focus is up-to-date.

amu_all:
		(cd $(WISP)/src/amu;		$(MAKE) -f amuunix.umf "WISPTRAN=../wisptran/wisp" both)

crid_mf:	wisptran wisplib videolib
		(cd $(WISP)/src/kcsi/crid;	$(MAKE) -f cridmake.umf WISPDIR=../.. crid_mf)

crid_acu:	wisptran wisplib videolib
		(cd $(WISP)/src/kcsi/crid;	$(MAKE) -f cridmake.umf WISPDIR=../.. crid_acu)

create_mf:	wisplib videolib
		(cd $(WISP)/src/kcsi/create;	$(MAKE) -f createunix.umf WISPDIR=../.. mfx_all)

create_acu:	wisplib videolib
		(cd $(WISP)/src/kcsi/create;	$(MAKE) -f createunix.umf WISPDIR=../.. acu_all)

edelib:	
		(cd $(WISP)/src/ede; 		$(MAKE) -f libede.umf install)

good:		videolib edelib
		(cd $(WISP)/src/ede; 		$(MAKE) -f good.umf)

proctran:
		(cd $(WISP)/src/proctran;	$(MAKE) -f proctran.umf)
		@echo PROCTRAN is up-to-date.

ivslib:
		(cd $(WISP)/src/ivslib;		$(MAKE) -f libivs.umf)
		@echo IVSLIB is up-to-date.

vsedit:		wisplib videolib
		(cd $(WISP)/src/vsedit;		$(MAKE) -f vsedit.umf)
		@echo VSEDIT is up-to-date.

videolib:	
		(cd $(WISP)/src/videolib;	$(MAKE) -f libvideo.umf install)
		@echo VIDEOLIB is up-to-date.

videotest:	videolib
		(cd $(WISP)/src/videotest;	$(MAKE) -f vtest.umf)
		@echo VIDEOTEST is up-to-date.

wisplib:
		(cd $(WISP)/src/wisplib;	$(MAKE) -f libwisp.umf install)
		@echo WISPLIB is up-to-date.

wisptran:
		(cd $(WISP)/src/wisptran;	$(MAKE) -f wisp.umf)
		@echo WISPTRAN is up-to-date.

wisputils:	wisplib videolib
		(cd $(WISP)/src/wisputils;	$(MAKE) -f utils.umf)
		@echo WISPUTILS is up-to-date.

mfsubs:		
		(cd $(WISP)/src/mf;		$(MAKE) -f wrunmf.umf WISPDIR=.. subsonly)
		@echo MFSUBS is up-to-date.

rts:		acu mf

acu:		wisplib videolib edelib
		(cd $(WISP)/src/acu;		$(MAKE) -f wruncbl.umf WISPDIR=.. ACUDIR=$(ACUDIR) both)
		@echo ACUCOBOL RTS is up-to-date.

mf:		wisplib videolib edelib
		(cd $(WISP)/src/mf;		$(MAKE) -f wrunmf.umf WISPDIR=.. COBDIR=$(COBDIR) both)
		@echo MICRO FOCUS RTS is up-to-date.

clean:
		(cd $(WISP)/src/acu; 		$(MAKE) -f wruncbl.umf  clean)
		(cd $(WISP)/src/amu;		$(MAKE) -f amuunix.umf  clean)
		(cd $(WISP)/src/ede; 		$(MAKE) -f libede.umf   clean)
		(cd $(WISP)/src/ede; 		$(MAKE) -f good.umf     clean)
		(cd $(WISP)/src/ivslib;		$(MAKE) -f libivs.umf	clean)
		(cd $(WISP)/src/kcsi/create;	$(MAKE) -f createunix.umf clean)
		(cd $(WISP)/src/kcsi/crid;	$(MAKE) -f cridmake.umf clean)
		rm -f $(WISP)/src/lib/*.a 
		(cd $(WISP)/src/mf;		$(MAKE) -f wrunmf.umf 	clean)
		(cd $(WISP)/src/proctran;	$(MAKE) -f proctran.umf clean)
		(cd $(WISP)/src/testacu;	$(MAKE) -f sampleacu.umf clean)
		(cd $(WISP)/src/testmf;		$(MAKE) -f samplemf.umf  clean)
		(cd $(WISP)/src/vsedit;		$(MAKE) -f vsedit.umf   clean)
		(cd $(WISP)/src/videolib;	$(MAKE) -f libvideo.umf clean)
		(cd $(WISP)/src/videotest;	$(MAKE) -f vtest.umf    clean)
		(cd $(WISP)/src/wisplib;	$(MAKE) -f libwisp.umf  clean)
		(cd $(WISP)/src/wisptran;	$(MAKE) -f wisp.umf     clean)
		(cd $(WISP)/src/wisputils;	$(MAKE) -f utils.umf    clean)
		(cd $(WISP)/src/wproc;		$(MAKE) -f wproc.umf     clean)
		@echo All is CLEAN.
#
# 	History:
#	$Log: makewisp.umf,v $
#	Revision 1.20  1998-07-31 10:01:26-04  gsl
#	Add dependencies for CRID and CREATE
#
#	Revision 1.19  1998-07-30 10:47:04-04  gsl
#	Changed to use new cridmake.umf makefile
#
#	Revision 1.18  1997-10-02 11:31:44-04  gsl
#	update the clean target
#
#	Revision 1.17  1997-08-13 10:59:05-04  scass
#	Changed to build CRID and CREATE with the ExtFH as the
#	default targets.
#
#	Revision 1.16  1997-05-20 16:34:56-04  scass
#	Added definition for WISPTRAN to make of AMU
#
#	Revision 1.15  1997-05-13 15:23:29-04  scass
#	Fixed amu_all.  Was on wrong line.
#
#	Revision 1.14  1997-05-06 10:27:34-04  scass
#	Changed AMU to build everything.  AcuCOBOL, Micro Focus COBOL
#	and a source kit.  This is done in amuunix.umf so don't need
#	to seperate here.
#
#	Revision 1.13  1997-05-05 16:22:34-04  scass
#	Added make rules for AMU with ACUCOBOL and Micro Focus COBOL
#
#	Revision 1.12  1997-03-26 11:26:52-05  gsl
#	Add auto build of the RTS
#	Reorg into products
#
#	Revision 1.11  1997-03-25 18:05:59-05  gsl
#	Add logic to build KCSI kits automatically
#
#	Revision 1.10  1996-07-26 17:44:00-04  gsl
#	Changed to specify each makefile name with  the -f option
#
#	Revision 1.9  1996-04-10 05:38:22-07  gsl
#	Fix the targets to install the libs into the ../lib dir
#
# Revision 1.8  1996/04/10  12:05:26  gsl
# Fix WPROC build stuff
#
# Revision 1.7  1996/01/08  10:07:58  gsl
# Removed UNIQUE and added WPROC and the start of KCSI
#
#			06/08/92	Written by GSL
#			06/09/92	Change to use Makefile instead of .umf. GSL
#			06/12/92	Added mfsubs. GSL
#			07/07/92	Added wisplib as a dependency of printq. GSL
#
