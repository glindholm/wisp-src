#-----------------------------------------------------------------------
#	Copyright (c) 1998-2003 NeoMedia Technologies, All rights reserved.
#	$Id:$
#
#	File:		kcsicob.mak
#
#	Project:	KCSI (CRID + CREATE)
#
#	RCS:		$Source:$
#
#	Purpose:	Build KCSI on WIN32
#
#-----------------------------------------------------------------------
#
#	Targets:	
#
#	all		All shipkits (default)
#	kcsi_acu	The Acucobol KCSI shipkit
#	clean		Remove all built pieces
#
#-----------------------------------------------------------------------
#
#	This makefile assumes that the environment is setup for porting
#	the WISP kit.
#
#	Usage:
#
#	$ nmake ff kcsicob.mak {target}
#
#-----------------------------------------------------------------------
kcsi_default: kcsi_acu

MAKEFILE=kcsicob.mak

#-----------------------------------------------------------------------
#
# Change copyright date in "cridvers.c"
#
# To change the version for KCSI, change KCSI_VERSION here. 
#	wisp/src/kcsi/cridvers.c
#	wisp/src/kcsi/version.c
#	wisp/src/kcsi/kcsilibs.umf
#	wisp/src/kcsi/kcsicob.mak
#	wisp/src/kcsi/kcsi_relnotes.txt
#	wisp/src/kcsi/kcsintsetup.txt
#	wisp/src/kcsi/kcsi_acu_install.txt
#	wisp/src/kcsi/kcsi_mf_install.txt
#	wisp/src/kcsi/kcsi_packlist.txt
#	wisp/src/doc/wisp_relnotes.txt
#	wisp/src/acu/wruncbl.umf
#	wisp/src/acu/wrun32wisp_kcsi_acu50.mak
#	wisp/src/acu/wrun32wisp_kcsi_acu51.mak
#	wisp/src/acu/wrun32wisp_kcsi_acu52.mak
#
KCSI_VERSION=4200

#-----------------------------------------------------------------------
#
#WISPDIR=C:\WISP5001
WISPTRAN=$(WISPDIR)\bin\wisp.exe
WISPFLAGS= -VACU -u ACU50 
#-----------------------------------------------------------------------
#
# 	ACUCOBOL
#
#ACUDIR=C:\Acucorp\ACUCBL600\ACUGT
#ACUDIR=C:\Acucorp\ACUCBL610\ACUGT
#ACUDIR=C:\Acucorp\ACUCBL722\ACUGT
#ACUDIR=C:\Acucorp\ACUCBL810\ACUGT

ACU_COBOL = $(ACUDIR)\bin\ccbl32.exe
ACU_COBFLAGS = -Da4 -Za -C50 -Z50
CBLUTIL= $(ACUDIR)\bin\cblutl32.exe


#-----------------------------------------------------------------------
#
#	Component lists
#
CONTROL_WCB= ctrlmain.wcb ctrlary.wcb   ctrlfld.wcb   ctrlio.wcb \
  ctrlgrp.wcb   ctrllst.wcb   ctrlrng.wcb   ctrloptl.wcb  listmopt.wcb \
  ctrlerr.wcb   ctrlhdi.wcb   ctrlmap.wcb   ctrlseq.wcb   tableio.wcb \
  ctrlext.wcb   ctrlhdl.wcb   ctrload.wcb   ctrltbl.wcb   ctrlprt.wcb \
  ctrlfil.wcb   ctrlhdr.wcb   ctrlopt.wcb   ctrlval.wcb   ctrldif.wcb \
  ctrldepp.wcb	kcsextfh.wcb

REPORT_WCB = rptmain.wcb  \
 rptctlh.wcb  rptedt.wcb   rptmor.wcb   rptseq.wcb   \
 rptctll.wcb  rptedth.wcb  rptnew.wcb   rptsfl.wcb   rptxcl.wcb \
 rptaka.wcb   rptctlm.wcb  rptfil.wcb   rptnfl.wcb   rptsiz.wcb   rptxdt.wcb \
 rptckfl.wcb  rptctlp.wcb  rptlim.wcb   rptopt.wcb   rptsor.wcb   rptxfl.wcb \
 rptcln.wcb   rptdef.wcb   rptlod.wcb   rptpdt.wcb   rptspc.wcb   rptxln.wcb \
 rptcol.wcb   rptdnw.wcb   rptmdt.wcb   rptpfl.wcb   rptsum.wcb \
 rptcon.wcb   rptdup.wcb   rptmnu.wcb   rpttit.wcb \
 rptctl.wcb   rptedo.wcb   rptmod.wcb   rptsdt.wcb   rptwmn.wcb \
 blnksc.wcb   kcsextfh.wcb plswait.wcb  ctrldif.wcb  \
 rptcld.wcb   rptio.wcb    rptmak.wcb \
 ctrlary.wcb  ctrldio.wcb  ctrlhdi.wcb  ctrlhdl.wcb  ctrlio.wcb \
 ctrlmap.wcb  ctrlval.wcb  

INQUIRY_WCB = inqmain.wcb inqctl.wcb   inqent.wcb   inqhlp.wcb  \
 inqnam.wcb   inqrpt.wcb   inqxtr.wcb \
 inqdat.wcb   inqget.wcb   inqmak.wcb   inqopt.wcb   inqxst.wcb \
 dteload.wcb  ctrldif.wcb \
 blnksc.wcb   kcsextfh.wcb plswait.wcb \
 rptmak.wcb   rptcld.wcb   rptio.wcb \
 ctrlary.wcb  ctrlio.wcb   ctrlhdl.wcb  ctrlhdi.wcb  ctrlmap.wcb \
 ctrlval.wcb  ctrldio.wcb

DATENTRY_WCB=dtemain.wcb \
 dtectl.wcb   dtefac.wcb   dtemnu.wcb   dtespc.wcb   dteuph.wcb   \
 dteoop.wcb   dteupd.wcb   dtexst.wcb   dteinqpp.wcb \
 blnksc.wcb   kcsextfh.wcb ctrldif.wcb \
 dteload.wcb  ctrldio.wcb  ctrlval.wcb \
 ctrlio.wcb   ctrlhdl.wcb  ctrlhdi.wcb  \
 ctrlary.wcb  ctrlmap.wcb  

CONTROL_CBX=$(CONTROL_WCB:.wcb=.cbx)
REPORT_CBX=$(REPORT_WCB:.wcb=.cbx)
INQUIRY_CBX=$(INQUIRY_WCB:.wcb=.cbx)
DATENTRY_CBX=$(DATENTRY_WCB:.wcb=.cbx)



#-----------------------------------------------------------------------
#
#	Rules
#


.SUFFIXES:  .cbx .wcb

.wcb.cbx:
	$(WISPTRAN) $(WISPFLAGS) $*.wcb
	$(ACU_COBOL) $(ACU_COBFLAGS) -o $*.cbx $*.cob


#-----------------------------------------------------------------------
#
#	Targets
#


header_acu:
	@echo "=="
	@echo "== Building KCSI $(KCSI_VERSION) for ACUCOBOL"
	@echo "== ACUDIR=" $(ACUDIR)
	@echo "== WISPTRAN=" $(WISPTRAN)
	@echo "== PWD="
	@CD
	@echo "=="

kcsi_acu: header_acu required CONTROL REPORT INQUIRY DATENTRY CREATE

required: $(WISPTRAN)

$(WISPTRAN) :
	@echo "=="
	@echo "== ERROR: Required file $@ was not found"
	@echo "=="
	@exit 1

rebuild_all: clean kcsi_acu

clean:
	-del /Q CONTROL REPORT INQUIRY DATENTRY CREATE
	-del /Q *.cob *.cpy *.cbx


#-----------------------------------------------------------------------
#
#	ACUCOBOL Targets
#


CONTROL:	$(CONTROL_CBX)
	$(CBLUTIL) -lib -o $@ $(CONTROL_CBX)
	@echo $@ is UP-TO-DATE

REPORT:		$(REPORT_CBX)
	$(CBLUTIL) -lib -o $@ $(REPORT_CBX)
	@echo $@ is UP-TO-DATE

INQUIRY:	$(INQUIRY_CBX)
	$(CBLUTIL) -lib -o $@ $(INQUIRY_CBX)
	@echo $@ is UP-TO-DATE

DATENTRY:	$(DATENTRY_CBX)
	$(CBLUTIL) -lib -o $@ $(DATENTRY_CBX)
	@echo $@ is UP-TO-DATE

CREATE:	CREATE.wcb
	$(WISPTRAN) $(WISPFLAGS) CREATE.wcb
	$(ACU_COBOL) $(ACU_COBFLAGS) -o CREATE CREATE.cob
	@echo $@ is UP-TO-DATE


#-----------------------------------------------------------------------
#	End
#-----------------------------------------------------------------------
