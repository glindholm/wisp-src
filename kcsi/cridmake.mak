#-----------------------------------------------------------------------
#	Copyright (c) 1998-2002 NeoMedia Technologies, All rights reserved.
#	$Id:$
#
#	File:		cridmake.mak
#
#	Project:	CRID
#
#	RCS:		$Source:$
#
#	Purpose:	Build CRID on WIN32
#
#-----------------------------------------------------------------------
#
#	Targets:	
#
#	all		All shipkits (default)
#	crid_acu	The Acucobol CRID shipkit
#	crid_mf		The Micro Focus CRID shipkit
#	clean		Remove all built pieces
#
#-----------------------------------------------------------------------
#
#	This makefile assumes that the environment is setup for porting
#	the WISP kit.
#
#	Usage:
#
#	$ make -f cridmake.umf WISP=xxx WISPDIR=xxx {target}
#	or
#	$ make -e -f cridmake.umf {target}
#
#-----------------------------------------------------------------------
crid_default: crid

MAKEFILE=cridmake.mak

#-----------------------------------------------------------------------
#
# Change copyright date in "cridvers.c"
#
# To change the version for CRID, change CRID_VERSION here. 
#	wisp/src/kcsi/cridvers.c
#	wisp/src/kcsi/cridmake.umf
#	wisp/src/kcsi/cridmake.mak
#	wisp/src/kcsi/crid_relnotes.txt
#	wisp/src/kcsi/cridntsetup.txt
#	wisp/src/kcsi/crid_acu_install.txt
#	wisp/src/kcsi/crid_mf_install.txt
#	wisp/src/kcsi/crid_packlist.txt
#	wisp/src/doc/wisp_relnotes.lis
#	wisp/src/acu/wruncbl.umf
#	wisp/src/acu/wrun32wisp_crid_acu51.mak
#	wisp/src/acu/wrun32wisp_crid_acu52.mak
#
CRID_VERSION=3004

#-----------------------------------------------------------------------
#
WISPTRAN=c:\dev\wisp\src\bin\wisp.exe
#-----------------------------------------------------------------------
#
# 	ACUCOBOL
#
ACUDIR= C:\acucorp\ACUCBL520\acugt
ACU_COBOL = $(ACUDIR)\bin\ccbl32.exe
ACU_FLAGS = -Da4 -C32 -Z32 -Za
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


.SUFFIXES: .o .cbx .wcb

.wcb.cbx:
	$(WISPTRAN) -VACU $(WISPFLAGS) $*.wcb
	$(ACU_COBOL) $(ACU_FLAGS) -o $*.cbx $*.cob


#-----------------------------------------------------------------------
#
#	Targets
#


header_acu:
	@echo "=="
	@echo "== Building CRID $(CRID_VERSION) for ACUCOBOL"
	@echo "== ACUDIR=" $(ACUDIR)
	@echo "== PWD="
	@CD
	@echo "=="

crid: header_acu required CONTROL REPORT INQUIRY DATENTRY

required: $(WISPTRAN)

$(WISPTRAN) :
	@echo "=="
	@echo "== ERROR: Required file $@ was not found"
	@echo "=="
	@exit 1

rebuild_all: clean crid

clean:
	del /Q CONTROL REPORT INQUIRY DATENTRY
	del /Q *.cob *.cpy *.cbx


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


#-----------------------------------------------------------------------
#	End
#-----------------------------------------------------------------------
