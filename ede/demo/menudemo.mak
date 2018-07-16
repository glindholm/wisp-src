#	Copyright (c) Shell Stream Software LLC. All Rights Reserved.
#	$Id:$
#
#
#	File:	menudemo.mak
#
#	Function:
#		The WINNT/WIN95 makefile for building the MENUDEMO program.
#
#	Description:
#		This makefile can be used for ACUCOBOL or MicroFocus.
#
#		Targets
#		-------
#		acu		The ACUCOBOL version
#
#		mf		The MicroFocus version
#

WISPTRAN=wisp
WISPFLAGS=-m -I . -1 -O menudemo.opt
ACU_COBOL=ccbl32
ACU_COBFLAGS=-Da4 -Gd -Za

#
#	Micro Focus	wcb --> int rule
#
.wcb.int:
	$(WISPTRAN) -VMF $(WISPFLAGS) $*.wcb
	cob -i $*.cob

#
#	ACUCOBOL	wcb --> (no extension) rule
#
#	NOTE:  This rule does not create acu files
#
.wcb.acu:
	$(WISPTRAN) -VACU $(WISPFLAGS) $*.wcb
	$(ACU_COBOL) $(ACU_COBFLAGS) -o $*.acu $*.cob

.SUFFIXES: .int .acu .wcb


default:
	@echo You must select a target of "acu" or "mf"


acu: MCBBLD.acu MCBEDIT.acu MENUDISP.acu MENULOGO.acu MENUVECT.acu MENUDEMO.acu

mf:  MCBBLD.INT MCBEDIT.INT MENUDISP.INT MENULOGO.INT MENUVECT.INT MENUDEMO.INT



