#	Copyright (c) 1988-1995 DevTech Migrations, All rights reserved.
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

#
#	Micro Focus	wcb --> int rule
#
.wcb.int:
	$(WISPTRAN) -VMF $(WISPFLAGS) $*.wcb
	cob -i $*.cob

#
#	ACUCOBOL	wcb --> (no extension) rule
#
#	NOTE:  This rule does not create cbx files
#
.wcb.cbx:
	$(WISPTRAN) -VACU $(WISPFLAGS) $*.wcb
	ccbl32 -da4 -o $* $*.cob

.SUFFIXES: .int .cbx .wcb


default:
	@echo You must select a target of "acu" or "mf"


acu: MCBBLD.CBX MCBEDIT.CBX MENUDISP.CBX MENULOGO.CBX MENUVECT.CBX MENUDEMO.CBX

mf:  MCBBLD.INT MCBEDIT.INT MENUDISP.INT MENULOGO.INT MENUVECT.INT MENUDEMO.INT



