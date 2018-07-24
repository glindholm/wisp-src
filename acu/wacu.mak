#	Copyright (c) Shell Stream Software LLC, All Rights Reserved.
#
#	File:	wacu.mak
#
#	Function:
#		The makefile for building Acucobol object
#		needed by WISP.
#
#	Instructions:
#		To build all the targets:
#			C:\WISPxxxx\ACU> nmake /f wacu.mak
#
#	Targets:
#		When running the make utility you can specify what "targets"
#		to create.  You specify a target by adding it to the end
#		of the make command.
#
#		Usage:	nmake /f wacu.mak [targets...]
#
#		The following targets are recognized in this makefile.
#
#		acu		Build ACULINK.acu and ACUUSING.acu
#
#		acn		Build Acucobol Native Screen routines
#
#			

!include <win32.mak>

#============================================================================
#
# **** CHANGE WISPDIR AND ACUDIR HERE ****
#
# These macros represent environment variables.
#
# ACUDIR	The ACUCOBOL directory
# WISPDIR	The installed WISP directory
#
#
#ACUDIR=C:\data\Acucorp\ACUCBL810\ACUGT
#ACUDIR=C:\data\Acucorp\ACUCBL900\ACUGT
#ACUDIR=C:\data\Acucorp\ACUCBL910\ACUGT

## [WISPVER]
#WISPDIR=C:\WISP5112

COBOL=$(ACUDIR)\bin\ccbl32.exe
COBFLAGS = -Da4 -Gd -Te 800 -C50 -Z50

WISPTRAN=$(WISPDIR)\bin\wisp.exe
WISPFLAGS=-VACU -u ACU50


.wcb.cob:
	$(WISPTRAN) $(WISPFLAGS) $*.wcb

.wcb.acu:
	$(WISPTRAN) $(WISPFLAGS) $*.wcb
	$(COBOL) $(COBFLAGS) -o $*.acu $*.cob

.cob.acu:
	$(COBOL) $(COBFLAGS) -o $*.acu $*.cob

.SUFFIXES: .acu .cob .wcb


#============================================================================

default: acu acn

#============================================================================

$(WISPTRAN):
	@echo ">>>> ERROR: An WISP configuration error was detected!"
	@echo ">>>>"
	@echo ">>>> The WISP file $@ was not found."
	@echo ">>>>"
	@echo ">>>> Using WISPDIR = $(WISPDIR)"
	@echo ">>>>"
	@exit_with_error


$(COBOL):
	@echo ">>>> ERROR: An ACUCOBOL configuration error was detected!"
	@echo ">>>>"
	@echo ">>>> The ACUCOBOL file $@ was not found."
	@echo ">>>>"
	@echo ">>>> Using ACUDIR    = $(ACUDIR)"
	@echo ">>>>"
	@exit_with_error

#============================================================================
#
#	ACULINK.acu and ACUUSING.acu
#
ACU_BLD = ACULINK.acu ACULINK.cob ACUUSING.acu

acu:	acu_header $(ACU_BLD)
	@echo "WISP Acucobol programs:"
	@echo "$(ACU_BLD)"
	@echo "are up-to-date."

acu_header: $(COBOL) $(WISPTRAN)
	@echo "BUILDING WISP ACUCOBOL Programs"
	@echo " "
	@echo "COBOL    =  $(COBOL)"
	@echo "WISPTRAN =  $(WISPTRAN)"
	@echo ""


#============================================================================
#
#	ACUCOBOL Native Screens programs
#
#	$ nmake -f wacu.mak acn
#

ACN_BLD = 	WACUERROR.acu \
		WACUDISPLAY.acu \
		WACUFAC2SCREEN.acu \
		WACUGETPARM.acu \
		WACUGETPFKEY.acu \
		WACUHELP.acu \
		WACUWSB.acu

acn: acn_header $(ACN_BLD)
	@echo "Native Screens programs:"
	@echo "$(ACN_BLD)"
	@echo "are up-to-date."

acn_header: $(COBOL)
	@echo "BUILDING WISP ACUCOBOL Native Screens Programs"
	@echo " "
	@echo "COBOL  =  $(COBOL)"
	@echo ""

acn_clean:
	-del /Q $(ACN_BLD)

#============================================================================
#
#	clean

clean:
	-del /Q $(ACN_BLD) $(ACU_BLD) wc*.cpy

#
# End of file
#
