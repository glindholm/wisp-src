#	Copyright (c) 2002 NeoMedia Technologies, All rights reserved.
#	$Id:$
#
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
#		acu		Build ACULINK and ACUUSING
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
ACUDIR=C:\Acucorp\ACUCBL520\ACUGT
WISPDIR=C:\WISP4402

COBOL=$(ACUDIR)\bin\ccbl32.exe
COBFLAGS = -da4 -zd -te 800 -C32 -Z32

WISPTRAN=$(WISPDIR)\bin\wisp.exe
WISPFLAGS=-VACU

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
#	ACULINK and ACUUSING
#
ACU_BLD = ACULINK ACULINK.cob ACUUSING 

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

ACULINK: ACULINK.cob
	$(COBOL) $(COBFLAGS) -o $@ ACULINK.cob

ACULINK.cob: $(WISPTRAN) ACULINK.wcb
	$(WISPTRAN) $*.wcb

ACUUSING: ACUUSING.cob
	$(COBOL) $(COBFLAGS) -o $@ ACUUSING.cob

#============================================================================
#
#	ACUCOBOL Native Screens programs
#
#	$ nmake -f wwruncbl.mak acn
#

ACN_BLD = 	WACUERROR \
		WACUDISPLAY \
		WACUFAC2SCREEN \
		WACUGETPARM \
		WACUGETPFKEY \
		WACUHELP \
		WACUWSB

acn: acn_header $(ACN_BLD)
	@echo "Native Screens programs:"
	@echo "$(ACN_BLD)"
	@echo "are up-to-date."

acn_header: $(COBOL)
	@echo "BUILDING WISP ACUCOBOL Native Screens Programs"
	@echo " "
	@echo "COBOL  =  $(COBOL)"
	@echo ""

WACUERROR: wacuerror.cob
	$(COBOL) $(COBFLAGS) -o $@ wacuerror.cob

WACUDISPLAY: wacudisplay.cob
	$(COBOL) $(COBFLAGS) -o $@ wacudisplay.cob

WACUFAC2SCREEN: wacufac2screen.cob
	$(COBOL) $(COBFLAGS) -o $@ wacufac2screen.cob

WACUGETPARM: wacugetparm.cob
	$(COBOL) $(COBFLAGS) -o $@ wacugetparm.cob

WACUGETPFKEY: wacugetpfkey.cob
	$(COBOL) $(COBFLAGS) -o $@ wacugetpfkey.cob

WACUHELP: wacuhelp.cob
	$(COBOL) $(COBFLAGS) -o $@ wacuhelp.cob

WACUWSB: wacuwsb.cob
	$(COBOL) $(COBFLAGS) -o $@ wacuwsb.cob

#============================================================================
#
#	clean

clean:
	del /Q $(ACN_BLD) $(ACU_BLD)

#
# End of file
#