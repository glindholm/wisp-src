#	Copyright (c) 1988-2003 NeoMedia Technologies, All rights reserved.
#	$Id:$
#
#
#	File:	wwruncbl.mak
#
#	Function:
#		The WIN32 makefile for building the ACUCOBOL-GT runtime
#		systems that include the WISP runtime routines.
#
#		This file supports ACUCOBOL-GT 3.2 through 4.2.
#
#		For ACUCOBOL-GT 5.2 use:
#			wrun32wisp_acu52.mak
#			wrun32wisp_crid_acu52.mak
#			wrun32wisp_ede_acu52.mak
#
#		For ACUCOBOL-GT 4.3 and 5.1 use:
#			wrun32wisp_acu51.mak
#			wrun32wisp_crid_acu51.mak
#			wrun32wisp_ede_acu51.mak
#
#	Description:
#		This makefile can generate the following versions
#		of the ACUCOBOL runtime system.
#
#		wwruncbl.exe	The standard runtime for ACUCOBOL.
#
#		wwruncblk.exe	The CRID version of the runtime.
#
#		wwruncble.exe	The EDE version of the runtime.
#
#		wwruncblke.exe	The CRID + EDE runtime.
#
#		run32w.exe	The console runtime.
#
#		run32wk.exe	The console runtime with CRID.
#
#
#	Instructions:
#		To create an ACUCOBOL runtime system position to 
#		the wisp\acu directory and run the "nmake" utility 
#		using this file as input.
#
#		This makefile requires instructions on how to find
#		the WISP libraries and the ACUCOBOL files. This is
#		done using the variables WISPDIR and ACUDIR.
#
#			WISPDIR = The installed WISP directory
#			ACUDIR  = The ACUCOBOL directory
#			EDEDIR  = The EDE directory (if needed)
#			CRIDDIR = The CRID directory (if needed)
#
#
#		Edit this file and change WISPDIR and ACUDIR to 
#		point to the correct locations.
#
#			WISPDIR=C:\WISP44xx
#			ACUDIR=C:\ACUCBL42\ACUGT
#			EDEDIR=C:\EDE44xx
#			CRIDDIR=C:\CRIDACU3003
#
#		To build a standard runtime:
#			C:\WISP\ACU> nmake /f wwruncbl.mak
#
#		To build a CRID runtime:
#			C:\WISP\ACU> nmake /f wwruncbl.mak crid
#
#		To build a EDE runtime:
#			C:\WISP\ACU> nmake /f wwruncbl.mak ede
#
#		To build a CRID+EDE runtime:
#			C:\WISP\ACU> nmake /f wwruncbl.mak cridede
#
#		To build a console runtime:
#			C:\WISP\ACU> nmake /f wwruncbl.mak rtst
#
#		To build a console runtime with CRID:
#			C:\WISP\ACU> nmake /f wwruncbl.mak cridt
#
#
#	Targets:
#		When running the make utility you can specify what "targets"
#		to create.  You specify a target by adding it to the end
#		of the make command.
#
#		Usage:	nmake /f wwruncbl.mak [targets...]
#
#		The following targets are recognized in this makefile.
#		Each target that builds a runtime has a corresponding macro 
#		which can be used to change the name of the runtime.
#
#		rts		The default WISP + ACUCOBOL runtime.
#				Runtime = wwruncbl.exe
#				Macro   = RTS
#
#		crid		The CRID runtime.
#				Runtime = wwruncblk.exe
#				Macro   = RTSK
#
#		ede		The EDE runtime.
#				Runtime = wwruncble.exe
#				Macro   = RTSE
#
#		cridede		The CRID + EDE  runtime.
#				Runtime = wwruncblke.exe
#				Macro   = RTSKE
#
#		rtst		The console WISP + ACUCOBOL runtime.
#				Runtime = run32w.exe
#				Macro   = RTST
#
#		cridt		The console CRID runtime.
#				Runtime = run32wt.exe
#				Macro   = RTSKT
#
#		both		Create both rts and ede targets.
#
#		clean		This target is used to remove any files 
#				created by this makefile.
#
#		Examples:
#
#		(1) Build the CRID runtime using the name "wruncrid.exe".
#
#		   C:\WISP\ACU> nmake /f wwruncbl.mak RTSK=wruncrid.exe crid
#
#
#	NOTE:	This makefile supports different versions of ACUCOBOL.
#		Depending on the version of ACUCOBOL you are using you
#		may have to modifiy this makefile.   See the section on
#		changing version settings below.
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
# ACURPCDIR	The directory for FTP libraries for Acuserver. Supply if
#		an Acuserver client (CLIENT=1).
# WISPDIR	The installed WISP directory
# EDEDIR	The installed EDE directory
# CRIDDIR	The CRID directory
#
# OUTDIR	The output directory where the rts is created
#
#ACUDIR=C:\ACUCBL40\ACUGT
ACUDIR=C:\Acucorp\ACUCBL41
#ACUDIR=C:\Acucorp\ACUCBL42\ACUGT
#ACUDIR=C:\Acucorp\ACUCBL520\ACUGT


ACURPCDIR=C:\ACUFTP

# Uncomment for Acuserver client enabled runtime.
#CLIENT=1

WISPDIR=C:\WISP4407
EDEDIR=C:\EDE4407
CRIDDIR=C:\CRIDACU3004

OUTDIR=.

#============================================================================
#
# WISPLIBSDIR points to the directory of the WISP libraries.  This is built
# based on WISPDIR, however it can be overridden if the libraries have been
# moved out of the installed WISP directory tree.
#
# ACULIBSDIR is the location of the ACUCOBOL libraries
#

WISPLIBSDIR=$(WISPDIR)\lib
ACULIBSDIR=$(ACUDIR)\lib


#============================================================================
#
# Standard WISP items
#
L_WISP=wisp
L_VIDEO=video
L_EDE=ede
LIBWISP=$(L_WISP).lib
LIBVIDEO=$(L_VIDEO).lib
LIBEDE=$(L_EDE).lib

WISP_LIB_PATH  		= $(WISPLIBSDIR)\$(LIBWISP)
VIDEO_LIB_PATH 		= $(WISPLIBSDIR)\$(LIBVIDEO)
EDE_LIB_PATH   		= $(EDEDIR)\$(LIBEDE)

WISP_LIBS_PATHS 	= $(WISP_LIB_PATH) $(VIDEO_LIB_PATH)
WISPEDE_LIBS_PATHS	= $(EDE_LIB_PATH) $(WISP_LIB_PATH) $(VIDEO_LIB_PATH)

WISPICON		= wispicon.ico
WISPRTSRC		= wisprts.rc
WISPRTSRES		= wisprts.res

WISPACUFILES 		= sub85.c $(WISPICON) $(WISPRTSRC)

EXTRA_LINK = 
OTHER_LINK		= $(EXTRA_LINK) /MAP /DEBUG

#============================================================================
#
# Default RTS names
#
RTS	=$(OUTDIR)\wwruncbl.exe
RTSE	=$(OUTDIR)\wwruncble.exe
RTSK	=$(OUTDIR)\wwruncblk.exe
RTSKE	=$(OUTDIR)\wwruncblke.exe

RTST	=$(OUTDIR)\run32w.exe
RTSET	=$(OUTDIR)\run32we.exe
RTSKT	=$(OUTDIR)\run32wk.exe
RTSKET	=$(OUTDIR)\run32wke.exe

#============================================================================
#
# Standard CC flags
#
STD_CFLAGS=$(cflags) $(cvars) -nologo -D_WINDOWS -DWINNT -DWIN32 -DMSFS -Z7
LDFLAGS=$(lflags)
RCFLAGS=

# Uncomment for BoundsChecker build
#link=D:\BChecker\nmlink.exe

#============================================================================
#
# ACULIBS is the list of ACUCOBOL libraries
# ACUSUBS is the list of ACUCOBOL subroutines (other then sub.obj)
# ACUTEST a test file used to determine version of ACUCOBOL
# ACUVISN is the list of link items needed to link in VISION filesystem
# ACUBLD  is extra link items needed (was part of OTHER_LINK)
# ACUCFLAGS is extra C compiler flags
#
# Different versions of ACUCOBOL use different sets of
# libraries and subroutines.
#
# 6	ACUCOBOL Version 3.2 - 3.2.1
# 7	ACUCOBOL Version 3.2.2
# 8	ACUCOBOL Version 4.0.0 - 4.2.0
#
#####################################################
## 6
!ifdef CLIENT
ACULIBS6=$(ACULIBSDIR)\wterm32.lib \
	$(ACULIBSDIR)\wclnt32.lib \
	$(ACULIBSDIR)\wrun32.lib \
	$(ACULIBSDIR)\wfsi32.lib \
	$(ACULIBSDIR)\wvis32.lib \
	$(ACULIBSDIR)\wmsg32.lib \
	$(ACULIBSDIR)\wmem32.lib \
	$(ACULIBSDIR)\wstd32.lib \
	rpc4w32.lib \
	wsock32.lib
ACUSUBS6=filetbl.obj \
	mswinsub.obj
!else
ACULIBS6=$(ACULIBSDIR)\wterm32.lib \
	$(ACULIBSDIR)\wrun32.lib \
	$(ACULIBSDIR)\wfsi32.lib \
	$(ACULIBSDIR)\wvis32.lib \
	$(ACULIBSDIR)\wmsg32.lib \
	$(ACULIBSDIR)\wmem32.lib \
	$(ACULIBSDIR)\wstd32.lib \
	wsock32.lib
ACUSUBS6=filetbl.obj \
	mswinsub.obj \
	$(ACULIBSDIR)\clntstub.obj \
	$(ACULIBSDIR)\netstub.obj \
	$(ACULIBSDIR)\wsastub.obj
!endif
ACUVISN6=$(ACULIBSDIR)\wfsi32.lib \
	$(ACULIBSDIR)\wvis32.lib \
	$(ACULIBSDIR)\wmsg32.lib \
	$(ACULIBSDIR)\wmem32.lib \
	$(ACULIBSDIR)\wstd32.lib
ACUTEST6=$(ACULIBSDIR)\wrun32.lib
ACUBLD6 = $(guilibs) netapi32.lib comctl32.lib winmm.lib

#####################################################
## 7
!ifdef CLIENT
ACULIBS7=$(ACULIBSDIR)\wterm32.lib \
	$(ACULIBSDIR)\wclnt32.lib \
	$(ACULIBSDIR)\wrun32.lib \
	$(ACULIBSDIR)\wfsi32.lib \
	$(ACULIBSDIR)\wvis32.lib \
	$(ACULIBSDIR)\wmsg32.lib \
	$(ACULIBSDIR)\wmem32.lib \
	$(ACULIBSDIR)\wstd32.lib \
	$(ACULIBSDIR)\conc32.lib \
	$(ACURPCDIR)\rpc4w32.lib \
	wsock32.lib
ACUSUBS7=filetbl.obj \
	mswinsub.obj
ACUCFLAGS7=-DNO_CLIENT=0 -DNO_ACUCONNECT=0
!else
ACULIBS7=$(ACULIBSDIR)\wterm32.lib \
	$(ACULIBSDIR)\wrun32.lib \
	$(ACULIBSDIR)\wfsi32.lib \
	$(ACULIBSDIR)\wvis32.lib \
	$(ACULIBSDIR)\wmsg32.lib \
	$(ACULIBSDIR)\wmem32.lib \
	$(ACULIBSDIR)\wstd32.lib \
	$(ACULIBSDIR)\conc32.lib 
ACUSUBS7=filetbl.obj \
	mswinsub.obj \
	$(ACULIBSDIR)\clntstub.obj \
	$(ACULIBSDIR)\netstub.obj \
	$(ACULIBSDIR)\wsastub.obj
ACUCFLAGS7=-DNO_CLIENT=1 -DNO_ACUCONNECT=1
!endif
ACUVISN7=$(ACULIBSDIR)\wfsi32.lib \
	$(ACULIBSDIR)\wvis32.lib \
	$(ACULIBSDIR)\wmsg32.lib \
	$(ACULIBSDIR)\wmem32.lib \
	$(ACULIBSDIR)\wstd32.lib \
	$(ACULIBSDIR)\clntstub.obj \
	$(ACULIBSDIR)\netstub.obj

ACUTEST7=$(ACULIBSDIR)\conc32.lib
ACUBLD7 = $(guilibs) netapi32.lib comctl32.lib winmm.lib

#####################################################
## 8
ACUSUBS8=filetbl.obj \
	mswinsub.obj

!ifdef CLIENT
ACUCLIENT_LIBS8= $(ACULIBSDIR)\wclnt32.lib \
	$(ACURPCDIR)\rpc4w32.lib
ACUCFLAGS8=-DNO_CLIENT=0
!else
ACUCLIENT_LIBS8=
ACUCFLAGS8=-DNO_CLIENT=1
!endif

ACULIBS8= $(ACUCLIENT_LIBS8) \
	$(ACULIBSDIR)\wrun32.lib \
	$(ACULIBSDIR)\wfsi32.lib \
	$(ACULIBSDIR)\wvis32.lib \
	$(ACULIBSDIR)\wmsg32.lib \
	$(ACULIBSDIR)\wmem32.lib \
	$(ACULIBSDIR)\wlib32.lib \
	$(ACULIBSDIR)\wstd32.lib \
	$(ACULIBSDIR)\conc32.lib 

ACUWLIBS8= $(ACULIBSDIR)\wterm32.lib
ACUTLIBS8= $(ACULIBSDIR)\term32.lib

ACUVISN8=$(ACULIBSDIR)\wfsi32.lib \
	$(ACULIBSDIR)\wvis32.lib \
	$(ACULIBSDIR)\wmsg32.lib \
	$(ACULIBSDIR)\wmem32.lib \
	$(ACULIBSDIR)\wstd32.lib

ACUTEST8=$(ACULIBSDIR)\wlib32.lib
ACUBLD8 = $(guilibs) wsock32.lib netapi32.lib comctl32.lib winmm.lib


#============================================================================
#
# ****  CHANGE VERSION SETTINGS HERE ****
#
# The following lines are used to change the ACUCOBOL version 
# settings.  Uncomment the 3 macro lines which match the
# version of ACUCOBOL you are using.
#
#
# 6	ACUCOBOL Version 3.2 - 3.2.1
#
# ACULIBS=$(ACULIBS6)
# ACUSUBS=$(ACUSUBS6)
# ACUTEST=$(ACUTEST6)
# ACUVISN=$(ACUVISN6)
# ACUBLD=$(ACUBLD6)
# ACUSRCDIR=$(ACUDIR)\lib
# ACURESFILEDEP=acucobol.ico acudebug.ico arrows.bmp acudbg.bmp help.cur go.cur
#
# 7	ACUCOBOL Version 3.2.2
#
#ACULIBS=$(ACULIBS7)
#ACUSUBS=$(ACUSUBS7)
#ACUTEST=$(ACUTEST7)
#ACUVISN=$(ACUVISN7)
#ACUBLD=$(ACUBLD7)
#ACUCFLAGS=$(ACUCFLAGS7)
#ACUSRCDIR=$(ACUDIR)\lib
#ACURESFILEDEP=acucobol.ico acudebug.ico arrows.bmp acudbg.bmp help.cur go.cur
#
# 8	ACUCOBOL Version 4.0.0 - 4.2.0
#
ACULIBS=$(ACUWLIBS8) $(ACULIBS8)
ACUTLIBS=$(ACUTLIBS8) $(ACULIBS8)
ACUSUBS=$(ACUSUBS8)
ACUTEST=$(ACUTEST8)
ACUVISN=$(ACUVISN8)
ACUBLD=$(ACUBLD8)
ACUCFLAGS=$(ACUCFLAGS8)  -DACUCANCEL42=1
ACUSRCDIR=$(ACUDIR)\lib
ACURESFILEDEP=acucobol.ico acudebug.ico arrows.bmp acudbg.bmp help.cur go.cur divider.cur

#
#  ACUSRCDIRLIST	Source files that get copied from ACUSRCDIR
#
ACUSRCDIRLIST = \
	sub.c \
	filetbl.c \
	mswinsub.c \
	config85.c \
	direct.c \
	wruncbl.rc \
	$(ACURESFILEDEP)

#
# ACU_SUB_DEP		Source file dependencies of sub.obj 
#
ACU_SUB_DEP = \
	sub.c \
	sub85.c \
	config85.c \
	direct.c

#============================================================================
#
CLFLAGS=$(STD_CFLAGS) $(ACUCFLAGS)

#============================================================================
#
# ACUFILES is a list of any files which could be found in the ACUCOBOL 
# directory.  It is used ONLY for detecting and displaying an error
# message if the file is not found.
#
RTSALC = $(ACUDIR)\bin\wrun32.alc

ACUFILES=$(ACUSRCDIR)\sub.c \
	$(ACUSRCDIR)\filetbl.c \
	$(ACUSRCDIR)\config85.c \
	$(ACUSRCDIR)\mswinsub.c \
	$(ACUSRCDIR)\wruncbl.rc \
	$(ACUSRCDIR)\acucobol.ico \
	$(ACUSRCDIR)\acudebug.ico \
	$(ACUSRCDIR)\acudbg.bmp \
	$(ACUSRCDIR)\help.cur \
	$(ACUSRCDIR)\go.cur \
	$(ACUSRCDIR)\DIVIDER.cur \
	$(ACUSRCDIR)\direct.c \
	$(ACULIBSDIR)\clntstub.obj \
	$(ACULIBSDIR)\netstub.obj \
	$(ACULIBSDIR)\wsastub.obj \
	$(RTSALC)

#============================================================================
#
# WISPFILES is a list of any files which could be found in the WISP
# directory.  It is used ONLY for detecting and displaying an error
# message if the file is not found.
#
WISPFILES=$(WISPLIBSDIR)\$(LIBWISP) \
	$(WISPLIBSDIR)\$(LIBVIDEO) \
	$(WISPACUFILES)

EDEFILES=$(EDEDIR)\$(LIBEDE)

#============================================================================
#
# CRIDFILES is a list of CRID files used in building the runtime.
#
CRID_LIB_PATH=$(CRIDDIR)\cridacu.lib
CRIDFILES=$(CRIDDIR)\crid.h \
	$(CRIDDIR)\cridtbl.c \
	$(CRIDDIR)\crid85.c \
	$(CRID_LIB_PATH)

CRID_DEP = crid.h crid85.c cridtbl.c


#============================================================================
#
# Used to relink the CREATE utility (with Acucobol 4.1)
#
CREATE	=$(OUTDIR)\create.exe
CREATEDIR=C:\CREATEACU3501
L_CREATE=createacu
CREATEACULIB=$(CREATEDIR)\$(L_CREATE).lib
CREATEACUSUB=$(CREATEDIR)\vscrmain.obj
CREATESUBS=$(CREATEACUSUB) filetbl.obj
CREATELIBS=$(ACUVISN) $(CREATEACULIB) $(WISP_LIBS_PATHS)

#============================================================================
#
# CLEANUP is a list of files that are created by this
# makefile and can be deleted.
#
CLEANUP=$(RTS) $(RTSE) $(RTSK) $(RTSKE) \
	$(RTST) $(RTSET) $(RTSKT) $(RTSKET) \
	sub.obj sub_wisp.obj sub_crid.obj \
	tsub_wisp.obj tsub_crid.obj filetbl.obj mswinsub.obj

#============================================================================
#
# TARGETS:
#

default: rts

header: $(WISPDIR) $(ACUDIR) $(ACUTEST)
	@echo ">>>> BUILDING ACUCOBOL RUNTIME"
	@echo ">>>>"
	@echo ">>>> WISPDIR   = " $(WISPDIR)
	@echo ">>>> ACUDIR    = " $(ACUDIR)
	@echo ">>>> CD        = " 
	@CD
	@echo ">>>>"

headerede:
	@echo ">>>> EDEDIR    = " $(EDEDIR)
	@echo ">>>>"

headercrid:
	@echo ">>>> CRIDDIR   = " $(CRIDDIR)
	@echo ">>>>"

headercreate:
	@echo ">>>> CREATEDIR = " $(CREATEDIR)
	@echo ">>>>"

$(WISPDIR):
	@echo ">>>> ERROR: The WISP directory was not found!"
	@echo ">>>>"
	@echo ">>>> Using WISPDIR = " $(WISPDIR)
	@echo ">>>>"
	@echo ">>>> See the instructions at the beginning of this makefile"
	@echo ">>>> for information on setting WISPDIR."
	@echo ">>>>"
	@exit_with_error

$(EDEDIR):
	@echo ">>>> ERROR: The EDE directory was not found!"
	@echo ">>>>"
	@echo ">>>> Using EDEDIR = " $(EDEDIR)
	@echo ">>>>"
	@echo ">>>> See the instructions at the beginning of this makefile"
	@echo ">>>> for information on setting EDEDIR."
	@echo ">>>>"
	@exit_with_error

$(ACUDIR):
	@echo ">>>> ERROR: The ACUCOBOL directory was not found!"
	@echo ">>>>"
	@echo ">>>> Using ACUDIR    = $(ACUDIR)"
	@echo ">>>> Using ACUSRCDIR = $(ACUSRCDIR)"
	@echo ">>>>"
	@echo ">>>> See the instructions at the beginning of this makefile"
	@echo ">>>> for information on setting ACUDIR."
	@echo ">>>>"
	@exit_with_error

$(ACUTEST):
	@echo ">>>> ERROR: A configuration error was detected!"
	@echo ">>>>"
	@echo ">>>> The ACUCOBOL file $@ was not found."
	@echo ">>>>"
	@echo ">>>> Using ACUDIR    = $(ACUDIR)"
	@echo ">>>> Using ACUSRCDIR = $(ACUSRCDIR)"
	@echo ">>>>"
	@echo ">>>> This makefile may be configured for a different version"
	@echo ">>>> of ACUCOBOL then the one you are using."
	@echo ">>>> See the instructions at the beginning of this makefile"
	@echo ">>>> for information on changing the version settings."
	@echo ">>>>"
	@exit_with_error

$(ACUFILES):
	@echo ">>>> ERROR: An ACUCOBOL configuration error was detected!"
	@echo ">>>>"
	@echo ">>>> The ACUCOBOL file $@ was not found."
	@echo ">>>>"
	@echo ">>>> Using ACUDIR    = $(ACUDIR)"
	@echo ">>>> Using ACUSRCDIR = $(ACUSRCDIR)"
	@echo ">>>>"
	@exit_with_error

$(WISPFILES):
	@echo ">>>> ERROR: An WISP configuration error was detected!"
	@echo ">>>>"
	@echo ">>>> The WISP file $@ was not found."
	@echo ">>>>"
	@echo ">>>> Using WISPDIR = $(WISPDIR)"
	@echo ">>>>"
	@exit_with_error

$(EDEFILES):
	@echo ">>>> ERROR: An WISP EDE configuration error was detected!"
	@echo ">>>>"
	@echo ">>>> The EDE file $@ was not found."
	@echo ">>>>"
	@echo ">>>> Using WISPDIR = $(WISPDIR)"
	@echo ">>>> Using EDEDIR  = $(EDEDIR)"
	@echo ">>>>"
	@exit_with_error

$(CRIDFILES):
	@echo ">>>> ERROR: An CRID configuration error was detected!"
	@echo ">>>>"
	@echo ">>>> The CRID file $@ was not found."
	@echo ">>>>"
	@echo ">>>> Using CRIDDIR = $(CRIDDIR)"
	@echo ">>>>"
	@exit_with_error

rts:	header $(RTS) $(RTS:.exe=.alc)
	@echo ">>>>"
	@echo ">>>> RTS     = " $(RTS) is up to date.
	@echo ">>>>"

ede:	header headerede $(RTSE) $(RTSE:.exe=.alc)
	@echo ">>>>"
	@echo ">>>> RTS     = " $(RTSE) is up to date.
	@echo ">>>>"

rtst:	header $(RTST) $(RTST:.exe=.alc)
	@echo ">>>>"
	@echo ">>>> RTS     = " $(RTST) is up to date.
	@echo ">>>>"

edet:	header headerede $(RTSET) $(RTSET:.exe=.alc)
	@echo ">>>>"
	@echo ">>>> RTS     = " $(RTSET) is up to date.
	@echo ">>>>"

both:	rts ede

crid:	header headercrid $(RTSK) $(RTSK:.exe=.alc)
	@echo ">>>>"
	@echo ">>>> RTS     = " $(RTSK) is up to date.
	@echo ">>>>"

cridede: header headerede headercrid $(RTSKE)  $(RTSKE:.exe=.alc)
	@echo ">>>>"
	@echo ">>>> RTS     = " $(RTSKE) is up to date.
	@echo ">>>>"

cridt:	header headercrid $(RTSKT) $(RTSKT:.exe=.alc)
	@echo ">>>>"
	@echo ">>>> RTS     = " $(RTSKT) is up to date.
	@echo ">>>>"

cridedet: header headerede headercrid $(RTSKET)  $(RTSKET:.exe=.alc)
	@echo ">>>>"
	@echo ">>>> RTS     = " $(RTSKET) is up to date.
	@echo ">>>>"

create:	header headercreate $(CREATE)
	@echo ">>>>"
	@echo ">>>> RTS     = " $(CREATE) is up to date.
	@echo ">>>>"

clean:	
	del /Q $(CLEANUP) core *.exp *.map

#
#	Resource macros
#

ACURBJFILE=wruncbl.rbj
ACURESFILE=wruncbl.res
ACURCFILE=wruncbl.rc

#
#	LINK marcos
#

RTSLINKOBJS = $(ACUSUBS) $(WISPRTSRES)

#
#	RTS targets
#

$(RTS): $(WISP_LIBS_PATHS) $(RTSLINKOBJS) $(ACU_SUB_DEP) sub_wisp.obj
	@echo ">>>> Linking RTS = " $@
	$(link) $(LDFLAGS) $(guiflags) -out:$@ sub_wisp.obj $(RTSLINKOBJS) $(ACULIBS) \
		$(WISP_LIBS_PATHS) $(ACUBLD) $(OTHER_LINK)

$(RTSE): $(WISPEDE_LIBS_PATHS) $(RTSLINKOBJS) $(ACU_SUB_DEP) sub_wisp.obj
	@echo ">>>> Linking RTS = " $@
	$(link) $(LDFLAGS) $(guiflags) -out:$@ sub_wisp.obj $(RTSLINKOBJS) $(ACULIBS) \
		$(WISPEDE_LIBS_PATHS) $(ACUBLD) $(OTHER_LINK) 

$(RTSK): $(WISP_LIBS_PATHS) $(RTSLINKOBJS) $(CRID_LIB_PATH) $(ACU_SUB_DEP) $(CRID_DEP) sub_crid.obj
	@echo ">>>> Linking RTS = " $@
	$(link) $(LDFLAGS) $(guiflags) -out:$@ sub_crid.obj $(RTSLINKOBJS) $(ACULIBS) \
		$(WISP_LIBS_PATHS) $(CRID_LIB_PATH) $(ACUBLD) $(OTHER_LINK)

$(RTSKE): $(WISPEDE_LIBS_PATHS) $(RTSLINKOBJS) $(CRID_LIB_PATH) $(ACU_SUB_DEP) $(CRID_DEP) sub_crid.obj
	@echo ">>>> Linking RTS = " $@
	$(link) $(LDFLAGS) $(guiflags) -out:$@ sub_crid.obj $(RTSLINKOBJS) $(ACULIBS) \
		$(WISPEDE_LIBS_PATHS) $(CRID_LIB_PATH) $(ACUBLD) $(OTHER_LINK)

#
#	RTS (alternate terminal) targets - Requires Acucobol-GT 4.0 - 4.2
#

$(RTST): $(WISP_LIBS_PATHS) $(RTSLINKOBJS) $(ACU_SUB_DEP) tsub_wisp.obj
	@echo ">>>> Linking RTS = " $@
	$(link) $(LDFLAGS) $(conflags) -out:$@ tsub_wisp.obj $(RTSLINKOBJS) $(ACUTLIBS) \
		$(WISP_LIBS_PATHS) $(ACUBLD) $(OTHER_LINK) 

$(RTSET): $(WISPEDE_LIBS_PATHS) $(RTSLINKOBJS) $(ACU_SUB_DEP) tsub_wisp.obj
	@echo ">>>> Linking RTS = " $@
	$(link) $(LDFLAGS) $(conflags) -out:$@ tsub_wisp.obj $(RTSLINKOBJS) $(ACUTLIBS) \
		$(WISPEDE_LIBS_PATHS) $(ACUBLD) $(OTHER_LINK) 

$(RTSKT): $(WISP_LIBS_PATHS) $(RTSLINKOBJS) $(CRID_LIB_PATH) $(ACU_SUB_DEP) $(CRID_DEP) tsub_crid.obj
	@echo ">>>> Linking RTS = " $@
	$(link) $(LDFLAGS) $(conflags) -out:$@ tsub_crid.obj $(RTSLINKOBJS) $(ACUTLIBS) \
		$(WISP_LIBS_PATHS) $(CRID_LIB_PATH) $(ACUBLD) $(OTHER_LINK) 

$(RTSKET): $(WISPEDE_LIBS_PATHS) $(RTSLINKOBJS) $(CRID_LIB_PATH) $(ACU_SUB_DEP) $(CRID_DEP) tsub_crid.obj
	@echo ">>>> Linking RTS = " $@
	$(link) $(LDFLAGS) $(conflags) -out:$@ tsub_crid.obj $(RTSLINKOBJS) $(ACUTLIBS) \
		$(WISPEDE_LIBS_PATHS) $(CRID_LIB_PATH) $(ACUBLD) $(OTHER_LINK)

#
#	CREATE target
#

$(CREATE): $(CREATESUBS) $(CREATELIBS)
	$(link) $(LDFLAGS) $(conflags) -out:$@ $(CREATESUBS) $(CREATELIBS) $(ACUBLD) $(OTHER_LINK)

#
#	Component targets
#

$(WISPRTSRES): $(WISPRTSRC) $(WISPICON) $(ACURCFILE) $(ACURESFILEDEP)

$(ACURBJFILE): $(ACURESFILE)
	cvtres -$(CPU) $(ACURESFILE) -o $@

$(ACURESFILE): $(ACURCFILE) $(ACURESFILEDEP)
	$(rc) $(RCFLAGS) $(ACURCFILE)

.c.obj:
	$(cc) $(CLFLAGS) /I$(ACUSRCDIR) $<

#
#	sub.obj		- standard acucobol sub.obj, not used anymore
#	sub_wisp.obj	- sub.obj plus wisp routines
#	sub_crid.obj	- sub.obj plus wisp and crid routines
#
#	tsub_wisp.obj	- alternate terminal sub.obj with wisp routines
#	tsub_crid.obj	- alternate terminal sub.obj wisp wisp and crid routines
#
sub.obj: $(ACU_SUB_DEP)

sub_crid.obj: $(ACU_SUB_DEP)
	$(cc) $(CLFLAGS) /Fo$@ /DCRID /I$(ACUSRCDIR) sub.c

sub_wisp.obj: $(ACU_SUB_DEP)
	$(cc) $(CLFLAGS) /Fo$@ /I$(ACUSRCDIR) sub.c

tsub_wisp.obj: $(ACU_SUB_DEP)
	$(cc) $(CLFLAGS) /Fo$@ -DACU_ALWAYS_INIT /I$(ACUSRCDIR) sub.c

tsub_crid.obj: $(ACU_SUB_DEP)
	$(cc) $(CLFLAGS) /Fo$@ /DCRID -DACU_ALWAYS_INIT /I$(ACUSRCDIR) sub.c

#
#	Copy files to current dir
#
$(ACUSRCDIRLIST): $(ACUSRCDIR)\$(@F)
	copy $(ACUSRCDIR)\$@ $@

#
#	Copy Acucobol license file
#

$(RTS:.exe=.alc) $(RTSE:.exe=.alc) $(RTSK:.exe=.alc) $(RTSKE:.exe=.alc): $(RTSALC)
	copy $(RTSALC) $@

$(RTST:.exe=.alc) $(RTSET:.exe=.alc) $(RTSKT:.exe=.alc) $(RTSKET:.exe=.alc): $(RTSALC)
	copy $(RTSALC) $@

#
#	CRID targets
#

$(CRID_DEP): $(CRIDDIR)\$(@F)
	copy $(CRIDDIR)\$@ $@

#
#	CREATE targets
#

$(CREATEACUSUB):
	$(cc) $(CLFLAGS) -I ..\wispcommon /Fo$@ $*.c


#
# End of file
#
