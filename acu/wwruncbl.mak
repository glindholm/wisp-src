#	Copyright (c) 1988-1995 DevTech Migrations, All rights reserved.
#	$Id:$
#
#
#	File:	wwruncbl.mak
#
#	Function:
#		The WINNT/WIN95 makefile for building the ACUCOBOL runtime
#		systems that include the WISP runtime routines.
#
#	Description:
#		This makefile can generate the following versions
#		of the ACUCOBOL runtime system.
#
#		wwruncbl	The standard runtime for ACUCOBOL.
#
#		wwruncble	The EDE version of the runtime.
#
#		wwruncblk	The KCSI version of the runtime.
#
#		wwruncblke	The KCSI + EDE runtime.
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
#			KCSIDIR = The KCSI directory (if needed)
#
#
#		Edit this file and change WISPDIR and ACUDIR to 
#		point to the correct locations.
#
#			WISPDIR=C:\WISP
#			ACUDIR=C:\ACUCOBOL
#			KCSIDIR=C:\CRIDACU
#
#		To build a standard runtime:
#			C:\WISP\ACU> nmake /f wwruncbl.mak
#
#		To build a EDE runtime:
#			C:\WISP\ACU> nmake /f wwruncbl.mak ede
#
#		To build a KCSI runtime:
#			C:\WISP\ACU> nmake /f wwruncbl.mak kcsi
#
#		To build a KCSI+EDE runtime:
#			C:\WISP\ACU> nmake /f wwruncbl.mak kcsiede
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
#				Runtime = wwruncbl
#				Macro   = RTS
#
#		ede		The EDE runtime.
#				Runtime = wwruncble
#				Macro   = RTSE
#
#		kcsi		The KCSI runtime.
#				Runtime = wwruncblk
#				Macro   = RTSK
#
#		kcsiede		The KCSI + EDE  runtime.
#				Runtime = wwruncblke
#				Macro   = RTSKE
#
#		both		Create both rts and ede targets.
#
#		clean		This target is used to remove any files 
#				created by this makefile.
#
#		Examples:
#
#		(1) Build the EDE runtime using the name "wrunede".
#
#			C:\WISP\ACU> nmake .f wruncbl.umf RTSE=wrunede ede
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
# WISPDIR	The installed WISP directory
# ACUDIR	The ACUCOBOL directory
# KCSIDIR	The KCSI directory
#
# OUTDIR	The output directory where the rts is created
#
#WISPDIR=C:\WISP
#ACUDIR=C:\ACUCBL31
#KCSIDIR=C:\CRIDACU
#CREATEDIR=C:\CREATEACU
WISPDIR=..
ACUDIR=D:\ACUCBL321
KCSIDIR=..\kcsi\crid
CREATEDIR=..\kcsi\create

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

WISPTRAN=$(WISPDIR)\bin\wisp.exe
ACUCOBOL=$(ACUDIR)\bin\ccbl32.exe

#============================================================================
#
# Standard WISP libraries
#
L_WISP=wisp
L_VIDEO=video
L_EDE=ede
LIBWISP=$(L_WISP).lib
LIBVIDEO=$(L_VIDEO).lib
LIBEDE=$(L_EDE).lib

WISP_LIB_PATH  		= $(WISPLIBSDIR)\$(LIBWISP)
VIDEO_LIB_PATH 		= $(WISPLIBSDIR)\$(LIBVIDEO)
EDE_LIB_PATH   		= $(WISPLIBSDIR)\$(LIBEDE)

WISP_LIBS_PATHS 	= $(WISP_LIB_PATH) $(VIDEO_LIB_PATH)
WISPEDE_LIBS_PATHS	= $(EDE_LIB_PATH) $(WISP_LIB_PATH) $(VIDEO_LIB_PATH)

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

CREATE	=$(OUTDIR)\create.exe

#============================================================================
#
# Standard CC flags
#
CLFLAGS=$(cflags) $(cvars) -nologo -D_WINDOWS -DWINNT -DWIN32 -DMSFS -Z7
LDFLAGS=$(lflags)
RCFLAGS=

#============================================================================
#
# ACULIBS is the list of ACUCOBOL libraries
# ACUSUBS is the list of ACUCOBOL subroutines (other then sub.obj)
# ACUTEST a test file used to determine version of ACUCOBOL
# ACUVISN is the list of link items needed to link in VISION filesystem
# ACUBLD  is extra link items needed (was part of OTHER_LINK
#
# Different versions of ACUCOBOL use different sets of
# libraries and subroutines.
#
# 4	ACUCOBOL Version 2.4 - 2.4.2
# 5	ACUCOBOL Version 3.1
# 6	ACUCOBOL Version 3.2
#
## 4
ACULIBS4=$(ACULIBSDIR)\wruncbl.lib $(ACULIBSDIR)\wacuterm.lib $(ACULIBSDIR)\wvision.lib
ACUSUBS4=filetbl.obj mswinsub.obj
ACUVISN4=$(ACUDIR)\lib\wvision.lib $(ACUDIR)\lib\wruncbl.lib
ACUTEST4=$(ACULIBSDIR)\wruncbl.lib
ACUBLD4 = $(guilibs) netapi32.lib

## 5
!ifdef CLIENT
ACULIBS5=$(ACULIBSDIR)\wterm32.lib \
	$(ACULIBSDIR)\wclnt32.lib \
	$(ACULIBSDIR)\wrun32.lib \
	$(ACULIBSDIR)\wfsi32.lib \
	$(ACULIBSDIR)\wvis32.lib \
	$(ACULIBSDIR)\wmsg32.lib \
	$(ACULIBSDIR)\wmem32.lib \
	$(ACULIBSDIR)\wstd32.lib \
	rpc4w32.lib \
	wsock32.lib
ACUSUBS5=filetbl.obj \
	mswinsub.obj
!else
ACULIBS5=$(ACULIBSDIR)\wterm32.lib \
	$(ACULIBSDIR)\wrun32.lib \
	$(ACULIBSDIR)\wfsi32.lib \
	$(ACULIBSDIR)\wvis32.lib \
	$(ACULIBSDIR)\wmsg32.lib \
	$(ACULIBSDIR)\wmem32.lib \
	$(ACULIBSDIR)\wstd32.lib
ACUSUBS5=filetbl.obj \
	mswinsub.obj \
	$(ACULIBSDIR)\clntstub.obj \
	$(ACULIBSDIR)\netstub.obj
!endif
ACUVISN5=$(ACULIBSDIR)\wfsi32.lib \
	$(ACULIBSDIR)\wvis32.lib \
	$(ACULIBSDIR)\wmem32.lib \
	$(ACULIBSDIR)\wstd32.lib
ACUTEST5=$(ACULIBSDIR)\wrun32.lib
ACUBLD5 = $(guilibs) netapi32.lib

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


#============================================================================
#
# ****  CHANGE VERSION SETTINGS HERE ****
#
# The following lines are used to change the ACUCOBOL version 
# settings.  Uncomment the 3 macro lines which match the
# version of ACUCOBOL you are using.
#
#
# 4	ACUCOBOL Version 2.4 - 2.4.2
#
# ACULIBS=$(ACULIBS4)
# ACUSUBS=$(ACUSUBS4)
# ACUTEST=$(ACUTEST4)
# ACUVISN=$(ACUVISN4)
# ACUBLD=$(ACUBLD4)
# ACUSRCDIR=$(ACUDIR)\lib
# ACURESFILEDEP=acucobol.ico acudebug.ico arrows.bmp
#
# 5	ACUCOBOL Version 3.1
#
# ACULIBS=$(ACULIBS5)
# ACUSUBS=$(ACUSUBS5)
# ACUTEST=$(ACUTEST5)
# ACUVISN=$(ACUVISN5)
# ACUBLD=$(ACUBLD5)
# ACUSRCDIR=$(ACUDIR)\lib
# ACURESFILEDEP=acucobol.ico acudebug.ico arrows.bmp
#
# 6	ACUCOBOL Version 3.2
#
ACULIBS=$(ACULIBS6)
ACUSUBS=$(ACUSUBS6)
ACUTEST=$(ACUTEST6)
ACUVISN=$(ACUVISN6)
ACUBLD=$(ACUBLD6)
ACUSRCDIR=$(ACUDIR)\lib
ACURESFILEDEP=acucobol.ico acudebug.ico arrows.bmp acudbg.bmp help.cur go.cur


ACUSRCSUBLIST = sub.c filetbl.c mswinsub.c 
ACUSRCDIRLIST = $(ACUSRCSUBLIST) wruncbl.rc $(ACURESFILEDEP)

ACU_SUB_DEP = $(ACUSRCSUBLIST) sub85.c


#============================================================================
#
# ACUFILES is a list of any files which could be found in the ACUCOBOL 
# directory.  It is used ONLY for detecting and displaying an error
# message if the file is not found.
#
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
	$(ACUSRCDIR)\direct.c \
	$(ACUSRCDIR)\clntstub.obj \
	$(ACULIBSDIR)\netstub.obj \
	$(ACULIBSDIR)\wsastub.obj

#============================================================================
#
# WISPFILES is a list of any files which could be found in the WISP
# directory.  It is used ONLY for detecting and displaying an error
# message if the file is not found.
#
WISPFILES=$(WISPLIBSDIR)\$(LIBWISP) \
	$(WISPLIBSDIR)\$(LIBVIDEO) \
	$(WISPLIBSDIR)\$(LIBEDE) \
	$(WISPDIR)\acu\sub85.c

#============================================================================
#
# KCSIFILES is a list of KCSI files used in building the runtime.
#
KCSI_LIB_PATH=$(KCSIDIR)\cridacu.lib
KCSIFILES=$(KCSIDIR)\crid.h \
	$(KCSIDIR)\cridtbl.c \
	$(KCSIDIR)\crid85.c \
	$(KCSI_LIB_PATH)

KCSI_DEP = crid.h crid85.c cridtbl.c

CREATEACUSUB=$(CREATEDIR)\vscracu.obj
CREATESUBS=$(CREATEACUSUB) filetbl.obj
CREATELIBS=$(ACUVISN) $(CREATEDIR)\createacu.lib $(WISP_LIBS_PATHS)

#============================================================================
#
# CLEANUP is a list of files that are created by this
# makefile and can be deleted.
#
CLEANUP=$(RTS) $(RTSE) sub.obj filetbl.obj mswinsub.obj

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

headerkcsi:
	@echo ">>>> KCSIDIR   = " $(KCSIDIR)
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

$(ACUCOBOL):
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

$(WISPTRAN):
	@echo ">>>> ERROR: An WISP configuration error was detected!"
	@echo ">>>>"
	@echo ">>>> The WISP file $@ was not found."
	@echo ">>>>"
	@echo ">>>> Using WISPDIR = $(WISPDIR)"
	@echo ">>>>"
	@exit_with_error

$(KCSIFILES):
	@echo ">>>> ERROR: An KCSI configuration error was detected!"
	@echo ">>>>"
	@echo ">>>> The KCSI file $@ was not found."
	@echo ">>>>"
	@echo ">>>> Using KCSIDIR = $(KCSIDIR)"
	@echo ">>>>"
	@exit_with_error

rts:	header $(RTS) acu
	@echo ">>>>"
	@echo ">>>> RTS     = " $(RTS) is up to date.
	@echo ">>>>"

ede:	header $(RTSE) acu
	@echo ">>>>"
	@echo ">>>> RTS     = " $(RTSE) is up to date.
	@echo ">>>>"

both:	rts ede

kcsi:	header headerkcsi $(RTSK)
	@echo ">>>>"
	@echo ">>>> RTS     = " $(RTSK) is up to date.
	@echo ">>>>"

kcsiede: header headerkcsi $(RTSKE)
	@echo ">>>>"
	@echo ">>>> RTS     = " $(RTSKE) is up to date.
	@echo ">>>>"

create:	header headercreate $(CREATE)
	@echo ">>>>"
	@echo ">>>> RTS     = " $(CREATE) is up to date.
	@echo ">>>>"

clean:	
	del $(CLEANUP) core

RBJFILE=wruncbl.rbj
RESFILE=wruncbl.res
RCFILE=wruncbl.rc

$(RTS): $(WISP_LIBS_PATHS) $(RBJFILE) $(ACU_SUB_DEP) $(ACUSUBS)
	$(cc) $(CLFLAGS) /I$(ACUSRCDIR) sub.c
	$(link) $(LDFLAGS) $(guiflags) -out:$@ sub.obj $(ACUSUBS) $(RBJFILE) $(ACULIBS) \
		$(WISP_LIBS_PATHS) $(ACUBLD) $(OTHER_LINK)

$(RTSE): $(WISPEDE_LIBS_PATHS) $(RBJFILE) $(ACU_SUB_DEP) $(ACUSUBS)
	$(cc) $(CLFLAGS) /I$(ACUSRCDIR) sub.c
	$(link) $(LDFLAGS) $(guiflags) -out:$@ sub.obj $(ACUSUBS) $(RBJFILE) $(ACULIBS) \
		$(WISPEDE_LIBS_PATHS) $(ACUBLD) $(OTHER_LINK) 

$(RTSK): $(WISP_LIBS_PATHS) $(RBJFILE) $(KCSI_LIB_PATH) $(ACU_SUB_DEP) $(ACUSUBS) $(KCSI_DEP)
	$(cc) /DCRID $(CLFLAGS) /I$(ACUSRCDIR) sub.c
	$(link) $(LDFLAGS) $(guiflags) -out:$@ sub.obj $(ACUSUBS) $(RBJFILE) $(ACULIBS) \
		$(WISP_LIBS_PATHS) $(KCSI_LIB_PATH) $(ACUBLD) $(OTHER_LINK)

$(RTSKE): $(WISPEDE_LIBS_PATHS) $(RBJFILE) $(KCSI_LIB_PATH) $(ACU_SUB_DEP) $(ACUSUBS) $(KCSI_DEP)
	$(cc) /DCRID $(CLFLAGS) /I$(ACUSRCDIR) sub.c
	$(link) $(LDFLAGS) $(guiflags) -out:$@ sub.obj $(ACUSUBS) $(RBJFILE) $(ACULIBS) \
		$(WISPEDE_LIBS_PATHS) $(KCSI_LIB_PATH) $(ACUBLD) $(OTHER_LINK)


$(CREATE): $(CREATESUBS) $(CREATELIBS)
	$(link) $(LDFLAGS) $(conflags) -out:$@ $(CREATESUBS) $(CREATELIBS) $(ACUBLD) $(OTHER_LINK)



$(RBJFILE): $(RESFILE)
	cvtres -$(CPU) $(RESFILE) -o $@

$(RESFILE): $(RCFILE) $(ACURESFILEDEP)
	$(rc) $(RCFLAGS) $(RCFILE)

.c.obj:
	$(cc) $(CLFLAGS) /I$(ACUSRCDIR) $<


sub.obj: $(ACU_SUB_DEP)

$(ACUSRCDIRLIST): $(ACUSRCDIR)\$(@F)
	copy $(ACUSRCDIR)\$@ $@

sub85.c: $(WISPDIR)\acu\sub85.c
	copy $(WISPDIR)\acu\sub85.c $@

#
#	KCSI targets
#

$(KCSI_DEP): $(KCSIDIR)\$(@F)
	copy $(KCSIDIR)\$@ $@

$(CREATEACUSUB):
	$(cc) $(CLFLAGS) /Fo$@ $*.c

#
#	ACULINK and ACUUSING
#
acu:	$(ACUCOBOL) ACULINK ACUUSING
	@echo "Target $@ is up-to-date."

ACULINK: aculink.cob
	$(ACUCOBOL) -da4 -zd -o $@ aculink.cob

aculink.cob: $(WISPTRAN) aculink.wcb
	$(WISPTRAN) $*.wcb

ACUUSING: acuusing.cob
	$(ACUCOBOL) -da4 -zd -o $@ acuusing.cob

#============================================================================
#
#	ACUCOBOL Native Screens programs
#
#	$ nmake -f wwruncbl.umf acn
#

COBFLAGS = -da4 -zd -te 800 

acn: $(ACUCOBOL) acn_objs
	@echo "Target $@ is up-to-date."

acn_objs: WACUGETPFKEY WACUFAC2SCREEN WACUHELP WACUERROR WACUGETPARM

WACUGETPFKEY: wacugetpfkey.cob
	$(ACUCOBOL) $(COBFLAGS) -o $@ wacugetpfkey.cob

WACUHELP: wacuhelp.cob
	$(ACUCOBOL) $(COBFLAGS) -o $@ wacuhelp.cob

WACUFAC2SCREEN: wacufac2screen.cob
	$(ACUCOBOL) $(COBFLAGS) -o $@ wacufac2screen.cob

WACUERROR: wacuerror.cob
	$(ACUCOBOL) $(COBFLAGS) -o $@ wacuerror.cob

WACUGETPARM: wacugetparm.cob
	$(ACUCOBOL) $(COBFLAGS) -o $@ wacugetparm.cob

