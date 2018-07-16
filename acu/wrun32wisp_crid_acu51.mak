# wrun32wisp_crid_acu51.mak
#
# THIS FILE HAS BEEN MODIFIED TO ADD THE WISP AND CRID ROUTINES TO  
# THE ACUCOBOL RUNTIME.
#
#################################################################
#
# Creating an Acucobol 5.1 runtime requires WISP 4.4.00 and 
# CRID 3.0.00 or later.
#
# Follow these instructions carefully to build a custom Acucobol 
# runtime that includes the WISP and CRID runtime routines.  You are 
# going to build the runtime from a temporary folder, that is a copy
# of the Acucobol lib folder $(ACUDIR)\acugt\lib.  The Acucobol 
# runtime consists of two file, an exe and a dll. The custom WISP
# version is named wrun32wispc.exe and wrun32wispc.dll.
#
# 1) Create the temporary folder $(ACUDIR)\acugt\bldwispc by copying
#    and renaming the folder $(ACUDIR)\acugt\lib.  You can use
#    Windows Explorer to do this by opening $(ACUDIR)\acugt and
#    selecting the lib folder then doing a Copy then Paste command.
#
# 2) Copy the needed WISP and CRID files to the bldwispc folder. (You  
#    will be replacing the sub85.c file with the one supplied by WISP.)
#       Copy $(CRIDDIR)\wrun32wisp_crid_acu51.mak 
#            $(CRIDDIR)\crid.h
#            $(CRIDDIR)\cridtbl.c
#            $(CRIDDIR)\crid85.c
#            $(WISPDIR)\acu\sub85.c
#            $(WISPDIR)\acu\wisprts.rc
#            $(WISPDIR)\acu\wispicon.ico
#
#       to   $(ACUDIR)\acugt\bldwispc
#
# 3) Edit this file and set WISPDIR and CRIDDIR to the correct directory.
#
#       WISPDIR=C:\WISP4402
#       CRIDDIR=C:\CRIDACU3004
#
# 4) From a COMMAND/MSDOS window issue the NMAKE command.
#       $ cd $(ACUDIR)\acugt\bldwispc
#       $ NMAKE /f wrun32wisp_crid_acu51.mak
#
# 5) Copy the runtime files (wrun32wispc.exe and wrun32wispc.dll) to
#    their run location.
#       Copy $(ACUDIR)\acugt\bldwispc\wrun32wispc.exe
#            $(ACUDIR)\acugt\bldwispc\wrun32wispc.dll 
#
#       to   $(ACUDIR)\acugt\bin
#
#################################################################
#
# Makefile for recreating wrun32.dll from ACUCOBOL-GT libraries on 32-bit 
# Windows
# To relink the runtime type: nmake -f wrun32.mak
#   For AcuServer clients add : CLIENT=
# Acuthread.exe is the runtime used by the AcuConnect server
# To relink acuthread.exe type: nmake -f wrun32.mak acuthread.exe
#   For AcuServer clients add : CLIENT=
# Make sure your path for wsock32.lib is set to the correct location

# Distributed with ACUCOBOL-GT version 5.1.0.2
# PMK: 0, 1
#################################################################

# Set the Acucobol directory here
ACUDIR=C:\acucorp\acucbl510

# Set the installed WISP and CRID directory here.
WISPDIR=C:\WISP4402
CRIDDIR=C:\CRIDACU3004

#  Set the runtime name here. (Do not include a file extension.)
WRUN32=wrun32wispc

## WISP and CRID libraries
WISP_LIBS=     $(CRIDDIR)\cridacum.lib \
               $(WISPDIR)\lib\wispm.lib \
               $(WISPDIR)\lib\videom.lib

WISP_CFLAGS= /DCRID 

#################################################################

!include <ntwin32.mak>

!ifdef	DEBUG
DEBUG_CFLAGS=/MDd /D_WINDLL /D_USRDLL /D "_AFXDLL" 
DEBUG_LFLAGS=/debug
!else
DEBUG_CFLAGS=/MD /D_WINDLL /D_USRDLL /D "_AFXDLL"
DEBUG_LFLAGS=
!endif

LDFLAGS=/nologo /subsystem:windows /incremental:no $(DEBUG_LFLAGS) /machine:I386 
RCFLAGS=

## Changed from wruncbl to wisprts
RESFILE=wisprts.res
RBJFILE=wisprts.rbj
RCFILE=wisprts.rc

DLLRESFILE=wrundll.res
DLLRBJFILE=wrundll.rbj
DLLRCFILE=wrundll.rc

EXTRA_CFLAGS=-DNO_CLIENT=1

CLIENT_LIBS=

!ifdef  CLIENT
# AcuServer client version
EXTRA_CFLAGS=-DNO_CLIENT=0 
CLIENT_LIBS=wclnt32.lib $(CLIENT_LIBS)
!endif  # CLIENT #

ACUCONNECT_C = conc32.lib
ACUCONNECT_S = cons32.lib

SUBS=   \
	filetbl.obj \
	mswinsub.obj

LIBS=   \
	$(CLIENT_LIBS) \
	wrun32.lib \
	wcpp32.lib \
	wfsi32.lib \
	wvis32.lib \
	acme.lib \
	plugin32.lib

#  For building the Web Browser Plug-in
PLUGINSUBS=	\
	npwin.obj

PLUGINLIBS=wcpp32.lib
PLUGINRESFILE=plugin32.res
PLUGINRBJFILE=plugin32.rbj
PLUGINRCFILE=plugin32.rc

#  For building the Windowing version
WSUBS=sub.obj $(SUBS)
## Added WISP libraries
WLIBS=wterm32.lib $(LIBS) $(WISP_LIBS)

# MFC application class
MFCAPP =	\
	wcpp32.lib \
	wrunapp.obj
MFCDLL =	\
	wcpp32.lib \
	wdllapp.obj
MFCNPDLL =	\
	wcpp32.lib \
	wnpapp.obj
MFCSTAT = \
	wcpp32.lib \
	wstatapp.obj

## Added WISP CFLAGS
CLFLAGS=$(cflags) $(cvars) $(DEBUG_CFLAGS) -nologo -D_WINDOWS -DWINNT \
		$(EXTRA_CFLAGS) $(WISP_CFLAGS)

.c.obj:
	$(cc) $(CLFLAGS) $<

## Change wrun32 to $(WRUN32).
## Change wrundll.lib to $(WRUNDLL_LIB)
## Change wrundll.exp to $(WRUNDLL_EXP)

WRUNDLL_LIB=$(WRUN32)_import.lib
WRUNDLL_EXP=$(WRUN32)_import.exp

## Add default target to build both runtime files.

default: $(WRUN32).exe $(WRUN32).dll

$(WRUN32).dll: $(MFCDLL) $(WSUBS) $(DLLRBJFILE) $(WLIBS) $(ACUCONNECT_C)
	$(link) $(LDFLAGS) /dll /out:$@ /def:wrundll.def \
		/implib:$(WRUNDLL_LIB) \
		$(MFCDLL) $(WLIBS) $(WSUBS) $(ACUCONNECT_C) $(DLLRBJFILE)


$(WRUN32).exe: $(MFCAPP) $(WRUN32).dll wcpp32.lib $(RBJFILE)
	$(link) $(LDFLAGS) /out:$@ $(MFCAPP) wcpp32.lib \
		$(WRUNDLL_LIB) $(RBJFILE)

acuthread.exe: $(MFCSTAT) $(WSUBS) $(RBJFILE) $(WLIBS)
	$(cc) $(CLFLAGS) -DACUCONNECT_SRV sub.c
	$(link) $(LDFLAGS) -out:$@ -implib:wrunexe.lib $(MFCSTAT) $(WSUBS) \
		$(RBJFILE) $(WLIBS) $(ACUCONNECT_S) $(ACUCONNECT_C) wsock32.lib

$(DLLRBJFILE): $(DLLRESFILE)
	cvtres -$(CPU) $(DLLRESFILE) -o $@

$(DLLRESFILE): $(DLLRCFILE)
	$(rc) $(DLLRCFLAGS) $(DLLRCFILE)

$(RBJFILE): $(RESFILE)
	cvtres -$(CPU) $(RESFILE) -o $@

$(RESFILE): $(RCFILE)
	$(rc) $(RCFLAGS) $(RCFILE)

NPacu32.dll: $(MFCNPDLL) $(PLUGINSUBS) $(PLUGINLIBS) $(WRUN32).dll \
	$(PLUGINRBJFILE)
	$(link) $(LDFLAGS) /dll /out:$@ /def:plugin32.def \
		/implib:NPacudll.lib \
		$(MFCNPDLL) $(PLUGINSUBS) $(PLUGINLIBS) $(WRUNDLL_LIB) \
		$(PLUGINRBJFILE)

$(PLUGINRBJFILE): $(PLUGINRESFILE)
	cvtres -$(CPU) $(PLUGINRESFILE) -o $@

$(PLUGINRESFILE): $(PLUGINRCFILE)
	$(rc) $(PLUGINRCFLAGS) $(PLUGINRCFILE)

sub.obj: sub.c sub85.c config85.c direct.c

tsub.obj: sub.c sub85.c config85.c direct.c
	$(cc) $(CLFLAGS) /Fo$@ -DACU_ALWAYS_INIT sub.c

clean:
	-del $(WRUN32).dll
	-del $(WRUN32).exe
	-del acuthread.exe 
	-del NPacu32.dll
	-del sub.obj 
	-del filetbl.obj 
	-del mswinsub.obj
	-del $(RESFILE)
	-del $(RBJFILE)
	-del $(DLLRESFILE)
	-del $(DLLRBJFILE)
	-del $(WRUNDLL_LIB)
	-del $(WRUNDLL_EXP)
	-del $(PLUGINRESFILE)
	-del $(PLUGINRBJFILE)
	-del NPacudll.lib
	-del NPacudll.exp
