# wrun32wisp.mak
#
# THIS FILE HAS BEEN MODIFIED TO ADD THE WISP ROUTINES TO THE 
# ACUCOBOL RUNTIME.
#
#################################################################
#
# Creating an Acucobol 4.3 runtime requires WISP 4.3.06.
#
# Follow these instructions carefully to build a custom Acucobol 
# runtime that includes the WISP runtime routines.  You are going
# to build the runtime from a temporary folder, that is a copy
# of the Acucobol lib folder $(ACUDIR)\acugt\lib.  The Acucobol 
# runtime consists of two file, an  exe and a dll. The custom WISP
# version is named wrun32wisp.exe  and wrun32wisp.dll.
#
# 1) Create the temporary folder $(ACUDIR)\acugt\bldwisp by copying
#    and renaming the folder $(ACUDIR)\acugt\lib.  You can use
#    Windows Explorer to do this by opening $(ACUDIR)\acugt and
#    selecting the lib folder then doing a Copy then Paste command.
#
# 2) Copy the needed WISP files to the bldwisp folder. (You will be 
#    replacing the sub85.c file with the one supplied by WISP.)
#       Copy $(WISPDIR)\acu\wrun32wisp.mak 
#            $(WISPDIR)\acu\sub85.c
#            $(WISPDIR)\acu\wisprts.rc
#            $(WISPDIR)\acu\wispicon.ico
#
#       to   $(ACUDIR)\acugt\bldwisp
#
# 3) Edit this file $(ACUDIR)\acugt\bldwisp\wrun32wisp.mak and set 
#    WISPDIR to the correct directory.
#
#       WISPDIR=C:\WISP4306
#
# 4) From a DOS window issue the NMAKE command.
#       $ cd $(ACUDIR)\acugt\bldwisp
#       $ NMAKE /f wrun32wisp.mak
#
# 5) Copy the runtime files (wrun32wisp.exe and wrun32wisp.dll) to
#    their run location.
#       Copy $(ACUDIR)\acugt\bldwisp\wrun32wisp.exe
#            $(ACUDIR)\acugt\bldwisp\wrun32wisp.dll 
#
#       to   \\server\wisp\bin
#
# 6) Copy and rename the Acucobol license file to the run location.
#       Copy $(ACUDIR)\acugt\bin\wrun32.alc 
#
#       to   \\server\wisp\bin\wrun32wisp.alc
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

# Distributed with ACUCOBOL-GT version 4.3.0
# PMK: WinNTS, WinWKS, Win9x

#  Set the runtime name here. (Do not include a file extension.)
WRUN32WISP=wrun32wisp

# Set the installed WISP directory here.
WISPDIR=C:\WISP4306

## WISP libraries
WISP_LIBS=      $(WISPDIR)\lib\wispm.lib \
                $(WISPDIR)\lib\videom.lib

!include <ntwin32.mak>

!ifdef  DEBUG
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

RPCDIR=c:\pctcp32\sdk\netmsc

EXTRA_CFLAGS=-DNO_CLIENT=1

CLIENT_LIBS=

!ifdef  CLIENT
# AcuServer client version
EXTRA_CFLAGS=-DNO_CLIENT=0 
CLIENT_LIBS=wclnt32.lib $(RPCDIR)\rpc4w32.lib $(CLIENT_LIBS)
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
        wmsg32.lib \
        wmem32.lib \
        wlib32.lib \
        wstd32.lib \
        plugin32.lib

#  For building the Web Browser Plug-in
PLUGINSUBS=     \
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
MFCAPP =        \
        wcpp32.lib \
        wrunapp.obj
MFCDLL =        \
        wcpp32.lib \
        wdllapp.obj
MFCNPDLL =      \
        wcpp32.lib \
        wnpapp.obj
MFCSTAT = \
        wcpp32.lib \
        wstatapp.obj


CLFLAGS=$(cflags) $(cvars) $(DEBUG_CFLAGS) -nologo -D_WINDOWS -DWINNT \
                $(EXTRA_CFLAGS)

.c.obj:
        $(cc) $(CLFLAGS) $<

## Add default target to build both runtime files.

default: $(WRUN32WISP).exe $(WRUN32WISP).dll

## Change to $(WRUN32WISP) from wrun32.

$(WRUN32WISP).dll: $(MFCDLL) $(WSUBS) $(DLLRBJFILE) $(WLIBS) $(ACUCONNECT_C)
        $(link) $(LDFLAGS) /dll /out:$@ /def:wrundll.def \
                /implib:$(WRUN32WISP)_import.lib \
                $(MFCDLL) $(WLIBS) $(WSUBS) $(ACUCONNECT_C) $(DLLRBJFILE)


$(WRUN32WISP).exe: $(MFCAPP) $(WRUN32WISP).dll wcpp32.lib $(RBJFILE)
        $(link) $(LDFLAGS) /out:$@ $(MFCAPP) wcpp32.lib \
                $(WRUN32WISP)_import.lib $(RBJFILE)

acuthread.exe: $(MFCSTAT) $(WSUBS) $(RBJFILE) $(WLIBS)
        $(link) $(LDFLAGS) -out:$@ -implib:wrunexe.lib $(MFCSTAT) $(WSUBS) \
                $(RBJFILE) $(WLIBS) $(ACUCONNECT_S) wsock32.lib

$(DLLRBJFILE): $(DLLRESFILE)
        cvtres -$(CPU) $(DLLRESFILE) -o $@

$(DLLRESFILE): $(DLLRCFILE)
        $(rc) $(DLLRCFLAGS) $(DLLRCFILE)

$(RBJFILE): $(RESFILE)
        cvtres -$(CPU) $(RESFILE) -o $@

$(RESFILE): $(RCFILE)
        $(rc) $(RCFLAGS) $(RCFILE)

NPacu32.dll: $(MFCNPDLL) $(PLUGINSUBS) $(PLUGINLIBS) $(WRUN32WISP).dll \
        $(PLUGINRBJFILE)
        $(link) $(LDFLAGS) /dll /out:$@ /def:plugin32.def \
                /implib:NPacudll.lib \
                $(MFCNPDLL) $(PLUGINSUBS) $(PLUGINLIBS) $(WRUN32WISP)_import.lib \
                $(PLUGINRBJFILE)

$(PLUGINRBJFILE): $(PLUGINRESFILE)
        cvtres -$(CPU) $(PLUGINRESFILE) -o $@

$(PLUGINRESFILE): $(PLUGINRCFILE)
        $(rc) $(PLUGINRCFLAGS) $(PLUGINRCFILE)

sub.obj: sub.c sub85.c config85.c direct.c

tsub.obj: sub.c sub85.c config85.c direct.c
        $(cc) $(CLFLAGS) /Fo$@ -DACU_ALWAYS_INIT sub.c

clean:
        -del $(WRUN32WISP).dll
        -del $(WRUN32WISP).exe
        -del acuthread.exe 
        -del NPacu32.dll
        -del sub.obj 
        -del filetbl.obj 
        -del mswinsub.obj
