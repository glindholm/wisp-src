# run32wisp_acu52.mak
#
# THIS FILE HAS BEEN MODIFIED TO ADD THE WISP ROUTINES TO THE 
# ACUCOBOL ALTERNATE TERMINAL MANAGER (ATM) RUNTIME.
#
#################################################################
#
# Creating an Acucobol 5.2.1 ATM runtime requires WISP 5.1.00
#
# ACUDIR=C:\acucorp\acucbl520\acugt
#
# Follow these instructions carefully to build a custom Acucobol 
# ATM runtime that includes the WISP runtime routines.  You are going
# to build the runtime from a temporary folder, that is a copy
# of the Acucobol lib folder $(ACUDIR)\lib.
#
# 1) Create the temporary folder $(ACUDIR)\bldwisp by copying
#    and renaming the folder $(ACUDIR)\lib.  You can use
#    Windows Explorer to do this by opening $(ACUDIR) and
#    selecting the lib folder then doing a Copy then Paste command.
#
# 2) Copy the needed WISP files to the bldwisp folder. (You will be 
#    replacing the sub85.c file with the one supplied by WISP.)
#       Copy $(WISPDIR)\acu\run32wisp_acu52.mak 
#            $(WISPDIR)\acu\sub85.c
#
#       to   $(ACUDIR)\bldwisp
#
# 3) Edit this file and set WISPDIR to the correct directory.
#
#       WISPDIR=C:\WISPxxxx
#
# 4) From a COMMAND/MSDOS window issue the NMAKE command. 
#    (You may need to first run the VCVARS32.bat file that comes with MS 
#    Visual C++ in order to run NMAKE from a command prompt.)
#
#       $ cd $(ACUDIR)\bldwisp
#       $ "C:\Program Files\Microsoft Visual Studio\VC98\Bin\VCVARS32.BAT"
#       $ NMAKE /f run32wisp_acu52.mak
#
# 5) Copy and rename the runtime to the bin folder (with the dll's)
#       Copy $(ACUDIR)\bldwisp\run32wisp.exe
#       to   $(ACUDIR)\bin\run32wisp.exe
#
# 6) Copy and rename the Acucobol license file to match the new
#    runtime name.
#       Copy $(ACUDIR)\bin\run32.alc
#       to   $(ACUDIR)\bin\run32wisp.alc
#
#################################################################
#
# NOTES:
#
# 1) The Acucobol 5.2.1 distribution is missing the two libraries
#    ccvt32.lib and cconc32.lib.  These can be obtained from
#    Acucorp tech support.
#
# 2) The CFLAGS have been changed from /MD to /ML as the Acucobol
#    libraries appear to be single-threaded (non-DLL) instead of 
#    mult-threaded DLL.
#    Also the single-threaded WISP libraries are being used.
#
#################################################################
#
# run32.mak
# Makefile for recreating run32.exe from ACUCOBOL-GT libraries on 32-bit 
# Windows
# To relink the runtime type: nmake -f run32.mak
#   For AcuServer clients add : CLIENT=
# Make sure your path for wsock32.lib is set to the correct location
#
# Distributed with ACUCOBOL-GT version 5.2.1
# PMK: 0, 1
#################################################################

# Set the installed WISP directory here.
WISPDIR=C:\WISP5100

## WISP libraries
WISP_LIBS=      $(WISPDIR)\lib\wisp.lib  \
                $(WISPDIR)\lib\video.lib \
		gdi32.lib winspool.lib Comdlg32.lib

#################################################################

!include <ntwin32.mak>

!ifdef	DEBUG
DEBUG_CFLAGS=/MDd
DEBUG_LFLAGS=/debug:full /debugtype:cv
!else
DEBUG_CFLAGS=/ML
DEBUG_LFLAGS=
!endif

LDFLAGS=/nologo /nodefaultlib /subsystem:console /incremental:no $(DEBUG_LFLAGS) /machine:I386
RCFLAGS=

RESFILE=run32.res
RBJFILE=run32.rbj
RCFILE=run32.rc

EXTRA_CFLAGS=-DNO_CLIENT=1
CLIENT_LIBS=

!ifdef  CLIENT
# AcuServer client version
EXTRA_CFLAGS=-DNO_CLIENT=0 
CLIENT_LIBS=cclnt32.lib $(CLIENT_LIBS)
!endif  # CLIENT #

ACUCONNECT_C = cconc32.lib

SUBS=   \
	filetbl.obj \
	mswinsub.obj \
	sub.obj

LIBS=   crun32.lib \
	ccvt32.lib \
	term32.lib \
	cfsi32.lib \
	$(ACUCONNECT_C) \
	$(CLIENT_LIBS) \
	avision4.lib \
	acme.lib

## Added WISP CFLAGS
CLFLAGS=$(cflags) $(cvars) $(DEBUG_CFLAGS) -nologo -D_WINDOWS -DWINNT \
		-DACU_ALWAYS_INIT $(EXTRA_CFLAGS)

.c.obj:
	$(cc) $(CLFLAGS) $<

## Changed names and added WISP libraries
run32wisp.exe: $(SUBS) $(LIBS) $(RBJFILE)
	$(link) $(LDFLAGS) /out:$@ /implib:runexe.lib $(LIBS) $(SUBS) \
		$(RBJFILE) $(conlibs) $(WISP_LIBS)

$(RBJFILE): $(RESFILE)
	cvtres -$(CPU) $(RESFILE) -o $@

$(RESFILE): $(RCFILE)
	$(rc) $(RCFLAGS) $(RCFILE)

sub.obj: sub.c sub85.c config85.c direct.c

clean:
	-del run32wisp.exe
	-del sub.obj 
	-del filetbl.obj 
	-del mswinsub.obj
	-del runexe.lib
	-del $(RBJFILE)
	-del $(RESFILE)
