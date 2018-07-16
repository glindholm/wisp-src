# wrun32.mak
# Makefile for recreating wrun32.dll from ACUCOBOL-GT libraries on 32-bit 
# Windows
# To relink the runtime type: nmake -f wrun32.mak
#   For AcuServer clients add : CLIENT=
# Acuthread.exe is the runtime used by the AcuConnect server
# To relink acuthread.exe type: nmake -f wrun32.mak acuthread.exe
#   For AcuServer clients add : CLIENT=
# Make sure your path for wsock32.lib is set to the correct location

# Distributed with ACUCOBOL-GT version 5.0.0
# PMK: 0, 1

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

RESFILE=wruncbl.res
RBJFILE=wruncbl.rbj
RCFILE=wruncbl.rc

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
WLIBS=wterm32.lib $(LIBS)

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


CLFLAGS=$(cflags) $(cvars) $(DEBUG_CFLAGS) -nologo -D_WINDOWS -DWINNT \
		$(EXTRA_CFLAGS)

.c.obj:
	$(cc) $(CLFLAGS) $<

wrun32.dll: $(MFCDLL) $(WSUBS) $(DLLRBJFILE) $(WLIBS) $(ACUCONNECT_C)
	$(link) $(LDFLAGS) /dll /out:$@ /def:wrundll.def \
		/implib:wrundll.lib \
		$(MFCDLL) $(WLIBS) $(WSUBS) $(ACUCONNECT_C) $(DLLRBJFILE)


wrun32.exe: $(MFCAPP) wrun32.dll wcpp32.lib $(RBJFILE)
	$(link) $(LDFLAGS) /out:$@ $(MFCAPP) wcpp32.lib \
		wrundll.lib $(RBJFILE)

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

NPacu32.dll: $(MFCNPDLL) $(PLUGINSUBS) $(PLUGINLIBS) wrun32.dll \
	$(PLUGINRBJFILE)
	$(link) $(LDFLAGS) /dll /out:$@ /def:plugin32.def \
		/implib:NPacudll.lib \
		$(MFCNPDLL) $(PLUGINSUBS) $(PLUGINLIBS) wrundll.lib \
		$(PLUGINRBJFILE)

$(PLUGINRBJFILE): $(PLUGINRESFILE)
	cvtres -$(CPU) $(PLUGINRESFILE) -o $@

$(PLUGINRESFILE): $(PLUGINRCFILE)
	$(rc) $(PLUGINRCFLAGS) $(PLUGINRCFILE)

sub.obj: sub.c sub85.c config85.c direct.c

tsub.obj: sub.c sub85.c config85.c direct.c
	$(cc) $(CLFLAGS) /Fo$@ -DACU_ALWAYS_INIT sub.c

clean:
	-del wrun32.dll
	-del wrun32.exe
	-del acuthread.exe 
	-del NPacu32.dll
	-del sub.obj 
	-del filetbl.obj 
	-del mswinsub.obj
	-del $(RESFILE)
	-del $(RBJFILE)
	-del $(DLLRESFILE)
	-del $(DLLRBJFILE)
	-del wrundll.lib
	-del wrundll.exp
	-del $(PLUGINRESFILE)
	-del $(PLUGINRBJFILE)
	-del NPacudll.lib
	-del NPacudll.exp
