# run32.mak
# Makefile for recreating run32.exe from ACUCOBOL-GT libraries on 32-bit 
# Windows
# To relink the runtime type: nmake -f run32.mak
#   For AcuServer clients add : CLIENT=
# Make sure your path for wsock32.lib is set to the correct location

# Distributed with ACUCOBOL-GT version 5.2.1
# PMK: 0, 1

!include <ntwin32.mak>

!ifdef	DEBUG
DEBUG_CFLAGS=/MDd
DEBUG_LFLAGS=/debug:full /debugtype:cv
!else
DEBUG_CFLAGS=/MD
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

CLFLAGS=$(cflags) $(cvars) $(DEBUG_CFLAGS) -nologo -D_WINDOWS -DWINNT \
		-DACU_ALWAYS_INIT $(EXTRA_CFLAGS)

.c.obj:
	$(cc) $(CLFLAGS) $<

run32.exe: $(SUBS) $(LIBS) $(RBJFILE)
	$(link) $(LDFLAGS) /out:$@ /implib:runexe.lib $(LIBS) $(SUBS) \
		$(RBJFILE) $(conlibs)

$(RBJFILE): $(RESFILE)
	cvtres -$(CPU) $(RESFILE) -o $@

$(RESFILE): $(RCFILE)
	$(rc) $(RCFLAGS) $(RCFILE)

sub.obj: sub.c sub85.c config85.c direct.c

clean:
	-del run32.exe
	-del sub.obj 
	-del filetbl.obj 
	-del mswinsub.obj
	-del runexe.lib
	-del $(RBJFILE)
	-del $(RESFILE)
