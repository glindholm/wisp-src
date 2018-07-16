# Microsoft Developer Studio Generated NMAKE File, Based on run32_wisp_acu60.dsp
!IF "$(CFG)" == ""
CFG=run32 - Win32 Release
!MESSAGE No configuration specified. Defaulting to run32 - Win32 Release.
!ENDIF 

!IF "$(CFG)" != "run32 - Win32 Release"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "run32_wisp_acu60.mak" CFG="run32 - Win32 Release"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "run32 - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 

OUTDIR=.\run32___Win32_Release
INTDIR=.\run32___Win32_Release
# Begin Custom Macros
OutDir=.\run32___Win32_Release
# End Custom Macros

ALL : ".\run32_wisp.exe" "$(OUTDIR)\run32_wisp_acu60.bsc"


CLEAN :
	-@erase "$(INTDIR)\filetbl.obj"
	-@erase "$(INTDIR)\filetbl.sbr"
	-@erase "$(INTDIR)\mswinsub.obj"
	-@erase "$(INTDIR)\mswinsub.sbr"
	-@erase "$(INTDIR)\run32.res"
	-@erase "$(INTDIR)\sub.obj"
	-@erase "$(INTDIR)\sub.sbr"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(OUTDIR)\run32_wisp_acu60.bsc"
	-@erase ".\run32_wisp.exe"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MD /W3 /GX /Zi /Ox /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /D "_WINDOWS" /FR"$(INTDIR)\\" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

.c{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.c{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

RSC=rc.exe
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\run32.res" /d "NDEBUG" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\run32_wisp_acu60.bsc" 
BSC32_SBRS= \
	"$(INTDIR)\filetbl.sbr" \
	"$(INTDIR)\mswinsub.sbr" \
	"$(INTDIR)\sub.sbr"

"$(OUTDIR)\run32_wisp_acu60.bsc" : "$(OUTDIR)" $(BSC32_SBRS)
    $(BSC32) @<<
  $(BSC32_FLAGS) $(BSC32_SBRS)
<<

LINK32=link.exe
LINK32_FLAGS=setargv advapi32.lib user32.lib gdi32.lib winspool.lib Comdlg32.lib /nologo /subsystem:console /incremental:no /pdb:"$(OUTDIR)\run32_wisp.pdb" /machine:I386 /out:"run32_wisp.exe" 
LINK32_OBJS= \
	"$(INTDIR)\filetbl.obj" \
	"$(INTDIR)\mswinsub.obj" \
	"$(INTDIR)\sub.obj" \
	"$(INTDIR)\run32.res" \
	".\wfsi32.lib" \
	".\avision5.lib" \
	".\wcvt32.lib" \
	".\acme.lib" \
	".\crunlib.lib" \
	".\term32.lib" \
	".\video.lib" \
	".\wisp.lib"

".\run32_wisp.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<


!IF "$(NO_EXTERNAL_DEPS)" != "1"
!IF EXISTS("run32_wisp_acu60.dep")
!INCLUDE "run32_wisp_acu60.dep"
!ELSE 
!MESSAGE Warning: cannot find "run32_wisp_acu60.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "run32 - Win32 Release"
SOURCE=.\config85.c
SOURCE=.\direct.c
SOURCE=.\filetbl.c

"$(INTDIR)\filetbl.obj"	"$(INTDIR)\filetbl.sbr" : $(SOURCE) "$(INTDIR)"


SOURCE=.\mswinsub.c

"$(INTDIR)\mswinsub.obj"	"$(INTDIR)\mswinsub.sbr" : $(SOURCE) "$(INTDIR)"


SOURCE=.\run32.rc

"$(INTDIR)\run32.res" : $(SOURCE) "$(INTDIR)"
	$(RSC) $(RSC_PROJ) $(SOURCE)


SOURCE=.\sub.c

"$(INTDIR)\sub.obj"	"$(INTDIR)\sub.sbr" : $(SOURCE) "$(INTDIR)"


SOURCE=.\sub85.c

!ENDIF 

