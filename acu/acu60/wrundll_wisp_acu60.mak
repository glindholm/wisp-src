# Microsoft Developer Studio Generated NMAKE File, Based on wrundll.dsp
!IF "$(CFG)" == ""
CFG=wrundll - Win32 Release
!MESSAGE No configuration specified. Defaulting to wrundll - Win32 Release.
!ENDIF 

!IF "$(CFG)" != "wrundll - Win32 Release"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "wrundll.mak" CFG="wrundll - Win32 Release"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "wrundll - Win32 Release" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 

CPP=cl.exe
MTL=midl.exe
RSC=rc.exe
OUTDIR=.
INTDIR=.
# Begin Custom Macros
OutDir=.
# End Custom Macros

ALL : "$(OUTDIR)\wrun32.dll"


CLEAN :
	-@erase "$(INTDIR)\filetbl.obj"
	-@erase "$(INTDIR)\mswinsub.obj"
	-@erase "$(INTDIR)\sub.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\wrundll.res"
	-@erase "$(OUTDIR)\wrun32.dll"
	-@erase "$(OUTDIR)\wrun32.exp"
	-@erase "$(OUTDIR)\wrun32.lib"

BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\wrundll.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /incremental:no /pdb:"$(OUTDIR)\wrun32.pdb" /machine:I386 /def:".\wrundll.def" /out:"$(OUTDIR)\wrun32.dll" /implib:"$(OUTDIR)\wrun32.lib" 
DEF_FILE= \
	".\wrundll.def"
LINK32_OBJS= \
	"$(INTDIR)\filetbl.obj" \
	"$(INTDIR)\mswinsub.obj" \
	"$(INTDIR)\sub.obj" \
	"$(INTDIR)\wrundll.res" \
	".\zlib.lib" \
	".\atermmgr.lib" \
	".\avision5.lib" \
	".\wcvt32.lib" \
	".\wfsi32.lib" \
	".\acme.lib" \
	".\wrunlib.lib" \
	".\wisp.lib" \
	".\video.lib"

"$(OUTDIR)\wrun32.dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

CPP_PROJ=/nologo /MD /W3 /Ox /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "_WINDLL" /FD /c 

.c.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.c.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

MTL_PROJ=/nologo /D "NDEBUG" /mktyplib203 /win32 
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\wrundll.res" /d "NDEBUG" 

!IF "$(NO_EXTERNAL_DEPS)" != "1"
!IF EXISTS("wrundll.dep")
!INCLUDE "wrundll.dep"
!ELSE 
!MESSAGE Warning: cannot find "wrundll.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "wrundll - Win32 Release"
SOURCE=.\config85.c
SOURCE=.\direct.c
SOURCE=.\filetbl.c

"$(INTDIR)\filetbl.obj" : $(SOURCE) "$(INTDIR)"


SOURCE=.\mswinsub.c

"$(INTDIR)\mswinsub.obj" : $(SOURCE) "$(INTDIR)"


SOURCE=.\sub.c

"$(INTDIR)\sub.obj" : $(SOURCE) "$(INTDIR)"


SOURCE=.\sub85.c
SOURCE=.\wrundll.rc

"$(INTDIR)\wrundll.res" : $(SOURCE) "$(INTDIR)"
	$(RSC) $(RSC_PROJ) $(SOURCE)



!ENDIF 

