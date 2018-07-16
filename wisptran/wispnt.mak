# Microsoft Developer Studio Generated NMAKE File, Format Version 4.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

!IF "$(CFG)" == ""
CFG=wisp - Win32 Debug
!MESSAGE No configuration specified.  Defaulting to wisp - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "wisp - Win32 Release" && "$(CFG)" != "wisp - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
!MESSAGE 
!MESSAGE NMAKE /f "wisp.mak" CFG="wisp - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "wisp - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "wisp - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 
################################################################################
# Begin Project
# PROP Target_Last_Scanned "wisp - Win32 Debug"
RSC=rc.exe
CPP=cl.exe

!IF  "$(CFG)" == "wisp - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Target_Dir ""
OUTDIR=.\Release
INTDIR=.\Release

ALL : "$(OUTDIR)\wisp.exe"

CLEAN : 
	-@erase ".\Release\wisp.exe"
	-@erase ".\Release\wt_start.obj"
	-@erase ".\Release\wisp.obj"
	-@erase ".\Release\getopt.obj"
	-@erase ".\Release\tokenize.obj"
	-@erase ".\Release\wt_ident.obj"
	-@erase ".\Release\reduce.obj"
	-@erase ".\Release\wt_datad.obj"
	-@erase ".\Release\wt_wsdiv.obj"
	-@erase ".\Release\node.obj"
	-@erase ".\Release\stats.obj"
	-@erase ".\Release\wt_acept.obj"
	-@erase ".\Release\wt_debug.obj"
	-@erase ".\Release\wt_call.obj"
	-@erase ".\Release\wt_if.obj"
	-@erase ".\Release\wt_io.obj"
	-@erase ".\Release\input.obj"
	-@erase ".\Release\wt_read.obj"
	-@erase ".\Release\wt_input.obj"
	-@erase ".\Release\keywords.obj"
	-@erase ".\Release\wt_crtrw.obj"
	-@erase ".\Release\wt_procd.obj"
	-@erase ".\Release\wt_disp.obj"
	-@erase ".\Release\untabstr.obj"
	-@erase ".\Release\wt_write.obj"
	-@erase ".\Release\wt_scrn.obj"
	-@erase ".\Release\dataconv.obj"
	-@erase ".\Release\wt_sort.obj"
	-@erase ".\Release\wt_free.obj"
	-@erase ".\Release\wt_cli.obj"
	-@erase ".\Release\wt_files.obj"
	-@erase ".\Release\ring.obj"
	-@erase ".\Release\output.obj"
	-@erase ".\Release\wt_utils.obj"
	-@erase ".\Release\wt_decl.obj"
	-@erase ".\Release\wt_delet.obj"
	-@erase ".\Release\wt_opcls.obj"
	-@erase ".\Release\wisp_pic.obj"
	-@erase ".\Release\wt_wsdat.obj"
	-@erase ".\Release\wt_locks.obj"
	-@erase ".\Release\wmalloc.obj"
	-@erase ".\Release\wt_divs.obj"
	-@erase ".\Release\statment.obj"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /YX /c
# ADD CPP /nologo /W3 /GX /O2 /D "NDEBUG" /D "WIN32" /D "_CONSOLE" /D " WINNT" /D " MSFS" /YX /c
CPP_PROJ=/nologo /ML /W3 /GX /O2 /D "NDEBUG" /D "WIN32" /D "_CONSOLE" /D\
 " WINNT" /D " MSFS" /Fp"$(INTDIR)/wisp.pch" /YX /Fo"$(INTDIR)/" /c 
CPP_OBJS=.\Release/
CPP_SBRS=
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/wisp.bsc" 
BSC32_SBRS=
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib\
 odbccp32.lib /nologo /subsystem:console /incremental:no\
 /pdb:"$(OUTDIR)/wisp.pdb" /machine:I386 /out:"$(OUTDIR)/wisp.exe" 
LINK32_OBJS= \
	"$(INTDIR)/wt_start.obj" \
	"$(INTDIR)/wisp.obj" \
	"$(INTDIR)/getopt.obj" \
	"$(INTDIR)/tokenize.obj" \
	"$(INTDIR)/wt_ident.obj" \
	"$(INTDIR)/reduce.obj" \
	"$(INTDIR)/wt_datad.obj" \
	"$(INTDIR)/wt_wsdiv.obj" \
	"$(INTDIR)/node.obj" \
	"$(INTDIR)/stats.obj" \
	"$(INTDIR)/wt_acept.obj" \
	"$(INTDIR)/wt_debug.obj" \
	"$(INTDIR)/wt_call.obj" \
	"$(INTDIR)/wt_if.obj" \
	"$(INTDIR)/wt_io.obj" \
	"$(INTDIR)/input.obj" \
	"$(INTDIR)/wt_read.obj" \
	"$(INTDIR)/wt_input.obj" \
	"$(INTDIR)/keywords.obj" \
	"$(INTDIR)/wt_crtrw.obj" \
	"$(INTDIR)/wt_procd.obj" \
	"$(INTDIR)/wt_disp.obj" \
	"$(INTDIR)/untabstr.obj" \
	"$(INTDIR)/wt_write.obj" \
	"$(INTDIR)/wt_scrn.obj" \
	"$(INTDIR)/dataconv.obj" \
	"$(INTDIR)/wt_sort.obj" \
	"$(INTDIR)/wt_free.obj" \
	"$(INTDIR)/wt_cli.obj" \
	"$(INTDIR)/wt_files.obj" \
	"$(INTDIR)/ring.obj" \
	"$(INTDIR)/output.obj" \
	"$(INTDIR)/wt_utils.obj" \
	"$(INTDIR)/wt_decl.obj" \
	"$(INTDIR)/wt_delet.obj" \
	"$(INTDIR)/wt_opcls.obj" \
	"$(INTDIR)/wisp_pic.obj" \
	"$(INTDIR)/wt_wsdat.obj" \
	"$(INTDIR)/wt_locks.obj" \
	"$(INTDIR)/wmalloc.obj" \
	"$(INTDIR)/wt_divs.obj" \
	"$(INTDIR)/statment.obj"

"$(OUTDIR)\wisp.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Target_Dir ""
OUTDIR=.\Debug
INTDIR=.\Debug

ALL : "$(OUTDIR)\wisp.exe" "$(OUTDIR)\wisp.bsc"

CLEAN : 
	-@erase ".\Debug\vc40.pdb"
	-@erase ".\Debug\vc40.idb"
	-@erase ".\Debug\wisp.bsc"
	-@erase ".\Debug\statment.sbr"
	-@erase ".\Debug\wt_start.sbr"
	-@erase ".\Debug\wt_write.sbr"
	-@erase ".\Debug\node.sbr"
	-@erase ".\Debug\tokenize.sbr"
	-@erase ".\Debug\reduce.sbr"
	-@erase ".\Debug\wt_ident.sbr"
	-@erase ".\Debug\wt_cli.sbr"
	-@erase ".\Debug\wt_datad.sbr"
	-@erase ".\Debug\wt_wsdiv.sbr"
	-@erase ".\Debug\wt_call.sbr"
	-@erase ".\Debug\output.sbr"
	-@erase ".\Debug\wt_debug.sbr"
	-@erase ".\Debug\wt_if.sbr"
	-@erase ".\Debug\wt_read.sbr"
	-@erase ".\Debug\wt_io.sbr"
	-@erase ".\Debug\input.sbr"
	-@erase ".\Debug\wisp.sbr"
	-@erase ".\Debug\wt_input.sbr"
	-@erase ".\Debug\wt_disp.sbr"
	-@erase ".\Debug\keywords.sbr"
	-@erase ".\Debug\wt_scrn.sbr"
	-@erase ".\Debug\wt_crtrw.sbr"
	-@erase ".\Debug\wt_procd.sbr"
	-@erase ".\Debug\wisp_pic.sbr"
	-@erase ".\Debug\untabstr.sbr"
	-@erase ".\Debug\wt_sort.sbr"
	-@erase ".\Debug\wt_free.sbr"
	-@erase ".\Debug\ring.sbr"
	-@erase ".\Debug\dataconv.sbr"
	-@erase ".\Debug\getopt.sbr"
	-@erase ".\Debug\stats.sbr"
	-@erase ".\Debug\wt_files.sbr"
	-@erase ".\Debug\wt_decl.sbr"
	-@erase ".\Debug\wt_acept.sbr"
	-@erase ".\Debug\wt_utils.sbr"
	-@erase ".\Debug\wt_delet.sbr"
	-@erase ".\Debug\wt_opcls.sbr"
	-@erase ".\Debug\wmalloc.sbr"
	-@erase ".\Debug\wt_wsdat.sbr"
	-@erase ".\Debug\wt_divs.sbr"
	-@erase ".\Debug\wt_locks.sbr"
	-@erase ".\Debug\wisp.exe"
	-@erase ".\Debug\wt_acept.obj"
	-@erase ".\Debug\wt_utils.obj"
	-@erase ".\Debug\wt_delet.obj"
	-@erase ".\Debug\wt_opcls.obj"
	-@erase ".\Debug\wmalloc.obj"
	-@erase ".\Debug\wt_wsdat.obj"
	-@erase ".\Debug\wt_divs.obj"
	-@erase ".\Debug\wt_locks.obj"
	-@erase ".\Debug\statment.obj"
	-@erase ".\Debug\wt_start.obj"
	-@erase ".\Debug\wt_write.obj"
	-@erase ".\Debug\node.obj"
	-@erase ".\Debug\tokenize.obj"
	-@erase ".\Debug\reduce.obj"
	-@erase ".\Debug\wt_ident.obj"
	-@erase ".\Debug\wt_cli.obj"
	-@erase ".\Debug\wt_datad.obj"
	-@erase ".\Debug\wt_wsdiv.obj"
	-@erase ".\Debug\wt_call.obj"
	-@erase ".\Debug\output.obj"
	-@erase ".\Debug\wt_debug.obj"
	-@erase ".\Debug\wt_if.obj"
	-@erase ".\Debug\wt_read.obj"
	-@erase ".\Debug\wt_io.obj"
	-@erase ".\Debug\input.obj"
	-@erase ".\Debug\wisp.obj"
	-@erase ".\Debug\wt_input.obj"
	-@erase ".\Debug\wt_disp.obj"
	-@erase ".\Debug\keywords.obj"
	-@erase ".\Debug\wt_scrn.obj"
	-@erase ".\Debug\wt_crtrw.obj"
	-@erase ".\Debug\wt_procd.obj"
	-@erase ".\Debug\wisp_pic.obj"
	-@erase ".\Debug\untabstr.obj"
	-@erase ".\Debug\wt_sort.obj"
	-@erase ".\Debug\wt_free.obj"
	-@erase ".\Debug\ring.obj"
	-@erase ".\Debug\dataconv.obj"
	-@erase ".\Debug\getopt.obj"
	-@erase ".\Debug\stats.obj"
	-@erase ".\Debug\wt_files.obj"
	-@erase ".\Debug\wt_decl.obj"
	-@erase ".\Debug\wisp.ilk"
	-@erase ".\Debug\wisp.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /YX /c
# ADD CPP /nologo /W3 /Gm /GX /Zi /Od /D "_DEBUG" /D "WIN32" /D "_CONSOLE" /D " WINNT" /D " MSFS" /FR /YX /c
CPP_PROJ=/nologo /MLd /W3 /Gm /GX /Zi /Od /D "_DEBUG" /D "WIN32" /D "_CONSOLE"\
 /D " WINNT" /D " MSFS" /FR"$(INTDIR)/" /Fp"$(INTDIR)/wisp.pch" /YX\
 /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c 
CPP_OBJS=.\Debug/
CPP_SBRS=.\Debug/
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/wisp.bsc" 
BSC32_SBRS= \
	"$(INTDIR)/statment.sbr" \
	"$(INTDIR)/wt_start.sbr" \
	"$(INTDIR)/wt_write.sbr" \
	"$(INTDIR)/node.sbr" \
	"$(INTDIR)/tokenize.sbr" \
	"$(INTDIR)/reduce.sbr" \
	"$(INTDIR)/wt_ident.sbr" \
	"$(INTDIR)/wt_cli.sbr" \
	"$(INTDIR)/wt_datad.sbr" \
	"$(INTDIR)/wt_wsdiv.sbr" \
	"$(INTDIR)/wt_call.sbr" \
	"$(INTDIR)/output.sbr" \
	"$(INTDIR)/wt_debug.sbr" \
	"$(INTDIR)/wt_if.sbr" \
	"$(INTDIR)/wt_read.sbr" \
	"$(INTDIR)/wt_io.sbr" \
	"$(INTDIR)/input.sbr" \
	"$(INTDIR)/wisp.sbr" \
	"$(INTDIR)/wt_input.sbr" \
	"$(INTDIR)/wt_disp.sbr" \
	"$(INTDIR)/keywords.sbr" \
	"$(INTDIR)/wt_scrn.sbr" \
	"$(INTDIR)/wt_crtrw.sbr" \
	"$(INTDIR)/wt_procd.sbr" \
	"$(INTDIR)/wisp_pic.sbr" \
	"$(INTDIR)/untabstr.sbr" \
	"$(INTDIR)/wt_sort.sbr" \
	"$(INTDIR)/wt_free.sbr" \
	"$(INTDIR)/ring.sbr" \
	"$(INTDIR)/dataconv.sbr" \
	"$(INTDIR)/getopt.sbr" \
	"$(INTDIR)/stats.sbr" \
	"$(INTDIR)/wt_files.sbr" \
	"$(INTDIR)/wt_decl.sbr" \
	"$(INTDIR)/wt_acept.sbr" \
	"$(INTDIR)/wt_utils.sbr" \
	"$(INTDIR)/wt_delet.sbr" \
	"$(INTDIR)/wt_opcls.sbr" \
	"$(INTDIR)/wmalloc.sbr" \
	"$(INTDIR)/wt_wsdat.sbr" \
	"$(INTDIR)/wt_divs.sbr" \
	"$(INTDIR)/wt_locks.sbr"

"$(OUTDIR)\wisp.bsc" : "$(OUTDIR)" $(BSC32_SBRS)
    $(BSC32) @<<
  $(BSC32_FLAGS) $(BSC32_SBRS)
<<

LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib\
 odbccp32.lib /nologo /subsystem:console /incremental:yes\
 /pdb:"$(OUTDIR)/wisp.pdb" /debug /machine:I386 /out:"$(OUTDIR)/wisp.exe" 
LINK32_OBJS= \
	"$(INTDIR)/wt_acept.obj" \
	"$(INTDIR)/wt_utils.obj" \
	"$(INTDIR)/wt_delet.obj" \
	"$(INTDIR)/wt_opcls.obj" \
	"$(INTDIR)/wmalloc.obj" \
	"$(INTDIR)/wt_wsdat.obj" \
	"$(INTDIR)/wt_divs.obj" \
	"$(INTDIR)/wt_locks.obj" \
	"$(INTDIR)/statment.obj" \
	"$(INTDIR)/wt_start.obj" \
	"$(INTDIR)/wt_write.obj" \
	"$(INTDIR)/node.obj" \
	"$(INTDIR)/tokenize.obj" \
	"$(INTDIR)/reduce.obj" \
	"$(INTDIR)/wt_ident.obj" \
	"$(INTDIR)/wt_cli.obj" \
	"$(INTDIR)/wt_datad.obj" \
	"$(INTDIR)/wt_wsdiv.obj" \
	"$(INTDIR)/wt_call.obj" \
	"$(INTDIR)/output.obj" \
	"$(INTDIR)/wt_debug.obj" \
	"$(INTDIR)/wt_if.obj" \
	"$(INTDIR)/wt_read.obj" \
	"$(INTDIR)/wt_io.obj" \
	"$(INTDIR)/input.obj" \
	"$(INTDIR)/wisp.obj" \
	"$(INTDIR)/wt_input.obj" \
	"$(INTDIR)/wt_disp.obj" \
	"$(INTDIR)/keywords.obj" \
	"$(INTDIR)/wt_scrn.obj" \
	"$(INTDIR)/wt_crtrw.obj" \
	"$(INTDIR)/wt_procd.obj" \
	"$(INTDIR)/wisp_pic.obj" \
	"$(INTDIR)/untabstr.obj" \
	"$(INTDIR)/wt_sort.obj" \
	"$(INTDIR)/wt_free.obj" \
	"$(INTDIR)/ring.obj" \
	"$(INTDIR)/dataconv.obj" \
	"$(INTDIR)/getopt.obj" \
	"$(INTDIR)/stats.obj" \
	"$(INTDIR)/wt_files.obj" \
	"$(INTDIR)/wt_decl.obj"

"$(OUTDIR)\wisp.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ENDIF 

.c{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

.cpp{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

.cxx{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

.c{$(CPP_SBRS)}.sbr:
   $(CPP) $(CPP_PROJ) $<  

.cpp{$(CPP_SBRS)}.sbr:
   $(CPP) $(CPP_PROJ) $<  

.cxx{$(CPP_SBRS)}.sbr:
   $(CPP) $(CPP_PROJ) $<  

################################################################################
# Begin Target

# Name "wisp - Win32 Release"
# Name "wisp - Win32 Debug"

!IF  "$(CFG)" == "wisp - Win32 Release"

!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"

!ENDIF 

################################################################################
# Begin Source File

SOURCE=.\cobfiles.h

!IF  "$(CFG)" == "wisp - Win32 Release"

!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\crt.h

!IF  "$(CFG)" == "wisp - Win32 Release"

!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\dataconv.c

!IF  "$(CFG)" == "wisp - Win32 Release"

DEP_CPP_DATAC=\
	".\wisp.h"\
	".\cobfiles.h"\
	".\token.h"\
	".\node.h"\
	".\statment.h"\
	".\wt_datad.h"\
	".\output.h"\
	".\proto.h"\
	".\idsistd.h"\
	".\wispfile.h"\
	".\intdef.h"\
	".\tokenize.h"\
	

"$(INTDIR)\dataconv.obj" : $(SOURCE) $(DEP_CPP_DATAC) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"

DEP_CPP_DATAC=\
	".\wisp.h"\
	".\cobfiles.h"\
	".\token.h"\
	".\node.h"\
	".\statment.h"\
	".\wt_datad.h"\
	".\output.h"\
	".\proto.h"\
	".\keywords.h"\
	".\idsistd.h"\
	".\wispfile.h"\
	".\intdef.h"\
	".\tokenize.h"\
	

"$(INTDIR)\dataconv.obj" : $(SOURCE) $(DEP_CPP_DATAC) "$(INTDIR)"

"$(INTDIR)\dataconv.sbr" : $(SOURCE) $(DEP_CPP_DATAC) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\demovali.h

!IF  "$(CFG)" == "wisp - Win32 Release"

!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\directiv.h

!IF  "$(CFG)" == "wisp - Win32 Release"

!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\filext.h

!IF  "$(CFG)" == "wisp - Win32 Release"

!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\getopt.c
DEP_CPP_GETOP=\
	{$(INCLUDE)}"\sys\Types.h"\
	".\idsistd.h"\
	".\getopt.h"\
	".\intdef.h"\
	

!IF  "$(CFG)" == "wisp - Win32 Release"


"$(INTDIR)\getopt.obj" : $(SOURCE) $(DEP_CPP_GETOP) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"


"$(INTDIR)\getopt.obj" : $(SOURCE) $(DEP_CPP_GETOP) "$(INTDIR)"

"$(INTDIR)\getopt.sbr" : $(SOURCE) $(DEP_CPP_GETOP) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\idsistd.h

!IF  "$(CFG)" == "wisp - Win32 Release"

!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\idsisubs.h

!IF  "$(CFG)" == "wisp - Win32 Release"

!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\input.c
DEP_CPP_INPUT=\
	".\wisp.h"\
	".\directiv.h"\
	".\wispfile.h"\
	".\token.h"\
	".\node.h"\
	".\lines.h"\
	".\wmalloc.h"\
	".\input.h"\
	".\proto.h"\
	".\output.h"\
	".\keywords.h"\
	".\idsistd.h"\
	".\intdef.h"\
	".\tokenize.h"\
	

!IF  "$(CFG)" == "wisp - Win32 Release"


"$(INTDIR)\input.obj" : $(SOURCE) $(DEP_CPP_INPUT) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"


"$(INTDIR)\input.obj" : $(SOURCE) $(DEP_CPP_INPUT) "$(INTDIR)"

"$(INTDIR)\input.sbr" : $(SOURCE) $(DEP_CPP_INPUT) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\input.h

!IF  "$(CFG)" == "wisp - Win32 Release"

!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\intdef.h

!IF  "$(CFG)" == "wisp - Win32 Release"

!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\keylist.h

!IF  "$(CFG)" == "wisp - Win32 Release"

!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\keywords.c

!IF  "$(CFG)" == "wisp - Win32 Release"

DEP_CPP_KEYWO=\
	".\wmalloc.h"\
	".\keywords.h"\
	".\proto.h"\
	".\idsistd.h"\
	".\token.h"\
	".\node.h"\
	".\wispfile.h"\
	".\intdef.h"\
	".\tokenize.h"\
	
NODEP_CPP_KEYWO=\
	".\strcmpptr"\
	

"$(INTDIR)\keywords.obj" : $(SOURCE) $(DEP_CPP_KEYWO) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"

DEP_CPP_KEYWO=\
	".\wmalloc.h"\
	".\keywords.h"\
	".\proto.h"\
	".\idsistd.h"\
	".\token.h"\
	".\node.h"\
	".\wispfile.h"\
	".\intdef.h"\
	".\tokenize.h"\
	

"$(INTDIR)\keywords.obj" : $(SOURCE) $(DEP_CPP_KEYWO) "$(INTDIR)"

"$(INTDIR)\keywords.sbr" : $(SOURCE) $(DEP_CPP_KEYWO) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\keywords.h

!IF  "$(CFG)" == "wisp - Win32 Release"

!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\level.h

!IF  "$(CFG)" == "wisp - Win32 Release"

!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\lines.h

!IF  "$(CFG)" == "wisp - Win32 Release"

!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\link.h

!IF  "$(CFG)" == "wisp - Win32 Release"

!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\node.c
DEP_CPP_NODE_=\
	".\wmalloc.h"\
	".\token.h"\
	".\node.h"\
	".\proto.h"\
	".\tokenize.h"\
	".\idsistd.h"\
	".\wispfile.h"\
	".\intdef.h"\
	

!IF  "$(CFG)" == "wisp - Win32 Release"


"$(INTDIR)\node.obj" : $(SOURCE) $(DEP_CPP_NODE_) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"


"$(INTDIR)\node.obj" : $(SOURCE) $(DEP_CPP_NODE_) "$(INTDIR)"

"$(INTDIR)\node.sbr" : $(SOURCE) $(DEP_CPP_NODE_) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\node.h

!IF  "$(CFG)" == "wisp - Win32 Release"

!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\output.c

!IF  "$(CFG)" == "wisp - Win32 Release"

DEP_CPP_OUTPU=\
	".\wisp.h"\
	".\wispfile.h"\
	".\token.h"\
	".\node.h"\
	".\output.h"\
	".\statment.h"\
	".\tokenize.h"\
	".\proto.h"\
	".\idsistd.h"\
	".\intdef.h"\
	
NODEP_CPP_OUTPU=\
	".\increment_out_line_count"\
	

"$(INTDIR)\output.obj" : $(SOURCE) $(DEP_CPP_OUTPU) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"

DEP_CPP_OUTPU=\
	".\wisp.h"\
	".\wispfile.h"\
	".\token.h"\
	".\node.h"\
	".\output.h"\
	".\statment.h"\
	".\tokenize.h"\
	".\proto.h"\
	".\keywords.h"\
	".\idsistd.h"\
	".\intdef.h"\
	

"$(INTDIR)\output.obj" : $(SOURCE) $(DEP_CPP_OUTPU) "$(INTDIR)"

"$(INTDIR)\output.sbr" : $(SOURCE) $(DEP_CPP_OUTPU) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\output.h

!IF  "$(CFG)" == "wisp - Win32 Release"

!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\paths.h

!IF  "$(CFG)" == "wisp - Win32 Release"

!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\prompt.h

!IF  "$(CFG)" == "wisp - Win32 Release"

!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\proto.h

!IF  "$(CFG)" == "wisp - Win32 Release"

!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\putparm.h

!IF  "$(CFG)" == "wisp - Win32 Release"

!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\reduce.c
DEP_CPP_REDUC=\
	".\token.h"\
	".\node.h"\
	".\reduce.h"\
	".\tokenize.h"\
	

!IF  "$(CFG)" == "wisp - Win32 Release"


"$(INTDIR)\reduce.obj" : $(SOURCE) $(DEP_CPP_REDUC) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"


"$(INTDIR)\reduce.obj" : $(SOURCE) $(DEP_CPP_REDUC) "$(INTDIR)"

"$(INTDIR)\reduce.sbr" : $(SOURCE) $(DEP_CPP_REDUC) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\reduce.h

!IF  "$(CFG)" == "wisp - Win32 Release"

!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\ring.c
DEP_CPP_RING_=\
	".\ring.h"\
	

!IF  "$(CFG)" == "wisp - Win32 Release"


"$(INTDIR)\ring.obj" : $(SOURCE) $(DEP_CPP_RING_) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"


"$(INTDIR)\ring.obj" : $(SOURCE) $(DEP_CPP_RING_) "$(INTDIR)"

"$(INTDIR)\ring.sbr" : $(SOURCE) $(DEP_CPP_RING_) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\ring.h

!IF  "$(CFG)" == "wisp - Win32 Release"

!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\scnfacs.h

!IF  "$(CFG)" == "wisp - Win32 Release"

!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\scrn.h

!IF  "$(CFG)" == "wisp - Win32 Release"

!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\sharemem.h

!IF  "$(CFG)" == "wisp - Win32 Release"

!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\sortseqf.h

!IF  "$(CFG)" == "wisp - Win32 Release"

!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\statment.c
DEP_CPP_STATM=\
	".\token.h"\
	".\node.h"\
	".\lines.h"\
	".\statment.h"\
	".\tokenize.h"\
	".\proto.h"\
	".\ring.h"\
	".\idsistd.h"\
	".\wispfile.h"\
	".\intdef.h"\
	

!IF  "$(CFG)" == "wisp - Win32 Release"


"$(INTDIR)\statment.obj" : $(SOURCE) $(DEP_CPP_STATM) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"


"$(INTDIR)\statment.obj" : $(SOURCE) $(DEP_CPP_STATM) "$(INTDIR)"

"$(INTDIR)\statment.sbr" : $(SOURCE) $(DEP_CPP_STATM) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\statment.h

!IF  "$(CFG)" == "wisp - Win32 Release"

!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\stats.c

!IF  "$(CFG)" == "wisp - Win32 Release"


"$(INTDIR)\stats.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"


"$(INTDIR)\stats.obj" : $(SOURCE) "$(INTDIR)"

"$(INTDIR)\stats.sbr" : $(SOURCE) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\sysdev.h

!IF  "$(CFG)" == "wisp - Win32 Release"

!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\token.h

!IF  "$(CFG)" == "wisp - Win32 Release"

!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\tokenize.c
DEP_CPP_TOKEN=\
	".\wmalloc.h"\
	".\token.h"\
	".\lines.h"\
	".\tokenize.h"\
	".\ring.h"\
	".\input.h"\
	".\output.h"\
	".\keywords.h"\
	".\proto.h"\
	".\wispfile.h"\
	".\node.h"\
	".\idsistd.h"\
	".\intdef.h"\
	

!IF  "$(CFG)" == "wisp - Win32 Release"


"$(INTDIR)\tokenize.obj" : $(SOURCE) $(DEP_CPP_TOKEN) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"


"$(INTDIR)\tokenize.obj" : $(SOURCE) $(DEP_CPP_TOKEN) "$(INTDIR)"

"$(INTDIR)\tokenize.sbr" : $(SOURCE) $(DEP_CPP_TOKEN) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\tokenize.h

!IF  "$(CFG)" == "wisp - Win32 Release"

!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\untabstr.c
DEP_CPP_UNTAB=\
	".\wmalloc.h"\
	

!IF  "$(CFG)" == "wisp - Win32 Release"


"$(INTDIR)\untabstr.obj" : $(SOURCE) $(DEP_CPP_UNTAB) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"


"$(INTDIR)\untabstr.obj" : $(SOURCE) $(DEP_CPP_UNTAB) "$(INTDIR)"

"$(INTDIR)\untabstr.sbr" : $(SOURCE) $(DEP_CPP_UNTAB) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\vssort.h

!IF  "$(CFG)" == "wisp - Win32 Release"

!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\vwang.h

!IF  "$(CFG)" == "wisp - Win32 Release"

!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\wangkeys.h

!IF  "$(CFG)" == "wisp - Win32 Release"

!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\wanguid.h

!IF  "$(CFG)" == "wisp - Win32 Release"

!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\wcommon.h

!IF  "$(CFG)" == "wisp - Win32 Release"

!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\wdefines.h

!IF  "$(CFG)" == "wisp - Win32 Release"

!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\werrlog.h

!IF  "$(CFG)" == "wisp - Win32 Release"

!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\wexit.h

!IF  "$(CFG)" == "wisp - Win32 Release"

!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\wfiles.h

!IF  "$(CFG)" == "wisp - Win32 Release"

!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\wfname.h

!IF  "$(CFG)" == "wisp - Win32 Release"

!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\wglobals.h

!IF  "$(CFG)" == "wisp - Win32 Release"

!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\wisp.c

!IF  "$(CFG)" == "wisp - Win32 Release"

DEP_CPP_WISP_=\
	".\wisp.h"\
	".\scrn.h"\
	".\wispfile.h"\
	".\crt.h"\
	".\cobfiles.h"\
	".\keylist.h"\
	".\directiv.h"\
	".\wmalloc.h"\
	".\input.h"\
	".\ring.h"\
	".\statment.h"\
	".\wt_procd.h"\
	".\wcommon.h"\
	".\demovali.h"\
	".\proto.h"\
	".\output.h"\
	".\keywords.h"\
	".\idsistd.h"\
	".\token.h"\
	".\node.h"\
	".\intdef.h"\
	".\tokenize.h"\
	{$(INCLUDE)}"\sys\Types.h"\
	{$(INCLUDE)}"\sys\Stat.h"\
	

"$(INTDIR)\wisp.obj" : $(SOURCE) $(DEP_CPP_WISP_) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"

DEP_CPP_WISP_=\
	".\wisp.h"\
	".\scrn.h"\
	".\wispfile.h"\
	".\crt.h"\
	".\cobfiles.h"\
	".\keylist.h"\
	".\directiv.h"\
	".\wmalloc.h"\
	".\input.h"\
	".\ring.h"\
	".\statment.h"\
	".\wt_procd.h"\
	".\wcommon.h"\
	".\proto.h"\
	".\demovali.h"\
	".\output.h"\
	".\keywords.h"\
	".\token.h"\
	".\node.h"\
	".\tokenize.h"\
	".\idsistd.h"\
	".\intdef.h"\
	{$(INCLUDE)}"\sys\Types.h"\
	{$(INCLUDE)}"\sys\Stat.h"\
	

"$(INTDIR)\wisp.obj" : $(SOURCE) $(DEP_CPP_WISP_) "$(INTDIR)"

"$(INTDIR)\wisp.sbr" : $(SOURCE) $(DEP_CPP_WISP_) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\wisp.h

!IF  "$(CFG)" == "wisp - Win32 Release"

!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\wisp_pic.c
DEP_CPP_WISP_P=\
	".\idsistd.h"\
	".\intdef.h"\
	

!IF  "$(CFG)" == "wisp - Win32 Release"


"$(INTDIR)\wisp_pic.obj" : $(SOURCE) $(DEP_CPP_WISP_P) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"


"$(INTDIR)\wisp_pic.obj" : $(SOURCE) $(DEP_CPP_WISP_P) "$(INTDIR)"

"$(INTDIR)\wisp_pic.sbr" : $(SOURCE) $(DEP_CPP_WISP_P) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\wispfile.h

!IF  "$(CFG)" == "wisp - Win32 Release"

!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\wispvers.h

!IF  "$(CFG)" == "wisp - Win32 Release"

!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\wlicense.h

!IF  "$(CFG)" == "wisp - Win32 Release"

!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\wmalloc.c
DEP_CPP_WMALL=\
	".\wmalloc.h"\
	".\output.h"\
	".\proto.h"\
	".\token.h"\
	".\node.h"\
	".\wispfile.h"\
	".\tokenize.h"\
	".\idsistd.h"\
	".\intdef.h"\
	

!IF  "$(CFG)" == "wisp - Win32 Release"


"$(INTDIR)\wmalloc.obj" : $(SOURCE) $(DEP_CPP_WMALL) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"


"$(INTDIR)\wmalloc.obj" : $(SOURCE) $(DEP_CPP_WMALL) "$(INTDIR)"

"$(INTDIR)\wmalloc.sbr" : $(SOURCE) $(DEP_CPP_WMALL) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\wmalloc.h

!IF  "$(CFG)" == "wisp - Win32 Release"

!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\wperson.h

!IF  "$(CFG)" == "wisp - Win32 Release"

!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\wrunconf.h

!IF  "$(CFG)" == "wisp - Win32 Release"

!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\wsysconf.h

!IF  "$(CFG)" == "wisp - Win32 Release"

!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\wt_acept.c
DEP_CPP_WT_AC=\
	".\wisp.h"\
	".\keywords.h"\
	".\proto.h"\
	".\output.h"\
	".\idsistd.h"\
	".\token.h"\
	".\node.h"\
	".\wispfile.h"\
	".\intdef.h"\
	".\tokenize.h"\
	

!IF  "$(CFG)" == "wisp - Win32 Release"


"$(INTDIR)\wt_acept.obj" : $(SOURCE) $(DEP_CPP_WT_AC) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"


"$(INTDIR)\wt_acept.obj" : $(SOURCE) $(DEP_CPP_WT_AC) "$(INTDIR)"

"$(INTDIR)\wt_acept.sbr" : $(SOURCE) $(DEP_CPP_WT_AC) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\wt_call.c
DEP_CPP_WT_CA=\
	".\wisp.h"\
	".\cobfiles.h"\
	".\wmalloc.h"\
	".\proto.h"\
	".\output.h"\
	".\keywords.h"\
	".\idsistd.h"\
	".\token.h"\
	".\node.h"\
	".\wispfile.h"\
	".\intdef.h"\
	".\tokenize.h"\
	

!IF  "$(CFG)" == "wisp - Win32 Release"


"$(INTDIR)\wt_call.obj" : $(SOURCE) $(DEP_CPP_WT_CA) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"


"$(INTDIR)\wt_call.obj" : $(SOURCE) $(DEP_CPP_WT_CA) "$(INTDIR)"

"$(INTDIR)\wt_call.sbr" : $(SOURCE) $(DEP_CPP_WT_CA) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\wt_cli.c
DEP_CPP_WT_CL=\
	".\wisp.h"\
	".\wcommon.h"\
	".\wispfile.h"\
	".\getopt.h"\
	".\keywords.h"\
	".\proto.h"\
	".\output.h"\
	".\idsistd.h"\
	".\token.h"\
	".\node.h"\
	".\intdef.h"\
	".\tokenize.h"\
	

!IF  "$(CFG)" == "wisp - Win32 Release"


"$(INTDIR)\wt_cli.obj" : $(SOURCE) $(DEP_CPP_WT_CL) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"


"$(INTDIR)\wt_cli.obj" : $(SOURCE) $(DEP_CPP_WT_CL) "$(INTDIR)"

"$(INTDIR)\wt_cli.sbr" : $(SOURCE) $(DEP_CPP_WT_CL) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\wt_crtrw.c
DEP_CPP_WT_CR=\
	".\wisp.h"\
	".\crt.h"\
	".\proto.h"\
	".\output.h"\
	".\keywords.h"\
	".\idsistd.h"\
	".\token.h"\
	".\node.h"\
	".\wispfile.h"\
	".\intdef.h"\
	".\tokenize.h"\
	

!IF  "$(CFG)" == "wisp - Win32 Release"


"$(INTDIR)\wt_crtrw.obj" : $(SOURCE) $(DEP_CPP_WT_CR) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"


"$(INTDIR)\wt_crtrw.obj" : $(SOURCE) $(DEP_CPP_WT_CR) "$(INTDIR)"

"$(INTDIR)\wt_crtrw.sbr" : $(SOURCE) $(DEP_CPP_WT_CR) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\wt_datad.c
DEP_CPP_WT_DA=\
	".\wisp.h"\
	".\crt.h"\
	".\wispfile.h"\
	".\cobfiles.h"\
	".\keylist.h"\
	".\token.h"\
	".\node.h"\
	".\statment.h"\
	".\proto.h"\
	".\output.h"\
	".\keywords.h"\
	".\idsistd.h"\
	".\intdef.h"\
	".\tokenize.h"\
	

!IF  "$(CFG)" == "wisp - Win32 Release"


"$(INTDIR)\wt_datad.obj" : $(SOURCE) $(DEP_CPP_WT_DA) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"


"$(INTDIR)\wt_datad.obj" : $(SOURCE) $(DEP_CPP_WT_DA) "$(INTDIR)"

"$(INTDIR)\wt_datad.sbr" : $(SOURCE) $(DEP_CPP_WT_DA) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\wt_datad.h

!IF  "$(CFG)" == "wisp - Win32 Release"

!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\wt_debug.c
DEP_CPP_WT_DE=\
	".\token.h"\
	".\node.h"\
	".\wispfile.h"\
	".\wisp.h"\
	".\tokenize.h"\
	".\proto.h"\
	".\output.h"\
	".\keywords.h"\
	".\idsistd.h"\
	".\intdef.h"\
	

!IF  "$(CFG)" == "wisp - Win32 Release"


"$(INTDIR)\wt_debug.obj" : $(SOURCE) $(DEP_CPP_WT_DE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"


"$(INTDIR)\wt_debug.obj" : $(SOURCE) $(DEP_CPP_WT_DE) "$(INTDIR)"

"$(INTDIR)\wt_debug.sbr" : $(SOURCE) $(DEP_CPP_WT_DE) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\wt_decl.c
DEP_CPP_WT_DEC=\
	".\wisp.h"\
	".\crt.h"\
	".\cobfiles.h"\
	".\proto.h"\
	".\output.h"\
	".\keywords.h"\
	".\idsistd.h"\
	".\token.h"\
	".\node.h"\
	".\wispfile.h"\
	".\intdef.h"\
	".\tokenize.h"\
	

!IF  "$(CFG)" == "wisp - Win32 Release"


"$(INTDIR)\wt_decl.obj" : $(SOURCE) $(DEP_CPP_WT_DEC) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"


"$(INTDIR)\wt_decl.obj" : $(SOURCE) $(DEP_CPP_WT_DEC) "$(INTDIR)"

"$(INTDIR)\wt_decl.sbr" : $(SOURCE) $(DEP_CPP_WT_DEC) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\wt_delet.c
DEP_CPP_WT_DEL=\
	".\wisp.h"\
	".\cobfiles.h"\
	".\proto.h"\
	".\output.h"\
	".\keywords.h"\
	".\idsistd.h"\
	".\token.h"\
	".\node.h"\
	".\wispfile.h"\
	".\intdef.h"\
	".\tokenize.h"\
	

!IF  "$(CFG)" == "wisp - Win32 Release"


"$(INTDIR)\wt_delet.obj" : $(SOURCE) $(DEP_CPP_WT_DEL) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"


"$(INTDIR)\wt_delet.obj" : $(SOURCE) $(DEP_CPP_WT_DEL) "$(INTDIR)"

"$(INTDIR)\wt_delet.sbr" : $(SOURCE) $(DEP_CPP_WT_DEL) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\wt_disp.c
DEP_CPP_WT_DI=\
	".\wisp.h"\
	".\scrn.h"\
	".\crt.h"\
	".\statment.h"\
	".\wt_procd.h"\
	".\wt_disp.h"\
	".\reduce.h"\
	".\proto.h"\
	".\output.h"\
	".\keywords.h"\
	".\idsistd.h"\
	".\token.h"\
	".\node.h"\
	".\wispfile.h"\
	".\intdef.h"\
	".\tokenize.h"\
	

!IF  "$(CFG)" == "wisp - Win32 Release"


"$(INTDIR)\wt_disp.obj" : $(SOURCE) $(DEP_CPP_WT_DI) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"


"$(INTDIR)\wt_disp.obj" : $(SOURCE) $(DEP_CPP_WT_DI) "$(INTDIR)"

"$(INTDIR)\wt_disp.sbr" : $(SOURCE) $(DEP_CPP_WT_DI) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\wt_disp.h

!IF  "$(CFG)" == "wisp - Win32 Release"

!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\wt_divs.c
DEP_CPP_WT_DIV=\
	".\wisp.h"\
	".\wispfile.h"\
	".\cobfiles.h"\
	".\directiv.h"\
	".\proto.h"\
	".\output.h"\
	".\keywords.h"\
	".\idsistd.h"\
	".\token.h"\
	".\node.h"\
	".\intdef.h"\
	".\tokenize.h"\
	

!IF  "$(CFG)" == "wisp - Win32 Release"


"$(INTDIR)\wt_divs.obj" : $(SOURCE) $(DEP_CPP_WT_DIV) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"


"$(INTDIR)\wt_divs.obj" : $(SOURCE) $(DEP_CPP_WT_DIV) "$(INTDIR)"

"$(INTDIR)\wt_divs.sbr" : $(SOURCE) $(DEP_CPP_WT_DIV) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\wt_files.c
DEP_CPP_WT_FI=\
	".\wisp.h"\
	".\scrn.h"\
	".\wispfile.h"\
	".\cobfiles.h"\
	".\keylist.h"\
	".\wmalloc.h"\
	".\input.h"\
	".\ring.h"\
	{$(INCLUDE)}"\sys\Types.h"\
	".\proto.h"\
	".\output.h"\
	".\keywords.h"\
	".\idsistd.h"\
	".\token.h"\
	".\node.h"\
	".\intdef.h"\
	".\tokenize.h"\
	

!IF  "$(CFG)" == "wisp - Win32 Release"


"$(INTDIR)\wt_files.obj" : $(SOURCE) $(DEP_CPP_WT_FI) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"


"$(INTDIR)\wt_files.obj" : $(SOURCE) $(DEP_CPP_WT_FI) "$(INTDIR)"

"$(INTDIR)\wt_files.sbr" : $(SOURCE) $(DEP_CPP_WT_FI) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\wt_free.c
DEP_CPP_WT_FR=\
	".\wisp.h"\
	".\proto.h"\
	".\output.h"\
	".\keywords.h"\
	".\idsistd.h"\
	".\token.h"\
	".\node.h"\
	".\wispfile.h"\
	".\intdef.h"\
	".\tokenize.h"\
	

!IF  "$(CFG)" == "wisp - Win32 Release"


"$(INTDIR)\wt_free.obj" : $(SOURCE) $(DEP_CPP_WT_FR) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"


"$(INTDIR)\wt_free.obj" : $(SOURCE) $(DEP_CPP_WT_FR) "$(INTDIR)"

"$(INTDIR)\wt_free.sbr" : $(SOURCE) $(DEP_CPP_WT_FR) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\wt_ident.c
DEP_CPP_WT_ID=\
	".\wisp.h"\
	".\directiv.h"\
	".\wispfile.h"\
	".\token.h"\
	".\node.h"\
	".\wmalloc.h"\
	".\statment.h"\
	".\ring.h"\
	".\proto.h"\
	".\output.h"\
	".\keywords.h"\
	".\idsistd.h"\
	".\intdef.h"\
	".\tokenize.h"\
	

!IF  "$(CFG)" == "wisp - Win32 Release"


"$(INTDIR)\wt_ident.obj" : $(SOURCE) $(DEP_CPP_WT_ID) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"


"$(INTDIR)\wt_ident.obj" : $(SOURCE) $(DEP_CPP_WT_ID) "$(INTDIR)"

"$(INTDIR)\wt_ident.sbr" : $(SOURCE) $(DEP_CPP_WT_ID) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\wt_if.c
DEP_CPP_WT_IF=\
	".\wisp.h"\
	".\token.h"\
	".\node.h"\
	".\statment.h"\
	".\reduce.h"\
	".\proto.h"\
	".\output.h"\
	".\keywords.h"\
	".\idsistd.h"\
	".\wispfile.h"\
	".\intdef.h"\
	".\tokenize.h"\
	

!IF  "$(CFG)" == "wisp - Win32 Release"


"$(INTDIR)\wt_if.obj" : $(SOURCE) $(DEP_CPP_WT_IF) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"


"$(INTDIR)\wt_if.obj" : $(SOURCE) $(DEP_CPP_WT_IF) "$(INTDIR)"

"$(INTDIR)\wt_if.sbr" : $(SOURCE) $(DEP_CPP_WT_IF) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\wt_input.c
DEP_CPP_WT_IN=\
	".\wisp.h"\
	".\crt.h"\
	".\cobfiles.h"\
	".\keylist.h"\
	".\directiv.h"\
	".\node.h"\
	".\token.h"\
	".\wmalloc.h"\
	".\statment.h"\
	".\proto.h"\
	".\output.h"\
	".\keywords.h"\
	".\idsistd.h"\
	".\wispfile.h"\
	".\intdef.h"\
	".\tokenize.h"\
	

!IF  "$(CFG)" == "wisp - Win32 Release"


"$(INTDIR)\wt_input.obj" : $(SOURCE) $(DEP_CPP_WT_IN) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"


"$(INTDIR)\wt_input.obj" : $(SOURCE) $(DEP_CPP_WT_IN) "$(INTDIR)"

"$(INTDIR)\wt_input.sbr" : $(SOURCE) $(DEP_CPP_WT_IN) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\wt_io.c
DEP_CPP_WT_IO=\
	".\wisp.h"\
	".\wispfile.h"\
	".\token.h"\
	".\lines.h"\
	".\input.h"\
	".\proto.h"\
	".\output.h"\
	".\keywords.h"\
	".\idsistd.h"\
	".\node.h"\
	".\intdef.h"\
	".\tokenize.h"\
	

!IF  "$(CFG)" == "wisp - Win32 Release"


"$(INTDIR)\wt_io.obj" : $(SOURCE) $(DEP_CPP_WT_IO) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"


"$(INTDIR)\wt_io.obj" : $(SOURCE) $(DEP_CPP_WT_IO) "$(INTDIR)"

"$(INTDIR)\wt_io.sbr" : $(SOURCE) $(DEP_CPP_WT_IO) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\wt_locks.c
DEP_CPP_WT_LO=\
	".\wisp.h"\
	".\cobfiles.h"\
	".\proto.h"\
	".\output.h"\
	".\keywords.h"\
	".\idsistd.h"\
	".\token.h"\
	".\node.h"\
	".\wispfile.h"\
	".\intdef.h"\
	".\tokenize.h"\
	

!IF  "$(CFG)" == "wisp - Win32 Release"


"$(INTDIR)\wt_locks.obj" : $(SOURCE) $(DEP_CPP_WT_LO) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"


"$(INTDIR)\wt_locks.obj" : $(SOURCE) $(DEP_CPP_WT_LO) "$(INTDIR)"

"$(INTDIR)\wt_locks.sbr" : $(SOURCE) $(DEP_CPP_WT_LO) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\wt_opcls.c
DEP_CPP_WT_OP=\
	".\wisp.h"\
	".\crt.h"\
	".\cobfiles.h"\
	".\directiv.h"\
	".\wcommon.h"\
	".\proto.h"\
	".\output.h"\
	".\keywords.h"\
	".\idsistd.h"\
	".\token.h"\
	".\node.h"\
	".\wispfile.h"\
	".\intdef.h"\
	".\tokenize.h"\
	

!IF  "$(CFG)" == "wisp - Win32 Release"


"$(INTDIR)\wt_opcls.obj" : $(SOURCE) $(DEP_CPP_WT_OP) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"


"$(INTDIR)\wt_opcls.obj" : $(SOURCE) $(DEP_CPP_WT_OP) "$(INTDIR)"

"$(INTDIR)\wt_opcls.sbr" : $(SOURCE) $(DEP_CPP_WT_OP) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\wt_procd.c
DEP_CPP_WT_PR=\
	".\wisp.h"\
	".\scrn.h"\
	".\crt.h"\
	".\cobfiles.h"\
	".\wispfile.h"\
	".\statment.h"\
	".\wt_disp.h"\
	".\wt_procd.h"\
	".\reduce.h"\
	".\proto.h"\
	".\output.h"\
	".\keywords.h"\
	".\idsistd.h"\
	".\token.h"\
	".\node.h"\
	".\intdef.h"\
	".\tokenize.h"\
	

!IF  "$(CFG)" == "wisp - Win32 Release"


"$(INTDIR)\wt_procd.obj" : $(SOURCE) $(DEP_CPP_WT_PR) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"


"$(INTDIR)\wt_procd.obj" : $(SOURCE) $(DEP_CPP_WT_PR) "$(INTDIR)"

"$(INTDIR)\wt_procd.sbr" : $(SOURCE) $(DEP_CPP_WT_PR) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\wt_procd.h

!IF  "$(CFG)" == "wisp - Win32 Release"

!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\wt_read.c
DEP_CPP_WT_RE=\
	".\wisp.h"\
	".\wispfile.h"\
	".\crt.h"\
	".\cobfiles.h"\
	".\proto.h"\
	".\output.h"\
	".\keywords.h"\
	".\idsistd.h"\
	".\token.h"\
	".\node.h"\
	".\intdef.h"\
	".\tokenize.h"\
	

!IF  "$(CFG)" == "wisp - Win32 Release"


"$(INTDIR)\wt_read.obj" : $(SOURCE) $(DEP_CPP_WT_RE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"


"$(INTDIR)\wt_read.obj" : $(SOURCE) $(DEP_CPP_WT_RE) "$(INTDIR)"

"$(INTDIR)\wt_read.sbr" : $(SOURCE) $(DEP_CPP_WT_RE) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\wt_scrn.c
DEP_CPP_WT_SC=\
	".\wisp.h"\
	".\scrn.h"\
	".\crt.h"\
	".\token.h"\
	".\node.h"\
	".\directiv.h"\
	".\wmalloc.h"\
	".\wcommon.h"\
	".\reduce.h"\
	".\ring.h"\
	".\statment.h"\
	".\proto.h"\
	".\output.h"\
	".\keywords.h"\
	".\idsistd.h"\
	".\wispfile.h"\
	".\intdef.h"\
	".\tokenize.h"\
	

!IF  "$(CFG)" == "wisp - Win32 Release"


"$(INTDIR)\wt_scrn.obj" : $(SOURCE) $(DEP_CPP_WT_SC) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"


"$(INTDIR)\wt_scrn.obj" : $(SOURCE) $(DEP_CPP_WT_SC) "$(INTDIR)"

"$(INTDIR)\wt_scrn.sbr" : $(SOURCE) $(DEP_CPP_WT_SC) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\wt_sort.c
DEP_CPP_WT_SO=\
	".\wisp.h"\
	".\cobfiles.h"\
	".\proto.h"\
	".\output.h"\
	".\keywords.h"\
	".\idsistd.h"\
	".\token.h"\
	".\node.h"\
	".\wispfile.h"\
	".\intdef.h"\
	".\tokenize.h"\
	

!IF  "$(CFG)" == "wisp - Win32 Release"


"$(INTDIR)\wt_sort.obj" : $(SOURCE) $(DEP_CPP_WT_SO) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"


"$(INTDIR)\wt_sort.obj" : $(SOURCE) $(DEP_CPP_WT_SO) "$(INTDIR)"

"$(INTDIR)\wt_sort.sbr" : $(SOURCE) $(DEP_CPP_WT_SO) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\wt_start.c
DEP_CPP_WT_ST=\
	".\wisp.h"\
	".\cobfiles.h"\
	".\token.h"\
	".\node.h"\
	".\reduce.h"\
	".\statment.h"\
	".\proto.h"\
	".\output.h"\
	".\keywords.h"\
	".\idsistd.h"\
	".\wispfile.h"\
	".\intdef.h"\
	".\tokenize.h"\
	

!IF  "$(CFG)" == "wisp - Win32 Release"


"$(INTDIR)\wt_start.obj" : $(SOURCE) $(DEP_CPP_WT_ST) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"


"$(INTDIR)\wt_start.obj" : $(SOURCE) $(DEP_CPP_WT_ST) "$(INTDIR)"

"$(INTDIR)\wt_start.sbr" : $(SOURCE) $(DEP_CPP_WT_ST) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\wt_utils.c
DEP_CPP_WT_UT=\
	".\wisp.h"\
	".\proto.h"\
	".\output.h"\
	".\keywords.h"\
	".\idsistd.h"\
	".\token.h"\
	".\node.h"\
	".\wispfile.h"\
	".\intdef.h"\
	".\tokenize.h"\
	

!IF  "$(CFG)" == "wisp - Win32 Release"


"$(INTDIR)\wt_utils.obj" : $(SOURCE) $(DEP_CPP_WT_UT) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"


"$(INTDIR)\wt_utils.obj" : $(SOURCE) $(DEP_CPP_WT_UT) "$(INTDIR)"

"$(INTDIR)\wt_utils.sbr" : $(SOURCE) $(DEP_CPP_WT_UT) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\wt_write.c
DEP_CPP_WT_WR=\
	".\wisp.h"\
	".\cobfiles.h"\
	".\proto.h"\
	".\output.h"\
	".\keywords.h"\
	".\idsistd.h"\
	".\token.h"\
	".\node.h"\
	".\wispfile.h"\
	".\intdef.h"\
	".\tokenize.h"\
	

!IF  "$(CFG)" == "wisp - Win32 Release"


"$(INTDIR)\wt_write.obj" : $(SOURCE) $(DEP_CPP_WT_WR) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"


"$(INTDIR)\wt_write.obj" : $(SOURCE) $(DEP_CPP_WT_WR) "$(INTDIR)"

"$(INTDIR)\wt_write.sbr" : $(SOURCE) $(DEP_CPP_WT_WR) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\wt_wsdat.c
DEP_CPP_WT_WS=\
	".\wisp.h"\
	".\crt.h"\
	".\wispfile.h"\
	".\cobfiles.h"\
	".\wcommon.h"\
	".\intdef.h"\
	".\proto.h"\
	".\output.h"\
	".\keywords.h"\
	".\idsistd.h"\
	".\token.h"\
	".\node.h"\
	".\tokenize.h"\
	

!IF  "$(CFG)" == "wisp - Win32 Release"


"$(INTDIR)\wt_wsdat.obj" : $(SOURCE) $(DEP_CPP_WT_WS) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"


"$(INTDIR)\wt_wsdat.obj" : $(SOURCE) $(DEP_CPP_WT_WS) "$(INTDIR)"

"$(INTDIR)\wt_wsdat.sbr" : $(SOURCE) $(DEP_CPP_WT_WS) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\wt_wsdiv.c
DEP_CPP_WT_WSD=\
	".\wisp.h"\
	".\keylist.h"\
	".\token.h"\
	".\node.h"\
	".\statment.h"\
	".\proto.h"\
	".\output.h"\
	".\keywords.h"\
	".\idsistd.h"\
	".\wispfile.h"\
	".\intdef.h"\
	".\tokenize.h"\
	

!IF  "$(CFG)" == "wisp - Win32 Release"


"$(INTDIR)\wt_wsdiv.obj" : $(SOURCE) $(DEP_CPP_WT_WSD) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"


"$(INTDIR)\wt_wsdiv.obj" : $(SOURCE) $(DEP_CPP_WT_WSD) "$(INTDIR)"

"$(INTDIR)\wt_wsdiv.sbr" : $(SOURCE) $(DEP_CPP_WT_WSD) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\getopt.h

!IF  "$(CFG)" == "wisp - Win32 Release"

!ELSEIF  "$(CFG)" == "wisp - Win32 Debug"

!ENDIF 

# End Source File
# End Target
# End Project
################################################################################
