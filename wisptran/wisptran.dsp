# Microsoft Developer Studio Project File - Name="wisptran" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=wisptran - Win32 Release
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "wisptran.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "wisptran.mak" CFG="wisptran - Win32 Release"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "wisptran - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "wisptran - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "wisptran - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir ".\Release"
# PROP BASE Intermediate_Dir ".\Release"
# PROP BASE Target_Dir "."
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir ".\Release"
# PROP Intermediate_Dir ".\Release"
# PROP Target_Dir "."
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /YX /c
# ADD CPP /nologo /MD /W3 /GX /O2 /I "..\wispcommon" /D "NDEBUG" /D "WIN32" /D "_CONSOLE" /D "WINNT" /D "MSFS" /YX /FD /c
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386 /out:"..\bin\wisp.exe"
# SUBTRACT LINK32 /incremental:yes

!ELSEIF  "$(CFG)" == "wisptran - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir ".\Debug"
# PROP BASE Intermediate_Dir ".\Debug"
# PROP BASE Target_Dir "."
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir ".\Debug"
# PROP Intermediate_Dir ".\Debug"
# PROP Target_Dir "."
# ADD BASE CPP /nologo /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /YX /c
# ADD CPP /nologo /MDd /W3 /Gm /GX /ZI /Od /I "..\wispcommon" /D "_DEBUG" /D "WIN32" /D "_CONSOLE" /D "WINNT" /D "MSFS" /YX /FD /c
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /incremental:no /debug /machine:I386 /out:"..\bin\wisp.exe"

!ENDIF 

# Begin Target

# Name "wisptran - Win32 Release"
# Name "wisptran - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;hpj;bat;for;f90"
# Begin Source File

SOURCE=.\cobfiles.h
# End Source File
# Begin Source File

SOURCE=.\crt.h
# End Source File
# Begin Source File

SOURCE=.\dataconv.c
# End Source File
# Begin Source File

SOURCE=.\demovali.h
# End Source File
# Begin Source File

SOURCE=.\directiv.h
# End Source File
# Begin Source File

SOURCE=..\wispcommon\getopt.c
# End Source File
# Begin Source File

SOURCE=.\input.c
# End Source File
# Begin Source File

SOURCE=.\input.h
# End Source File
# Begin Source File

SOURCE=.\keylist.h
# End Source File
# Begin Source File

SOURCE=.\keywords.c
# End Source File
# Begin Source File

SOURCE=.\keywords.h
# End Source File
# Begin Source File

SOURCE=.\lines.h
# End Source File
# Begin Source File

SOURCE=.\node.c
# End Source File
# Begin Source File

SOURCE=.\node.h
# End Source File
# Begin Source File

SOURCE=.\output.c
# End Source File
# Begin Source File

SOURCE=.\output.h
# End Source File
# Begin Source File

SOURCE=.\proto.h
# End Source File
# Begin Source File

SOURCE=.\reduce.c
# End Source File
# Begin Source File

SOURCE=.\reduce.h
# End Source File
# Begin Source File

SOURCE=..\wispcommon\ring.c
# End Source File
# Begin Source File

SOURCE=.\scrn.h
# End Source File
# Begin Source File

SOURCE=.\statment.c
# End Source File
# Begin Source File

SOURCE=.\statment.h
# End Source File
# Begin Source File

SOURCE=.\stats.c
# End Source File
# Begin Source File

SOURCE=.\token.h
# End Source File
# Begin Source File

SOURCE=.\tokenize.c
# End Source File
# Begin Source File

SOURCE=.\tokenize.h
# End Source File
# Begin Source File

SOURCE=.\untabstr.c
# End Source File
# Begin Source File

SOURCE=.\wisp.c
# End Source File
# Begin Source File

SOURCE=.\wisp.h
# End Source File
# Begin Source File

SOURCE=..\wispcommon\wisp_pic.c
# End Source File
# Begin Source File

SOURCE=.\wispfile.h
# End Source File
# Begin Source File

SOURCE=.\wmalloc.c
# End Source File
# Begin Source File

SOURCE=.\wmalloc.h
# End Source File
# Begin Source File

SOURCE=.\wt_acept.c
# End Source File
# Begin Source File

SOURCE=.\wt_call.c
# End Source File
# Begin Source File

SOURCE=.\wt_cli.c
# End Source File
# Begin Source File

SOURCE=.\wt_crtrw.c
# End Source File
# Begin Source File

SOURCE=.\wt_datad.c
# End Source File
# Begin Source File

SOURCE=.\wt_datad.h
# End Source File
# Begin Source File

SOURCE=.\wt_debug.c
# End Source File
# Begin Source File

SOURCE=.\wt_decl.c
# End Source File
# Begin Source File

SOURCE=.\wt_delet.c
# End Source File
# Begin Source File

SOURCE=.\wt_disp.c
# End Source File
# Begin Source File

SOURCE=.\wt_disp.h
# End Source File
# Begin Source File

SOURCE=.\wt_divs.c
# End Source File
# Begin Source File

SOURCE=.\wt_files.c
# End Source File
# Begin Source File

SOURCE=.\wt_free.c
# End Source File
# Begin Source File

SOURCE=.\wt_ident.c
# End Source File
# Begin Source File

SOURCE=.\wt_if.c
# End Source File
# Begin Source File

SOURCE=.\wt_input.c
# End Source File
# Begin Source File

SOURCE=.\wt_io.c
# End Source File
# Begin Source File

SOURCE=.\wt_locks.c
# End Source File
# Begin Source File

SOURCE=.\wt_opcls.c
# End Source File
# Begin Source File

SOURCE=.\wt_procd.c
# End Source File
# Begin Source File

SOURCE=.\wt_procd.h
# End Source File
# Begin Source File

SOURCE=.\wt_read.c
# End Source File
# Begin Source File

SOURCE=.\wt_scrn.c
# End Source File
# Begin Source File

SOURCE=.\wt_sort.c
# End Source File
# Begin Source File

SOURCE=.\wt_start.c
# End Source File
# Begin Source File

SOURCE=.\wt_utils.c
# End Source File
# Begin Source File

SOURCE=.\wt_write.c
# End Source File
# Begin Source File

SOURCE=.\wt_wsdat.c
# End Source File
# Begin Source File

SOURCE=.\wt_wsdiv.c
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl;fi;fd"
# Begin Source File

SOURCE=..\wispcommon\getopt.h
# End Source File
# Begin Source File

SOURCE=..\wispcommon\ring.h
# End Source File
# Begin Source File

SOURCE=..\wispcommon\wisp_pic.h
# End Source File
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;cnt;rtf;gif;jpg;jpeg;jpe"
# End Group
# End Target
# End Project
