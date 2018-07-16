# Microsoft Developer Studio Project File - Name="testacu" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) External Target" 0x0106

CFG=testacu - Win32 Release
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "testacu.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "testacu.mak" CFG="testacu - Win32 Release"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "testacu - Win32 Release" (based on "Win32 (x86) External Target")
!MESSAGE "testacu - Win32 Debug" (based on "Win32 (x86) External Target")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""

!IF  "$(CFG)" == "testacu - Win32 Release"

# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir ".\Release"
# PROP BASE Intermediate_Dir ".\Release"
# PROP BASE Cmd_Line "NMAKE /f testacu.mak"
# PROP BASE Rebuild_Opt "/a"
# PROP BASE Target_File "testacu\testacu.exe"
# PROP BASE Bsc_Name "testacu\testacu.bsc"
# PROP BASE Target_Dir "."
# PROP Use_Debug_Libraries 0
# PROP Output_Dir ".\Release"
# PROP Intermediate_Dir ".\Release"
# PROP Cmd_Line "NMAKE /f sampleacu.umf OS=win32 WISPTRAN=..\bin\wisp COBOL=C:\acucbl31\bin\ccbl32 "
# PROP Rebuild_Opt "/a"
# PROP Target_File "testacu\SAMPLE"
# PROP Bsc_Name "testacu\SAMPLE.bsc"
# PROP Target_Dir "."

!ELSEIF  "$(CFG)" == "testacu - Win32 Debug"

# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir ".\Debug"
# PROP BASE Intermediate_Dir ".\Debug"
# PROP BASE Cmd_Line "NMAKE /f testacu.mak"
# PROP BASE Rebuild_Opt "/a"
# PROP BASE Target_File "testacu\testacu.exe"
# PROP BASE Bsc_Name "testacu\testacu.bsc"
# PROP BASE Target_Dir "."
# PROP Use_Debug_Libraries 1
# PROP Output_Dir ".\Debug"
# PROP Intermediate_Dir ".\Debug"
# PROP Cmd_Line "NMAKE /f sampleacu.umf OS=win32 WISPTRAN=..\bin\wisp COBOL=C:\acucbl31\bin\ccbl32 "
# PROP Rebuild_Opt "/a"
# PROP Target_File "testacu\SAMPLE"
# PROP Bsc_Name "testacu\SAMPLE.bsc"
# PROP Target_Dir "."

!ENDIF 

# Begin Target

# Name "testacu - Win32 Release"
# Name "testacu - Win32 Debug"

!IF  "$(CFG)" == "testacu - Win32 Release"

!ELSEIF  "$(CFG)" == "testacu - Win32 Debug"

!ENDIF 

# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;hpj;bat;for;f90"
# Begin Source File

SOURCE=.\sampleacu.umf
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl;fi;fd"
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;cnt;rtf;gif;jpg;jpeg;jpe"
# End Group
# End Target
# End Project
