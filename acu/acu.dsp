# Microsoft Developer Studio Project File - Name="acu" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) External Target" 0x0106

CFG=acu - Win32 Release
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "acu.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "acu.mak" CFG="acu - Win32 Release"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "acu - Win32 Release" (based on "Win32 (x86) External Target")
!MESSAGE "acu - Win32 Debug" (based on "Win32 (x86) External Target")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""

!IF  "$(CFG)" == "acu - Win32 Release"

# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir ".\Release"
# PROP BASE Intermediate_Dir ".\Release"
# PROP BASE Cmd_Line "NMAKE /f acu.mak"
# PROP BASE Rebuild_Opt "/a"
# PROP BASE Target_File "acu\acu.exe"
# PROP BASE Bsc_Name "acu\acu.bsc"
# PROP BASE Target_Dir "."
# PROP Use_Debug_Libraries 0
# PROP Output_Dir ".\Release"
# PROP Intermediate_Dir ".\Release"
# PROP Cmd_Line "NMAKE /f wacu.mak WISPDIR=.. acu"
# PROP Rebuild_Opt "/a"
# PROP Target_File "ACU"
# PROP Bsc_Name "acu\acu.bsc"
# PROP Target_Dir "."

!ELSEIF  "$(CFG)" == "acu - Win32 Debug"

# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir ".\Debug"
# PROP BASE Intermediate_Dir ".\Debug"
# PROP BASE Cmd_Line "NMAKE /f acu.mak"
# PROP BASE Rebuild_Opt "/a"
# PROP BASE Target_File "acu\acu.exe"
# PROP BASE Bsc_Name "acu\acu.bsc"
# PROP BASE Target_Dir "."
# PROP Use_Debug_Libraries 1
# PROP Output_Dir ".\Debug"
# PROP Intermediate_Dir ".\Debug"
# PROP Cmd_Line "NMAKE /f wacu.mak WISPDIR=.. acu"
# PROP Rebuild_Opt "/a"
# PROP Target_File "acu\wwruncbl.exe"
# PROP Bsc_Name "acu\acu.bsc"
# PROP Target_Dir "."

!ENDIF 

# Begin Target

# Name "acu - Win32 Release"
# Name "acu - Win32 Debug"

!IF  "$(CFG)" == "acu - Win32 Release"

!ELSEIF  "$(CFG)" == "acu - Win32 Debug"

!ENDIF 

# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;hpj;bat;for;f90"
# Begin Source File

SOURCE=.\aculink.wcb
# End Source File
# Begin Source File

SOURCE=.\acuusing.cob
# End Source File
# Begin Source File

SOURCE=.\sub85.c
# End Source File
# Begin Source File

SOURCE=.\wacu.mak
# End Source File
# Begin Source File

SOURCE=.\wrun32wisp_acu51.mak
# End Source File
# Begin Source File

SOURCE=.\wrun32wisp_acu52.mak
# End Source File
# Begin Source File

SOURCE=.\wrun32wisp_crid_acu51.mak
# End Source File
# Begin Source File

SOURCE=.\wrun32wisp_crid_acu52.mak
# End Source File
# Begin Source File

SOURCE=.\wrun32wisp_ede_acu51.mak
# End Source File
# Begin Source File

SOURCE=.\wrun32wisp_ede_acu52.mak
# End Source File
# Begin Source File

SOURCE=.\wwruncbl.mak
# End Source File
# End Group
# Begin Source File

SOURCE=.\acucobol.include
# End Source File
# Begin Source File

SOURCE=.\AcucobolLicenses.txt
# End Source File
# Begin Source File

SOURCE=.\wruncbl.umf
# End Source File
# End Target
# End Project
