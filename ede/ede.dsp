# Microsoft Developer Studio Project File - Name="ede" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) External Target" 0x0106

CFG=ede - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "ede.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "ede.mak" CFG="ede - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "ede - Win32 Release" (based on "Win32 (x86) External Target")
!MESSAGE "ede - Win32 Debug" (based on "Win32 (x86) External Target")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""

!IF  "$(CFG)" == "ede - Win32 Release"

# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Cmd_Line "NMAKE /f ede.mak"
# PROP BASE Rebuild_Opt "/a"
# PROP BASE Target_File "ede.exe"
# PROP BASE Bsc_Name "ede.bsc"
# PROP BASE Target_Dir ""
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Cmd_Line "ECHO EDE IS BUILT"
# PROP Rebuild_Opt "/a"
# PROP Target_File "EDE"
# PROP Bsc_Name "ede.bsc"
# PROP Target_Dir ""

!ELSEIF  "$(CFG)" == "ede - Win32 Debug"

# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Cmd_Line "NMAKE /f ede.mak"
# PROP BASE Rebuild_Opt "/a"
# PROP BASE Target_File "ede.exe"
# PROP BASE Bsc_Name "ede.bsc"
# PROP BASE Target_Dir ""
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Cmd_Line "ECHO EDE IS BUILT"
# PROP Rebuild_Opt "/a"
# PROP Target_File "EDE"
# PROP Bsc_Name "ede.bsc"
# PROP Target_Dir ""

!ENDIF 

# Begin Target

# Name "ede - Win32 Release"
# Name "ede - Win32 Debug"

!IF  "$(CFG)" == "ede - Win32 Release"

!ELSEIF  "$(CFG)" == "ede - Win32 Debug"

!ENDIF 

# Begin Group "DEMO"

# PROP Default_Filter "wcb; hlp"
# Begin Source File

SOURCE=.\helpmap.dat
# End Source File
# Begin Source File

SOURCE=.\mcbbld.wcb
# End Source File
# Begin Source File

SOURCE=.\mcbcode.hlp
# End Source File
# Begin Source File

SOURCE=.\mcbcol.hlp
# End Source File
# Begin Source File

SOURCE=.\mcbcpy1.wcb
# End Source File
# Begin Source File

SOURCE=.\mcbcpy2.wcb
# End Source File
# Begin Source File

SOURCE=.\mcbcpy3.wcb
# End Source File
# Begin Source File

SOURCE=.\mcbdisp.hlp
# End Source File
# Begin Source File

SOURCE=.\mcbedit.hlp
# End Source File
# Begin Source File

SOURCE=.\mcbedit.wcb
# End Source File
# Begin Source File

SOURCE=.\mcbicnt.hlp
# End Source File
# Begin Source File

SOURCE=.\mcbname.hlp
# End Source File
# Begin Source File

SOURCE=.\mcbopts.hlp
# End Source File
# Begin Source File

SOURCE=.\mcbrow.hlp
# End Source File
# Begin Source File

SOURCE=.\mcbtext.hlp
# End Source File
# Begin Source File

SOURCE=.\mcbtype.hlp
# End Source File
# Begin Source File

SOURCE=.\mcbvalue.hlp
# End Source File
# Begin Source File

SOURCE=.\mcbwidth.hlp
# End Source File
# Begin Source File

SOURCE=.\menudefs.wcb
# End Source File
# Begin Source File

SOURCE=.\menudemo.mak
# End Source File
# Begin Source File

SOURCE=.\menudemo.opt
# End Source File
# Begin Source File

SOURCE=.\menudemo.wcb
# End Source File
# Begin Source File

SOURCE=.\menudisp.wcb
# End Source File
# Begin Source File

SOURCE=.\menulogo.wcb
# End Source File
# Begin Source File

SOURCE=.\menumcbs.wcb
# End Source File
# Begin Source File

SOURCE=.\menuvect.wcb
# End Source File
# End Group
# Begin Group "ACU"

# PROP Default_Filter ""
# Begin Source File

SOURCE=..\acu\wrun32wisp_ede.mak
# End Source File
# End Group
# Begin Source File

SOURCE=.\edentsetup.txt
# End Source File
# End Target
# End Project
