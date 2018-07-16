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

SOURCE=.\demo\helpmap.dat
# End Source File
# Begin Source File

SOURCE=.\demo\helpmap.unix
# End Source File
# Begin Source File

SOURCE=.\demo\mcbbld.wcb
# End Source File
# Begin Source File

SOURCE=.\demo\mcbcode.hlp
# End Source File
# Begin Source File

SOURCE=.\demo\mcbcol.hlp
# End Source File
# Begin Source File

SOURCE=.\demo\mcbcpy1.wcb
# End Source File
# Begin Source File

SOURCE=.\demo\mcbcpy2.wcb
# End Source File
# Begin Source File

SOURCE=.\demo\mcbcpy3.wcb
# End Source File
# Begin Source File

SOURCE=.\demo\mcbdisp.hlp
# End Source File
# Begin Source File

SOURCE=.\demo\mcbedit.hlp
# End Source File
# Begin Source File

SOURCE=.\demo\mcbedit.wcb
# End Source File
# Begin Source File

SOURCE=.\demo\mcbicnt.hlp
# End Source File
# Begin Source File

SOURCE=.\demo\mcbname.hlp
# End Source File
# Begin Source File

SOURCE=.\demo\mcbopts.hlp
# End Source File
# Begin Source File

SOURCE=.\demo\mcbrow.hlp
# End Source File
# Begin Source File

SOURCE=.\demo\mcbtext.hlp
# End Source File
# Begin Source File

SOURCE=.\demo\mcbtype.hlp
# End Source File
# Begin Source File

SOURCE=.\demo\mcbvalue.hlp
# End Source File
# Begin Source File

SOURCE=.\demo\mcbwidth.hlp
# End Source File
# Begin Source File

SOURCE=.\demo\menubld.old
# End Source File
# Begin Source File

SOURCE=.\demo\menudefs.wcb
# End Source File
# Begin Source File

SOURCE=.\demo\menudemo.mak
# End Source File
# Begin Source File

SOURCE=.\demo\menudemo.opt
# End Source File
# Begin Source File

SOURCE=.\demo\menudemo.umf
# End Source File
# Begin Source File

SOURCE=.\demo\menudemo.wcb
# End Source File
# Begin Source File

SOURCE=.\demo\menudemomf.umf
# End Source File
# Begin Source File

SOURCE=.\demo\menudisp.wcb
# End Source File
# Begin Source File

SOURCE=.\demo\menulogo.wcb
# End Source File
# Begin Source File

SOURCE=.\demo\menumcbs.wcb
# End Source File
# Begin Source File

SOURCE=.\demo\menumgr.old
# End Source File
# Begin Source File

SOURCE=.\demo\menuvect.wcb
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
