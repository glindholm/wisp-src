# Microsoft Developer Studio Project File - Name="mfn" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Generic Project" 0x010a

CFG=mfn - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "mfn.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "mfn.mak" CFG="mfn - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "mfn - Win32 Release" (based on "Win32 (x86) Generic Project")
!MESSAGE "mfn - Win32 Debug" (based on "Win32 (x86) Generic Project")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
MTL=midl.exe

!IF  "$(CFG)" == "mfn - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "mfn___Win32_Release"
# PROP BASE Intermediate_Dir "mfn___Win32_Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "mfn___Win32_Release"
# PROP Intermediate_Dir "mfn___Win32_Release"
# PROP Target_Dir ""

!ELSEIF  "$(CFG)" == "mfn - Win32 Debug"

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

!ENDIF 

# Begin Target

# Name "mfn - Win32 Release"
# Name "mfn - Win32 Debug"
# Begin Source File

SOURCE=.\mfn.umf
# End Source File
# Begin Source File

SOURCE=..\doc\mfnativescreens.txt
# End Source File
# Begin Source File

SOURCE=.\WMFNDISPLAY.cob
# End Source File
# Begin Source File

SOURCE=.\WMFNERROR.cob
# End Source File
# Begin Source File

SOURCE=.\WMFNFAC2SCREEN.cob
# End Source File
# Begin Source File

SOURCE=.\WMFNGETPARM.cob
# End Source File
# Begin Source File

SOURCE=.\WMFNGETPFKEY.cob
# End Source File
# Begin Source File

SOURCE=.\WMFNHELP.cob
# End Source File
# Begin Source File

SOURCE=.\WMFNWSB.cob
# End Source File
# End Target
# End Project
