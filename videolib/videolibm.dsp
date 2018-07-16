# Microsoft Developer Studio Project File - Name="videolibm" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

CFG=videolibm - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "videolibm.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "videolibm.mak" CFG="videolibm - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "videolibm - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "videolibm - Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "videolibm - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "videolibm___Win32_Release"
# PROP BASE Intermediate_Dir "videolibm___Win32_Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "videolibm___Win32_Release"
# PROP Intermediate_Dir "videolibm___Win32_Release"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /YX /FD /c
# ADD CPP /nologo /MD /W3 /GX /O2 /D "NDEBUG" /D "WIN32" /D "_MBCS" /D "_LIB" /D "MSFS" /D "WINNT" /D "DIRECTVID" /YX /FD /c
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo /out:"..\lib\videom.lib"

!ELSEIF  "$(CFG)" == "videolibm - Win32 Debug"

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
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_MBCS" /D "_LIB" /YX /FD /GZ  /c
# ADD CPP /nologo /MDd /W3 /Gm /GX /ZI /Od /D "_DEBUG" /D "WIN32" /D "_MBCS" /D "_LIB" /D "MSFS" /D "WINNT" /D "DIRECTVID" /YX /FD /GZ  /c
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo /out:"..\lib\videomd.lib"

!ENDIF 

# Begin Target

# Name "videolibm - Win32 Release"
# Name "videolibm - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=.\gcal2.c
# End Source File
# Begin Source File

SOURCE=.\gcalc.c
# End Source File
# Begin Source File

SOURCE=.\gcalend.c
# End Source File
# Begin Source File

SOURCE=.\gclock.c
# End Source File
# Begin Source File

SOURCE=.\gnotepad.c
# End Source File
# Begin Source File

SOURCE=.\gpuzzle.c
# End Source File
# Begin Source File

SOURCE=.\gzones.c
# End Source File
# Begin Source File

SOURCE=.\sleepdos.c
# End Source File
# Begin Source File

SOURCE=.\strpos.c
# End Source File
# Begin Source File

SOURCE=.\valert.c
# End Source File
# Begin Source File

SOURCE=.\vbell.c
# End Source File
# Begin Source File

SOURCE=.\vcap.c
# End Source File
# Begin Source File

SOURCE=.\vcharset.c
# End Source File
# Begin Source File

SOURCE=.\vchstubs.c
# End Source File
# Begin Source File

SOURCE=.\vcommand.c
# End Source File
# Begin Source File

SOURCE=.\vcontrol.c
# End Source File
# Begin Source File

SOURCE=.\vcut.c
# End Source File
# Begin Source File

SOURCE=.\vdefer.c
# End Source File
# Begin Source File

SOURCE=.\vedge.c
# End Source File
# Begin Source File

SOURCE=.\verase.c
# End Source File
# Begin Source File

SOURCE=.\version.c
# End Source File
# Begin Source File

SOURCE=.\vexit.c
# End Source File
# Begin Source File

SOURCE=.\vfnkey.c
# End Source File
# Begin Source File

SOURCE=.\vgets0.c
# End Source File
# Begin Source File

SOURCE=.\vgrid.c
# End Source File
# Begin Source File

SOURCE=.\vinput.c
# End Source File
# Begin Source File

SOURCE=.\visdebug.c
# End Source File
# Begin Source File

SOURCE=.\vkeymap.c
# End Source File
# Begin Source File

SOURCE=.\vline.c
# End Source File
# Begin Source File

SOURCE=.\vlist.c
# End Source File
# Begin Source File

SOURCE=.\vloadch.c
# End Source File
# Begin Source File

SOURCE=.\vmacro.c
# End Source File
# Begin Source File

SOURCE=.\vmap.c
# End Source File
# Begin Source File

SOURCE=.\vmenu.c
# End Source File
# Begin Source File

SOURCE=.\vmode.c
# End Source File
# Begin Source File

SOURCE=.\vmove.c
# End Source File
# Begin Source File

SOURCE=.\vnewline.c
# End Source File
# Begin Source File

SOURCE=.\vonexit.c
# End Source File
# Begin Source File

SOURCE=.\vop.c
# End Source File
# Begin Source File

SOURCE=.\vopenf.c
# End Source File
# Begin Source File

SOURCE=.\vpaste.c
# End Source File
# Begin Source File

SOURCE=.\vpopscr.c
# End Source File
# Begin Source File

SOURCE=.\vprint.c
# End Source File
# Begin Source File

SOURCE=.\vpushscr.c
# End Source File
# Begin Source File

SOURCE=.\vputc.c
# End Source File
# Begin Source File

SOURCE=.\vrawntcn.c
# End Source File
# Begin Source File

SOURCE=.\vrefresh.c
# End Source File
# Begin Source File

SOURCE=.\vrelease.c
# End Source File
# Begin Source File

SOURCE=.\vroll.c
# End Source File
# Begin Source File

SOURCE=.\vscreen.c
# End Source File
# Begin Source File

SOURCE=.\vsection.c
# End Source File
# Begin Source File

SOURCE=.\vset.c
# End Source File
# Begin Source File

SOURCE=.\vsize.c
# End Source File
# Begin Source File

SOURCE=.\vslew.c
# End Source File
# Begin Source File

SOURCE=.\vstate.c
# End Source File
# Begin Source File

SOURCE=.\vsynch.c
# End Source File
# Begin Source File

SOURCE=.\vsystem.c
# End Source File
# Begin Source File

SOURCE=.\vtext.c
# End Source File
# Begin Source File

SOURCE=.\vtrace.c
# End Source File
# Begin Source File

SOURCE=.\vtrigger.c
# End Source File
# Begin Source File

SOURCE=.\vtrim.c
# End Source File
# Begin Source File

SOURCE=.\vuip.c
# End Source File
# Begin Source File

SOURCE=.\vuserex.c
# End Source File
# Begin Source File

SOURCE=.\vutil.c
# End Source File
# Begin Source File

SOURCE=.\vwait.c
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# Begin Source File

SOURCE=.\vcap.h
# End Source File
# Begin Source File

SOURCE=.\vchinese.h
# End Source File
# Begin Source File

SOURCE=.\vdata.h
# End Source File
# Begin Source File

SOURCE=.\verase.h
# End Source File
# Begin Source File

SOURCE=.\video.h
# End Source File
# Begin Source File

SOURCE=.\vintdef.h
# End Source File
# Begin Source File

SOURCE=.\vkeymap.h
# End Source File
# Begin Source File

SOURCE=.\vline.h
# End Source File
# Begin Source File

SOURCE=.\vlist.h
# End Source File
# Begin Source File

SOURCE=.\vlocal.h
# End Source File
# Begin Source File

SOURCE=.\vmenu.h
# End Source File
# Begin Source File

SOURCE=.\vmodules.h
# End Source File
# Begin Source File

SOURCE=.\vmove.h
# End Source File
# Begin Source File

SOURCE=.\vprint.h
# End Source File
# Begin Source File

SOURCE=.\vraw.h
# End Source File
# Begin Source File

SOURCE=.\vrawntcn.h
# End Source File
# Begin Source File

SOURCE=.\vscreen.h
# End Source File
# Begin Source File

SOURCE=.\vtrim.h
# End Source File
# Begin Source File

SOURCE=.\vutil.h
# End Source File
# End Group
# End Target
# End Project
