# Microsoft Developer Studio Project File - Name="wproc" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=wproc - Win32 Release
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "wproc.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "wproc.mak" CFG="wproc - Win32 Release"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "wproc - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "wproc - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "wproc - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir ".\Release"
# PROP BASE Intermediate_Dir ".\Release"
# PROP BASE Target_Dir "."
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir ".\Release"
# PROP Intermediate_Dir ".\Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir "."
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /YX /c
# ADD CPP /nologo /MD /W3 /GX /O2 /I "..\wispcommon" /I "..\videolib" /D "NDEBUG" /D "WIN32" /D "_CONSOLE" /D "DEBUG" /D "WINNT" /D "MSFS" /D "WANG" /D WPROC_VERSION=2000 /U "DOS" /YX /FD /c
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386 /out:"..\bin\wproc.exe"
# SUBTRACT LINK32 /incremental:yes

!ELSEIF  "$(CFG)" == "wproc - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir ".\Debug"
# PROP BASE Intermediate_Dir ".\Debug"
# PROP BASE Target_Dir "."
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir ".\Debug"
# PROP Intermediate_Dir ".\Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir "."
# ADD BASE CPP /nologo /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /YX /c
# ADD CPP /nologo /MDd /W3 /Gm /GX /ZI /Od /I "..\wispcommon" /I "..\videolib" /D "_DEBUG" /D "WIN32" /D "_CONSOLE" /D "DEBUG" /D "WINNT" /D "MSFS" /D "WANG" /D WPROC_VERSION=2000 /U "DOS" /FR /YX /FD /c
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /incremental:no /debug /machine:I386 /out:"..\bin\wproc.exe"

!ENDIF 

# Begin Target

# Name "wproc - Win32 Release"
# Name "wproc - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;hpj;bat;for;f90"
# Begin Source File

SOURCE=.\args.cpp
# End Source File
# Begin Source File

SOURCE=.\args.hpp
# End Source File
# Begin Source File

SOURCE=.\assert.cpp
# End Source File
# Begin Source File

SOURCE=.\assert.hpp
# End Source File
# Begin Source File

SOURCE=.\attribs.hpp
# End Source File
# Begin Source File

SOURCE=.\builtin.cpp
# End Source File
# Begin Source File

SOURCE=.\builtin.hpp
# End Source File
# Begin Source File

SOURCE=.\cancel.cpp
# End Source File
# Begin Source File

SOURCE=.\cancel.hpp
# End Source File
# Begin Source File

SOURCE=.\colors.cpp
# End Source File
# Begin Source File

SOURCE=.\colors.hpp
# End Source File
# Begin Source File

SOURCE=.\compiler.cpp
# End Source File
# Begin Source File

SOURCE=.\compiler.hpp
# End Source File
# Begin Source File

SOURCE=.\critical.cpp
# End Source File
# Begin Source File

SOURCE=.\critical.hpp
# End Source File
# Begin Source File

SOURCE=.\crt_io.cpp
# End Source File
# Begin Source File

SOURCE=.\crt_io.hpp
# End Source File
# Begin Source File

SOURCE=.\data.cpp
# End Source File
# Begin Source File

SOURCE=.\data.hpp
# End Source File
# Begin Source File

SOURCE=.\debugaid.cpp
# End Source File
# Begin Source File

SOURCE=.\debugaid.hpp
# End Source File
# Begin Source File

SOURCE=.\demovali.h
# End Source File
# Begin Source File

SOURCE=.\driver.cpp
# End Source File
# Begin Source File

SOURCE=.\e_assign.cpp
# End Source File
# Begin Source File

SOURCE=.\e_decl.cpp
# End Source File
# Begin Source File

SOURCE=.\e_math.cpp
# End Source File
# Begin Source File

SOURCE=.\e_os.cpp
# End Source File
# Begin Source File

SOURCE=.\e_run.cpp
# End Source File
# Begin Source File

SOURCE=.\e_scrnio.cpp
# End Source File
# Begin Source File

SOURCE=.\e_substr.cpp
# End Source File
# Begin Source File

SOURCE=.\e_ttyio.cpp
# End Source File
# Begin Source File

SOURCE=.\e_varref.cpp
# End Source File
# Begin Source File

SOURCE=.\e_wang.cpp
# End Source File
# Begin Source File

SOURCE=.\environ.hpp
# End Source File
# Begin Source File

SOURCE=.\envs.h
# End Source File
# Begin Source File

SOURCE=.\execute.cpp
# End Source File
# Begin Source File

SOURCE=.\exp.cpp
# End Source File
# Begin Source File

SOURCE=.\exp.hpp
# End Source File
# Begin Source File

SOURCE=.\ext.hpp
# End Source File
# Begin Source File

SOURCE=.\fileinfo.cpp
# End Source File
# Begin Source File

SOURCE=.\fileinfo.hpp
# End Source File
# Begin Source File

SOURCE=.\files.cpp
# End Source File
# Begin Source File

SOURCE=.\files.hpp
# End Source File
# Begin Source File

SOURCE=.\filext.h
# End Source File
# Begin Source File

SOURCE=.\fixup.cpp
# End Source File
# Begin Source File

SOURCE=.\fixup.hpp
# End Source File
# Begin Source File

SOURCE=.\getopt.h
# End Source File
# Begin Source File

SOURCE=.\holdev.h
# End Source File
# Begin Source File

SOURCE=.\idsistd.h
# End Source File
# Begin Source File

SOURCE=.\idsisubs.h
# End Source File
# Begin Source File

SOURCE=.\input.cpp
# End Source File
# Begin Source File

SOURCE=.\input.hpp
# End Source File
# Begin Source File

SOURCE=.\install.cpp
# End Source File
# Begin Source File

SOURCE=.\install.hpp
# End Source File
# Begin Source File

SOURCE=.\intdef.h
# End Source File
# Begin Source File

SOURCE=.\isspace.cpp
# End Source File
# Begin Source File

SOURCE=.\keyboard.hpp
# End Source File
# Begin Source File

SOURCE=.\level.h
# End Source File
# Begin Source File

SOURCE=.\link.h
# End Source File
# Begin Source File

SOURCE=.\machine.cpp
# End Source File
# Begin Source File

SOURCE=.\machine.hpp
# End Source File
# Begin Source File

SOURCE=.\memcheck.cpp
# End Source File
# Begin Source File

SOURCE=.\memcheck.hpp
# End Source File
# Begin Source File

SOURCE=.\memory.cpp
# End Source File
# Begin Source File

SOURCE=.\memory.hpp
# End Source File
# Begin Source File

SOURCE=.\miscsubs.c
# End Source File
# Begin Source File

SOURCE=.\miscsubs.h
# End Source File
# Begin Source File

SOURCE=.\network.cpp
# End Source File
# Begin Source File

SOURCE=.\network.hpp
# End Source File
# Begin Source File

SOURCE=.\object.cpp
# End Source File
# Begin Source File

SOURCE=.\object.hpp
# End Source File
# Begin Source File

SOURCE=.\opcodes.cpp
# End Source File
# Begin Source File

SOURCE=.\opcodes.hpp
# End Source File
# Begin Source File

SOURCE=.\options.cpp
# End Source File
# Begin Source File

SOURCE=.\options.hpp
# End Source File
# Begin Source File

SOURCE=.\p_assign.cpp
# End Source File
# Begin Source File

SOURCE=.\p_ctrl.cpp
# End Source File
# Begin Source File

SOURCE=.\p_decl.cpp
# End Source File
# Begin Source File

SOURCE=.\p_exp.cpp
# End Source File
# Begin Source File

SOURCE=.\p_os.cpp
# End Source File
# Begin Source File

SOURCE=.\p_run.cpp
# End Source File
# Begin Source File

SOURCE=.\p_scrnio.cpp
# End Source File
# Begin Source File

SOURCE=.\p_ttyio.cpp
# End Source File
# Begin Source File

SOURCE=.\p_var.cpp
# End Source File
# Begin Source File

SOURCE=.\p_wang.cpp
# End Source File
# Begin Source File

SOURCE=.\paths.h
# End Source File
# Begin Source File

SOURCE=.\pcode.cpp
# End Source File
# Begin Source File

SOURCE=.\pcode.hpp
# End Source File
# Begin Source File

SOURCE=.\platsubs.h
# End Source File
# Begin Source File

SOURCE=.\procedur.cpp
# End Source File
# Begin Source File

SOURCE=.\procedur.hpp
# End Source File
# Begin Source File

SOURCE=.\process.cpp
# End Source File
# Begin Source File

SOURCE=.\process.hpp
# End Source File
# Begin Source File

SOURCE=.\product.cpp
# End Source File
# Begin Source File

SOURCE=.\product.hpp
# End Source File
# Begin Source File

SOURCE=.\prompt.h
# End Source File
# Begin Source File

SOURCE=.\putparm.h
# End Source File
# Begin Source File

SOURCE=.\range.cpp
# End Source File
# Begin Source File

SOURCE=.\range.hpp
# End Source File
# Begin Source File

SOURCE=.\reader.cpp
# End Source File
# Begin Source File

SOURCE=.\reader.hpp
# End Source File
# Begin Source File

SOURCE=.\report.cpp
# End Source File
# Begin Source File

SOURCE=.\report.hpp
# End Source File
# Begin Source File

SOURCE=.\ring.h
# End Source File
# Begin Source File

SOURCE=.\runtype.h
# End Source File
# Begin Source File

SOURCE=.\scanner.cpp
# End Source File
# Begin Source File

SOURCE=.\scanner.hpp
# End Source File
# Begin Source File

SOURCE=.\scnfacs.h
# End Source File
# Begin Source File

SOURCE=.\scope.cpp
# End Source File
# Begin Source File

SOURCE=.\scope.hpp
# End Source File
# Begin Source File

SOURCE=.\screen.cpp
# End Source File
# Begin Source File

SOURCE=.\screen.hpp
# End Source File
# Begin Source File

SOURCE=.\sharemem.h
# End Source File
# Begin Source File

SOURCE=.\sortseqf.h
# End Source File
# Begin Source File

SOURCE=.\stack.cpp
# End Source File
# Begin Source File

SOURCE=.\stack.hpp
# End Source File
# Begin Source File

SOURCE=.\state.cpp
# End Source File
# Begin Source File

SOURCE=.\state.hpp
# End Source File
# Begin Source File

SOURCE=.\stateobj.cpp
# End Source File
# Begin Source File

SOURCE=.\stateobj.hpp
# End Source File
# Begin Source File

SOURCE=.\status.hpp
# End Source File
# Begin Source File

SOURCE=.\subops.hpp
# End Source File
# Begin Source File

SOURCE=.\symbols.cpp
# End Source File
# Begin Source File

SOURCE=.\symbols.hpp
# End Source File
# Begin Source File

SOURCE=.\sysdev.h
# End Source File
# Begin Source File

SOURCE=.\sysenv.cpp
# End Source File
# Begin Source File

SOURCE=.\sysenv.hpp
# End Source File
# Begin Source File

SOURCE=.\tables.cpp
# End Source File
# Begin Source File

SOURCE=.\tables.hpp
# End Source File
# Begin Source File

SOURCE=.\token.cpp
# End Source File
# Begin Source File

SOURCE=.\token.hpp
# End Source File
# Begin Source File

SOURCE=.\tracer.cpp
# End Source File
# Begin Source File

SOURCE=.\tracer.hpp
# End Source File
# Begin Source File

SOURCE=.\txt.cpp
# End Source File
# Begin Source File

SOURCE=.\txt.hpp
# End Source File
# Begin Source File

SOURCE=.\utility.cpp
# End Source File
# Begin Source File

SOURCE=.\utility.hpp
# End Source File
# Begin Source File

SOURCE=.\video_rt.h
# End Source File
# Begin Source File

SOURCE=.\vssort.h
# End Source File
# Begin Source File

SOURCE=.\vwang.h
# End Source File
# Begin Source File

SOURCE=.\wang_os.cpp
# End Source File
# Begin Source File

SOURCE=.\wang_os.hpp
# End Source File
# Begin Source File

SOURCE=.\wangfile.cpp
# End Source File
# Begin Source File

SOURCE=.\wangfile.hpp
# End Source File
# Begin Source File

SOURCE=.\wangkeys.h
# End Source File
# Begin Source File

SOURCE=.\wanguid.h
# End Source File
# Begin Source File

SOURCE=.\wcommon.h
# End Source File
# Begin Source File

SOURCE=.\wdefines.h
# End Source File
# Begin Source File

SOURCE=.\werrlog.h
# End Source File
# Begin Source File

SOURCE=.\wexit.h
# End Source File
# Begin Source File

SOURCE=.\wfiles.h
# End Source File
# Begin Source File

SOURCE=.\wfname.h
# End Source File
# Begin Source File

SOURCE=.\wglobals.h
# End Source File
# Begin Source File

SOURCE=.\wisp_pic.h
# End Source File
# Begin Source File

SOURCE=.\wisp_rts.h
# End Source File
# Begin Source File

SOURCE=..\wispcommon\wispicon.rc
# End Source File
# Begin Source File

SOURCE=.\wisplib.h
# End Source File
# Begin Source File

SOURCE=.\wispvers.h
# End Source File
# Begin Source File

SOURCE=.\wlicense.h
# End Source File
# Begin Source File

SOURCE=.\wperson.h
# End Source File
# Begin Source File

SOURCE=.\wrunconf.h
# End Source File
# Begin Source File

SOURCE=.\wsysconf.h
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
