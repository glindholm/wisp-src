#define	Junkcmt00100	"This file is included by makefiles, vers.h, acugt.odl,"
#define	Junkcmt00200	"and install.rul, so make sure you do not put"
#define	Junkcmt00300	"anything in it that might cause problems for these."

#define	Junkcmt00400	"$Id: vers.inc 54250 2008-09-22 07:15:56Z mark $"

#define	Junkcmt00500	"See Randy for details"

#define	Junkcmt00600	"Note that '#' is a comment character in makefiles."

#define	Junkcmt00700	"Many different ways of defining the version"
#define	Junkcmt00800	"See various .rc files for some of the uses of this"

#define	Junkcmt00900	"Make sure VERS_BUILD is the same as the 4th digit"
#define	Junkcmt01000	"in DLL_FILEVERS."

#define	VERS_MAJOR	8
#define	VERS_MINOR	1
#define	VERS_RELEASE	0
#define	VERS_BUILD	2576

#define	DLL_FILEVERS	"8, 1, 0, 2576"

#ifndef	INSTALLSHIELD
#define	OBJ_VERSION	((VERS_MAJOR) * 10 + (VERS_MINOR))
#endif

#define REL_MAJORMINOR		"81"
#define REL_MAJORMINORREL	"810"
#define REL_MAJORDOTMINOR	"8.1"
#define REL_MAJORDOTMINORDOTREL	"8.1.0"
#ifndef	INSTALLSHIELD
#define	REL_MAJDOTMIN		8.1
#endif

#define	Junkcmtlic0001	"As of 8.1 all products fall back to 8.0 licenses"
#define	Junkcmtlic0002	"See Gisle for details"
#define	Junkcmtlic0003	"To turn off this 'feature', undefine USE_V80_LICENSE_ALWAYS"

#define	USE_V80_LICENSE_ALWAYS	1
#ifndef	USE_V80_LICENSE_ALWAYS
#define	VERS_MAJOR_LIC			VERS_MAJOR
#define	VERS_MINOR_LIC			VERS_MINOR
#define	REL_MAJORMINOR_LIC		REL_MAJORMINOR
#define	REL_MAJORMINORREL_LIC		REL_MAJORMINORREL
#define	REL_MAJORDOTMINOR_LIC		REL_MAJORDOTMINOR
#define	REL_MAJORDOTMINORDOTREL_LIC	REL_MAJORDOTMINORDOTREL
#define	ACU_LIBRARY_LIC	"ACUCOBOL-GT Library " REL_MAJORDOTMINOR ".0"
#else
#define	VERS_MAJOR_LIC			8
#define	VERS_MINOR_LIC			0
#define	REL_MAJORMINOR_LIC		"80"
#define	REL_MAJORMINORREL_LIC		"800"
#define	REL_MAJORDOTMINOR_LIC		"8.0"
#define	REL_MAJORDOTMINORDOTREL_LIC	"8.0.0"
#define	ACU_LIBRARY_LIC	"ACUCOBOL-GT Library " REL_MAJORDOTMINOR_LIC ".0"
#endif /* USE_V80_LICENSE_ALWAYS */


#define	JunkcmtAR0001	"ACU_RELEASE should be set to the three-digit"
#define	JunkcmtAR0002	"version number.  For patch versions, append"
#define	JunkcmtAR0003	"+ ECN-####."

#define JunkcmtAR0004   "Note: ...\stdlib\vers.cs needs to be updated as well"

#define ACU_RELEASE	"8.1.0"

#define MF_NAME                 "Micro Focus"

#define	JunkcmtASR0001	"ACU_SUBRELEASE should be set to Alpha (YYYY-MM-DD)"
#define	JunkcmtASR0002	"for pre-beta versions, Beta (YYYY-MM-DD) for beta"
#define	JunkcmtASR0003	"versions, and the null string for release versions."

#define ACU_SUBRELEASE	""

#define	ACU_COMPANYNAME	"Micro Focus"
#define	ACU_COPYRIGHT	"Copyright (C) 1984-2008 Micro Focus (IP) Ltd."
#define ACU_PRODUCTNAME	"ACUCOBOL-GT"

#define	REG_COMPANYNAME	"Acucorp"
#define	REG_GTPRODNAME	"Acucobol-GT"

#ifndef	INSTALLSHIELD
#define	JunkcmtATC0001	"ACUTHIN_SETUP_PRODUCTCODE includes the three-digit"
#define	JunkcmtATC0002	"version number. This product code must be embedded in"
#define	JunkcmtATC0003	"the acuthin.msi installation file. To do this, open"
#define	JunkcmtATC0004	"misc/win/cobolgt.sln in Visual Studio and set the"
#define	JunkcmtATC0005	"ProductCode property of the acuthin_setup project"
#define	ACUTHIN_SETUP_PRODUCTCODE "{D7056" REL_MAJORMINORREL "-F76C-44AA-A7AF-EEA87541DC9D}"

#define	JunkcmtAL0001	"The ACU_LIBRARY macro will, as of version 7.0, always"
#define	JunkcmtAL0002	"be treated as if it ended in a release version of .0"
#define	JunkcmtAL0003	"Note that this doesn't need to be changed any longer"
#define	JunkcmtAL0004	"when a new version is branched.  --RLZ"
#define	ACU_LIBRARY	"ACUCOBOL-GT Library " REL_MAJORDOTMINOR ".0"

#define	ACU_CLSID BFBBB600-C4DB-11d2-B11B-74F606C10000
#define	ACUAX_CLSID 077C768D-64C1-4AC1-845D-4589B4B2C24E
#define	ACUTCAX_CLSID 087C768D-64C1-4AC1-845D-4589B4B2C24E
#endif

#define	ACU_RUNTIME_FILEDESCRIPTION	"ACUCOBOL-GT 32-bit Runtime"
#define	ACU_THIN_FILEDESCRIPTION	"ACUCOBOL-GT Thin Client"
#define	ACU_COMPILER_FILEDESCRIPTION	"ACUCOBOL-GT COBOL compiler"
#define	ACU_ACTIVATOR_FILEDESCRIPTION	"ACUCOBOL-GT License Activator"
#define	ACU_ACTIVEX_FILEDESCRIPTION	"ACUCOBOL-GT Web Runtime"
#define	ACU_THINCLIENTAX_FILEDESCRIPTION	"ACUCOBOL-GT Web Thin Client"
#define	ACU_SERVER_FILEDESCRIPTION	"AcuServer"
#define	ACU_RCL_FILEDESCRIPTION		"ACUCOBOL-GT Remote COBOL Launcher"

#define	JunkcmtRC01000	"This next section is for resources we use globally"
#define	IDD_PLUGIN_DIALOG	201
#define IDI_PLUGIN_ICON1	202
#define IDC_PLUGIN_OK_BTN	2000
#define IDC_PLUGIN_CANCEL_BTN	2001
#define IDC_UNRECOGNIZEDAPPLET_STATIC	2002
#define IDC_SECURITYMESSAGE_STATIC	2003
#define IDC_CONTINUEQUESTION_STATIC	2004

#if	0
THESE_ARE_USED_IN_MAKEFILES =1
DLL_FILEVERS =8,1,0,2576
VERS_STRING =810
#ifndef	INSTALLSHIELD
LICENSE_VERSION	=8.1.0
#endif
#endif
