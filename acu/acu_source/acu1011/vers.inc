#define	Junkcmt00100	"This file is included by makefiles, vers.h, acugt.odl,"
#define	Junkcmt00200	"and install.rul, so make sure you do not put"
#define	Junkcmt00300	"anything in it that might cause problems for these."

#define	Junkcmt00400	"$Id: vers.inc 71897 2017-08-11 07:15:29Z mark $"
#define	Junkcmt00500	"See Randy for details"

#define	Junkcmt00600	"Note that '#' is a comment character in makefiles."

#define	Avers_xstr(a)	Avers_str(a)
#define	Avers_str(a)	#a

#if	SOURCE_IS_TAGGED
#define	CARMA_BUILD	1
#else	/* SOURCE_IS_TAGGED */
#define	CARMA_BUILD	0
#endif	/* SOURCE_IS_TAGGED */

#define	Junkcmt00700	"Many different ways of defining the version"
#define	Junkcmt00800	"See various .rc files for some of the uses of this"

#define	Junkcmt01100	"There is also a version number and copyright date"
#define	Junkcmt01200	"in sample/alfred/source/alfred.cbl and"
#define	Junkcmt01300	"vision/relink/Makefile.kit."

#define	VERS_MAJOR	10
#define	VERS_MINOR	1
#define	VERS_RELEASE	1
#define	VERS_BUILD	6360

#define	JunkInfo00100	These macros are only used in PAM, and can be gotten rid of once cleaned there.
#define	DLL_FILEVERS	"10, 1, 1, 6360"
#define	ACU_COMPANYNAME	"Micro Focus"
#define	ACU_COPYRIGHT	"Copyright (C) Micro Focus 1984-2015. All rights reserved."
#define	ACU_PRODUCTNAME	"ACUCOBOL-GT"


#define	OBJ_VERSION	((VERS_MAJOR) * 10 + (VERS_MINOR))

#define REL_MAJORMINOR		Avers_xstr(VERS_MAJOR) Avers_xstr(VERS_MINOR)
#define REL_MAJORMINORREL	Avers_xstr(VERS_MAJOR) Avers_xstr(VERS_MINOR) Avers_xstr(VERS_RELEASE)
#define REL_MAJORDOTMINOR	Avers_xstr(VERS_MAJOR) "." Avers_xstr(VERS_MINOR)
#define REL_MAJORDOTMINORDOTREL	Avers_xstr(VERS_MAJOR) "." Avers_xstr(VERS_MINOR) "." Avers_xstr(VERS_RELEASE)
#define	REL_MAJDOTMIN		VERS_MAJOR.VERS_MINOR

#define	JunkcmtAR0001	"ACU_RELEASE should be set to the three-digit"
#define	JunkcmtAR0002	"version number.  For patch versions, append"
#define	JunkcmtAR0003	"+ ECN-####."

#define Junkcmt01400	We cannot replace this with a macro value, since the value is used directly in misc/unix/makefile.in and vfilt
#define ACU_RELEASE	"10.1.1"

#define	JunkcmtASR0001	"ACU_SUBRELEASE should be set to Alpha (YYYY-MM-DD)"
#define	JunkcmtASR0002	"for pre-beta versions, Beta (YYYY-MM-DD) for beta"
#define	JunkcmtASR0003	"versions, and the null string for release versions."

#define ACU_SUBRELEASE	"Alpha (2017-08-11)"

#if	CARMA_BUILD
#define	COMPANY_NAME	CARMA_COMPANY_NAME
#else	/* CARMA_BUILD */
#define	COMPANY_NAME	"Micro Focus"
#endif	/* CARMA_BUILD */

#define	JunkcmtAL0001	"The ACU_LIBRARY macro will, as of version 7.0, always"
#define	JunkcmtAL0002	"be treated as if it ended in a release version of .0"
#define	JunkcmtAL0003	"Note that this doesn't need to be changed any longer"
#define	JunkcmtAL0004	"when a new version is branched.  --RLZ"
#define	ACU_LIBRARY	"ACUCOBOL-GT Library " REL_MAJORDOTMINOR ".0"

#define	ACU_CLSID BFBBB600-C4DB-11d2-B11B-74F606C10000
#define	ACUAX_CLSID 077C768D-64C1-4AC1-845D-4589B4B2C24E
#define	ACUTCAX_CLSID 087C768D-64C1-4AC1-845D-4589B4B2C24E

#define JunkcmtMAK0001 "Event though the following has #if 0 it still gets used"
#define JunkcmtMAK0002 "by makefiles like mstr32.mak.  The values need to"
#define JunkcmtMAK0003 "reflect the current product version.  The UUID/GUIDS"
#define JunkcmtMAK0004 "need to be changed each release.  To create new GUIDS"
#define JunkcmtMAK0005 "in Visual Studio select Tools/Create GUID"
#define JunkcmtMAK0006 "Registry Format."

#if	0
COMMENT____THESE_ARE_USED_IN_MAKEFILES =1
DLL_FILEVERS =10,1,1,6360
VERS_STRING =1011
LICENSE_VERSION	=10.1.1
VERS_MAJOR =10
VERS_MINOR =1
VERS_RELEASE =1
COMMENT____THE_FOLLOWING_UUID_GUIDS_NEED_TO_BE_CHANGED_EACH_RELEASE =1
EXTENDGUIDX86 =1AB45659-9724-4307-8490-6D6157539427
EXTENDGUIDX64 =4F336976-A3AA-4D43-87F9-4019C4F10697
ACUXDBCGUIDX86 =774646C6-08B6-4439-836C-A226C3CC8F1F
ACUXDBCGUIDX64 =262AFF6F-CFBD-412B-A2DC-1E6A65639886
ACUTHINGUIDX86 =9A60E4EE-5565-45E5-BF31-3AD601176048
ATWDESKTOPGUIDX86 =7A1E7CA3-5C78-4819-BA7E-D349649B7268
#endif
