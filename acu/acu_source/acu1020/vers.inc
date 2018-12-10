#define	Junkcmt00100	"This file is included by makefiles, vers.h, acugt.odl,"
#define	Junkcmt00200	"and install.rul, so make sure you do not put"
#define	Junkcmt00300	"anything in it that might cause problems for these."

#define	Junkcmt00400	"$Id: vers.inc 73176 2018-03-12 07:15:30Z mark $"
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

#define	Junkcmt00900	"There is also a version number and copyright date"
#define	Junkcmt01000	"in sample/alfred/source/alfred.cbl and"
#define	Junkcmt01100	"vision/relink/Makefile.kit."

#define Junkcmt02000	"Please update any UUID values as well. In particular,"
#define Junkcmt02100	"dotnet/marshal/ParmStore.h.  UUIDs are of the form"
#define	Junkcmt02200	"NNNNNNNN-NNNN-NNNN-VVVV-NNNNNNNNNNNN. The VVVV value"
#define	Junkcmt02300	"is a hexadecimal representation of the version."
#define	Junkcmt02400	"So 10.2.0 becomes 0A20, and 11.4.3 would become 0B43."
#define	Junkcmt02500	"In ParmStore there are 4 UUIDs to update."

#define	Junkcmt02600	"Please update CURRENT_EPOCH in patchall/keypack.c when"
#define	Junkcmt02700	"VERS_MAJOR or VERS_MINOR changes. This value should be"
#define	Junkcmt02800	"the current or last year, depending on whether or not"
#define	Junkcmt02900	"we are close to the end of the year."

#define	VERS_MAJOR	10
#define	VERS_MINOR	2
#define	VERS_RELEASE	0
#define	VERS_BUILD	7639
#define	VERS_REVISION	73175

#define	JunkInfo00100	"These macros are only used in PAM, and can be gotten"
#define	JunkInfo00200	"rid of once cleaned there."

#define	DLL_FILEVERS	"10, 2, 0, 7639"
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

#define JunkcmtAR0004	"We cannot replace this with a macro value, since the"
#define	JunkcmtAR0005	"value is used directly in misc/unix/makefile.in and"
#define	JunkcmtAR0006	"vfilt."

#define ACU_RELEASE	"10.2.0"

#define	JunkcmtASR0001	"ACU_SUBRELEASE should be set to Alpha (YYYY-MM-DD)"
#define	JunkcmtASR0002	"for pre-beta versions, Beta (YYYY-MM-DD) for beta"
#define	JunkcmtASR0003	"versions, and the null string for release versions."

#define ACU_SUBRELEASE	"Alpha (2018-03-12)"

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

#define JunkcmtMAK0001 "Even though the following has #if 0 it still gets used"
#define JunkcmtMAK0002 "by makefiles like mstr32.mak.  The values need to"
#define JunkcmtMAK0003 "reflect the current product version.  The UUID/GUIDS"
#define JunkcmtMAK0004 "need to be changed each release.  To create new GUIDS"
#define JunkcmtMAK0005 "in Visual Studio select Tools/Create GUID"
#define JunkcmtMAK0006 "Registry Format."

#if	0
COMMENT____THESE_ARE_USED_IN_MAKEFILES =1
DLL_FILEVERS =10,2,0,7639
VERS_STRING =1020
LICENSE_VERSION	=10.2.0
VERS_MAJOR =10
VERS_MINOR =2
VERS_RELEASE =0
COMMENT____THE_FOLLOWING_UUID_GUIDS_NEED_TO_BE_CHANGED_EACH_RELEASE =1
EXTENDGUIDX86 =5A1064F3-5CE3-4CA2-BB88-02694D2F1E0B
EXTENDGUIDX64 =ADB4D480-80DD-4E48-9BE1-9DAE5587F0CB
ACUXDBCGUIDX86 =C153E849-6E53-42D4-AE4B-54CC6D21125D
ACUXDBCGUIDX64 =5942494E-F0EA-41D1-9E80-D34AED3DB016
ACUTHINGUIDX86 =B20C519F-D697-41FF-8F55-4C29C2612392
ATWDESKTOPGUIDX86 =EA898B6C-BA35-42E9-8E5F-68F8EB941023
#endif
