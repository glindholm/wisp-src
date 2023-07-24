#define	Junkcmt00100	"This file is included by makefiles, vers.h, acugt.odl,"
#define	Junkcmt00200	"and install.rul, so make sure you do not put"
#define	Junkcmt00300	"anything in it that might cause problems for these."

#define	Junkcmt00400	"$Id: vers.inc 77243 2020-06-05 07:16:07Z mhanson $"
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

#define	Junkcmt00900	"See $TOP/00README.html for a list of things to update"
#define	Junkcmt01000	"by hand."

#define	VERS_MAJOR	10
#define	VERS_MINOR	3
#define	VERS_RELEASE	1
#define	VERS_BUILD	11706
#define	VERS_REVISION	77242

#define	JunkInfo00100	"These macros are only used in PAM, and can be gotten"
#define	JunkInfo00200	"rid of once cleaned there."

#define	DLL_FILEVERS	"10, 3, 1, 11706"
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

#define ACU_RELEASE	"10.3.1"

#define	JunkcmtASR0001	"ACU_SUBRELEASE should be set to Alpha (YYYY-MM-DD)"
#define	JunkcmtASR0002	"for pre-beta versions, Beta (YYYY-MM-DD) for beta"
#define	JunkcmtASR0003	"versions, and the null string for release versions."

#define ACU_SUBRELEASE	"Alpha (2020-06-05)"

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

#define JunkcmtMAK0001 "Even though the following has #if 0 it still gets used"
#define JunkcmtMAK0002 "by makefiles like mstr32.mak.  The values need to"
#define JunkcmtMAK0003 "reflect the current product version.  The UUID/GUIDS"
#define JunkcmtMAK0004 "need to be changed each release.  To create new GUIDS"
#define JunkcmtMAK0005 "in Visual Studio select Tools/Create GUID"
#define JunkcmtMAK0006 "Registry Format."

#if	0
COMMENT____THESE_ARE_USED_IN_MAKEFILES =1
DLL_FILEVERS =10,3,1,11706
VERS_STRING =1031
LICENSE_VERSION	=10.3.1
VERS_MAJOR =10
VERS_MINOR =3
VERS_RELEASE =1
COMMENT____THE_FOLLOWING_UUID_GUIDS_NEED_TO_BE_CHANGED_EACH_RELEASE =1
EXTENDGUIDX86     =D830D375-BA4C-4567-BE34-D1BB694D5B66
EXTENDGUIDX64     =34374FF1-266B-4CB7-BA77-37BE8B66C701
ACUXDBCGUIDX86    =2F3F4A3F-34F1-4F42-985C-F28361F3D6DF
ACUXDBCGUIDX64    =2B79F471-1D8B-43F0-B4E4-16E374CD4B8F
ACUTHINGUIDX86    =7518DCD2-4E4D-46CC-8BB2-BD6741BD6FC2
ATWDESKTOPGUIDX86 =7AC973A2-2B20-4C87-BC31-82E926F974D3
#endif
