/*
******************************************************************************
** Copyright (c) Shell Stream Software, LLC. All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
** NOTICE:
** Confidential, unpublished property of Shell Stream Software, LLC.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission is strictly prohibited.
******************************************************************************
*/

/*
**	File:		wcommon.h
**
**	Project:	wisp
**
**	Purpose:	Common defines for all WISP sub-projects
**
*/

#ifndef WCOMMON_H
#define WCOMMON_H

/*
	WCOMMON.H		Define common defines for WISP and WISPLIB
*/

/*
	*** Search for "WISPVER" ***

	Change WISP_VERSION:
	- wispcommon/wcommon.h		WISP_VERSION
	- port/makewisp.umf		WISPVER
	- doc/wisp_packlist.txt		WISPVER
	- doc/wisp_relnotes.txt		WISPVER
	- doc/wisp_install_unix.txt	WISPVER
	- doc/wispntsetup.txt		WISPVER
	- ede/edentsetup.txt		WISPVER
	- acu/wruncbl.umf		WISPVER
	- acu/wacu.mak			WISPVER
	- kcsi/kcsi_relnotes.txt	WISPVER
	- kcsi/kcsintsetup.txt		WISPVER

	*** Search for "PRODUCTVERSION" ***
	
	Each RC file has four (4) places that need changing.
		- FILEVERSION 5, 1, 2, 1
		- PRODUCTVERSION 5, 1, 2, 1
		- VALUE "FileVersion", "5, 1, 2, 1"
		- VALUE "ProductVersion", "5.1.21"


	nt/wisptran/_WispWin.rc:	PRODUCTVERSION
	nt/wconfig/_ConfigUtil.rc:	PRODUCTVERSION
	vsedit/vsedit.rc:		PRODUCTVERSION
	wisptran/wisptran.rc:		PRODUCTVERSION
	wisputils/wcopy.rc:		PRODUCTVERSION
	wisputils/wrun.rc:		PRODUCTVERSION
	wisputils/wsort.rc:		PRODUCTVERSION
	wisputils/wlicense.rc:		PRODUCTVERSION
	wisputils/wshell.rc:		PRODUCTVERSION
	wisputils/wdiag.rc:		PRODUCTVERSION
	wisputils/display.rc:		PRODUCTVERSION
	wisputils/wusage.rc:		PRODUCTVERSION
	wproc/wproc.rc:			PRODUCTVERSION

*/
#define WISP_VERSION		"V5.1.21"	/* WISPTRAN version (char[20])		*/
#define WISP_VERSION_NUM	5121		/* WISPVER */
#define WISP_LICENSE_VERSION	51

#define WISP_SCREEN_VERSION	22		/* SCREEN version number.		*/

/* CHANGE-COPYRIGHT-DATE */
#define WISP_OWNER "Shell Stream Software LLC"
#define WISP_COPYRIGHT_YEAR_STR	"2023"
#define WISP_COPYRIGHT_YEAR_INT	2023
#define WISP_PHONE_NUMBER "(727) 474-4078"
#define WISP_EMAIL "wisp@shellstream.com"
#define WISP_WEBSITE "www.shellstream.com"

#define WISP_ADDRESS_STREET "586 Lakewood Drive"
#define WISP_ADDRESS_CITY_STATE_ZIP "Oldsmar, FL 34677"
#define WISP_ADDRESS_FULL "586 Lakewood Drive, Oldsmar, FL 34677"

/* 
**	NOTE: Screen version 20 and 21 are still supported.
**
** WISP_SCREEN_VERSION 20	- DISP-ITEM-LENGTH was PIC S9(4) binary.
** WISP_SCREEN_VERSION 21	- DISP-ITEM-LENGTH was PIC X, and compression greatly improved.
** WISP_SCREEN_VERSION 22	- New screen structure.
*/

/* #define IS_LIB	0x00000040 */		/* Generate a LIB only.			*/
/* #define IS_CASE_SEN	0x00000200 */		/* name is case sensitive don't change	*/
/* #define IS_NOTSHARE	0x00000400 */		/* File is not shared (no writers).	*/
/* #define IS_PRNAME	0x00000800 */		/* Its a call with a prname ref.	*/
/* #define IS_BACKFILL	0x00001000 */		/* Backfill file/lib/vol.		*/
/* #define IS_NODISPLAY	0x00008000 */		/* Select file "NODISPLAY".		*/
/* #define IS_NOEXTENSION 0x00400000 */		/* A file without an extension.		*/
/* #define IS_USE_PVPL    0x00000008 */		/* Use PV/PL Usage Constants		*/


#define IS_PRINTFILE	0x00000002	/* P */	/* printer file				*/
#define IS_INDEXED	0x00000100	/* I */	/* an indexed file			*/
#define IS_SEQSEQ	0x00002000	/* S */	/* It's a Sequential/Sequential file.	*/
#define IS_SEQDYN	0x00010000	/* Y */	/* File is SEQ/DYN without relative key.*/
#define IS_RELATIVE	0x01000000	/* R */	/* A Relative file			*/
#define IS_DBFILE	0x00800000	/* D */	/* A DATABASE file			*/

#define IS_OUTPUT	0x00000001	/* O */	/* output only file			*/
#define IS_IO		0x00000010	/* U */	/* This file is opened IO.		*/
#define IS_SORT		0x00000020	/* Z */	/* This file is s SORT file.		*/
#define IS_EXTEND	0x00200000	/* X */	/* File opened EXTEND.			*/

#define IS_GETPARM	0x00004000	/* G */	/* Has the initial hidden GP been done?	*/
#define IS_ERROR	0x00080000	/* E */	/* Set to force a GETPARM.		*/

#define IS_WORK		0x00020000	/* W */	/* Work file in WORKLIB on WORKVOL	*/
#define IS_SCRATCH	0x00000004	/* # */	/* Scratch #xxxx at end of linklevel    */
#define IS_TEMP		0x00040000	/* T */	/* A temporary filename (#xxxx)		*/

#define IS_NORESPECIFY	0x00000080	/* N */	/* No RESPECIFY Getparm			*/
#define IS_DECLARE	0x00100000	/* ! */	/* There are DECLARITIVES for this file */


#define WISP_FILE_ATTR_SIZE	10	/* PIC X(10) */

#define IS_PRINTFILE_ATTR	"P"	/* printer file				*/
#define IS_INDEXED_ATTR		"I"	/* an indexed file			*/
#define IS_SEQSEQ_ATTR		"S"	/* It's a Sequential/Sequential file.	*/
#define IS_SEQDYN_ATTR		"Y"	/* File is SEQ/DYN without relative key.*/
#define IS_RELATIVE_ATTR	"R"	/* A Relative file			*/
#define IS_DBFILE_ATTR		"D"	/* A DATABASE file			*/

#define IS_OUTPUT_ATTR		"O"	/* output only file			*/
#define IS_IO_ATTR		"U"	/* This file is opened IO.		*/
#define IS_SORT_ATTR		"Z"	/* This file is s SORT file.		*/
#define IS_EXTEND_ATTR		"X"	/* File opened EXTEND.			*/

#define IS_GETPARM_ATTR		"G"	/* Has the initial hidden GP been done?	*/
#define IS_ERROR_ATTR		"E"	/* Set to force a GETPARM.		*/

#define IS_WORK_ATTR		"W"	/* A WORK file.				*/
#define IS_SCRATCH_ATTR		"#"	/* scratch file	#xxxx			*/
#define IS_TEMP_ATTR		"T"	/* A temporary ##xxxx file		*/

#define IS_NORESPECIFY_ATTR	"N"	/* No RESPECIFY Getparm			*/
#define IS_DECLARE_ATTR		"!"	/* There are DECLARITIVES for this file */


#define	WFOPEN_UNKNOWN		0
#define	WFOPEN_INPUT		1
#define	WFOPEN_SHARED		2
#define	WFOPEN_OUTPUT		3
#define	WFOPEN_EXTEND		4
#define	WFOPEN_SPECIAL_INPUT	5
#define	WFOPEN_I_O		6
#define	WFOPEN_SORT		7


#define COB_FILEOP_SIZE	20
#define COB_SELECT_NAME_SIZE 40
#define COB_EXTENDED_FILE_STATUS_SIZE 10
#define WISP_VERSION_SIZE 20


#endif	/* WCOMMON_INCLUDED */
