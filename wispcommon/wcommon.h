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
 * $Id:$
 * $Source:$
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
	Change WISP_VERSION:
	- wispcommon/wcommon.h		WISP_VERSION
	- port/makewisp.umf		WISPVER
	- doc/wisp_packlist.txt
	- doc/wisp_relnotes.txt
	- doc/wisp_install_unix.txt
	- doc/wispntsetup.txt
	- ede/edentsetup.txt
	- acu/wruncbl.umf
	- acu/wrun32wisp_acu50.mak
	- acu/wrun32wisp_acu51.mak
	- acu/wrun32wisp_acu52.mak
	- acu/wrun32wisp_ede_acu50.mak
	- acu/wrun32wisp_ede_acu51.mak
	- acu/wrun32wisp_ede_acu52.mak
	- acu/wrun32wisp_kcsi_acu50.mak
	- acu/wrun32wisp_kcsi_acu51.mak
	- acu/wrun32wisp_kcsi_acu52.mak
	- acu/wacu.mak
	- acu/run32wisp_acu52.mak
	- kcsi/kcsi_relnotes.txt
	- kcsi/kcsintsetup.txt
*/
#define WISP_VERSION		"V5.1.00"	/* WISPTRAN version (char[20])		*/
#define WISP_VERSION_NUM	5100
#define WISP_LICENSE_VERSION	51

#define WISP_SCREEN_VERSION	22		/* SCREEN version number.		*/

/* CHANGE-COPYRIGHT-DATE */
#define WISP_OWNER "Shell Stream Software LLC"
#define WISP_COPYRIGHT_YEAR_STR	"2010"
#define WISP_COPYRIGHT_YEAR_INT	2010
#define WISP_PHONE_NUMBER "(813) 425-6344"
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

/*
**	History:
**	$Log: wcommon.h,v $
**	Revision 1.89  2010/01/16 02:04:28  gsl
**	new release
**	wisp 5.1.00
**	kcsi 4.2.00
**	
**	Revision 1.88  2010/01/10 17:18:28  gsl
**	Copyright year 2010
**	
**	Revision 1.87  2009/10/18 19:23:53  gsl
**	5.0.90
**	
**	Revision 1.86  2009/10/17 15:05:32  gsl
**	update phone number
**	
**	Revision 1.85  2009/07/19 20:53:51  gsl
**	Shell Stream
**	
**	Revision 1.84  2007/08/06 17:48:55  gsl
**	Version 5.0.54
**	
**	Revision 1.83  2007/01/03 14:11:44  gsl
**	copyright 2007
**	
**	Revision 1.82  2005/07/11 15:10:34  gsl
**	Moved to Suite 600
**	
**	Revision 1.81  2005/01/03 19:12:06  gsl
**	Copyright year 2005
**	
**	Revision 1.80  2004/01/05 19:34:14  gsl
**	2004
**	5.0.53
**	
**	Revision 1.79  2003/12/02 21:21:50  gsl
**	5.0.52
**	
**	Revision 1.78  2003/11/12 14:59:04  gsl
**	Add W@PRINTFILE with #NATIVE back in for Clerity's use.
**	
**	Revision 1.77  2003/11/11 15:41:48  gsl
**	Merge from V5_0_01
**	
**	Revision 1.76  2003/08/13 20:56:33  gsl
**	WISP 5.0.50 test version
**	
**	Revision 1.75  2003/08/11 21:08:15  gsl
**	Add WISP_VERSION_NUM a 4 digit version number
**	
**	Revision 1.74  2003/08/06 18:10:02  gsl
**	5.0.90
**	
**	Revision 1.73  2003/07/08 20:55:24  gsl
**	WISP 5000
**	
**	Revision 1.72  2003/06/26 16:18:35  gsl
**	version numbers
**	
**	Revision 1.71  2003/06/13 17:35:31  gsl
**	#define WISP_WEBSITE
**	
**	Revision 1.70  2003/06/12 20:54:29  gsl
**	Add support for ENTERPRISE licenses with a version number and remove
**	support for UNLIMITED license.
**	
**	Revision 1.69  2003/06/10 14:44:09  gsl
**	Fix abort when missing OPEN mode
**	
**	Revision 1.68  2003/04/11 18:34:16  gsl
**	Add support for Acucobol 5.0 and 5.1
**	
**	Revision 1.67  2003/03/19 21:11:44  gsl
**	Remove USE_PVPL flag
**	
**	Revision 1.66  2003/03/07 20:11:37  gsl
**	Use defines for all the field sizes passed from Cobol to C
**	
**	Revision 1.65  2003/03/06 21:29:40  gsl
**	Add a Relative File Attribute (Change Sort attr to Z)
**	
**	Revision 1.64  2003/02/19 19:03:02  gsl
**	Add run32wisp_acu32.mak for building an ATM runtime with Acu 5.2.1 and WISP 4.4.06
**	
**	Revision 1.63  2003/01/31 19:26:33  gsl
**	Fix copyright header
**	
**	Revision 1.62  2003/01/13 19:57:17  gsl
**	update version list
**	
**	Revision 1.61  2003/01/08 17:42:20  gsl
**	Update year 2003
**	
**	Revision 1.60  2002/12/05 16:47:42  gsl
**	update version numbers
**	
**	Revision 1.59  2002/12/05 15:13:43  gsl
**	Remove wwruncbl
**	
**	Revision 1.58  2002/10/09 18:17:34  gsl
**	update
**	
**	Revision 1.57  2002/08/20 20:46:59  gsl
**	update
**	
**	Revision 1.56  2002/07/18 20:15:16  gsl
**	4452 for testing
**	
**	Revision 1.55  2002/06/28 21:04:06  gsl
**	4.4.51
**	
**	Revision 1.54  2002/06/28 04:03:00  gsl
**	Work on native version of wfopen and wfname
**	
**	Revision 1.53  2002/06/27 04:12:42  gsl
**	Clean up status/mode bits
**	
**	Revision 1.52  2002/06/26 20:52:17  gsl
**	Fix phone number
**	
**	Revision 1.51  2002/06/26 04:25:03  gsl
**	Cleanup mode/status bit fields
**	
**	Revision 1.50  2002/06/25 03:47:59  gsl
**	remove IS_CASE_SEN - never actually implemented
**	
**	Revision 1.49  2002/06/21 20:49:28  gsl
**	Rework the IS_xxx bit flags and the WFOPEN_mode flags
**	
**	Revision 1.48  2002/06/21 03:49:55  gsl
**	Comments
**	
**	Revision 1.47  2002/06/21 03:12:36  gsl
**	Changes to IS_xxx bits
**	
**	Revision 1.46  2002/06/20 23:06:49  gsl
**	Add attributes characters to match bits
**	
**	Revision 1.45  2002/05/17 15:08:11  gsl
**	4450 testing version
**	
**	Revision 1.44  2002-03-28 09:40:30-05  gsl
**	Add defines for COPYRIGHT year
**
**	Revision 1.43  2002-03-21 17:01:54-05  gsl
**	4.4.02
**
**	Revision 1.42  2001-11-27 18:10:03-05  gsl
**	V4.4.01
**
**	Revision 1.41  2001-09-13 15:47:02-04  gsl
**	4.4.00
**
**	Revision 1.40  2000-03-16 10:22:17-05  gsl
**	4.3.06
**
**	Revision 1.39  1999-05-24 17:40:26-04  gsl
**	V4.3.05
**
**	Revision 1.38  1999-03-03 18:18:00-05  gsl
**	Update to 4.3.04
**
**	Revision 1.37  1999-01-04 16:43:15-05  gsl
**	4.3.03
**
**	Revision 1.36  1998-10-26 10:02:42-05  gsl
**	Change version to 4.3.02
**
**	Revision 1.35  1998-08-28 11:52:33-04  gsl
**	Update to 4.3.01
**
**	Revision 1.34  1998-06-23 16:13:17-04  gsl
**	Change version to 4.3.00
**
**	Revision 1.33  1998-05-08 15:12:33-04  gsl
**	change to v4.2.03
**
**	Revision 1.32  1998-03-23 08:52:41-05  gsl
**	Change to 4.2.02
**
**	Revision 1.31  1998-03-04 16:07:01-05  gsl
**	update for COBOL-85 features
**
**	Revision 1.30  1998-01-08 11:38:59-05  gsl
**	change version to 4.2.00
**
**	Revision 1.29  1997-12-02 09:40:37-05  gsl
**	Change to 4.1.04 for test build
**
**	Revision 1.28  1997-09-22 10:03:59-04  gsl
**	update
**
**	Revision 1.27  1997-08-23 17:17:41-04  gsl
**	change version to 4.1.02
**
**	Revision 1.26  1997-07-29 11:47:23-04  gsl
**	Change version to 4.1.01
**
**	Revision 1.25  1997-07-14 10:28:27-04  gsl
**	Change to v4.1.00
**
**	Revision 1.24  1997-06-17 17:50:07-04  gsl
**	Update to 4.0.00
**
**	Revision 1.23  1997-06-10 15:21:43-04  gsl
**	Change to V3.9.94
**
**	Revision 1.22  1997-05-20 11:02:36-04  gsl
**	Change version to 3.9.93
**
**	Revision 1.21  1997-04-30 10:13:54-04  gsl
**	Change version to 3.9.92
**
**	Revision 1.20  1997-02-25 17:42:04-05  gsl
**	Changed to 3.9.91
**
**	Revision 1.19  1996-12-20 12:36:06-05  gsl
**	Change version for Beta test of next release to 3.9.90
**	The next release will be 4.0.00
**
**	Revision 1.18  1996-01-05 02:01:06-08  gsl
**	Change to 3.3.19
**
 * Revision 1.17  1996/01/05  09:53:00  gsl
 * Change to 3.3.18+ for AcuServer patch kit
 *
 * Revision 1.16  1995/08/07  09:33:33  gsl
 * change to V3.3.18
 *
 * Revision 1.15  1995/07/10  13:43:51  gsl
 * change to V3.3.17+ for building a patch kit
 *
 * Revision 1.14  1995/07/07  10:00:38  gsl
 * Change to V3.3.17
 * Added standard headers
 *
**
*/
