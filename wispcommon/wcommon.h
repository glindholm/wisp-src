/* 
	Copyright (c) 1988-2002 NeoMedia Technologies, All rights reserved.
	$Id:$
*/

/*
**	File:		wcommon.h
**
**	Project:	wisp
**
**	RCS:		$Source:$
**
**	Purpose:	Common defines for all WISP sub-projects
**
*/

#ifndef WCOMMON_H
#define WCOMMON_H

/*
	WCOMMON.H		Define common defines for WISP and WISPLIB
*/

#define WISP_VERSION "V4.4.02"			 					/* WISPTRAN version (char[20])		*/
#define LIBRARY_VERSION	30								/* WISPLIB version number.		*/
#define SCREEN_VERSION  22								/* SCREEN version number.		*/

/* CHANGE-COPYRIGHT-DATE */
#define WISP_COPYRIGHT_YEAR_STR	"2002"
#define WISP_COPYRIGHT_YEAR_INT	2002


/* 
**	NOTE: Screen version 20 and 21 are still supported.
**
** SCREEN_VERSION 20	- DISP-ITEM-LENGTH was PIC S9(4) binary.
** SCREEN_VERSION 21	- DISP-ITEM-LENGTH was PIC X, and compression greatly improved.
** SCREEN_VERSION 22	- New screen structure.
*/


#define IS_OUTPUT	0x00000001							/* output only file			*/
#define IS_PRINTFILE	0x00000002							/* printerfile				*/
#define IS_SCRATCH	0x00000004							/* scratch file				*/
#define IS_SUBMIT       0x00000008							/* This file will be submitted.		*/
#define IS_IO		0x00000010							/* This file is opened IO.		*/
#define IS_SORT		0x00000020							/* This file is s SORT file.		*/
#define IS_LIB		0x00000040							/* Generate a LIB only.			*/
#define IS_NORESPECIFY	0x00000080							/* already done				*/
#define IS_INDEXED	0x00000100							/* an indexed file			*/
#define IS_CASE_SEN	0x00000200							/* name is case sensitive don't change	*/
#define IS_NOWRITE	0x00000400							/* File is opened allowing no writers.	*/
#define IS_PRNAME	0x00000800							/* Its a call with a prname ref.	*/
#define IS_BACKFILL	0x00001000							/* Backfill file/lib/vol.		*/
#define IS_SEQSEQ	0x00002000							/* It's a Sequential/Sequential file.	*/
#define IS_GETPARM	0x00004000							/* Has the initial hidden GP been done?	*/
#define IS_NODISPLAY	0x00008000							/* Select file "NODISPLAY".		*/
#define IS_SEQDYN	0x00010000							/* File is SEQ/DYN without relative key.*/
#define IS_WORK		0x00020000							/* A WORK file.				*/
#define IS_TEMP		0x00040000							/* A temporary ## file			*/
#define IS_ERROR	0x00080000							/* Set to force a GETPARM.		*/
#define IS_DECLARE	0x00100000							/* There are DECLARITIVES for this file */
#define IS_EXTEND	0x00200000							/* File opened EXTEND.			*/
#define IS_NOEXTENSION	0x00400000							/* A file without an extension.		*/
#define IS_DBFILE	0x00800000							/* A DATABASE file			*/

#define		OPEN_INPUT		1
#define		OPEN_SHARED		2
#define		OPEN_OUTPUT		3
#define		OPEN_EXTEND		4
#define		OPEN_SPECIAL_INPUT	5
#define		OPEN_I_O		6
#define		OPEN_SORT		7

#endif	/* WCOMMON_INCLUDED */

/*
**	History:
**	$Log: wcommon.h,v $
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
