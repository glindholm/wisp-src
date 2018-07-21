/*
******************************************************************************
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
** NOTICE:
** Confidential, unpublished property of Shell Stream Software LLC.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of Shell Stream Software LLC
** is strictly prohibited.
** 
******************************************************************************
*/

/*
**	File:		wdefines.h
**
**	Project:	wisp/lib
**
**	Purpose:	Standard defines
**
*/

#ifndef WDEFINES_H
#define WDEFINES_H

/*
**	Structures and Defines
*/

#define NULL_CHAR	'\0'						/* Use this in character strings instead of NULL	*/

#ifdef FALSE
#undef FALSE
#endif
#define FALSE	0

#ifdef TRUE
#undef TRUE
#endif
#define TRUE	!FALSE

#define	SIZEOF_FILE	8
#define SIZEOF_LIB	8
#define SIZEOF_VOL	6

#define SIZEOF_CMD	256
#define SIZEOF_MESS	256

#define MAX_SCREEN_FIELD_SIZE	79		/* Max file path len handling a COBOL file name in a screen			*/
#define COB_FILEPATH_LEN	80		/* Max file path len in COBOL for wfopen/wfname/wfilechk/wdellock/wfclose	*/
#define WISP_FILEPATH_LEN	256		/* Internal file path lengths */

#define 	WISP_ERROR_FILE			"wisperr.log"

/* Operating System Specific Filename definitions. */

#define		WISP_TTY_ENV			"WISPTTY"
#define		WISP_PID_ENV			"WISPPID"
#define		WISP_GID_ENV			"WISPGID"
#define		WISP_BACKGROUND_ENV		"WISPBACKGRD"
#define		WISP_LINK_ENV			"WISPLINK"
#define		WISP_CANCELEXIT_ENV		"WISPCANEXIT"
#define		WISP_PROGVOL_ENV		"WISPPROGVOL"
#define		WISP_PROGLIB_ENV		"WISPPROGLIB"
#define		WISP_LINKLEVEL_ENV		"WISPLINKLEVEL"
#define		WISP_CPU_ENV			"WISPCPU"
#define		WISP_NETID_ENV			"WISPNETID"
#define		WISP_DEBUG_ENV			"WISPDEBUG"


#if defined(WIN32)
#define		AQM_COMPUTER_NAME		(const char *)"AQM_COMPUTER_NAME"
#define		AQM_JOB_NAME			(const char *)"AQM_JOB_NAME"
#define		AQM_JOB_NUMBER 			(const char *)"AQM_JOB_NUMBER"
#define 	AQM_JOB_FILE			(const char *)"AQM_JOB_FILE"
#define 	AQM_LOG_FILE			(const char *)"AQM_LOG_FILE"
#define 	AQM_QUEUE      			(const char *)"AQM_QUEUE"
#endif /* WIN32 */


/* The following 2 defines were in borderscrn.h or BORDER_SCREEN.H which has been deleted.					*/
/*		 ... An include file defining constants for the bordering of screens.  These values represent the added lines	*/
/*                     and characters to the printed output.  Remember to change these values when altering the border format.	*/
/*                     This file must be included after the inclusion of VWANG.H.						*/

#define BORDERED_COLUMN_TOTAL 10							/* Number of characters added to each	*/
											/* line of text.			*/
#define BORDERED_LINE_TOTAL   10							/* Number of lines added to the output.	*/

/*
**	Function Prototypes
*/

#endif	/* WDEFINES_H */

/*	End of WDEFINES.H	*/


/*
**	History:
**	$Log: wdefines.h,v $
**	Revision 1.23  2003/07/09 20:09:13  gsl
**	Change internal PROGLIB/PROGVOL vars
**	
**	Revision 1.22  2003/01/31 19:26:33  gsl
**	Fix copyright header
**	
**	Revision 1.21  2002/12/11 14:08:45  gsl
**	Removed wispmsg.dat/txt and makemsg
**	
**	Revision 1.20  2001/10/31 20:24:41  gsl
**	Remove VMS and MSDOS
**	Remove the WISP_TEMP_PERSON_PREFIX
**	
**	Revision 1.19  1998-10-14 14:01:30-04  gsl
**	Move isexec() defines to runtype.h
**
**	Revision 1.18  1998-08-03 16:33:26-04  jlima
**	Support Logical Volume Translation to long file names containing eventual embedded blanks.
**
**	Revision 1.17  1997-10-23 15:23:55-04  gsl
**	Add WISP_FILEPATH_LEN for internal file path lengths
**
**	Revision 1.16  1997-05-12 17:20:57-04  gsl
**	Centralized some of the ENV defines
**
**	Revision 1.15  1996-12-06 18:41:57-05  jockc
**	split unix and win32 defs of WISP env variable names.. win32
**	names cast with const char *
**
**	Revision 1.14  1996-10-08 17:27:30-07  gsl
**	removed unused define WISP_CONFIG_ENV
**
**	Revision 1.13  1996-08-23 14:05:14-07  gsl
**	Moved defines into wperson.c and removed unneed ones
**
**	Revision 1.12  1996-07-09 17:01:39-07  gsl
**	Add WISP_TEMP_DIR for MSFS
**
**	Revision 1.11  1996-06-24 11:28:31-07  gsl
**	Fix for both MSDOS and WINNT
**
**	Revision 1.10  1996-01-03 04:20:38-08  gsl
**	Add COB_FILEPATH_LEN
**
 * Revision 1.9  1995/04/25  09:51:54  gsl
 * drcs state V3_3_15
 *
 * Revision 1.8  1995/04/17  11:45:26  gsl
 * drcs state V3_3_14
 *
 * Revision 1.7  1995/03/20  11:14:03  gsl
 * moved the WISPCONFIG file defines into wperson.c
 * and added standard headers
 *
**
*/
