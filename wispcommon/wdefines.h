/* 
	Copyright (c) 1988-1995 NeoMedia Technologies, Inc., All rights reserved.
	$Id:$
*/

/*
**	File:		wdefines.h
**
**	Project:	wisp/lib
**
**	RCS:		$Source:$
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
#define 	WISP_MESSAGE_FILE		"wispmsg.dat"

/* Operating System Specific Filename definitions. */

#ifdef VMS
#define		WISP_TAPE_FILE			"WISP$CONFIG:MTMAP.DAT"
#endif	/* VMS */

#if defined(unix)
#define		WISP_TEMP_PERSON_PREFIX		"PERSON"
#define		WISP_TEMP_PERSUB_PREFIX		"PERSUB"
#endif	/* unix */

#if defined(WIN32)
#define		WISP_TEMP_PERSON_PREFIX		"WPS_"
#define		WISP_TEMP_PERSUB_PREFIX		"WPB_"
#endif	/* WIN32 */

#if defined(MSDOS)
#define		WISP_TEMP_PERSON_PREFIX		"DFTS-"
#endif	/* MSDOS */

#define		WISP_TTY_ENV			"WISPTTY"
#define		WISP_PID_ENV			"WISPPID"
#define		WISP_GID_ENV			"WISPGID"
#define		WISP_BACKGROUND_ENV		"WISPBACKGRD"
#define		WISP_LINK_ENV			"WISPLINK"
#define		WISP_CANCELEXIT_ENV		"WISPCANEXIT"
#define		WISP_PROGVOL_ENV		"_PROGVOL"
#define		WISP_PROGLIB_ENV		"_PROGLIB"
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
