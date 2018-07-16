			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/


/*
	wdefines.h	- Standard defines
*/

#ifndef WDEFINES_H
#define WDEFINES_H

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


#define 	WISP_ERROR_FILE			"wisperr.log"
#define 	WISP_MESSAGE_FILE		"wispmsg.dat"

#define 	MAX_LINK_PARMS		32						/* Maximum number of parameters to LINK	*/
#define 	NULL_FILE_NAME		"*NULL*"					/* No LINK arg file, See VMSLINKSUB.WCB	*/

/* Operating System Specific Filename definitions. */

#ifdef VMS
#define 	WISP_USER_PERSON_FILE		"SYS$LOGIN:PERSONALITY.DAT"
#define		WISP_SYSTEM_PERSON_FILE		"WISP$CONFIG:PERSONALITY.DAT"
#define		WISP_TERMINAL_FILE		"WISP$CONFIG:TTMAP.DAT"
#define		WISP_PRINTER_FILE		"WISP$CONFIG:LPMAP.DAT"
#define		WISP_PROC_FILE			"WISP$CONFIG:PQMAP.DAT"
#define		WISP_TAPE_FILE			"WISP$CONFIG:MTMAP.DAT"
#define		WISP_OPTIONS_FILE		"WISP$CONFIG:OPTIONS.DAT"
#endif	/* VMS */

#ifndef VMS	/* unix and MSDOS */

#ifdef unix
#define		TMP_DIR		"/usr/tmp"		/* Temporary diretory path.	*/
#define		DSS		"/"			/* Directory Separator String.	*/

#define		WISP_SYSTEM_PERSON_FILE		"PERSONALITY"
#define 	WISP_USER_PERSON_FILE		"PERSONALITY"
#define		WISP_TEMP_PERSON_PREFIX		"PERSON"
#define		WISP_TEMP_PERSUB_PREFIX		"PERSUB"
#define 	WISP_PRB_DIR 			"/usr/tmp/wpparms"
#define 	WISP_LINK_DIR 			"/usr/tmp/wisplink"
#define		WISP_RETCOD_FILE		"/usr/tmp/RETCOD"
#endif	/* #ifdef unix */

#ifdef MSDOS
#define		TMP_DIR		"C:\\TMP"		/* Temporary diretory path.	*/
#define		DSS		"\\"			/* Directory Separator String.	*/

#define		WISP_SYSTEM_PERSON_FILE		"DEFAULTS.SYS"
#define 	WISP_USER_PERSON_FILE		"DEFAULTS.USR"
#define		WISP_TEMP_PERSON_PREFIX		"DFTS-"
#define		WISP_TEMP_PERSON_SUFFIX		".TMP"
#define 	WISP_PRB_DIR 			"C:\\TMP\\WPPARMS.TMP"
#define 	WISP_LINK_DIR 			"C:\\TMP\\WISPLINK.TMP"
#define		WISP_RETCOD_FILE		"C:\\TMP\\RETCOD.TMP"
#endif	/* #ifdef MSDOS */

#define		WISP_CONFIG_ENV			"WISPCONFIG"
#define		WISP_HOME_ENV			"HOME"
#define		WISP_TTY_ENV			"WISPTTY"
#define		WISP_PID_ENV			"WISPPID"
#define		WISP_GID_ENV			"WISPGID"
#define		WISP_BACKGROUND_ENV		"WISPBACKGRD"
#define		WISP_LINK_ENV			"WISPLINK"
#define		WISP_CANCELEXIT_ENV		"WISPCANEXIT"

#define		WISP_TERMINAL_FILE		"TTMAP"
#define		WISP_PRINTER_FILE		"LPMAP"
#define		WISP_PROC_FILE			"PQMAP"
#define		WISP_SUBCLASS_FILE		"SCMAP"
#define		WISP_TAPE_FILE			"MTMAP"
#define		WISP_LOGICAL_FILE		"LGMAP"
#define		WISP_FORMS_FILE			"FORMS"
#define		WISP_PRMAP_FILE			"PRMAP"
#define		WISP_OPTIONS_FILE		"OPTIONS"


#endif	/* unix and MSDOS	*/

#define NOTFOUND	-1
#define DOTEXE 		0								/* filename needs .exe 			*/
#define DOTCOM 		1								/* filename needs .com 			*/
#define DOTSH 		2								/* filename needs .sh 			*/
#define DOTGNT 		3								/* filename needs .gnt 			*/
#define DOTINT 		4								/* filename needs .int 			*/
#define NOEXT 		5								/* no extension (use fname as passed) 	*/


#define ACCERR -1
#define ISEXEC 	0
#define NOTEXEC 1
#define ISACU 	2
#define ISMFINT	3

/* The following 2 defines were in borderscrn.h or BORDER_SCREEN.H which has been deleted.					*/
/*		 ... An include file defining constants for the bordering of screens.  These values represent the added lines	*/
/*                     and characters to the printed output.  Remember to change these values when altering the border format.	*/
/*                     This file must be included after the inclusion of VWANG.H.						*/

#define BORDERED_COLUMN_TOTAL 10							/* Number of characters added to each	*/
											/* line of text.			*/
#define BORDERED_LINE_TOTAL   10							/* Number of lines added to the output.	*/

#endif	/* WDEFINES_H */

/*	End of WDEFINES.H	*/

