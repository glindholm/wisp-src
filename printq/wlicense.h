			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991, 1992	*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/


/*
**	File:		wlicense.h
**
**	Function:	Provide defines for the wisp license routines.
**
**	History:
**			05/19/92	Written GSL
**			07/31/92	Added WISP_LICENSE_FILE_X the temp inode file. GSL
*/

#ifndef WLICENSE_H
#define WLICENSE_H

#define		LICENSE_SINGLE		1
#define		LICENSE_UNLIMITED	2
#define		LICENSE_TIMED		3

#define		LICENSE_KEY_SIZE	16
#define		VALIDATION_CODE_SIZE	3

#define		LICENSE_OK		1000
#define		LICENSE_MISSING		1001
#define		LICENSE_TIMEDOUT	1002
#define		LICENSE_INVALID		1003
#define		LICENSE_UNKNOWN		1004


#define		WISP_LICENSE_FILE	"/lib/wisp.license"
#define		WISP_LICENSE_FILE_X	"/lib/.wisp.license"

#endif

