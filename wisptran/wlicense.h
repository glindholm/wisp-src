			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*	      Copyright (c) 1988, 1989, 1990, 1991, 1992, 1993		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
**	File:		wlicense.h
**
**	Function:	Provide defines for the license routines.
**
**	History:
**	05/19/92	Written GSL
**	07/31/92	Added WISP_LICENSE_FILE_X the temp inode file. GSL
**	09/25/92	Added LICENSE_CLUSTER. GSL
**	09/13/93	Remove path defines. Generalize for UniQue. GSL
*/

#ifndef WLICENSE_H
#define WLICENSE_H

#define		LICENSE_SINGLE		1
#define		LICENSE_UNLIMITED	2
#define		LICENSE_TIMED		3
#define		LICENSE_CLUSTER		4

#define		LICENSE_KEY_SIZE	16
#define		VALIDATION_CODE_SIZE	3

#define		LICENSE_OK		1000
#define		LICENSE_MISSING		1001
#define		LICENSE_TIMEDOUT	1002
#define		LICENSE_INVALID		1003
#define		LICENSE_UNKNOWN		1004

char *product_name();
char *license_filepath();
char *x_license_filepath();
char *lic_trantable();
char *authlogfile();


#endif /* WLICENSE_H */
