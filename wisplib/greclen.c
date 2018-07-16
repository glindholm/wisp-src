static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*			Copyright (c) 1988, 1989, 1990			*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/************************************************************************/

/*						Include files and definitions.							*/
										/* This is a sample.				*/
#include "idsistd.h"
#ifdef VMS
#include <stdio.h>
#include <rms.h>
#include <types.h>
#include <stat.h>
#include <statbuf.h>

int greclen(char* file_name)							/* Get record length of fixed_size record file. */
{
	struct stat stbuf;							/* This structure describes the file descriptor. */
	int status;								/* Status will be returned from STAT here.	*/
	unsigned int reclen=0;							/* This will contain function return value.	*/

	status = stat(file_name, &stbuf);					/* Get the file descriptor information about 	*/
										/* the file. */
	if (status < 0)  return(status);					/* Status=-1 indicates that STAT is unsuccessful*/
										/* Status=-2 indicates protection violation	*/
	else if (status == 0)
	{
		if (stbuf.st_fab_rfm == FAB$C_FIX && stbuf.st_fab_rat != FAB$V_CR)  reclen = stbuf.st_fab_mrs;
										/* If the record is fixed length and no Carriage */
										/* return, then return the record length, else 	*/
										/* return 0					*/
	}
	return(reclen);
}
#else /* unix and MSDOS */

int greclen(char* file_name)							/* Get record length of fixed_size record file. */
{
	int tmp=0;
	return(tmp);
}
#endif		
/*
**	History:
**	$Log: greclen.c,v $
**	Revision 1.10  1996/09/05 00:02:09  gsl
**	fix def of stat()
**	
**	Revision 1.9  1996-08-19 15:32:23-07  gsl
**	drcs update
**
**
**
*/
