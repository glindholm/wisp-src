			/************************************************************************/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*			Copyright (c) 1988, 1989, 1990			*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/************************************************************************/

/*						Include files and definitions.							*/
										/* This is a sample.				*/
#ifdef VMS
#include <stdio.h>
#include <rms.h>
#include <types.h>
#include <stat.h>

int greclen(file_name) unsigned char *file_name;				/* Get record length of fixed_size record file. */
{
	struct stat stbuf;							/* This structure describes the file descriptor. */
	extern int stat();							/* This is C run_time library routine. 		*/
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
#endif		

#ifdef unix
int greclen(file_name) unsigned char *file_name;				/* Get record length of fixed_size record file. */
{
	int tmp=0;
	return(tmp);
}
#endif		
