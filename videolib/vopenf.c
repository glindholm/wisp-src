			/************************************************************************/
			/*	     VIDEO - Video Interactive Development Environment		*/
			/*			Copyright (c) 1987-1991				*/
			/*	An unpublished work by International Digital Scientific Inc.	*/
			/*			  All rights reserved.				*/
			/************************************************************************/

/*						Keystroke Macro Manager								*/

#include "video.h"									/* Include header files.		*/
#include "vlocal.h"
#include "vdata.h"

static char filename[64];								/* Name of the file.			*/

char *vfilename(ext) char *ext;								/* Make a video filename.		*/
{
	char *ptr;									/* Temp char ptr			*/

#ifdef vms
	strcpy(filename,"sys$login:video.");						/* Store the root name.			*/
	strcat(filename,ext);								/* Add the extension.			*/
#endif

#ifdef unix
	if (!(ptr=(char *)getenv("HOME")))						/* Get the HOME dir.			*/
	{
		ptr = ".";								/* If home not found use "."		*/
	}
	strcpy(filename,ptr);
	strcat(filename,"/.video");							/* Make it a non-visible file.		*/
	strcat(filename,ext);								/* Add the extension.			*/
#endif

#ifdef MSDOS
	strcpy(filename,"\video.");							/* Store in the root.			*/
	strcat(filename,ext);
#endif

	return(filename);								/* Return a pointer to the filename.	*/
}

/*						Subroutine entry point.								*/

FILE *vopenf(ext,how) char *ext, *how;
{
	FILE *fopen();									/* Reference the fopen routine.		*/

#ifdef vms
	if ((how[0] == 'w') && (how[1] = '+')) 						/* Delete previous if open for update.	*/
	{
		delete(vfilename(ext));							/* Delete the file.			*/
		how[1] = CHAR_NULL;							/* Don't do an update write.		*/
	}
#endif

	return(fopen(vfilename(ext),how));						/* Open the file.			*/
}
