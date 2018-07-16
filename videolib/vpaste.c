			/************************************************************************/
			/*	     VIDEO - Video Interactive Development Environment		*/
			/*			Copyright (c) 1987-1991				*/
			/*	An unpublished work by International Digital Scientific Inc.	*/
			/*			  All rights reserved.				*/
			/************************************************************************/

/*						Standard references								*/

#include <stdio.h>
#include "video.h"
#include "vlocal.h"
#include "vdata.h"

/*						Subroutine entry point.								*/

vpaste(max) int max;
{
	register int i;									/* Working register.			*/
	int result;									/* Result of the paste - good or bad.	*/
	FILE *fp, *vopenf();								/* File pointer.			*/

	if ((fp = vopenf("cut","r")) != NULL)						/* Is the video.cut file there?		*/
	{
		fgets((char *)paste_buffer, MAX_COLUMNS_PER_LINE, fp);			/* Yes, so read the paste line.		*/
		i = 0;
		while (paste_buffer[i] != '\n') i++;					/* Trim off the trailing new line.	*/
		paste_buffer[i] = CHAR_NULL;						/* Terminate the paste buffer.		*/
		if (max && (max < MAX_COLUMNS_PER_LINE)) paste_buffer[max] = CHAR_NULL;	/* Don't allow more than requested.	*/
		paste_index = 0;							/* Set the paste index to 0.		*/
		fclose(fp);								/* Close the file.			*/
		result = SUCCESS;							/* The paste was successful.		*/
	}
	else result = FAILURE;								/* Oops, no cut file available.		*/
	return(result);									/* Return the result.			*/
}
