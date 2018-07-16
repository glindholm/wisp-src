			/************************************************************************/
			/*	     VIDEO - Video Interactive Development Environment		*/
			/*			Copyright (c) 1987-1991				*/
			/*	An unpublished work by International Digital Scientific Inc.	*/
			/*			  All rights reserved.				*/
			/************************************************************************/

/*						Standard References								*/

#include <stdio.h>
#include "video.h"

/*						Subroutine Entry Point								*/

vcut(string) unsigned char *string;							/* Cut a string to the video.cut file.	*/
{
	FILE *fp, *vopenf();								/* File references.			*/

	fp = vopenf("cut","w+");								/* Open the cut file.			*/
	fprintf(fp,"%s\n",string);							/* Write to the file.			*/
	fclose(fp);									/* Close the file (what else eh?)	*/
	return(SUCCESS);								/* Lie, assume we're always successful.	*/
}
