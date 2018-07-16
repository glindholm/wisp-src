static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
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

int vcut(char* string)									/* Cut a string to the video.cut file.	*/
{
	FILE *fp, *vopenf();								/* File references.			*/

	fp = vopenf("cut","w+");							/* Open the cut file.			*/
	fprintf(fp,"%s\n",string);							/* Write to the file.			*/
	fclose(fp);									/* Close the file (what else eh?)	*/
	return(SUCCESS);								/* Lie, assume we're always successful.	*/
}
/*
**	History:
**	$Log: vcut.c,v $
**	Revision 1.10  1996/10/11 22:16:02  gsl
**	drcs update
**	
**
**
*/
