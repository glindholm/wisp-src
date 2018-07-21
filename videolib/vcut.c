/*
******************************************************************************
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
**
** NOTICE:
** Confidential, unpublished property of Shell Stream Software LLC.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of Shell Stream Software LLC
** is strictly prohibited.
** 
******************************************************************************
*/

			/************************************************************************/
			/*	     VIDEO - Video Interactive Development Environment		*/
			/*			Copyright (c) 1987-1991				*/
			/*	An unpublished work by International Digital Scientific Inc.	*/
			/*			  All rights reserved.				*/
			/************************************************************************/

/*						Standard References								*/

#include <stdio.h>
#include "video.h"
#include "vmodules.h"

/*						Subroutine Entry Point								*/

int VL_vcut(char* string)									/* Cut a string to the video.cut file.	*/
{
	FILE *fp;								/* File references.			*/

	fp = VL_vopenf("cut","w+");							/* Open the cut file.			*/
	fprintf(fp,"%s\n",string);							/* Write to the file.			*/
	fclose(fp);									/* Close the file (what else eh?)	*/
	return(SUCCESS);								/* Lie, assume we're always successful.	*/
}
/*
**	History:
**	$Log: vcut.c,v $
**	Revision 1.13  2003/01/31 19:25:56  gsl
**	Fix copyright header
**	
**	Revision 1.12  2002/07/15 20:56:38  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.11  2002/07/15 20:16:08  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.10  1996/10/11 22:16:02  gsl
**	drcs update
**	
**
**
*/
