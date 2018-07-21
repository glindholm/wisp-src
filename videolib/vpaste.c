/*
******************************************************************************
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** NOTICE:
** Confidential, unpublished property of NeoMedia Technologies, Inc.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of NeoMedia 
** Technologies, Inc. is strictly prohibited.
** 
******************************************************************************
*/

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
#include "vmodules.h"

/*						Subroutine entry point.								*/

int VL_vpaste(max) int max;
{
	register int i;									/* Working register.			*/
	int result;									/* Result of the paste - good or bad.	*/
	FILE *fp;								/* File pointer.			*/

	if ((fp = VL_vopenf("cut","r")) != NULL)						/* Is the video.cut file there?		*/
	{
		fgets((char *)VL_paste_buffer, MAX_COLUMNS_PER_LINE, fp);		/* Yes, so read the paste line.		*/
		i = 0;
		while (VL_paste_buffer[i] != '\n') i++;					/* Trim off the trailing new line.	*/
		VL_paste_buffer[i] = CHAR_NULL;						/* Terminate the paste buffer.		*/
		if (max && (max < MAX_COLUMNS_PER_LINE)) VL_paste_buffer[max] = CHAR_NULL; /* Don't allow more than requested.	*/
		VL_paste_index = 0;							/* Set the paste index to 0.		*/
		fclose(fp);								/* Close the file.			*/
		result = SUCCESS;							/* The paste was successful.		*/
	}
	else result = FAILURE;								/* Oops, no cut file available.		*/
	return(result);									/* Return the result.			*/
}
/*
**	History:
**	$Log: vpaste.c,v $
**	Revision 1.14  2003/01/31 20:58:40  gsl
**	Fix -Wall warnings
**	
**	Revision 1.13  2003/01/31 19:25:56  gsl
**	Fix copyright header
**	
**	Revision 1.12  2002/07/15 20:56:40  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.11  2002/07/15 20:16:12  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.10  2002/07/15 17:10:05  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.9  1996/10/11 22:16:15  gsl
**	drcs update
**	
**
**
*/
