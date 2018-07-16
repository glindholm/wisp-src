/*
******************************************************************************
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** $Id:$
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
** CVS
** $Source:$
** $Author: gsl $
** $Date:$
** $Revision:$
******************************************************************************
*/

				/************************************************************************/
				/*									*/
				/* Simple get string routine.						*/
				/*									*/
				/************************************************************************/

#include <stdio.h>
#include "video.h"
#include "vmodules.h"

int VL_vgets0(char* string, int count)
{
	int i;											/* Working variables.		*/
	int the_meta_char = 0;

	i = 0;											/* Assume no chars yet.		*/

	while (count>0 && i<count)
	{
		the_meta_char = vgetm();
		
		if (the_meta_char >= ' ' && the_meta_char <= '~')
		{
			vputc((char)the_meta_char);						/* Echo the character.		*/
			string[i++] = the_meta_char;						/* Record the string.		*/
		}
		else if ((the_meta_char == return_key)  || 
			 (the_meta_char == enter_key)   ||
			 (the_meta_char == newline_key)   ) 
		{
			break;
		}
		else if (the_meta_char == delete_key) 
		{
			if (i)								/* Anything to rub out?		*/
			{
				vprint("\010 \010");					/* Rub out the last char.	*/
				i = i - 1;						/* Set counter back one char.	*/
			}
		}
		else if (the_meta_char < ' ')
		{
			break;
		}
		else
		{
			vbell();
		}
		
	}

	string[i] = '\0';									/* Terminate the string.	*/
	return(the_meta_char);									/* Return the terminator.	*/
}
/*
**	History:
**	$Log: vgets0.c,v $
**	Revision 1.13  2003/01/31 20:35:57  gsl
**	Fix -Wall warnings
**	
**	Revision 1.12  2003/01/31 19:25:56  gsl
**	Fix copyright header
**	
**	Revision 1.11  2002/07/15 20:56:38  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.10  1997/07/08 20:59:48  gsl
**	rewrite
**	
**	Revision 1.9  1996-10-11 18:16:05-04  gsl
**	drcs update
**
**
**
*/
