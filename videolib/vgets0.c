static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
				/************************************************************************/
				/*									*/
				/* Simple get string routine.						*/
				/*									*/
				/************************************************************************/

#include <stdio.h>
#include "video.h"
#include "vmodules.h"

int vgets0(char* string, int count)
{
	int i;											/* Working variables.		*/
	int the_meta_char;

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
**	Revision 1.10  1997/07/08 20:59:48  gsl
**	rewrite
**	
**	Revision 1.9  1996-10-11 18:16:05-04  gsl
**	drcs update
**
**
**
*/
