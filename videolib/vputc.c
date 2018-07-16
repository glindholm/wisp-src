			/************************************************************************/
			/*									*/
			/*	     VIDEO - Video Interactive Development Environment		*/
			/*									*/
			/*			    Copyright (c) 1987				*/
			/*									*/
			/*	An unpublished work by Greg L. Adams.  All rights reserved.	*/
			/*									*/
			/************************************************************************/


/*					Include standard header files.								*/

#include <stdio.h>
#include "video.h"									/* Include video definitions.		*/

/*					Subroutine entry point.									*/

int vputc(ch) char ch;									/* Put a single character.		*/

{
	char string[2];									/* Conversion area.			*/

	string[0] = ch;									/* Convert the char to a string.	*/
	string[1] = CHAR_NULL;								/* Terminate with a null.		*/
	return(vprint("%s",string));							/* Return after printing.		*/
}
