			/************************************************************************/
			/*									*/
			/*	     VIDEO - Video Interactive Development Environment		*/
			/*									*/
			/*			    Copyright (c) 1987				*/
			/*									*/
			/*	An unpublished work by Greg L. Adams.  All rights reserved.	*/
			/*									*/
			/************************************************************************/


/*						Include standard header files.							*/

#include <stdio.h>									/* We need standard I/O.		*/
#include "video.h"									/* We need standard video definitions.	*/

/*						Local definitions.								*/

#define HT '\t'										/* Define horizontal tab.		*/
#define SP ' '										/* Define space character.		*/

/*						Subroutine entry point.								*/

int vtrim(string) char string[];							/* Trim a string of trailing blanks.	*/

{
	register int i;									/* Working register storage.		*/

	for (i = strlen(string)-1; (i >= 0) && ((string[i] == SP) || (string[i] == HT) || (string[i] == CHAR_NULL)); i--)
	{
		string[i] = CHAR_NULL;
	}
	return(i+1);									/* Return the string length.		*/
}
