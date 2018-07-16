			/************************************************************************/
			/*	     VIDEO - Video Interactive Development Environment		*/
			/*			    Copyright (c) 1987				*/
			/*	An unpublished work by Greg L. Adams.  All rights reserved.	*/
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

int vtrimlen(outstr,instr,length) char outstr[]; char instr[]; int length;		/* Trim a string of length.		*/
{	
	register int i;

	for (i = 0; i < length; i++) outstr[i] = instr[i];				/* Copy the string.			*/
	outstr[i] = CHAR_NULL;								/* Null terminate.			*/
	return(vtrim(outstr));								/* Trim it.				*/
}

int vputlen(outstr,instr,length) char outstr[]; char instr[]; int length;		/* Opposite of vtrimlen.		*/
{
	register int i;
	int eos;

	eos = FALSE;									/* Not end of string yet.		*/
	for (i = 0; i < length; i++)							/* Loop through the whole string.	*/
	{
		if (eos) outstr[i] = ' ';						/* Pad to the end if at eos.		*/
		else
		{
			if (instr[i]) outstr[i] = instr[i];				/* A valid character?			*/
			else
			{
				eos = TRUE;						/* No more valid chars.			*/
				outstr[i] = ' ';					/* Store a space.			*/
			}
		}
	}
}
