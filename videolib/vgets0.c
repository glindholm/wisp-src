				/************************************************************************/
				/*									*/
				/* Simple get string routine.						*/
				/*									*/
				/************************************************************************/

#include <stdio.h>
#include "video.h"

vgets0(string,count) char *string; int count;
{
	register int i;										/* Working variables.		*/
	register char c;
	int term;										/* Terminator.			*/

	i = 0;											/* Assume no chars yet.		*/
	term = 0;										/* No terminator depressed yet.	*/

	if (count)										/* Does he want any chars?	*/
	{
		while ((i < count) && !term)							/* Repeat until term or full.	*/
		{
			c = vgetc();								/* Get the meta character.	*/
#ifdef	MSDOS
			if (c == '\200')							/* Is this an escape char?	*/
#else	/* VMS or unix */
			if (c == '\033')							/* Is this an escape char?	*/
#endif	/* VMS or unix */
			{
				vpushc(c);							/* Yes then push it back.	*/
				term = vgetm();							/* It's a terminator so done.	*/
			}
			else if (c == 21)							/* A control U?			*/
			{
				while (i)							/* Continue until all gone.	*/
				{
					vprint("\010 \010");					/* Rub out the last char.	*/
					i = i - 1;						/* Set counter back one char.	*/
				}
			}
			else if (c < 040) term = c;						/* Is it a control char?	*/
			else if (c >= 0177)							/* Is it a delete?		*/
			{
				if (i)								/* Anything to rub out?		*/
				{
					vprint("\010 \010");					/* Rub out the last char.	*/
					i = i - 1;						/* Set counter back one char.	*/
				}
			}
			else
			{
				vputc(c);							/* Echo the character.		*/
				string[i++] = c;						/* Record the string.		*/
			}
		}
	}

	string[i] = CHAR_NULL;									/* Terminate the string.	*/
	return(term);										/* Return the terminator.	*/
}
