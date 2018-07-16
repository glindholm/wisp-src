/************************************************************************/
/*									*/
/*	     VIDEO - Video Interactive Development Environment		*/
/*									*/
/*			    Copyright (c) 1987				*/
/*									*/
/*	An unpublished work by Greg L. Adams.  All rights reserved.	*/
/*									*/
/************************************************************************/

#include <v/video.h>

testj()
{
	unsigned char c,vgetc();
	register int retcount;

	verase(FULL_SCREEN);
	vmove(0,0);
	retcount = 0;
vprint("Enter any character and its sequence will be echoed back.\n\n");

again:	c = vgetc();
	if (c == '\15')
	{
		retcount = retcount + 1;
		if (retcount >= 2) return(SUCCESS);
	}
	else retcount = 0;
	if (c <= 040)
	{
		vprint("Character is = %o octal, %d decimal.\n",c,c);
	}
	else vprint("Character is %c = %o octal = %d decimal.\n",c,c,c);
	goto again;
}
