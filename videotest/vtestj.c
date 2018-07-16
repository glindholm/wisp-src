/************************************************************************/
/*									*/
/*	     VIDEO - Video Interactive Development Environment		*/
/*									*/
/*			    Copyright (c) 1987				*/
/*									*/
/*	An unpublished work by Greg L. Adams.  All rights reserved.	*/
/*									*/
/************************************************************************/

#include <ctype.h>
#include <video.h>
#include <vmodules.h>

testj()
{
	char c,vgetc();
	int retcount;
	unsigned char uc;

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

	uc = (unsigned char)c;
	if (isprint(c) && isascii(c))
	{
		vprint("Character is \'%c\' = %o octal = %u decimal.\n",c,(int)uc,(int)uc);
	}
	else 
	{
		vprint("Character is     = %o octal, %u decimal.\n",(int)uc,(int)uc);
	}
	goto again;
}
