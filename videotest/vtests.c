
/************************************************************************/
/*									*/
/*	     VIDEO - Video Interactive Development Environment		*/
/*									*/
/*			    Copyright (c) 1987				*/
/*									*/
/*	An unpublished work by Greg L. Adams.  All rights reserved.	*/
/*									*/
/************************************************************************/

#include <stdio.h>
#include <video.h>
#include <vmodules.h>

int tests()
{
	int i;

	vstate(0);
	verase(FULL_SCREEN);
	vscreen(NARROW|DARK);
	vmove(0,0);

	for (i = 0; i < 10; i++)
	{
		vprint("%d---------------------------------\n",i);
	}
	vprint("Depress return to continue..."); vgetc();

	vroll(5,7);
	vmove(5,0);
	for (i = 0; i < 50; i++)
	{
		vprint("........... Hose off eh!........\n");
	}

	vprint("Resetting the scroll region to full page. Depress return to continue..."); vgetc();

	vroll(0,MAX_LINES_PER_SCREEN-1);
	vprint("\nDepress return to continue..."); vgetc();


	vroll(5,7);
	vmove(5,0);
	for (i = 0; i < 50; i++)
	{
		vprint("........... More hose off eh?........\n");
	}

	vprint("Depress return to test moving outside the scroll region..."); vgetc();

	vmove(12,0);
	vprint("This is outside the scroll region.\n\n");
	vprint("Depress any key to continue...");vgetc();
	vroll(0,23);
	return(SUCCESS);
}
