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

static testi1();
static testi2();
static testi3();

testi()
{
	extern int line();
	verase(FULL_SCREEN);
	vmove(0,0);
	testi1();
	line();
	testi2();
	line();
	testi3();
	while (vcheck() != 0);
	vprint("\n\nDepress any character to continue..."); vgetc();
	return(SUCCESS);
}

static testi1()
{
	char c;

	vprint("\nTest I1 - input single character in wait mode.\n");
	vprint("\nPlease input a character? ");
	c = vgetc();
	vprint("\n");
	vprint("The character was %c.\n",c);
	return(SUCCESS);
}

static testi2()
{
	char c;

	vprint("\nTest I2 - input single character non-wait mode.\n");

	c = 0;
	while (c == 0)
	{
		vprint("Looping - waiting for a character to be entered\n");
		c = vcheck();
	}

	vprint("The character was %c.\n",c);
	return(SUCCESS);
}

statictesti3()
{
	char c;
	char x;
	register int i,j;

	vprint("\nTest I3 - character push back.\n");

	vprint("\nPlease input a character? ");
	c = vgetc();
	vprint("\n");
	vprint("The character was %c.\n",c);

	while (x = vcheck() != 0) vprint("Purging typeahead.\n");

	vprint("Pushing back the character\n");
	i = vpushc(c);
	vprint("Return value indicates push back was ");
	if (i == FAILURE) {vprint("a failure\n"); return(FAILURE);}
	if (i == SUCCESS) vprint("a success.\n");

	vprint("Now reading the same character back\n");
	x = vcheck();
	if (x == 0)
	{
		vprint("ERROR - no character was available.\n");
		return(FAILURE);
	}
	else
	{
		vprint("The character %c was returned.\nThis ",x);
		if (c == x) {vprint("agrees "); j = SUCCESS;}
		if (c != x)
		{
			vprint("is an ERROR and does not agree");
			j = FAILURE;
		}
		vprint("with the character pushed (%c).\n",x);
		if (j == FAILURE) return(j);
	}
	vprint("Now reading again, this should return a NULL\n");
	x = vcheck();
	if (x == 0) vprint("And it was!\n");
	else
	{
		vprint("ERROR - the character %c was returned instead.\n",x);
		return(FAILURE);
	}
	vprint("Now checking error report of double push back.\n");
	if (i = vpushc('X') == SUCCESS)
	{
		vprint("First push back gave success, which is correct\n");
	}
	else
	{
		vprint("First push back gave failure, which is an ERROR!\n");
		return(FAILURE);
	}

	if (i = vpushc('X') == FAILURE)
	{
		vprint("Second push back gave failure, which is correct.\n");
	}
	else
	{
		vprint("Second push back gave success, which is an ERROR!\n");
		return(FAILURE);
	}

	return(j);
}
