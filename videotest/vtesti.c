/*
******************************************************************************
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
**
** NOTICE:
** Confidential, unpublished property of Shell Stream Software LLC.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of Shell Stream Software LLC
** is strictly prohibited.
** 
******************************************************************************
*/

#include <stdio.h>
#include <video.h>
#include <vmodules.h>

static int testi1();
static int testi2();
static int testi3();

int testi()
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

static int testi1()
{
	char c;

	vprint("\nTest I1 - input single character in wait mode.\n");
	vprint("\nPlease input a character? ");
	c = vgetc();
	vprint("\n");
	vprint("The character was %c.\n",c);
	return(SUCCESS);
}

static int testi2()
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

static int testi3()
{
	char c;
	char x;
	register int i = 0,j = 0;

	vprint("\nTest I3 - character push back.\n");

	vprint("\nPlease input a character? ");
	c = vgetc();
	vprint("\n");
	vprint("The character was %c.\n",c);

	while ((x = vcheck()) != 0) vprint("Purging typeahead.\n");

	vprint("Pushing back the character\n");
	i = VL_vpushc(c);
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
	if ((i = VL_vpushc('X')) == SUCCESS)
	{
		vprint("First push back gave success, which is correct\n");
	}
	else
	{
		vprint("First push back gave failure, which is an ERROR!\n");
		return(FAILURE);
	}

	if ((i = VL_vpushc('X')) == FAILURE)
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
