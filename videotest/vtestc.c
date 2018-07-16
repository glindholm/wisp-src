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
#include <time.h>
#include <v/video.h>

testc()
{
/*	long time();	*/
	char *ctime();
	long time_data;
	register int i;

	verase(FULL_SCREEN);
	vset(CURSOR,OFF);
	vtext(DOUBLE_HEIGHT|BOLD,10,10,"The Atomic Clock");

	for (i = 0; (i < 2000) && !vcheck(); i++)
	{
		time_data = time(NULL);
		vtext(DOUBLE_HEIGHT|BOLD,14,10,"%s",ctime(&time_data));
	}
	vset(CURSOR,ON);
	return(SUCCESS);
}
