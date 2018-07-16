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
#include <sys/types.h>
#include <time.h>

#include "video.h"

testc()
{
	time_t the_time, end_time;

	end_time = time(NULL) + 30;
	
	verase(FULL_SCREEN);
	vset_cursor_off();
	vtext(VMODE_BOLD,10,10,"The Atomic Clock");

	while(the_time < end_time && 0==vcheck())
	{
		the_time = time(NULL);
		vtext(VMODE_BOLD,14,10,"%s",ctime(&the_time));
		vwait(1,0);
	}
	vset_cursor_on();
	return(SUCCESS);
}
