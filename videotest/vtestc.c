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
#include <sys/types.h>
#include <time.h>

#include "video.h"

int testc()
{
	time_t the_time, end_time;

	the_time = time(NULL);
	end_time = the_time + 30;
	
	verase(FULL_SCREEN);
	vset_cursor_off();
	vtext(VMODE_BOLD,10,10,"The Atomic Clock");

	while(the_time < end_time && 0==vcheck())
	{
		the_time = time(NULL);
		vtext(VMODE_BOLD,14,10,"%s",ctime(&the_time));
		VL_vwait(1,0);
	}
	vset_cursor_on();
	return(SUCCESS);
}
