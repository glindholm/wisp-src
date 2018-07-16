static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
/* 			WDFINISH, subroutine to restore screen after a DISPLAY verb.						*/

#include "video.h"

#include "idsistd.h"
#include "vwang.h"

void wdfinish()
{
	vmove(23,0);
	vprint("Press RETURN to continue.");
	vgetc();
	wpopscr();								/* Pop the current screen.			*/
	vset_cursor_off();							/* Set the cursor invisible.			*/
	vdefer_restore();							/* Bring it up to date.				*/
}
/*
**	History:
**	$Log: wdfinish.c,v $
**	Revision 1.9  1997-07-09 12:43:09-04  gsl
**	Change to use new video.h intefaces
**
**	Revision 1.8  1996-08-19 18:33:10-04  gsl
**	drcs update
**
**
**
*/
