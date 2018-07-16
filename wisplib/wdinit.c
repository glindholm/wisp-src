static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
/* 			WDINIT, subroutine to close out, save screen, set up for a DISPLAY verb.				*/

#include "video.h"

#include "idsistd.h"
#include "vwang.h"

void wdinit(void )
{
	wpushscr();								/* Push the current screen.			*/
	vmove(0,0);
	vmode(CLEAR);
	verase(FULL_SCREEN);							/* Erase it.					*/
	vset_cursor_on();							/* Set the cursor visible.			*/
	vdefer_restore();							/* Make it up to date.				*/
}
/*
**	History:
**	$Log: wdinit.c,v $
**	Revision 1.9  1997/07/09 16:42:46  gsl
**	Change to use new video.h intefaces.
**	
**	Revision 1.8  1996-08-19 18:33:10-04  gsl
**	drcs update
**
**
**
*/
