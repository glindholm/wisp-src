static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
/*
**	WISPSYNC	This is a special wisp COBOL interface to vwang_synch() that is to be inserted into wang cobol
**			programs to let wisp and video known that the screen has been changed behind its back, so
**			that it can re-synchronize it's video maps.
*/

#include "idsistd.h"
#include "vwang.h"

void WISPSYNC(void)
{
	vwang_synch();
}



/*
**	WISPSHUT	This is a special wisp COBOL interface to vwang_shut() that is to be inserted into wang cobol
**			programs to let wisp and video known that the screen is going to be altered by a non-WISP
**			routine, so it should turn off video and put the screen back into a known state.
*/

void WISPSHUT(void)
{
	vwang_shut();
}

/*
**	History:
**	$Log: wispsync.c,v $
**	Revision 1.9  1996/08/19 22:33:18  gsl
**	drcs update
**	
**
**
*/
