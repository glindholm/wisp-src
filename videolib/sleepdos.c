static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";


#ifdef WIN32

#include <windows.h>

/*
**	ROUTINE:	sleep()  (for WIN32)
**
**	FUNCTION:	Emulate unix sleep() routine
**
**	DESCRIPTION:	Call Sleep(millisecs)
**
**	ARGUMENTS:	
**	secs		Number of seconds to sleep for
**
**	GLOBALS:	None
**
**	RETURN:		unslept amount of time (always 0)
**
**	WARNINGS:	None
**
*/
unsigned sleep(unsigned secs)
{
	Sleep(1000 * secs);
	return 0;
}
#endif /* WIN32 */


#if defined(MSDOS) && !defined(WATCOM)

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <sys/types.h>


/*
**	Routine:	sleep()   (for MSDOS)
**
**	Function:	sleep for MSDOS Intel C Code builder 1.0e
**
**	Description:	1.0e Code Builder doesn't come with sleep()
**
**	Arguments:
**	secs		The number of seconds to sleep.
**
**	Globals:	None
**
**	Return:		None
**
**	Warnings:	None
**
**
*/

unsigned sleep(unsigned secs)
{
	time_t	wake;

	wake = secs + time( NULL );
	while( wake > time( NULL ) );

	return( 0 );
}

#endif /* MSDOS */

/*
**	History:
**	$Log: sleepdos.c,v $
**	Revision 1.8  1997-07-08 16:20:17-04  gsl
**	Add sleep() routine for WIN32
**
**	Revision 1.7  1996-10-11 18:15:56-04  gsl
**	drcs update
**
**
**
*/
