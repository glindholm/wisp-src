static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
	/************************************************************************/
	/*									*/
	/*	        WISP - Wang Interchange Source Pre-processor		*/
	/*	      Copyright (c) 1988, 1989, 1990, 1991, 1992, 1993		*/
	/*	 An unpublished work of International Digital Scientific Inc.	*/
	/*			    All rights reserved.			*/
	/*									*/
	/************************************************************************/


#if defined(MSDOS) && !defined(WATCOM)

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <sys/types.h>


/*
**	Routine:	sleep()
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

unsigned sleep(secs)
unsigned secs;
{
	time_t	wake;

	wake = secs + time( NULL );
	while( wake > time( NULL ) );

	return( 0 );
}

#else
static int dummy_sleepdos;
#endif
/*
**	History:
**	$Log: sleepdos.c,v $
**	Revision 1.6  1996-07-23 14:17:51-04  gsl
**	drcs update
**
**
**
*/
