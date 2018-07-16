	/************************************************************************/
	/*									*/
	/*	        WISP - Wang Interchange Source Pre-processor		*/
	/*	      Copyright (c) 1988, 1989, 1990, 1991, 1992, 1993		*/
	/*	 An unpublished work of International Digital Scientific Inc.	*/
	/*			    All rights reserved.			*/
	/*									*/
	/************************************************************************/


#ifdef MSDOS

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

#endif /* DACU */
