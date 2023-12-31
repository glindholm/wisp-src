/*
******************************************************************************
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
**
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
**
******************************************************************************
*/

/*

	wdelay		Wisp Delay

			This utility is passed a clock time HHMM and sleeps until that time.

			If it is 8:00PM (2000) and you set wdelay to 6:00PM (1800) it will sleep until the next day.

*/

#include <sys/types.h>
#include <time.h>
#include <stdio.h>
#include <stdlib.h>

#ifdef unix
#include <unistd.h>
#endif

static int getnowtime();
static void wakeup();
static void badusage();

int main(argc,argv)
int	argc;
char	*argv[];
{
	int	endtime;							/* Time to sleep to   (24 hour min clock)	*/
	int	nowtime;							/* The time right now (24 hour min clock)	*/
	unsigned int numseconds;						/* number of seconds to sleep			*/
	unsigned int unslept;							/* number of seconds left to sleep		*/
	int	trouble;							/* Trouble counter				*/
	int	temp;

	if ( argc != 2 ) badusage();						/* Not enough args				*/

	temp = atoi(argv[1]);							/* Make endtime into int			*/

	if ( temp > 2400 || temp < 0 ) badusage();				/* Test range					*/

	if ( (temp % 100) > 59 ) badusage();					/* MM of HHMM can't be greater then 59		*/

	endtime = temp % 100 + (temp/100 * 60);					/* make into 24 hour minute clock		*/

	if ( endtime > 24*60 ) badusage();					/* test range					*/

	nowtime = getnowtime();							/* get the time					*/

	if ( nowtime > endtime )
		numseconds = (24*60 - nowtime + endtime) * 60;			/* number of seconds until endtime tomorow	*/
	else
		numseconds = (endtime - nowtime) * 60;				/* number of seconds until endtime		*/

	trouble = 0;

	while( numseconds > 5 )
	{
		unslept = sleep( numseconds );					/* try to sleep					*/
	
		if ( numseconds - unslept <= 2 )				/* if we slept for < then 2 seconds ....	*/
		{
			if ( trouble++ > 100 ) wakeup(1);			/* Were in a busy loop - so wakeup		*/
		}
		numseconds = unslept;
	}
	wakeup(0);
	return 0;
}

static int getnowtime()
{
	struct tm *tm_ptr;							/* Ptr to time struct				*/
	time_t	tt;

	tt = time(0);
	tm_ptr = localtime(&tt);
	return( tm_ptr->tm_hour * 60 + tm_ptr->tm_min );
}

static void wakeup(rc)
int rc;
{
	time_t	tt;

	if (rc) printf( "wdelay: SORRY I COULDN'T SLEEP ANY LONGER\n");
	tt = time(0);
	printf( "wdelay: %s\n", ctime(&tt) );
	exit(rc);
}

static void badusage()
{
	printf("\n");
	printf("Usage: wdelay {HHMM}\n");
	printf("\n");
	printf("       wdelay sleeps until time HHMM.\n");
	printf("\n");
	printf("       Returns: 0 = Success  1 = Awoke early\n");
	printf("\n");
	exit(-1);
} 
/*
**	History:
**	$Log: wdelay.c,v $
**	Revision 1.9  2003/02/04 20:42:49  gsl
**	fix -Wall warnings
**	
**	Revision 1.8  2003/02/04 18:50:25  gsl
**	fix copyright header
**	
**	Revision 1.7  1996/07/23 18:13:06  gsl
**	drcs update
**	
**
**
*/
