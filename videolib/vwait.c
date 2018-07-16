static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";

/*
**	ROUTINE:	vwait()
**
**	FUNCTION:	Sleep for a while
**
**	DESCRIPTION:	The seconds and hundredths are added together and thats 
**			how long to sleep for.
**
**	ARGUMENTS:	
**	seconds		Number of seconds to sleep
**	hundredths	Number of hundredths of seconds to sleep
**
**	GLOBALS:	None
**
**	RETURN:		None
**
**	WARNINGS:	Time is not exact.
**
*/


#ifdef unix
#include <unistd.h>
void vwait(int seconds, int hundredths)
{
	unsigned sleep_secs;
	
	sleep_secs = seconds + hundredths/100;
	hundredths = hundredths % 100;

	while(sleep_secs > 0)
	{
		sleep_secs = sleep(sleep_secs);
	}

	if (hundredths > 0)
	{
		/*
		 *	Sleep for remaining microseconds (1/1000000).
		 */
		usleep((unsigned int)hundredths * 10000U);
	}	
}
#endif

#ifdef OLD_UNIX
extern unsigned sleep(unsigned secs);

void vwait(int seconds, int hundredths)
{
	unsigned sleep_secs;
	
	sleep_secs = seconds + (hundredths + 50)/100;

	while(sleep_secs > 0)
	{
		sleep_secs = sleep(sleep_secs);
	}
}
#endif


#ifdef WIN32
#include <windows.h>

void vwait(int seconds, int hundredths)
{
	Sleep(seconds * 1000 + hundredths * 10);
}
#endif


/*
**	History:
**	$Log: vwait.c,v $
**	Revision 1.12  2001-10-03 14:59:12-04  gsl
**	Solaris 2.5.1 doesn't define useconds_t so cast to unsigned
**
**	Revision 1.11  2001-09-27 15:43:47-04  gsl
**	Remove VMS code
**	Rework unix code to use usleep() for 1/100th second sleeping
**
**	Revision 1.10  1997-07-09 12:39:12-04  gsl
**	CHange interface to use only two args (secs, hundredths)
**	Rewrite for WIN32 so accurate.
**
**	Revision 1.9  1996-10-11 18:16:24-04  gsl
**	drcs update
**
**
**
*/
