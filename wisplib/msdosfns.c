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

/*
	MSDOSFNS.C	-	MSDOS Functions from unix system calls.


		MSDOSFNS.C - Functions written for MSDOS (many are from unix).
		WISPDMF.C  - Like WISPAIX.C, includes shutexitcobol().

	Current list of functions:

		cuserid()  [unix] returns env var USER or DEFAULT_UID.
		ttyname()  [unix] returns env var TTY  or DEFAULT_TTY.
		PARSECOM() [IDSI] parses COBOL startup command line.
*/

#if defined(MSDOS)

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>

#include "idsistd.h"
#include "wanguid.h"

#define DEFAULT_UID	"USER"
#define DEFAULT_TTY	"CON:"
#define UID_ENV_NAME	"USER"
#define TTY_ENV_NAME	"TTY"

static	char	dosuid[40] = "";

void set_cuserid(char* newid, int len)
{
	memcpy(dosuid,newid,len);
	dosuid[len] = (char)0;

	reset_wanguid3();
	reset_longuid();
}

char *cuserid(char* cuid)
{
	char	*env_uid;

	if( !dosuid[0] )
	{
		if( NULL != ( env_uid = getenv ( UID_ENV_NAME ) ) )
		{
			strcpy( dosuid, env_uid );
		}
		else
		{
			strcpy( dosuid, DEFAULT_UID );
		}
	}

	if( NULL == cuid )
	{
		return( dosuid );
	}
	else
	{
		strcpy( cuid, dosuid );
		return( cuid );
	}
}

int getuid(void)	/* This is a stub for MSDOS. It always returns a User Id == 1. */
{
	return(1);
}



const char *ttyname(int fd)
{
	static	int	first_time = 1;
	char	*env_tty;
	static	char	dostty[40] = DEFAULT_TTY;

	if( first_time )
	{
		first_time = 0 ;
		if( NULL != ( env_tty = getenv ( TTY_ENV_NAME ) ) )
		{
			strcpy( dostty, env_tty );
		}
	}

	return( dostty );
}

void PARSECOM( char* com_line, char* com_link )
{
	char	*cptr;
	int	i;

	cptr = NULL;

	for( i = 0 ; (i < 80) && ( '\0' != com_line[i] ) ; ++i )
	{
		if ( ' ' == com_line[i] )
		{
			if ( ' ' != com_line[i+1] )
			{
				cptr = &com_line[i+1];
			}
		}
	}

	if ( NULL == cptr )
	{
		strncpy( com_link, cptr, 12 );
	}
	else
	{
		memset( com_link, ' ', 12 );
	}
}


#endif	/* MSDOS  */
/*
**	History:
**	$Log: msdosfns.c,v $
**	Revision 1.9  1996-08-26 20:05:52-04  gsl
**	Moved all the NT stuff to winnt.c
**
**	Revision 1.8  1996-08-19 15:32:33-07  gsl
**	drcs update
**
**
**
*/
