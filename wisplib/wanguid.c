static char copyright[]="Copyright (c) 1988-1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
/*
**	File:		wanguid.c
**
**	Project:	wisp/lib
**
**	RCS:		$Source:$
**
**	Purpose:	Wang Userid's
**
**	Routines:	
*/

/*
**	Includes
*/

#include <stdio.h>
#include <string.h>
#include <sys/types.h>

#ifdef unix
#include <unistd.h>
#endif

#include "idsistd.h"
#include "wdefines.h"
#include "wanguid.h"
#include "wglobals.h"
#include "idsisubs.h"
#include "wisplib.h"

/*
**	Structures and Defines
*/

/*
**	Globals and Externals
*/

/*
**	Static data
*/
static	int	first_wanguid3 = 1;
static	int	first_longuid = 1;

/*
**	Static Function Prototypes
*/


void reset_wanguid3(void)
{
	first_wanguid3 = 1;
}

const char *wanguid3(void)								/* This routine returns the first 3 	*/
{											/* characters in the user ID returned 	*/
											/* by cuserid().  Used in EXTRACT when	*/
											/* keyword is ID.			*/
											/* If the IDFIVE option is set in the	*/
											/* OPTIONS file	(opt_idfive = 1) then	*/
											/* the 3 characters starting at position*/
											/* 5 are returned.			*/
	static	char	uid3[4];
	char	tstr[40];
	int	i,x;

	if ( first_wanguid3 )
	{
		first_wanguid3 = 0;

		uid3[0]=' '; uid3[1]=' '; uid3[2]=' ';
		cuserid(tstr);								/* Ask the system for user id		*/
		x = 0;                                          			/* Initial values.			*/

		if ( opt_idfive )
		{
			i = 4;								/* Get the 3 chars starting at pos 5,	*/
		}
		else i = 0;								/* else get 3 chars starting at pos 1.	*/

		do									/* Copy the characters.			*/
		{									/* Test for Wang style.			*/
			if (tstr[i] == '_' )						/* Is this character an underscore ?	*/
			{
				i++;							/* Yup.  Skip that character.		*/
			}
			else
			{
				uid3[x++] = tstr[i++];					/* Nope.  Copy that character.		*/
			}                                                                                 
		} while (tstr[i] && x < 3);						/* Null char or rlen chars moved ?	*/
		upper_mem(uid3,3);							/* Make UPPERCASE.			*/
		uid3[3] = '\0';
	}

	return( uid3 );
}


const char *numuid3(void)								/* This routine returns the last 3 	*/
{											/* numerals in the user ID returned 	*/
											/* by getuid().  Used in EXTRACT when	*/
											/* keyword is ID and the IDNUMERIC	*/
											/* option is set in the OPTIONS file	*/
											/* (opt_idnumeric = 1).			*/
	static	char	numid3[4];
	static	int	first = 1;

	if ( first )
	{
		char	uid_str[20];
		long	uid;

		first = 0;

		uid = getuid();
		sprintf( uid_str, "%08.8ld", uid );		/* Convert to an 8 digit string with leading zeros 	*/
		memcpy( numid3, &uid_str[8-3], 3 );		/* Store the last 3 numerals of UID.			*/
		numid3[3] = '\0';
	}

	return numid3;
}

void reset_longuid(void)
{
	first_longuid = 1;
}

const char *longuid(void)							/* This routine will return the long userid	*/
{										/* It's lenght is system dependant so the	*/
										/* receiver has to be long enough. It will be	*/
										/* null terminated and case sensitive.		*/
	static	char	uid[40];

	if ( first_longuid )
	{
		first_longuid = 0;
		cuserid( uid );							/* ask the system for user id			*/
	}
	return( uid );
}

/*
**	History:
**	$Log: wanguid.c,v $
**	Revision 1.11  1996-10-25 17:08:11-04  gsl
**	Fix to return const ptrs
**
**	Revision 1.10  1996-07-08 16:39:47-07  gsl
**	fix for NT
**
**	Revision 1.9  1995-05-22 07:44:03-07  gsl
**	Fixed problem with numuid3() which was causing sig 11 on BSD machines
**	when used with the IDNUMERIC option.
**
**
**
*/
