			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/


/*
**	wanguid.c
*/

#include <stdio.h>
#include <string.h>

#include "wdefines.h"

char *wanguid3()
{
static	char	uid3[3];
static	int	first = 1;
	char	tstr[40];
	int	i,x;

	if ( first )
	{
		first = 0;
		uid3[0]=' '; uid3[1]=' '; uid3[2]=' ';
		cuserid(tstr);					/* ask the system for user id		*/
		x = 0;                                          /* Initial values.			*/
		i = 0;						/* Initial values.			*/
		do						/* Copy the characters.			*/
		{						/* Test for Wang style.			*/
			if (tstr[i] == '_' )			/* Is this character an underscore ?	*/
			{
				i++;				/* Yup.  Skip that character.		*/
			}
			else
			{
				uid3[x++] = tstr[i++];		/* Nope.  Copy that character.		*/
			}                                                                                 
		} while (tstr[i] && x < 3);			/* Null char or rlen chars moved ?	*/
		upper_mem(uid3,3);				/* Make UPPERCASE.			*/
	}

	return( &uid3[0] );
}


char *numuid3()							/* This routine returns the last 3 	*/
{								/* numerals in the user ID returned 	*/
								/* by getuid().  Used in EXTRACT when	*/
								/* keyword is ID and the IDNUMERIC	*/
								/* option is set in the OPTIONS file	*/
								/* (opt_idnumeric = 1).			*/
	static	char	numid3[4];
	static	int	first = 1;
	char	uid_str[8];
	int	uid_len;

	if ( first )
	{
		first = 0;					/* Unset flag so only build it once.	*/

		uid_len=(int)sprintf( uid_str, "%3.3d", getuid() );	/* Convert numeric UID to a string.	*/
		strcpy( numid3, &uid_str[uid_len-3] );		/* Store the last 3 numerals of UID.	*/
	}

	return( &numid3[0] );
}


char *longuid()									/* This routine will return the long userid	*/
{										/* It's lenght is system dependant so the	*/
										/* receiver has to be long enough. It will be	*/
										/* null terminated and case sensitive.		*/
static	char	uid[40];
static	int	first = 1;

	if ( first )
	{
		first = 0;
		cuserid( uid );					/* ask the system for user id		*/
	}
	return( &uid[0] );
}

