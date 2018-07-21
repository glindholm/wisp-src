/*
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
** NOTICE:
** Confidential, unpublished property of NeoMedia Technologies, Inc.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of NeoMedia 
** Technologies, Inc. is strictly prohibited.
** 
*/

/*
**	File:		wanguid.c
**
**	Project:	wisp/lib
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
#include <pwd.h>
#endif

#include "idsistd.h"
#include "wdefines.h"
#include "wanguid.h"
#include "wglobals.h"
#include "idsisubs.h"
#include "wisplib.h"
#include "wmalloc.h"
#include "wperson.h"

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

/*
**	Static Function Prototypes
*/


void WL_reset_wanguid3(void)
{
	first_wanguid3 = 1;
}

const char *WL_wanguid3(void)								/* This routine returns the first 3 	*/
{											/* characters in the user ID returned 	*/
											/* by longuid().  Used in EXTRACT when	*/
											/* keyword is ID.			*/
											/* If the IDFIVE option is set in the	*/
											/* OPTIONS file	(opt_idfive = 1) then	*/
											/* the 3 characters starting at position*/
											/* 5 are returned.			*/
	static	char	uid3[4];
	char	tstr[40];
	int	i,x;

#ifdef NISEEAST
	if (first_wanguid3)
	{
		/*
		**	Custom patch for NISEEAST to emulate the way Wang Resource
		**	works.  Get the userid from environmment variable ID or VSID.
		**	If neither is set then default to the WISP way.
		*/
		char *ptr;
		int len;
					
		ptr = getenv("ID");			/* Check if ID set 			*/
		if (!ptr || !ptr[0])			/* If ID not set then check VSID 	*/
		{
			ptr = getenv("VSID");
		}
		if (ptr && ptr[0])			/* If ID or VSID the use it		*/
		{

			memset(uid3,' ',3);
			len = strlen(ptr);
			if (len > 3) len = 3;
			memcpy(uid3, ptr, len);

			upper_mem(uid3,3);		/* Make UPPERCASE.			*/
			uid3[3] = '\0';

			first_wanguid3 = 0;
		}
	}
	
#endif /* NISEEAST */

	if ( first_wanguid3 )
	{
		first_wanguid3 = 0;

		uid3[0]=' '; uid3[1]=' '; uid3[2]=' ';
		strcpy(tstr, WL_longuid());
		x = 0;                                          			/* Initial values.			*/

		if ( OPTION_IDFIVE )
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


const char *WL_numuid3(void)								/* This routine returns the last 3 	*/
{											/* numerals in the user ID returned 	*/
											/* by getuid().  Used in EXTRACT when	*/
											/* keyword is ID and the IDNUMERIC	*/
											/* option is set in the OPTIONS file	*/
											/* (IDNUMERIC).				*/
	static	char	numid3[4];
	static	int	first = 1;

	if ( first )
	{
		char	uid_str[20];
		long	uid;

		first = 0;

		uid = getuid();
		sprintf( uid_str, "%08ld", uid );		/* Convert to an 8 digit string with leading zeros 	*/
		memcpy( numid3, &uid_str[8-3], 3 );		/* Store the last 3 numerals of UID.			*/
		numid3[3] = '\0';
	}

	return numid3;
}

#ifdef unix
const char *WL_longuid(void)							/* This routine will return the long userid	*/
{										/* It's lenght is system dependant so the	*/
										/* receiver has to be long enough. It will be	*/
										/* null terminated and case sensitive.		*/
	static	char	*uid = NULL;

	if ( NULL == uid )
	{
		struct  passwd *p;
		uid_t   euid;
		char	buf[80];
	
		euid = geteuid();
		p = getpwuid(euid);

		if (NULL != p)
		{
			strcpy(buf,p->pw_name);
		}
		else
		{
			sprintf(buf,"EUID=%d", (int)euid);
		}
		uid = wisp_strdup(buf);
	}

	return uid ;
}
#endif

/*
**	History:
**	$Log: wanguid.c,v $
**	Revision 1.20  2003/02/04 16:30:02  gsl
**	Fix -Wall warnings
**	
**	Revision 1.19  2003/01/31 18:54:37  gsl
**	Fix copyright header
**	
**	Revision 1.18  2002/07/12 20:40:40  gsl
**	Global unique WL_ changes
**	
**	Revision 1.17  2002/07/11 20:29:16  gsl
**	Fix WL_ globals
**	
**	Revision 1.16  2002/07/10 21:05:29  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.15  2002/07/09 04:13:55  gsl
**	Rename global WISPLIB routines WL_ for uniqueness
**	
**	Revision 1.14  2002/07/02 21:15:32  gsl
**	Rename wstrdup
**	
**	Revision 1.13  2001/11/27 21:58:47  gsl
**	Change longuid() to use geteuid()
**	
**	Revision 1.12  1998-03-31 13:48:00-05  gsl
**	Move NISEEAST patch from EXTRACT ID to here
**
**	Revision 1.11  1996-10-25 17:08:11-04  gsl
**	Fix to return const ptrs
**
**	Revision 1.10  1996-07-08 16:39:47-07  gsl
**	fix for NT
**
**	Revision 1.9  1995-05-22 07:44:03-07  gsl
**	Fixed problem with WL_numuid3() which was causing sig 11 on BSD machines
**	when used with the IDNUMERIC option.
**
**
**
*/
