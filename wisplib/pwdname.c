static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
**	pwdname.c
*/

#include <stdio.h>

#include "idsistd.h"
#include "wisplib.h"

#if defined(MSDOS) || defined(WIN32)
void passwdname(char* name)
{
	cuserid( name );						/* Use short name from environment variable WISP-UID	*/
}
#endif	/* MSDOS || WIN32 */

#ifdef unix
#include <pwd.h>

void passwdname(char* name)
{
	struct  passwd *p;
	char    *id;
#ifdef OSF1_ALPHA
	char *cuserid();
#endif
	
	id = cuserid(NULL);
	p = getpwnam(id);
	if (p)
	{
		strcpy(name,p->pw_gecos);
	}
	else
        {
		strcpy(name,id);
        }
}
#endif	/* unix */

#ifdef VMS
#include <descrip.h>
#include <uaidef.h>
#include <ssdef.h>

authowner(name)										/* Use SYSUAF.DAT for VMS.		*/
char *name;
{
	long	stat_ss;
	int	retlen;
	int	size_byte;
	register int i;
	char	usrnam[13], ret_owner[32], *cptr, *nptr;
	struct	{
		short	buflen;								/* the length of the buffer		*/
		short 	item_code;							/* the code for the request to GETUAI	*/
		char 	*bufptr;							/* a pointer to the buffer		*/
		int	*bretlen;							/* the return length of the buffer	*/
		int	endbuf;								/* the end of the buffer		*/
	} uidbuf;
#include "pwdname.d"

	uidbuf.item_code = UAI$_OWNER;
	uidbuf.buflen = 32;
	uidbuf.bufptr = ret_owner;
	uidbuf.bretlen = &retlen;
	uidbuf.endbuf = 0;

	strncpy(usrnam,longuid(),12);							/* First get the username.		*/
	usrnam[12] = '\0';								/* Terminate the user name string.	*/
	aname_desc.dsc$w_length = strlen(usrnam);					/* Set the length of desriptor.		*/

	stat_ss = sys$getuai((long) 0, (long) 0, &aname_desc, &uidbuf, (long) 0, (long) 0, (long) 0);

	if (stat_ss != SS$_NORMAL) return(stat_ss);					/* Some error.				*/

	cptr = ret_owner;
	size_byte = *cptr++;								/* Assign and step over size byte prefix.*/
	nptr = name;
	for (i = 0; i < size_byte; i++)							/* Maximum length is 31.		*/
	{
		*nptr++ = *cptr++;							/* Copy the returned account owner	*/
	}										/*  to the passed in variable.		*/

	*nptr = '\0';									/* Null terminate the string.		*/
}
#endif	/* VMS */
/*
**	History:
**	$Log: pwdname.c,v $
**	Revision 1.12  1997-03-12 13:00:17-05  gsl
**	change to use WIN32 define
**
**	Revision 1.11  1996-08-19 18:32:40-04  gsl
**	drcs update
**
**
**
*/
