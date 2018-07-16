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
#include "wanguid.h"

#ifdef WIN32
void passwdname(char* name)
{
	strcpy(name, longuid());
}
#endif	/* WIN32 */

#ifdef unix
#include <sys/types.h>
#include <unistd.h>
#include <pwd.h>

void passwdname(char* name)
{
	struct  passwd *p;
	uid_t   euid;
	
	euid = geteuid();
	p = getpwuid(euid);

	if (NULL != p)
	{
		strcpy(name,p->pw_gecos);
	}
	else
        {
		sprintf(name,longuid());
        }
}
#endif	/* unix */

/*
**	History:
**	$Log: pwdname.c,v $
**	Revision 1.14  2001-11-27 17:02:19-05  gsl
**	Change to use longuid()
**
**	Revision 1.13  2001-11-20 10:58:35-05  gsl
**	Change cuserid(NULL) to getpwuid(geteuid()))
**
**	Revision 1.12  1997-03-12 13:00:17-05  gsl
**	change to use WIN32 define
**
**	Revision 1.11  1996-08-19 18:32:40-04  gsl
**	drcs update
**
**
**
*/
