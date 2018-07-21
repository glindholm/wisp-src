/*
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
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
*/


/*
**	pwdname.c
*/

#include <stdio.h>
#include <string.h>

#include "idsistd.h"
#include "wisplib.h"
#include "wanguid.h"

#ifdef WIN32
void WL_passwdname(char* name)
{
	strcpy(name, WL_longuid());
}
#endif	/* WIN32 */

#ifdef unix
#include <sys/types.h>
#include <unistd.h>
#include <pwd.h>

void WL_passwdname(char* name)
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
		sprintf(name,WL_longuid());
        }
}
#endif	/* unix */

/*
**	History:
**	$Log: pwdname.c,v $
**	Revision 1.18  2003/01/31 18:54:38  gsl
**	Fix copyright header
**	
**	Revision 1.17  2003/01/31 17:33:55  gsl
**	Fix  copyright header
**	
**	Revision 1.16  2002/07/10 21:05:22  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.15  2002/06/21 03:10:39  gsl
**	Remove VMS & MSDOS
**	
**	Revision 1.14  2001/11/27 22:02:19  gsl
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
