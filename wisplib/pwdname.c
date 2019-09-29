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
		strcpy(name,WL_longuid());
	}
}
#endif	/* unix */
