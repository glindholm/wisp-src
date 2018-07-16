			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
**	passwdname.c
*/

#ifdef MSDOS
#include <stdio.h>

void passwdname(name)
char *name;
{
	cuserid( name );						/* Use short name from environment variable WISP-UID	*/
}
#endif	/* MSDOS */

#ifdef unix
#include <stdio.h>
#include <pwd.h>

passwdname(name)
char *name;
{
	struct  passwd *p;
	char    *id;

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


#ifdef OLD
passwdname(name)
char *name;
{
	char	*id;
	FILE	*fp;
	char	inline[256], *inptr, src[80];
	int	srclen;

	id = cuserid(0);
	fp = fopen("/etc/passwd","r");

	if (!fp)
	{
		strcpy(name,id);
		return;
	}

	strcpy(src,id);
	strcat(src, ":");
	srclen = strlen(src);

	while(fgets(inline,255,fp))
	{

		if (0 == memcmp(inline,src,srclen))
		{
			if (!(inptr = strtok(inline,":"))) break;
			if (!(inptr = strtok(0,":"))) break;
			if (!(inptr = strtok(0,":"))) break;
			if (!(inptr = strtok(0,":"))) break;
			if (!(inptr = strtok(0,":"))) break;
			strcpy(name,inptr);
			return;
		}
	}

	fclose(fp);
	strcpy(name,id);
	return;
}
#endif	/* OLD */

