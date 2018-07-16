static char copyright[]="Copyright (c) 1988-1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
/* these routines are needed to allow C programs to link against the ACUCOBOL runtime
   (to access the vision portion only) to prevent conflicts with the main() function
   that is present in runcbl.a.. do not modify
 */

#ifdef INCLUDE_VDISPIDX
#include <stdio.h>
char *Agetenv(name)
char *name;
{
	extern char *getenv();
	return getenv(name);
}
char *Abasename(pathname)
char *pathname;
{
	static char name[256];
	extern char *strrchr();
	char *p;

#if defined(unix) || defined(MSDOS)
# ifdef unix	
	p = strrchr(pathname,'/');
# else
	p = strrchr(pathname,'\\');	
# endif
#endif
	if (p)
	{
		strcpy(name,p);
		return name;
	}
	else 
	{
		return NULL;
	}
}
#endif
/*
**	History:
**	$Log: vsn_only.c,v $
**	Revision 1.4  1996-07-23 14:13:03-04  gsl
**	drcs update
**
**
**
*/
