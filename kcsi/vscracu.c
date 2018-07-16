static char copyright[]="Copyright (c) 1988-1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";

#ifdef OLD
#include <stdio.h>

static char sccs_id[]="@(#)vscracu.c	1.1 8/15/93";

char *Agetenv(char *name)
{
	return(NULL);
}

char *Abasename(char *pathname)
{
	return(NULL);
}

/*----
Added for AcuCOBOL vversions 2.3 +
------*/
int eprintf(void)
{
	return 0;
}
#endif


/*
**	History:
**	$Log: vscracu.c,v $
**	Revision 1.5  1998/11/05 14:39:36  gsl
**	Not needed anymore
**	
**	Revision 1.4  1996-10-03 15:07:05-04  gsl
**	Fixed warning in eprintf()
**
**	Revision 1.3  1996-10-02 09:07:11-07  gsl
**	Add standard headers
**
**
**
*/
