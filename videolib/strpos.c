static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";

#ifdef OLD
/*
**	strpos() has been replaced by the ansi standard routine strstr()
*/
#include <string.h>

int strpos(src,srch) unsigned char *src,*srch;						/* Search a string for a string.	*/
{
	int i,j0;
	j0 = strlen(src) - strlen(srch) + 1;						/* Stop search when j0 = 0.		*/
	i = 0;
	while (*src && (j0 > 0))
	{
		if (!memcmp(srch,src,strlen(srch))) return(i);				/* If compare is good, return.		*/
		src++;
		i++;
		j0--;
	}
	return(-1);
}
#endif

/*
**	History:
**	$Log: strpos.c,v $
**	Revision 1.9  1997/01/08 21:37:39  gsl
**	Removed strpos() as all calls replaced with ansi standard strstr() calls.
**	
**	Revision 1.8  1996-10-11 15:15:57-07  gsl
**	drcs update
**
**
**
*/
