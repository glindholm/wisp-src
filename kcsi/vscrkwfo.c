static char copyright[]="Copyright (c) 1988-1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "kcsio.h"


#ifdef OLD

/*
DUPLICATE with rcvs.c
*/
void KCSI_strunc(char *dest)
{
	char *end;

	end = dest + strlen(dest);
	if(end == dest)
		return;
	--end;
	while((*end == ' ') && (end >= dest))
		*end-- = 0;
}
/*----
Pad a str from first null to len with spaces.
------*/
void KCSI_unstrunc(char *str, int len)
{
	while(*str)
		{
		++str;
		--len;
		}
	while(len-- > 0)
		*str++ = ' ';
}
#endif /* OLD */

/*
**	History:
**	$Log: vscrkwfo.c,v $
**	Revision 1.3.2.1  2002/11/12 15:56:42  gsl
**	Sync with $HEAD Combined KCSI 4.0.00
**	
**	Revision 1.6  2002/10/22 17:59:02  gsl
**	Combined KCSI
**	
**	Revision 1.5  2002/07/25 15:20:22  gsl
**	Globals
**	
**	Revision 1.4  2002/06/21 20:48:14  gsl
**	Rework the IS_xxx bit flags and now include from wcommon.h instead of duplicate
**	definitions.
**	
**	Revision 1.3  1996/10/02 16:10:46  gsl
**	Add standard headers
**	Fix prototypes
**	
**
**
*/
