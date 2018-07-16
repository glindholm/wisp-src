static char copyright[]="Copyright (c) 1988-1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "kcsio.h"

static char sccs_id[]="@(#)vscrkwfo.c	1.2 10/31/93";

/* now included in vscsio.c
kcsio_wfopen(mode,kfb)
long mode;
KCSIO_BLOCK *kfb;
{
	wfopen(&mode,
			kfb->_volume,kfb->_library,kfb->_name,kfb->_sys_name,
			kfb->_prname);
	strunc(kfb->_sys_name);
}
*/

void strunc(char *dest)
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
void unstrunc(char *str, int len)
{
	while(*str)
		{
		++str;
		--len;
		}
	while(len-- > 0)
		*str++ = ' ';
}

/*
**	History:
**	$Log: vscrkwfo.c,v $
**	Revision 1.3  1996-10-02 12:10:46-04  gsl
**	Add standard headers
**	Fix prototypes
**
**
**
*/
