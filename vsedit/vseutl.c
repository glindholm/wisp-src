static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*	      Copyright (c) 1988, 1989, 1990, 1991, 1992, 1993		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*----
General utilties and routines for miscellaneous field
and file manipulation.
------*/

#ifndef VMS
#include <sys/types.h>
#include <sys/stat.h>
#endif
#ifdef VMS
#include <stat.h>
#endif
#include <ctype.h>
#include <string.h>

#include "idsistd.h"

#include "vseutl.h"

/*----
Truncate to last non-space
----*/
void trunc(char *str)
{
	int len;
	len = strlen(str)-1;

	while(len>=0)
	{
		if(str[len]==' '  ||
                   str[len]=='\t' ||
                   str[len]=='\r' ||
                   str[len]=='\n' ||
		   str[len]=='\214')
			str[len--] = '\0';
		else
			break;
	}
}

/*----
Add spaces to the end of a field to len.
Returns a null terminated string that is blank padded so strlen(s) == len.
*** NOTE *** s must be at least len+1 bytes long.
------*/
void untrunc(char *s, int len)
{
	int	i;

	s[len] = (char)0;

	i = strlen(s);
	if (i != len)
	{
		memset(&s[i],' ',len-i);
	}		
}

/*----
Returns true if str is spaces for len
------*/
int isblankstr(char *str, int len)
{
	while((*str) &&
	      (len)   )
		{
		if(*str != ' ')
			return(0);
		++str;
		--len;
		}
	return(1);
}

/*----
Returns true is a system named file exists.
------*/
int exists(char *name)
{
	struct stat stat_buf;
	int rc;

	rc = stat(name,&stat_buf);
	if(rc == 0)
		return(1);
	return(0);
}

/*
**	Routine:	vse_untabify()
**
**	Function:	To change tabs into spaces.
**
**	Description:	Is passed a string that may contain tabs, change them to correct number
**			of spaces.
**
**	Arguments:
**	str		The incoming string
**	size		The maximum size of str.
**
**	Globals:	None
**
**	Return:		None
**
**	Warnings:	If the expanded string is greater then size then in will be truncated to size.
**
*/
void vse_untabify(char *str, int size)
{
	char 	*tmpbuf;
	int 	iidx,oidx;
	
	tmpbuf = (char *)malloc(size);

	for (iidx=oidx=0; iidx < size && oidx < size && str[iidx]; )
	{
		switch (str[iidx])
		{
		case '\t':
			tmpbuf[oidx++]=' ';
			while (oidx % 8)
			  	tmpbuf[oidx++]=' ';
			break;
		default:
			tmpbuf[oidx++] = str[iidx];
			break;
		}
		++iidx;
	}
	tmpbuf[(oidx<size)?oidx:size-1]=(char)0;
	strncpy(str,tmpbuf,size-1);
	str[size-1]=(char)0;
	free(tmpbuf);
}
/*
**	History:
**	$Log: vseutl.c,v $
**	Revision 1.10  1996-09-03 18:24:13-04  gsl
**	drcs update
**
**
**
*/
