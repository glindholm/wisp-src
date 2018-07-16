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

#ifdef unix
#include <sys/types.h>
#include <sys/stat.h>
#endif
#ifdef VMS
#include <stat.h>
#endif
#include <ctype.h>
#include "idsistd.h"

/*----
Truncate to last non-space
----*/
trunc(str)
char *str;
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
Add spaces to the end of a field to len
------*/
untrunc(s,len)
char *s,len;
{
	while(strlen(s) < len)
		strcat(s," ");
}

/*----
Returns true if str is spaces for len
------*/
isblank(str,len)
char *str;
int len;
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
exists(name)
char *name;
{
	struct stat stat_buf;
	int rc;

	rc = stat(name,&stat_buf);
	if(rc == 0)
		return(1);
	return(0);
}

/*
**	Routine:	untabify()
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
untabify(str,size)
char *str;
int size;
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

		
	       
