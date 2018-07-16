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

#include <string.h>
#include "wmalloc.h"

/*
**	Routine:	untabstr()
**
**	Function:	To change tabs into spaces.
**
**	Description:	Is passed a string that may contain tabs, change them to correct number
**			of spaces.
**
**	Arguments:
**	str		The incoming string (null terminated)
**	maxsize		The maximum size of str.
**
**	Globals:	None
**
**	Return:		None
**
**	Warnings:	If the expanded string is greater then maxsize-1 then in will be truncated to maxsize-1.
**
*/
int untabstr(str,maxsize)
char *str;
int maxsize;
{
	char 	*tmpbuf;
	int 	iidx,oidx;
	
	if (!strchr(str,'\t')) return 0;
	
	tmpbuf = wmalloc(maxsize+8);

	for (iidx=oidx=0; str[iidx] && oidx < maxsize; iidx++)
	{
		if ('\t' == str[iidx])
		{
			tmpbuf[oidx++]=' ';
			while (oidx % 8)
			{
			  	tmpbuf[oidx++]=' ';
			}
		}
		else
		{
			tmpbuf[oidx++] = str[iidx];
		}
	}
	tmpbuf[(oidx<maxsize)?oidx:maxsize-1]=(char)0;
	strcpy(str,tmpbuf);
	wfree(tmpbuf);
	return 0;
}
/*
**	History:
**	$Log: untabstr.c,v $
**	Revision 1.6  1996/08/31 01:56:11  gsl
**	drcs update
**	
**
**
*/
