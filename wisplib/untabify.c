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
#include "idsistd.h"
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
int untabify(char* str, int size)
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
	return 0;
}
/*
**	History:
**	$Log: untabify.c,v $
**	Revision 1.6  1996/08/19 22:33:01  gsl
**	drcs update
**	
**
**
*/
