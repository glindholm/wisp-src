/*
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
** NOTICE:
** Confidential, unpublished property of Shell Stream Software LLC.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of Shell Stream Software LLC
** is strictly prohibited.
** 
*/


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
**	Revision 1.7  2003/02/04 17:33:19  gsl
**	fix copyright header
**	
**	Revision 1.6  1996/08/31 01:56:11  gsl
**	drcs update
**	
**
**
*/
