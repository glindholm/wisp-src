/*
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
** $Id:$
**
** NOTICE:
** Confidential, unpublished property of NeoMedia Technologies, Inc.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of NeoMedia 
** Technologies, Inc. is strictly prohibited.
** 
** CVS
** $Source:$
** $Author: gsl $
** $Date:$
** $Revision:$
*/


#include <string.h>
#include "idsistd.h"
#include "wmalloc.h"

/*
**	Routine:	WL_untabify()
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
int WL_untabify(char* str, int size)
{
	char 	*tmpbuf;
	int 	iidx,oidx;
	
	tmpbuf = (char *)wisp_malloc(size);

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
**	Revision 1.10  2003/01/31 18:54:37  gsl
**	Fix copyright header
**	
**	Revision 1.9  2002/07/12 20:40:39  gsl
**	Global unique WL_ changes
**	
**	Revision 1.8  2002/07/02 21:15:30  gsl
**	Rename wstrdup
**	
**	Revision 1.7  2002/06/25 15:21:54  gsl
**	Change to use wmalloc()
**	
**	Revision 1.6  1996/08/19 22:33:01  gsl
**	drcs update
**	
**
**
*/
