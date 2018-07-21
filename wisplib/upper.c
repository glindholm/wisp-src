/*
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
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
*/


/* UPPER ... convert a string from lower case to upper case									*/

#include <string.h>
#include <ctype.h>
#include "idsistd.h"
#include "wisplib.h"
#include "werrlog.h"

void UPPER(char *line, int4 *len)
{
	int 	i;
	int4	length;

	WL_wtrace_entry("UPPER");

	length = WL_get_swap(len);

	for (i=0; i < length; i++)
	{
		line[i] = toupper(line[i]);
	}
}
/*
**	History:
**	$Log: upper.c,v $
**	Revision 1.13  2003/01/31 18:54:37  gsl
**	Fix copyright header
**	
**	Revision 1.12  2002/12/10 17:09:15  gsl
**	Use WL_wtrace for all warning messages (odd error codes)
**	
**	Revision 1.11  2002/07/12 17:01:02  gsl
**	Make WL_ global unique changes
**	
**	Revision 1.10  1996/08/19 22:33:02  gsl
**	drcs update
**	
**
**
*/
