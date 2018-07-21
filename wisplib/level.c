/*
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
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


/*
**	File:		level.c
**
**	Purpose:	To maintain link-level counter.
**
**	Routines:	WL_linklevel()	Return the current link-level
**			WL_newlevel()	Increment the link-level counter
**			WL_oldlevel()	Decrement the link-level counter
**			WL_zerolevel()	Set the link-level to zero.
**
**	Static:		WL_setlevel()	Stores the link-level counter in an environmental variable.
**
**	History:
**	08/12/92	Written by GSL
**
*/

#include <stdio.h>
#include <stdlib.h>
#include "idsistd.h"
#include "level.h"
#include "wisplib.h"

#define ENV_LINKLEVEL	"WISPLINKLEVEL"

static	int	link_level;							/* The link-level counter			*/
static 	int	first=1;							/* The first time flag				*/

/*
**	Routine:	WL_linklevel()
**
**	Function:	Return the current link-level
**
**	Description:	If link_level is known then just return it, otherwise get it from the environmental variable.
**
**	Arguments:	None
**
**	Globals:	link_level	The link-level counter.
**			first		The first time flag.
**
**	Return:		The link-level
**
**	Warnings:	None
**
**	History:	
**	08/12/92	Written by GSL
**
*/

int WL_linklevel(void)
{
	char	*ptr;

	if (first)
	{
		first = 0;
		link_level = 0;
		ptr = getenv(ENV_LINKLEVEL);
		if (ptr)
		{
			sscanf(ptr,"%d",&link_level);
		}
	}

	return( link_level );
}

/*
**	Routine:	WL_newlevel()
**
**	Function:	Increment the link-level counter.
**
**	Description:	Increment the link-level counter and store it in an environmental variable/symbol.
**
**	Arguments:	None
**
**	Globals:	link_level	The link-level counter.
**
**	Return:		The new link-level
**
**	Warnings:	None
**
**	History:	
**	08/12/92	Written by GSL
**
*/

int WL_newlevel(void)
{
	return( WL_setlevel(WL_linklevel()+1) );
}

/*
**	Routine:	WL_oldlevel()
**
**	Function:	Decrement the link-level counter.
**
**	Description:	Decrement the link-level counter and store it in an environmental variable/symbol.
**
**	Arguments:	None
**
**	Globals:	link_level	The link-level counter.
**
**	Return:		The new link-level
**
**	Warnings:	None
**
**	History:	
**	08/12/92	Written by GSL
**
*/

int WL_oldlevel(void)
{
	return( WL_setlevel(WL_linklevel()-1) );
}

/*
**	Routine:	WL_zerolevel()
**
**	Function:	Set the link-level counter to zero.
**
**	Description:	This routine resets the link-level counter to zero.  This is used by SUBMIT() and wchain().
**
**	Arguments:	None
**
**	Globals:	link_level	The link-level counter.
**
**	Return:		The new link-level
**
**	Warnings:	None
**
**	History:	
**	08/12/92	Written by GSL
**
*/

int WL_zerolevel(void)
{
	return(WL_setlevel(0));
}

/*
**	Routine:	WL_setlevel()
**
**	Function:	Stores the link_level in an environmental variable.
**
**	Arguments:	level 		The link level.
**
**	Globals:	link_level	The link-level counter.
**
**	Return:		level
**
**	Warnings:	None
**
**	History:	
**	08/12/92	Written by GSL
**
*/
int WL_setlevel(int level)
{
	char	buff[128];

	link_level = level;
	if (link_level < 0) link_level = 0;

	sprintf(buff,"%s=%d",ENV_LINKLEVEL,link_level);					/* Store the link_level in env		*/
	WL_setenvstr(buff);

	return( link_level );
}
/*
**	History:
**	$Log: level.c,v $
**	Revision 1.12  2003/01/31 17:33:55  gsl
**	Fix  copyright header
**	
**	Revision 1.11  2002/07/11 15:21:41  gsl
**	Fix WL_ globals
**	
**	Revision 1.10  2002/07/09 04:14:01  gsl
**	Rename global WISPLIB routines WL_ for uniqueness
**	
**	Revision 1.9  2002/07/01 04:02:38  gsl
**	Replaced globals with accessors & mutators
**	
**	Revision 1.8  2002/06/21 03:10:37  gsl
**	Remove VMS & MSDOS
**	
**	Revision 1.7  1996/08/19 22:32:25  gsl
**	drcs update
**	
**
**
*/
