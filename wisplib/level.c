			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		 Copyright (c) 1988, 1989, 1990, 1991, 1992		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
**	File:		level.c
**
**	Purpose:	To maintain link-level counter.
**
**	Routines:	linklevel()	Return the current link-level
**			newlevel()	Increment the link-level counter
**			oldlevel()	Decrement the link-level counter
**			zerolevel()	Set the link-level to zero.
**
**	Static:		setlevel()	Stores the link-level counter in an environmental variable.
**
**	History:
**	08/12/92	Written by GSL
**
*/

#include "idsistd.h"

#define ENV_LINKLEVEL	"WISPLINKLEVEL"
#define SYMB_LINKLEVEL	"$W_LINK_LEVEL"

char *getenv();

static	int	link_level;							/* The link-level counter			*/
static 	int	first=1;							/* The first time flag				*/

/*
**	Routine:	linklevel()
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

int linklevel()
{
	char	*ptr;
	char	buff[20];

	if (first)
	{
		first = 0;
		link_level = 0;
#ifdef VMS
		if (getsymb(SYMB_LINKLEVEL,buff,0))
		{
			ptr = (char)0;
		}
		else
		{
			ptr = buff;
		}
#else
		ptr = getenv(ENV_LINKLEVEL);
#endif
		if (ptr)
		{
			sscanf(ptr,"%d",&link_level);
		}
	}

	return( link_level );
}

/*
**	Routine:	newlevel()
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

int newlevel()
{
	return( setlevel(linklevel()+1) );
}

/*
**	Routine:	oldlevel()
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

int oldlevel()
{
	return( setlevel(linklevel()-1) );
}

/*
**	Routine:	zerolevel()
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

int zerolevel()
{
	return(setlevel(0));
}

/*
**	Routine:	setlevel()
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
int setlevel(level)
int level;
{
	char	buff[128];

	link_level = level;
	if (link_level < 0) link_level = 0;

#ifdef VMS
	sprintf(buff,"%d",link_level);
	setsymb(SYMB_LINKLEVEL,buff,strlen(buff),0);					/* Store the link_level in a symbol	*/
#else
	sprintf(buff,"%s=%d",ENV_LINKLEVEL,link_level);					/* Store the link_level in env		*/
	setenvstr(buff);
#endif

	return( link_level );
}
