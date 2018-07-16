static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*	      Copyright (c) 1988,1989,1990,1991,1992,1993,1994		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
**	File:		dbfile.c
**
**	Purpose:	To hold routines for DataBase files.
**
**	Routines:	
**	setdbfile()	Set the IS_DBFILE flag in a status word.
**
**
*/

#include "idsistd.h"
#include "wcommon.h"

/*
**	Routine:	setdbfile()
**
**	Function:	Set the IS_DBFILE flag in a status word.
**
**	Description:	If flag is set then set IS_DBFILE otherwise clear it.
**
**	Arguments:
**	mode		The file status mode
**	flag		The state we want IS_DBFILE set to.
**
**	Globals:	None
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	
**	01/06/94	Written by GSL
**
*/
void setdbfile(mode,flag)
int4	*mode;
int	flag;
{
	if (flag)
	{
		*mode |= IS_DBFILE;
	}
	else
	{
		if (*mode & IS_DBFILE)
		{
			*mode ^= IS_DBFILE;
		}
	}
}
/*
**	History:
**	$Log: dbfile.c,v $
**	Revision 1.5  1996-08-19 18:32:16-04  gsl
**	drcs update
**
**
**
*/
