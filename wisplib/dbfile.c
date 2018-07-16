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
void setdbfile(int4 *mode, int flag)
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
void WL_setdbfile(int4 *mode, int flag)
{
	setdbfile(mode, flag);
}

/*
**	History:
**	$Log: dbfile.c,v $
**	Revision 1.5.2.1  2002/11/14 21:12:21  gsl
**	Replace WISPFILEXT and WISPRETURNCODE with set/get calls
**	
**	Revision 1.5  1996/08/19 22:32:16  gsl
**	drcs update
**	
**
**
*/
