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

/*
**	File:		operator.c
**
**	Purpose:	To hold routines dealing with operator privledges.
**
**	Routines:	
**	woperator()	Tests if currect process is an operator.
**
**
*/

#ifdef unix
/*
**	Routine:	woperator()
**
**	Function:	Tests if currect process is an operator.
**
**	Description:	Check if effective or real UID is root.
**
**			(Later may add someway of specifing a list of users as 
**			being "operators".)
**
**	Arguments:	None
**
**	Globals:	None
**
**	Return:
**	1		Is an operator
**	0		Not an operator
**
**	Warnings:	None
**
**	History:	
**	11/30/93	Written by GSL
**
*/

int woperator()
{
	if (0==getuid() || 0==geteuid())
	{
		return 1;
	}
	else
	{
		return 0;
	}
}
#endif
/*
**	History:
**	$Log: operator.c,v $
**	Revision 1.5  1996/08/19 22:32:37  gsl
**	drcs update
**	
**
**
*/
