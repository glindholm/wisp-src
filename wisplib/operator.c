/*
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
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


/*
**	File:		operator.c
**
**	Purpose:	To hold routines dealing with operator privledges.
**
**	Routines:	
**	WL_woperator()	Tests if currect process is an operator.
**
**
*/

#ifdef unix
#include <sys/types.h>
#include <unistd.h>

/*
**	Routine:	WL_woperator()
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

int WL_woperator(void)
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
**	Revision 1.9  2003/01/31 18:48:36  gsl
**	Fix  copyright header and -Wall warnings
**	
**	Revision 1.8  2003/01/31 18:25:18  gsl
**	Fix  copyright header and -Wall warnings
**	
**	Revision 1.7  2003/01/31 17:33:55  gsl
**	Fix  copyright header
**	
**	Revision 1.6  2002/07/12 19:10:14  gsl
**	Global unique WL_ changes
**	
**	Revision 1.5  1996/08/19 22:32:37  gsl
**	drcs update
**	
**
**
*/
