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

/* MENU  interface for WANG vs programs to the VAX menu system									*/

#include "idsistd.h"
#include "menu.h"

void WL_menu(char *name,	/* The name of the menu file			*/
	     char *rval)	/* The return value				*/
{
	int i;
	char lname[16],lval[6];

	for (i=0; i<8; i++)
	{
		if ((lname[i] = name[i]) == ' ') break;				/* copy the name into a local buffer		*/
	}
	lname[i++] = '.';
	lname[i++] = 'M';
	lname[i++] = 'E';
	lname[i++] = 'N';
	lname[i++] = 'U';
	lname[i++] = '\0';

	WL_menu_go(lname,lval);

	rval[0] = lval[0];							/* copy the return value			*/
	rval[1] = lval[1];
}
/*
**	History:
**	$Log: wangmenu.c,v $
**	Revision 1.11  2003/01/31 18:54:37  gsl
**	Fix copyright header
**	
**	Revision 1.10  2002/07/10 21:05:28  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.9  1996/08/19 22:33:07  gsl
**	drcs update
**	
**
**
*/
