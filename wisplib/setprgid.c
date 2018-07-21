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


/*
**	SETPROGID()
*/

#include <string.h>
#include "setprgid.h"

static char WISPPROGID[9];

/* Set global variable to current program id.	*/
void SETPROGID(const char *wisp_application_name)
{
	WL_setprogid(wisp_application_name);
}

void WL_setprogid(const char *wisp_application_name)
{
	memcpy(WISPPROGID,wisp_application_name,8);
	WISPPROGID[8] = '\0';
}

const char* WL_getprogid(void)
{
	return WISPPROGID;
}



/*
**	History:
**	$Log: setprgid.c,v $
**	Revision 1.14  2003/01/31 18:54:38  gsl
**	Fix copyright header
**	
**	Revision 1.13  2002/08/01 02:42:48  gsl
**	globals
**	
**	Revision 1.12  2002/07/12 19:10:16  gsl
**	Global unique WL_ changes
**	
**	Revision 1.11  2002/07/10 21:05:24  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.10  1997/10/21 14:16:32  gsl
**	Removed global WISPPROGID and addded getprogid()
**	
**	Revision 1.9  1996-08-19 18:32:54-04  gsl
**	drcs update
**
**
**
*/
