/*
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
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

/*
**	File:		acustubs.c
**
**	Purpose:	To hold stub routines that are normally found in the ACUCOBOL rts.
**			This is to allow linking to resolve undefines that are only used
**			when then ACUCOBOL rts is included. 
**			I.e. It is used to build utilities that don't include runcbl.a or sub85.c etc.
**
**	Routines:	
**	WL_call_acucobol()	Stub used in LINK normally found in sub85.c
**
**
**	History:
**	12/28/92	Written by GSL
**
*/

#include "idsistd.h"
#include "werrlog.h"

void WL_call_acucobol(char* name, int parmcnt, char* parms[], int lens[], int* rc)
{
	WL_werrlog_error(WERRCODE(104),"ACUSTUBS","CALL", 
		"Calling stub routine WL_call_acucobol()");
	return;
}

/*
**	Routine:	wisp_acu_cobol()
**
**	Function:	To identify if we are in an Acucobol RTS.
**
**	Description:	Two versions exist.
**			The one in sub85.c returns TRUE.
**			The one in acustubs.c returns FALSE.
**
**	Arguments:	None
**
**	Return:		
**	1		TRUE	- in an Acucobol RTS
**	0		FALSE	- not in an Acucobol RTS
**
*/
int wisp_acu_cobol()
{
	return 0; /* FALSE */
}

/*
**	History:
**	$Log: acustubs.c,v $
**	Revision 1.10  2003/01/31 17:23:49  gsl
**	Fix  copyright header
**	
**	Revision 1.9  2002/12/09 19:15:30  gsl
**	Change to use WL_werrlog_error()
**	
**	Revision 1.8  2002/10/18 17:32:55  gsl
**	Identify if in a  COBOL runtime
**	
**	Revision 1.7  2002/07/10 21:05:14  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.6  1996/08/19 22:32:08  gsl
**	drcs update
**	
**
**
*/
