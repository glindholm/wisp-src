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
**	File:		mfstubs.c
**
**	Purpose:	To hold stub routines that are normally found in the MF rts.
**			This is to allow linking to resolve undefines that are only used
**			when then MF rts is included. 
**			I.e. It is used to build utilities that don't include the cobol rts.
**
**	Routines:	
**	WL_call_mfcobol()		Stub used in LINK normally found in wispmf.c
**
*/

#include "idsistd.h"
#include "werrlog.h"

void WL_call_mfcobol()
{
	WL_werrlog_error(WERRCODE(104),"MFSTUBS","CALL", 
		"Calling stub routine WL_call_mfcobol()");
	return;
}


/*
**	Routine:	wisp_mf_cobol()
**
**	Function:	To identify if we are in an Mico Focus RTS.
**
**	Description:	Two versions exist.
**			The one in wispmf.c returns TRUE.
**			The one in mfstubs.c returns FALSE.
**
**	Arguments:	None
**
**	Return:		
**	1		TRUE	- in an Micro Focus RTS
**	0		FALSE	- not in an Micro Focus RTS
**
*/
int wisp_mf_cobol()
{
	return 0; /* FALSE */
}

/*
**	History:
**	$Log: mfstubs.c,v $
**	Revision 1.6  2003/01/31 17:33:55  gsl
**	Fix  copyright header
**	
**	Revision 1.5  2002/12/09 19:15:32  gsl
**	Change to use WL_werrlog_error()
**	
**	Revision 1.4  2002/10/18 17:32:55  gsl
**	Identify if in a  COBOL runtime
**	
**	Revision 1.3  2002/07/11 20:29:11  gsl
**	Fix WL_ globals
**	
**	Revision 1.2  1998/12/14 16:42:15  gsl
**	Fix history
**	
**
**
*/
