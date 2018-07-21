/*
******************************************************************************
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
******************************************************************************
*/


/*
	GENVEC		General Program Vectoring Routine.
			This routine is passed the name of a program to run plus two parameters.

			NOTE: If called from COBOL the parameters are not NULL terminated.
*/
#include "werrlog.h"
#include "wexit.h"

void GENVEC( 
	char	*program,		/* The PROGRAM to vector to			*/
	char	*inparms,		/* Input parameters				*/
	char	*outparms)		/* Output parameters				*/
{
	WL_werr_message_box("GENVEC: unable to vector program. Aborting!");
	WL_wexit(0);
}

/*
**	History:
**	$Log: genvec.c,v $
**	Revision 1.11  2003/02/04 18:57:00  gsl
**	fix copyright header
**	
**	Revision 1.10  2002/07/10 21:06:34  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.9  2002/07/09 04:14:07  gsl
**	Rename global WISPLIB routines WL_ for uniqueness
**	
**	Revision 1.8  1996/09/13 17:54:33  gsl
**	Fix warnings for NT
**	
**
**
*/
