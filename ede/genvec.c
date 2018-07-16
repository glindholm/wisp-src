static char copyright[]="Copyright (c) 1988-1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

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
	werr_message_box("GENVEC: unable to vector program. Aborting!");
	wexit(0);
}

/*
**	History:
**	$Log: genvec.c,v $
**	Revision 1.8  1996-09-13 13:54:33-04  gsl
**	Fix warnings for NT
**
**
**
*/
