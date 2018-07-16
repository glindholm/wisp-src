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

/*
**	File:		setenvst.c
**
**	Project:	WISP
**
**	RCS:		$Source:$
**
**	Purpose:	Simplify setting an environment string
**
**	Routines:	
**	setenvstr()
*/

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

#include "setenvst.h"
#include "wmalloc.h"
#include "werrlog.h"

/*
**	ROUTINE:	WL_setenvstr()
**
**	FUNCTION:	Set a environment string
**
**	DESCRIPTION:	This is a frontend for putenv() which mallocs a duplicate of the string.
**
**	ARGUMENTS:	
**	envstring	The envirnment string of the form "variable=value" eg. "WISPTERM=vt100"
**
**	GLOBALS:	none
**
**	RETURN:		
**	0		Success
**	2		putenv failed
**
**	WARNINGS:	None
**
*/
int WL_setenvstr(const char* envstring)
{
	wtrace("PUTENV","ENTRY", "%s", envstring);

	if ( 0 != putenv( wisp_strdup(envstring) ) )
	{
		WL_werrlog_error(WERRCODE(104),"PUTENV", "FAILED", 
			"putenv(\"%s\") failed errno=[%d]", envstring, errno);
		return(2);
	}

	return(0);
}

/*
**	History:
**	$Log: setenvst.c,v $
**	Revision 1.17  2003/01/31 18:54:38  gsl
**	Fix copyright header
**	
**	Revision 1.16  2002/12/09 21:09:31  gsl
**	Use WL_wtrace(ENTRY)
**	
**	Revision 1.15  2002/12/09 19:15:33  gsl
**	Change to use WL_werrlog_error()
**	
**	Revision 1.14  2002/07/09 04:13:58  gsl
**	Rename global WISPLIB routines WL_ for uniqueness
**	
**	Revision 1.13  2002/07/02 21:15:28  gsl
**	Rename wstrdup
**	
**	Revision 1.12  1998/12/09 15:46:24  gsl
**	up buff to 256
**	
**	Revision 1.11  1998-12-09 10:45:44-05  gsl
**	Add tracing and error checking
**
**	Revision 1.10  1996-10-14 12:25:34-04  gsl
**	rewrote to use wstrdup()
**
**	Revision 1.9  1996-08-19 15:32:53-07  gsl
**	drcs update
**
**
**
*/
