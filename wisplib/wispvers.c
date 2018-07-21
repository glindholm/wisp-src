/*
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
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
**	File:		wispvers.c
**
**	Project:	wisp/lib
**
**	Purpose:	WISP version numbers
**
**	Routines:	
**	wisp_version()	Return the WISP version string.
*/

/*
**	Includes
*/
#include "wcommon.h"
#include "wispvers.h"

char *wisp_version(void)
{
	static char the_version[] = WISP_VERSION;
	return the_version;
}

/*
**	History:
**	$Log: wispvers.c,v $
**	Revision 1.2  2003/01/31 19:08:37  gsl
**	Fix copyright header  and -Wall warnings
**	
**	Revision 1.1  1995/06/14 15:05:53  gsl
**	Initial revision
**	
**
**
*/
