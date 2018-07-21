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
	WL_shutexitcobol:	This is the "default" version of shutexitcobol() just in case no defines one for the specific cobol.
			It is called from wexit() to shutdown cobol and exit().  This one will occur in WISPLIB so the real
			one needs to be linked in ahead of the lib.

			*** Don't add cobol specific code to this file. ***
*/

#include "idsistd.h"

void WL_shutexitcobol(int exit_code)			
{
	exit(exit_code);
}

/*
**	History:
**	$Log: shutexit.c,v $
**	Revision 1.11  2003/01/31 18:54:37  gsl
**	Fix copyright header
**	
**	Revision 1.10  2002/07/12 19:10:17  gsl
**	Global unique WL_ changes
**	
**	Revision 1.9  1996/08/19 22:32:56  gsl
**	drcs update
**	
**
**
*/
