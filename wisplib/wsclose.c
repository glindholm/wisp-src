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

/*  Terminates screen 'file'	*/

#include "idsistd.h"
#include "vwang.h"

#ifndef NULL
#define NULL 0
#endif

void WSCLOSE(void)
{
	WS_CLOSE();
}
/*
**	History:
**	$Log: wsclose.c,v $
**	Revision 1.11  2003/01/31 19:08:37  gsl
**	Fix copyright header  and -Wall warnings
**	
**	Revision 1.10  2002/08/01 14:09:09  gsl
**	type warnings
**	
**	Revision 1.9  1996/08/19 22:33:21  gsl
**	drcs update
**	
**
**
*/
