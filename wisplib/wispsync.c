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
**	WISPSYNC	This is a special wisp COBOL interface to vwang_synch() that is to be inserted into wang cobol
**			programs to let wisp and video known that the screen has been changed behind its back, so
**			that it can re-synchronize it's video maps.
*/

#include "idsistd.h"
#include "vwang.h"

void WISPSYNC(void)
{
	vwang_synch();
}



/*
**	WISPSHUT	This is a special wisp COBOL interface to vwang_shut() that is to be inserted into wang cobol
**			programs to let wisp and video known that the screen is going to be altered by a non-WISP
**			routine, so it should turn off video and put the screen back into a known state.
*/

void WISPSHUT(void)
{
	vwang_shut();
}

/*
**	History:
**	$Log: wispsync.c,v $
**	Revision 1.10  2003/01/31 19:08:37  gsl
**	Fix copyright header  and -Wall warnings
**	
**	Revision 1.9  1996/08/19 22:33:18  gsl
**	drcs update
**	
**
**
*/
