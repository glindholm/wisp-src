/*
******************************************************************************
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
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



#ifdef WIN32

#include <windows.h>

/*
**	ROUTINE:	sleep()  (for WIN32)
**
**	FUNCTION:	Emulate unix sleep() routine
**
**	DESCRIPTION:	Call Sleep(millisecs)
**
**	ARGUMENTS:	
**	secs		Number of seconds to sleep for
**
**	GLOBALS:	None
**
**	RETURN:		unslept amount of time (always 0)
**
**	WARNINGS:	None
**
*/
unsigned sleep(unsigned secs)
{
	Sleep(1000 * secs);
	return 0;
}
#endif /* WIN32 */


/*
**	History:
**	$Log: sleepdos.c,v $
**	Revision 1.10  2003/01/31 19:25:57  gsl
**	Fix copyright header
**	
**	Revision 1.9  2002/07/18 21:04:20  gsl
**	Remove MSDOS code
**	
**	Revision 1.8  1997/07/08 20:20:17  gsl
**	Add sleep() routine for WIN32
**	
**	Revision 1.7  1996-10-11 18:15:56-04  gsl
**	drcs update
**
**
**
*/
