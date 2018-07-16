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

#ifdef unix

/*
**	File:		wftok.c
**
**	Purpose:	File to KEY functions
**
**	Routines:	
**	wftok()		Wisp File TO Key: Create the key from the file inode.
**	myftok()	Local ftok()
**
*/

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include "idsistd.h"

#ifdef USE_MYFTOK
/*
	myftok()	Local ftok()

	This was originally written for AIX 3 because according to Jock it was broken.

	*** NOTE ***
	Current unix systems use a combination of the prefix, the device and inode
	to create the key of 8 hex digits.

	Solaris 8:	PP+DDD+III
	SCO 5.0.2:	PP+DD+IIII
	HP-UX 11:	PP+DD+IIII  - Device id is munged
	AIX 4.3.3:	PP+00+IIII  - no device
	LINUX:		PP+DD+IIII
*/
static key_t myftok(char *file, char x)
{
	struct stat buf;
	int	rc;

	rc = stat(file,&buf);
	if (rc == -1)
	{
		return( (key_t) -1 );
	}
	return (key_t)(((key_t)x<<24)|(buf.st_ino&0x00ffffff));
}
#endif /* USE_MYFTOK */

#define WISP_FTOK_PREFIX 0xd5
/*
	wftok()		Wisp File TO Key: Create the key from the file inode.
*/
key_t wftok(char* file)
{
#ifdef USE_MYFTOK
	return( myftok(file,(char)WISP_FTOK_PREFIX) );
#else
	return( ftok(file,(int)WISP_FTOK_PREFIX) );
#endif
}

#endif /* unix */
/*
**	History:
**	$Log: wftok.c,v $
**	Revision 1.8.2.1  2003/02/06 19:08:23  gsl
**	Remove AIX and SCO stuff, document
**	
**	Revision 1.8  1999/01/19 16:13:37  gsl
**	fix last fix
**	
**	Revision 1.7  1999-01-19 10:51:03-05  gsl
**	fix warnings for AIX
**
**	Revision 1.6  1996-08-19 18:33:16-04  gsl
**	drcs update
**
**
**
*/
