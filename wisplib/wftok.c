static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*	      Copyright (c) 1988, 1989, 1990, 1991, 1992, 1993		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

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
**
**	History:
**	mm/dd/yy	Written by ...
**
*/

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include "idsistd.h"

#ifdef AIX
static key_t myftok(char *file, char x);
#endif

/*
	wftok()		Wisp File TO Key: Create the key from the file inode.
*/
key_t wftok(file)									/* WISP ftok				*/
char *file;
{
#ifdef AIX
	return( myftok(file,0xd5) );
#else
#ifdef SCO
	return( ftok(file,(char)0xd5) );
#else
	return( ftok(file,(int)0xd5) );
#endif
#endif
}

#ifdef AIX
/*
	myftok()	Local ftok()
*/
static key_t myftok(char *file,char x)							/* For IBM - generate unique numbers.	*/
											/* Replaces FTOK (system one doesn't	*/
											/*  work properly for IBM.)		*/
{
	struct stat buf;
	int	rc;

	rc = stat(file,&buf);
	if (rc == -1)
	{
		return( (key_t) -1 );
	}
	if (buf.st_ino&0xff000000) printf("warning %08x\n",buf.st_ino);
	return (key_t)(((key_t)x<<24)|(buf.st_ino&0x00ffffff));
}
#endif /* AIX */
#endif /* unix */
/*
**	History:
**	$Log: wftok.c,v $
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
