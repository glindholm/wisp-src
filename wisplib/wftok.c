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

/*
	wftok()		Wisp File TO Key: Create the key from the file inode.
*/
key_t wftok(file)									/* WISP ftok				*/
char *file;
{
	key_t myftok(), ftok();
#ifdef _AIX
	return( myftok(file,0xd5) );
#else
	return( ftok(file,0xd5) );
#endif
}

#ifdef _AIX
/*
	myftok()	Local ftok()
*/
static key_t myftok(file,x)								/* For IBM - generate unique numbers.	*/
char *file;										/* Replaces FTOK (system one doesn't	*/
char x;											/*  work properly for IBM.)		*/
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
#endif /* _AIX */
#endif /* unix */
