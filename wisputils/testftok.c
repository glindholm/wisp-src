/*
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
**
**
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
**
**
**
**
*/


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/ipc.h>
#include <sys/shm.h>


/*
	myftok()	Local ftok()
*/
/* For IBM - generate unique numbers.	*/
/* Replaces FTOK (system one doesn't	*/
/*  work properly for IBM.)		*/

static key_t myftok(const char *file,char x)
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

static long st_ino(const char *file)
{
	struct stat buf;
	int	rc;

	rc = stat(file,&buf);
	if (rc == -1)
	{
		return(  -1 );
	}
	else
	{
		return buf.st_ino;
	}
}

static long st_dev(const char *file)
{
	struct stat buf;
	int	rc;

	rc = stat(file,&buf);
	if (rc == -1)
	{
		return(  -1 );
	}
	else
	{
		return buf.st_dev;
	}
}

#define FTOK_ID	0xd5

int main(int argc, char*argv[])
{

	printf("st_ino(\"[%s]\")            = %lx\n", argv[0], (long) st_ino(argv[0]));
	printf("st_dev(\"[%s]\")            = %lx\n", argv[0], (long) st_dev(argv[0]));
	printf("ftok  (\"[%s]\",(int)0x%0x) = %lx\n", argv[0], (int)FTOK_ID, (long) ftok(argv[0],FTOK_ID));
	printf("myftok(\"[%s]\",(int)0x%0x) = %lx\n", argv[0], (int)FTOK_ID, (long) myftok(argv[0],FTOK_ID));
	
	return 0;
}


/*
**	History:
**	$Log: testftok.c,v $
**	Revision 1.3  2003/02/07 15:08:33  gsl
**	fix warning
**	
**	Revision 1.2  2003/02/06 18:45:45  gsl
**	Add st_dev
**	
**	Revision 1.1  2003/02/05 19:05:55  gsl
**	test
**	
**
**
**
*/
