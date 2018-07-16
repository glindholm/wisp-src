/* 
	Copyright (c) 1995 DevTech Migrations, All rights reserved.
	$Id:$
*/
/*
	wfaccess.h 		Define status codes for wfaccess()
*/

#ifndef WFACCESS_DEF
#define WFACCESS_DEF

#include "intdef.h"

#define ACC_ALLOWED	0
#define ACC_DENIED	1
#define ACC_NOFILE	2
#define ACC_NODIR	3
#define ACC_LOCKED	4
#define ACC_NOLOCK	5
#define ACC_NOLINK	6
#define ACC_BADDIR	7
#define ACC_READONLY	8
#define ACC_INUSE	9
#define ACC_EXISTS	10
#define ACC_SYSLIMIT	11
#define ACC_MISSING	12
#define ACC_BADDEV	13
#define ACC_OUTEXISTS	14
#define ACC_ERROPEN	15
#define ACC_BADVOL	16
#define ACC_UNKNOWN	99

int wfaccess(char* native_path, int4* mode);

#endif

                
/*
**	History:
**	$Log: wfaccess.h,v $
**	Revision 1.10  1997/04/29 17:38:08  gsl
**	Moved acc_message() to wfopen.c
**	
**	Revision 1.9  1996-08-19 18:33:13-04  gsl
**	drcs update
**
**
**
*/
