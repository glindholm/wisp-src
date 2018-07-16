/* 
	Copyright (c) 1997 NeoMedia Migrations, All rights reserved.
	$Id:$
*/

/*
**	File:		filgparm.h
**
**	Project:	WISP/LIB
**
**	RCS:		$Source:$
**
**	Purpose:	Header for filgparm.c
**
*/

#ifndef filgparm_H
#define filgparm_H
/*
**	Includes
*/
#include "intdef.h"

/*
**	Structures and Defines
*/

/*
**	Function Prototypes
*/
void file_getparm2(int4 f_mode, char *file, char *lib, char *vol, char *prname, 
		char *issuer, int4 *entry_mode, char *getparm_type, char *native_path,
		char *msg1, char *msg2, char *pfkey_rcvr, char intv_type,	
		char *orig_file, char *prtclass, int4 *form, int4* copies);

int password_getparm(int initial, char* uservalue, int userlen, char* passvalue, int passlen, char* savevalue, char* messtext);


#endif /* filgparm_H */

/*
**	History:
**	$Log: filgparm.h,v $
**	Revision 1.2  1997-08-22 17:39:24-04  gsl
**	Finished PASSWORD getparm
**
**	Revision 1.1  1997-08-19 17:11:24-04  gsl
**	Initial revision
**
**
**
**
*/
