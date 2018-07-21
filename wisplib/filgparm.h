/*
******************************************************************************
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
******************************************************************************
*/

/*
**	File:		filgparm.h
**
**	Project:	WISP/LIB
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
/*
void file_getparm2(int4 f_mode, char *file, char *lib, char *vol, char *prname, 
		char *issuer, int4 *entry_mode, char *getparm_type, char *native_path,
		char *msg1, char *msg2, char *pfkey_rcvr, char intv_type,	
		char *orig_file, char *prtclass, int4 *form, int4* copies);
*/
void WL_file_getparm3(
	char *file, char *lib, char *vol, 	/* Wang style file spec */
	const char *prname, 			/* Prname[8]		*/
	const char *issuer, 			/* Issuer[6]		*/
	int4 *entry_mode, 			/* 0 = Wang style, 1 = Native style filepath */
	char *getparm_type,
	char *native_path,
	char *msg1, char *msg2, 
	char *pfkey_rcvr, 
	char intv_type,					/* 'R' - Rename PF3,  'E' - Everything else */
	char *orig_file, 				/* Original Wang file name */
	char *prtclass, 
	int4 *form, 
	int4 *copies,
	int  is_output,
	int  is_printer,
	int  is_io,
	int  is_shared);

int WL_password_getparm(int initial, char* uservalue, int userlen, char* passvalue, int passlen, char* savevalue, char* messtext);
int WL_display_util_options_getparm(int *recsize);


#endif /* filgparm_H */

/*
**	History:
**	$Log: filgparm.h,v $
**	Revision 1.7  2003/02/20 23:14:34  gsl
**	Add OPTIONS get to DISPLAY utility that gets the record size RECSIZE
**	
**	Revision 1.6  2003/01/31 19:18:00  gsl
**	Fix copyright header  and -Wall warnings
**	
**	Revision 1.5  2002/07/11 20:29:08  gsl
**	Fix WL_ globals
**	
**	Revision 1.4  2002/07/01 04:02:37  gsl
**	Replaced globals with accessors & mutators
**	
**	Revision 1.3  2002/06/26 04:25:04  gsl
**	Cleanup mode/status bit fields
**	
**	Revision 1.2  1997/08/22 21:39:24  gsl
**	Finished PASSWORD getparm
**	
**	Revision 1.1  1997-08-19 17:11:24-04  gsl
**	Initial revision
**
**
**
**
*/
