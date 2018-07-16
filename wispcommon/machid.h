/* 
	Copyright (c) 1997 NeoMedia Migrations, All rights reserved.
	$Id:$
*/

/*
**	File:		machid.h
**
**	Project:	WISP COMMON
**
**	RCS:		$Source:$
**
**	Purpose:	Header for machid.c
**
*/

#ifndef machid_H
#define machid_H
/*
**	Includes
*/

/*
**	Structures and Defines
*/
#ifndef MAX_COMPUTERNAME_LENGTH
#define MAX_COMPUTERNAME_LENGTH         32
#endif

#define MAX_MACHINEID_LENGTH		256

/*
**	Function Prototypes
*/
int getmachineid(char* machineid);
char *computername(char *cname);
void encodemachid(const char *source, char *target);

#endif /* machid_H */

/*
**	History:
**	$Log: machid.h,v $
**	Revision 1.2  2001-09-27 09:45:28-04  gsl
**	Add define for machineid length
**
**	Revision 1.1  1997-03-06 16:22:04-05  gsl
**	Initial revision
**
**
**
**
*/
