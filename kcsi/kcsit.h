/* 
	Copyright (c) 1997 NeoMedia Technologies, Inc., All rights reserved.
	$Id:$
*/

/*
**	File:		kcsit.h
**
**	Project:	WISP/KCSI/COMMON
**
**	Purpose:	Header file for kcsit.c
**
**	History:
**	08/01/97	Written by SMC
**
*/

#ifndef KCSIT_H
#define KCSIT_H

/*
**	Includes
*/

/*
**	Structures and Defines
*/

/*
**	Function Prototypes
*/
void kcsitrace(int sever, const char *routine, const char *mess, const char *lform, ...);
int kcsi_tracelevel(void);
void kcsi_exit(int num);

#endif /* KCSIT_H */

/*
**	History:
**	$Log: kcsit.h,v $
**	Revision 1.4  2003/03/20 15:21:18  gsl
**	Fix -Wall warnings
**	
**	Revision 1.3  2002/10/17 21:22:42  gsl
**	cleanup
**	
**	Revision 1.2  2002/04/22 18:27:09  gsl
**	Add kcsi_tracelevel()
**	
**	Revision 1.1  1997-08-01 12:00:24-04  scass
**	Initial revision
**
**
*/
