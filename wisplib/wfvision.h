/* 
	Copyright (c) 1998 NeoMedia Technologies, All rights reserved.
	$Id:$
*/

/*
**	File:		wfvision.h
**
**	Project:	WISP/LIB
**
**	RCS:		$Source:$
**
**	Purpose:	Vision file routines
**
*/

#ifndef wfvision_H
#define wfvision_H
/*
**	Includes
*/

/*
**	Structures and Defines
*/

/*
**	Function Prototypes
*/
int acuvision( const char* path, const char* code, void* raw_field );
int visionversion(const char *filename);

#endif /* wfvision_H */

/*
**	History:
**	$Log: wfvision.h,v $
**	Revision 1.1  1998-05-14 14:29:31-04  gsl
**	Initial revision
**
**
**
**
*/
