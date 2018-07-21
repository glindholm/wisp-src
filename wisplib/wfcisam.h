/* 
	Copyright (c) Shell Stream Software LLC, All Rights Reserved.
	$Id:$
*/

/*
**	File:		wfcisam.h
**
**	Project:	WISP/LIB
**
**
**
**	Purpose:	CISAM file routines
**
*/

#ifndef wfcisam_H
#define wfcisam_H
/*
**	Includes
*/

/*
**	Structures and Defines
*/

/*
**	Function Prototypes
*/
int4 WL_cisaminfo( const char* path, const char* code, void* field );

int WL_unloadcisam(const char *inname, const char *outname, int4 recsize);
int WL_unloadfhisam(const char *inname, const char *outname, int4 recsize);


#endif /* wfcisam_H */

/*
**	History:
**	$Log: wfcisam.h,v $
**	Revision 1.3  2002/07/12 19:10:19  gsl
**	Global unique WL_ changes
**	
**	Revision 1.2  2001/10/26 19:44:09  gsl
**	Move the unloadcisam() and unloadfhisam() from wispsort.c
**	
**	Revision 1.1  2001-10-22 11:12:00-04  gsl
**	Initial revision
**
**
**
*/
