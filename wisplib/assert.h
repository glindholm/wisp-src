/* 
	Copyright (c) 1995-1996 DevTech Migrations, All rights reserved.
	$Id:$
*/

/*
**	File:		wassert.h
**
**	Project:	WISP
**
**	RCS:		$Source:$
**
**	Purpose:	Define ASSERT macros
**
*/

#ifndef wassert_H
#define wassert_H
/*
**	Includes
*/

/*
**	Structures and Defines
*/
#ifdef _DEBUG
#ifndef DEBUG
#define DEBUG
#endif
#endif

#ifdef DEBUG
#define ASSERT(COND)  { if (!(COND)) wisp_assert(# COND, __FILE__, __LINE__); }
#else
#define ASSERT(cond)
#endif

/*
**	Function Prototypes
*/
#ifdef DEBUG
void wisp_assert(char *cond, char *file, int line);
#endif

#endif /* wassert_H */

/*
**	History:
**	$Log: assert.h,v $
**	Revision 1.3  1997/08/27 19:04:01  gsl
**	If _DEBUG is defined then define DEBUG
**	Windows uses _DEBUG instead of DEBUG
**	
**	Revision 1.2  1996-07-10 12:03:50-04  gsl
**	Renamed from wassert.h to assert.h
**
**	Revision 1.1  1996-01-02 08:23:50-08  gsl
**	Initial revision
**
**
*/
