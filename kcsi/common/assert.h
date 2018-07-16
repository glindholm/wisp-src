/* 
	Copyright (c) 1997 NeoMedia Technologies, Inc., All rights reserved.
	$Id:$
*/

/*
**	File:		assert.h
**
**	Project:	WISP/KCSI/COMMON
**
**	RCS:		$Source:$
**
**	Purpose:	Define ASSERT macros
**
*/

#ifndef assert_H
#define assert_H
/*
**	Includes
*/

/*
**	Structures and Defines
*/
#ifdef DEBUG
#define ASSERT(COND)  { if (!(COND)) kcsi_assert(# COND, __FILE__, __LINE__); }
#else
#define ASSERT(cond)
#endif

/*
**	Function Prototypes
*/
#ifdef DEBUG
void kcsi_assert(char *cond, char *file, int line);
#endif

#endif /* assert_H */

/*
**	History:
**	$Log: assert.h,v $
**	Revision 1.1  1997-08-01 11:55:07-04  scass
**	Initial revision
**
**
*/
