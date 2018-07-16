/* 
	Copyright (c) 1995-2002 NeoMedia Technologies, All rights reserved.
	$Id:$
*/

			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*	      Copyright (c) 1988, 1989, 1990, 1991, 1992, 1993		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
**	File:		intdef.h
**
**	Purpose:	To define int sizes for all platforms.
**                      Basically we want to provide defs for two byte and four byte ints.
**
**
*/


#ifndef INTDEF_H
#define INTDEF_H

#define ATOI4(x) atoi((x))

typedef short		int2; 
typedef unsigned short	uint2;  

typedef int		int4;
typedef unsigned int	uint4;

#ifdef OSF1_ALPHA
#define INT8_DEFINED
typedef	long		INT8;
typedef	unsigned long	UINT8;
#endif

#if defined(AIX) || defined(HPUX) || defined(SOLARIS) || defined(LINUX)
#define INT8_DEFINED
typedef	long long		INT8;
typedef	unsigned long long	UINT8;
#endif

#ifdef WIN32
#define INT8_DEFINED
typedef __int64			INT8;
typedef unsigned __int64	UINT8;
#endif

/*
**	NOTE: SCO 5.02 does not have a 8 byte (64-bit) integer type.
*/

#endif /*INTDEF_H*/

/*
**	History:
**	$Log: intdef.h,v $
**	Revision 1.5.2.1  2002/10/09 19:18:24  gsl
**	Add INT8
**	
**	Revision 1.8  2002/10/08 15:44:38  gsl
**	Change int8 to INT8 to avoid conficts
**	
**	Revision 1.7  2002/10/04 20:54:45  gsl
**	Add int8 for 64-bit integers
**	
**	Revision 1.6  2002/07/18 21:04:23  gsl
**	Remove MSDOS code
**	
**	Revision 1.5  1996/07/23 18:17:48  gsl
**	drcs update
**	
**
**
*/
