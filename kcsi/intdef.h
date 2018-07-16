/* 
	Copyright (c) 1995 DevTech Migrations, All rights reserved.
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
**	History:
**	07/08/93	Written by JEC
**
*/


#ifndef INTDEF_H
#define INTDEF_H

/* ALPHA has the following sizes: short:2, int:4, long:8 */
#if defined(OSF1_ALPHA)

typedef short int2; 
typedef int   int4;

typedef  unsigned short uint2;  
typedef  unsigned int   uint4;

#define ATOI4(x) atoi((x))

#endif /*ALPHA*/


/* everyone else seems to have:  short:2, int:4, long:4 */
#if !defined(OSF1_ALPHA)

typedef short int2; 
typedef int   int4;

typedef  unsigned short    uint2;  
typedef  unsigned int      uint4;

#define ATOI4(x) atoi((x))

#endif /*CATCHALL*/

#endif /*INTDEF_H*/
/*
**	History:
**	$Log: intdef.h,v $
**	Revision 1.2.2.1  2002/11/12 15:56:26  gsl
**	Sync with $HEAD Combined KCSI 4.0.00
**	
**	Revision 1.3  2002/07/18 21:04:26  gsl
**	Remove MSDOS code
**	
**	Revision 1.2  1996/09/17 23:34:09  gsl
**	drcs update
**	
**
**
*/
