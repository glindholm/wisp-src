/*
******************************************************************************
**
** KCSI - King Computer Services Inc.
**
** 
******************************************************************************
*/


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
**	Revision 1.4  2003/02/05 15:50:11  gsl
**	Fix copyright headers
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
