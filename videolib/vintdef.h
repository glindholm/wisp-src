/*
******************************************************************************
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** $Id:$
**
** NOTICE:
** Confidential, unpublished property of NeoMedia Technologies, Inc.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of NeoMedia 
** Technologies, Inc. is strictly prohibited.
** 
** CVS
** $Source:$
** $Author: gsl $
** $Date:$
** $Revision:$
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

#endif /*ALPHA*/


/* everyone else seems to have:  short:2, int:4, long:4 */
#if !defined(OSF1_ALPHA)

typedef short int2; 
typedef int   int4;

typedef  unsigned short    uint2;  
typedef  unsigned int      uint4;

#endif /*CATCHALL*/

#endif /*INTDEF_H*/
/*
**	History:
**	$Log: vintdef.h,v $
**	Revision 1.8  2003/01/31 19:25:56  gsl
**	Fix copyright header
**	
**	Revision 1.7  2002/07/18 21:04:22  gsl
**	Remove MSDOS code
**	
**	Revision 1.6  1996/10/11 22:16:06  gsl
**	drcs update
**	
**
**
*/
