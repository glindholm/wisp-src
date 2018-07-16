/*
******************************************************************************
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
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
**	Revision 1.9  2003/01/31 19:26:33  gsl
**	Fix copyright header
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
