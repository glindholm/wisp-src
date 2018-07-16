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
**	File:		vline.h
**
**	Project:	VIDEO/LIB
**
**	RCS:		$Source:$
**
**	Purpose:	???
**
*/

#ifndef vline_H
#define vline_H
/*
**	Includes
*/

/*
**	Structures and Defines
*/

/*
**	Function Prototypes
*/

#define VERTICAL	0			/* Draw vertical line	*/
#define	HORIZONTAL	3			/* Horizontal line.	*/
#define FAT_VERTICAL    6			/* Reverse space vert.	*/
#define FAT_HORIZONTAL  7			/* Reverse space horiz.	*/

#define VLINE_VERTICAL		0		/* Draw vertical line	*/
#define VLINE_HORIZONTAL	3		/* Horizontal line.	*/
#define VLINE_FAT_VERTICAL	6		/* Reverse space vert.	*/
#define VLINE_FAT_HORIZONTAL	7		/* Reverse space horiz.	*/

#ifdef vline
#undef vline
#endif
#define vline VL_vline
extern int VL_vline (int type, int length);

#endif /* vline_H */

/*
**	History:
**	$Log: vline.h,v $
**	Revision 1.3  2003/01/31 19:25:56  gsl
**	Fix copyright header
**	
**	Revision 1.2  2002/07/15 17:52:55  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.1  1996/03/28 21:20:26  gsl
**	Initial revision
**	
**
*/
