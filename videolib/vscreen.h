/*
******************************************************************************
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
**
** NOTICE:
** Confidential, unpublished property of Shell Stream Software LLC.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of Shell Stream Software LLC
** is strictly prohibited.
** 
******************************************************************************
*/


/*
**	File:		vscreen.h
**
*/

#ifndef vscreen_H
#define vscreen_H
/*
**	Includes
*/

/*
**	Structures and Defines
*/

#define VSCREEN_DEFAULT		0		/* Default narrow+dark	*/
#define VSCREEN_NARROW		1		/* Set screen narrow.	*/
#define VSCREEN_WIDE		2		/* Set screen wide.	*/
#define VSCREEN_LIGHT		4		/* Set screen light.	*/
#define VSCREEN_DARK		8		/* Set screen dark.	*/
#define VSCREEN_NOOP		16		/* Clear first flags	*/

/*
**	Function Prototypes
*/
#define vscreen		VL_vscreen

int VL_vscreen(int state);		/* Set screen to a given state.		*/
int VL_vscreen_check(int state);

#endif /* vscreen_H */

/*
**	History:
**	$Log: vscreen.h,v $
**	Revision 1.3  2003/01/31 19:25:55  gsl
**	Fix copyright header
**	
**	Revision 1.2  2002/07/15 20:16:14  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.1  1996/03/28 22:04:38  gsl
**	Initial revision
**	
**
*/
