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
**	File:		vmove.h
**
*/

#ifndef vmove_H
#define vmove_H
/*
**	Includes
*/

/*
**	Structures and Defines
*/

/*
**	Function Prototypes
*/
int VL_vmove(int line, int column);							/* Move to a location on the screen.	*/
int VL_vgoto(int nl, int nc, int ol, int oc, int op);					/* Got to (nl,nc) from (ol,oc).		*/

#endif /* vmove_H */

/*
**	History:
**	$Log: vmove.h,v $
**	Revision 1.4  2003/01/31 19:25:56  gsl
**	Fix copyright header
**	
**	Revision 1.3  2002/07/15 20:16:11  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.2  1997/07/09 15:06:29  gsl
**	Remove static routines
**	
**	Revision 1.1  1996-03-28 17:23:41-05  gsl
**	Initial revision
**
**
*/
