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
**	File:		verase.h
**
**	Project:	???
**
**	RCS:		$Source:$
**
**	Purpose:	???
**
*/

#ifndef verase_H
#define verase_H
/*
**	Includes
*/
#include "vmenu.h"

/*
**	Structures and Defines
*/

/*
**	Function Prototypes
*/


#define verase		VL_verase
#define verase_menu	VL_verase_menu

int VL_verase(int control);								/* Erase all or part of the screen.	*/
int VL_verase_menu(int control, struct video_menu *md);					/* Erase all, top or bottom menus.	*/

#endif /* verase_H */

/*
**	History:
**	$Log: verase.h,v $
**	Revision 1.3  2003/01/31 19:25:56  gsl
**	Fix copyright header
**	
**	Revision 1.2  2002/07/15 20:16:08  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.1  1996/03/28 21:32:35  gsl
**	Initial revision
**	
**
*/
