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


/*					Include standard header files								*/

#include	"video.h"
#include	"vlocal.h"
#include	"vmodules.h"
#include	"vdata.h"

/*					Subroutine entry point.									*/

int VL_vonexit(int new_op)									/* Change the exit defaults.		*/
{
	if ((new_op & WIDE) && (new_op & NARROW))						/* Validate width requests.		*/
	{
		vre("vonexit(%o)-F-Cannot have both WIDE and NARROW screen on exit.",new_op);
		return(FAILURE);
	}
	if ((new_op & LIGHT) && (new_op & DARK))						/* Validate color.			*/
	{
		vre("vonexit(%o)-F-Cannot have both LIGHT and DARK screen on exit.",new_op);
		return(FAILURE);
	}

	VL_exit_options = new_op;								/* Store the new exit options.		*/
	
	return(SUCCESS);								/* Now let the caller know how it went.	*/
}
/*
**	History:
**	$Log: vonexit.c,v $
**	Revision 1.13  2003/02/04 17:05:01  gsl
**	Fix -Wall warnings
**	
**	Revision 1.12  2003/01/31 19:25:56  gsl
**	Fix copyright header
**	
**	Revision 1.11  2002/07/15 20:16:11  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.10  2002/07/15 17:10:05  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.9  1996/10/11 22:16:13  gsl
**	drcs update
**	
**
**
*/
