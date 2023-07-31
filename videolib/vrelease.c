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



/*						Include standard header files.							*/

#include "video.h"									/* Include video definitions.		*/

/*						Subroutine entry point.								*/

int VL_vrelease(void)									/* Release from optimization.		*/
{
	vdefer_restore();								/* Restore from current optimization.	*/
	return(SUCCESS);								/* Return to the caller.		*/
}
/*
**	History:
**	$Log: vrelease.c,v $
**	Revision 1.12  2003/01/31 19:25:56  gsl
**	Fix copyright header
**	
**	Revision 1.11  2002/07/15 20:16:14  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.10  1997/07/09 16:09:23  gsl
**	Removed the BLOCK_MODE support so now is the same as vdefer_restore()
**	
**	Revision 1.9  1996-10-11 18:16:19-04  gsl
**	drcs update
**
**
**
*/
