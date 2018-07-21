/*
******************************************************************************
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
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
******************************************************************************
*/

			/************************************************************************/
			/*									*/
			/*	     VIDEO - Video Interactive Development Environment		*/
			/*									*/
			/*			    Copyright (c) 1987				*/
			/*									*/
			/*	An unpublished work by Greg L. Adams.  All rights reserved.	*/
			/*									*/
			/************************************************************************/


/*						Include standard header files.							*/

#include "video.h"									/* Include video definitions.		*/
#include "vdata.h"

/*						Subroutine entry point.								*/

int VL_vedge(int line)									/* Determine edge of screen.		*/
{
	return(VL_vscr_wid);								/* All done.				*/
}
/*
**	History:
**	$Log: vedge.c,v $
**	Revision 1.13  2003/01/31 19:25:56  gsl
**	Fix copyright header
**	
**	Revision 1.12  2002/07/16 13:40:22  gsl
**	VL_ globals
**	
**	Revision 1.11  2002/07/15 20:16:08  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.10  1997/07/08 20:55:57  gsl
**	Removed the double width logic so vedge()
**	now returns the screen width
**	
**	Revision 1.9  1996-10-11 18:16:03-04  gsl
**	drcs update
**
**
**
*/
