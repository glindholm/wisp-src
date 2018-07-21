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


/*						Fast memory to memory integer copy.						*/

int VL_imemcpy(dst,src,count)								/* Fast memory to memory byte copy.	*/
	register int *dst,*src;								/* Pointers to src and dest locatiions.	*/
	register int count;								/* Count of how many bytes to copy.	*/
{
	while (count--) *(dst++) = *(src++);						/* Copy the integer.			*/
	return(SUCCESS);								/* And return that all is well.		*/
}


/*						Fast integer memory fill routine.						*/

int VL_imemset(dst,src,count)								/* Dest gets count bytes of src.	*/
	register int *dst,src;								/* Pointer to dest location, and src.	*/
	register int count;								/* Count of how many bytes to fill.	*/
{
	while (count--) *(dst++) = src;							/* Fill it.				*/
	return(SUCCESS);								/* Return that all is fine.		*/
}
/*
**	History:
**	$Log: vsystem.c,v $
**	Revision 1.11  2003/01/31 19:25:55  gsl
**	Fix copyright header
**	
**	Revision 1.10  2002/07/15 17:10:07  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.9  1996/10/11 22:16:21  gsl
**	drcs update
**	
**
**
*/
