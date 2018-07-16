static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
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

int imemcpy(dst,src,count)								/* Fast memory to memory byte copy.	*/
	register int *dst,*src;								/* Pointers to src and dest locatiions.	*/
	register int count;								/* Count of how many bytes to copy.	*/
{
	while (count--) *(dst++) = *(src++);						/* Copy the integer.			*/
	return(SUCCESS);								/* And return that all is well.		*/
}


/*						Fast integer memory fill routine.						*/

int imemset(dst,src,count)								/* Dest gets count bytes of src.	*/
	register int *dst,src;								/* Pointer to dest location, and src.	*/
	register int count;								/* Count of how many bytes to fill.	*/
{
	while (count--) *(dst++) = src;							/* Fill it.				*/
	return(SUCCESS);								/* Return that all is fine.		*/
}
/*
**	History:
**	$Log: vsystem.c,v $
**	Revision 1.9  1996-10-11 18:16:21-04  gsl
**	drcs update
**
**
**
*/
