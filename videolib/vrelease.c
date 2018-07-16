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

/*						Subroutine entry point.								*/

int vrelease(void)									/* Release from optimization.		*/
{
	vdefer_restore();								/* Restore from current optimization.	*/
	return(SUCCESS);								/* Return to the caller.		*/
}
/*
**	History:
**	$Log: vrelease.c,v $
**	Revision 1.10  1997-07-09 12:09:23-04  gsl
**	Removed the BLOCK_MODE support so now is the same as vdefer_restore()
**
**	Revision 1.9  1996-10-11 18:16:19-04  gsl
**	drcs update
**
**
**
*/
