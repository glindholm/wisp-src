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
#include "vdata.h"

/*						Subroutine entry point.								*/

int vedge(int line)									/* Determine edge of screen.		*/
{
	return(vscr_wid);								/* All done.				*/
}
/*
**	History:
**	$Log: vedge.c,v $
**	Revision 1.10  1997-07-08 16:55:57-04  gsl
**	Removed the double width logic so vedge()
**	now returns the screen width
**
**	Revision 1.9  1996-10-11 18:16:03-04  gsl
**	drcs update
**
**
**
*/
