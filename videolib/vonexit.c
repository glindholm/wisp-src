			/************************************************************************/
			/*	      VIDEO - Video Interactive Development Environment		*/
			/*			Copyright (c) 1988, 1989, 1990			*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/************************************************************************/

/*					Include standard header files								*/

#include	"video.h"
#include	"vlocal.h"

/*					Subroutine entry point.									*/

vonexit(new) int new;									/* Change the exit defaults.		*/
{
	extern int exit_options;							/* Reference the exit options.		*/

	if ((new & WIDE) && (new & NARROW))						/* Validate width requests.		*/
	{
		vre("vonexit(%o)-F-Cannot have both WIDE and NARROW screen on exit.",new);
		return(FAILURE);
	}
	if ((new & LIGHT) && (new & DARK))						/* Validate color.			*/
	{
		vre("vonexit(%o)-F-Cannot have both LIGHT and DARK screen on exit.",new);
		return(FAILURE);
	}

	exit_options = new;								/* Store the new exit options.		*/
	
	return(SUCCESS);								/* Now let the caller know how it went.	*/
}
