static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*									*/
			/*	     VIDEO - Video Interactive Development Environment		*/
			/*									*/
			/*			    Copyright (c) 1989				*/
			/*									*/
			/*	An unpublished work by Greg L. Adams.  All rights reserved.	*/
			/*									*/
			/************************************************************************/


/*						Include standard header files.							*/

#include <stdio.h>									/* Include standard I/O definitions.	*/
#include <stdlib.h>
#include <string.h>
#include "video.h"									/* Include video definitions.		*/
#include "vlocal.h"									/* Include video internal definitions.	*/
#include "vmodules.h"
#include "vdata.h"

vpopscr()										/* A function to restore the screen and	*/
											/* associated variables.		*/
{
	extern struct save_screen *vscrn_stack;						/* Reference to external variable.	*/
	struct save_screen *ss_ptr;							/* Local pointer to the save area.	*/
	int x;										/* A working variable.			*/
                                                                                                                                  
	ss_ptr = vscrn_stack;								/* Point to the most recently saved 	*/
											/* structure of pointers and variables.	*/

	if (!ss_ptr)									/* Are we pointing to never never land?	*/
	{
		vre("Error, vpopscr() stack empty.");					/* Give them an error message.		*/
		return(0);								/* Outta here.				*/
	}

	x = sizeof(vchr_map);								/* What's the size of this item.	*/
	memcpy(vchr_map, ss_ptr->xchr_map, x);						/* Copy the saved bytes.		*/
	free(ss_ptr->xchr_map);								/* Free up this area.			*/
	memcpy(vatr_map, ss_ptr->xatr_map, x);
	free(ss_ptr->xatr_map);								/* Free up this area.			*/

	x = sizeof(vcur_set);								/* Figure out the size of the area.	*/
	memcpy(vcur_set, ss_ptr->xcur_set, x);						/* Copy the saved bytes.		*/
	free(ss_ptr->xcur_set);								/* Free up the area.			*/

	vcur_lin = ss_ptr->xcur_lin;
	vcur_col = ss_ptr->xcur_col;
	vcur_atr = ss_ptr->xcur_atr;
	vchr_set = ss_ptr->xchr_set;
	vscr_atr = ss_ptr->xscr_atr;
	vrol_top = ss_ptr->xrol_top;
	vrol_bot = ss_ptr->xrol_bot;
	vmap_top = ss_ptr->xmap_top;

	vscrn_stack = ss_ptr->prev_ptr;							/* Get address of the new top screen.	*/
	free(ss_ptr);									/* Free up the allocated area.		*/

	vrefresh(HARD_REFRESH);								/* Refresh the screen.			*/
	return(0);
}
/*
**	History:
**	$Log: vpopscr.c,v $
**	Revision 1.10  1997-07-09 11:49:25-04  gsl
**	remove extern references.
**	remove the line attribute stuff as not used
**
**	Revision 1.9  1996-10-11 18:16:15-04  gsl
**	drcs update
**
**
**
*/
