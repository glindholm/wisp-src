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
#ifndef NOSTDLIB
#include <stdlib.h>
#endif
#include <string.h>
#include "video.h"									/* Include video definitions.		*/
#include "vlocal.h"									/* Include video internal definitions.	*/
#include "vintdef.h"
#include "vmodules.h"
#include "vdata.h"


/*						Global data storage.								*/

struct save_screen *vscrn_stack;							/* Reference to external variable.	*/

/*						Push screen routine.								*/

void vpushscr()										/* A function to save the screen addrs	*/
											/* and variables.			*/
{
	struct save_screen *ss_ptr;							/* Point to the the save area.		*/
	int x;										/* A local, working variable.		*/

	vdefer_restore();								/* Be sure to bring screen up to date.	*/
	x = sizeof(struct save_screen);
	ss_ptr = (struct save_screen *) malloc(x);					/* Alloc storage for the info to save.	*/
	if (!ss_ptr)
	{
		vre("Error, vpushscr() unable to obtain memory to store screen maps");	/* Dis is a baaad error, boz.		*/
		exit(0);								/* Unconditional exit to DCL.		*/
	}
                                                                                        
	x = sizeof(vchr_map);	   							/* Size of the data to store.		*/
	ss_ptr->xchr_map = malloc(x);							/* Alloc. storage and save the addr.	*/
	if (!ss_ptr->xchr_map)
	{
		vre("Error, vpushscr() unable to obtain memory to store screen maps");	/* Dis is a baaad error, boz.		*/
		exit(0);								/* Unconditional exit to DCL.		*/
	}    

	memcpy(ss_ptr->xchr_map, vchr_map, x);						/* Copy contents to storage area.	*/

	ss_ptr->xatr_map = malloc(x);							/* Alloc. storage and save the addr.	*/
	if (!ss_ptr->xatr_map)
	{
		vre("Error, vpushscr() unable to obtain memory to store screen maps");	/* Dis is a baaad error, boz.		*/
		exit(0);								/* Unconditional exit to DCL.		*/
	}         
	memcpy(ss_ptr->xatr_map, vatr_map, x);						/* Copy contents to storage area.	*/

	x = sizeof(vcur_set);								/* Size of the data to store.		*/
	ss_ptr->xcur_set = (int *) malloc(x);						/* Allocate some memory.		*/
	if (!ss_ptr->xcur_set)
	{
		vre("Error, vpushscr() unable to obtain memory to store screen maps");	/* Dis is a baaad error, boz.		*/
		exit(0);								/* Unconditional exit to DCL.		*/
	}         
	memcpy(ss_ptr->xcur_set, vcur_set, x);						/* Copy contents to storage area.	*/

	ss_ptr->xcur_lin = vcur_lin;
	ss_ptr->xcur_col = vcur_col;
	ss_ptr->xcur_atr = vcur_atr;
	ss_ptr->xchr_set = vchr_set;                                                                          
	ss_ptr->xscr_atr = vscr_atr;
	ss_ptr->xrol_top = vrol_top;
	ss_ptr->xrol_bot = vrol_bot;
	ss_ptr->xmap_top = vmap_top;

	ss_ptr->prev_ptr = vscrn_stack;							/* Save the address of the prior screen	*/
	vscrn_stack = ss_ptr;								/* Save addr. of the new top screen.	*/
}                                                                                       
/*
**	History:
**	$Log: vpushscr.c,v $
**	Revision 1.10  1997/07/09 15:56:11  gsl
**	Change to use new video.h interfaces
**	Remove line attribute handling
**	
**	Revision 1.9  1996-10-11 18:16:16-04  gsl
**	drcs update
**
**
**
*/
