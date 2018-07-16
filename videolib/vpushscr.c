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
#include "video.h"									/* Include video definitions.		*/
#include "vlocal.h"									/* Include video internal definitions.	*/
#include "vintdef.h"

/*						Global data storage.								*/

struct save_screen *vscrn_stack;							/* Reference to external variable.	*/

/*						Push screen routine.								*/

vpushscr()										/* A function to save the screen addrs	*/
											/* and variables.			*/
{
	extern char vchr_map[MAX_LINES_PER_SCREEN][MAX_COLUMNS_PER_LINE];		/* Reference to external variable array.*/
	extern char vatr_map[MAX_LINES_PER_SCREEN][MAX_COLUMNS_PER_LINE];		/* Reference to external variable array.*/
	extern int vcur_lin, vcur_col, vcur_atr, vchr_set;				/* Reference to external variables.	*/
	extern int vcur_set[INT_SET_TABLE_SIZE];					/* Reference to external variables.	*/
	extern int vlin_atr[MAX_LINES_PER_SCREEN];					/* Reference to external variables.	*/
	extern int vscr_atr, vrol_top, vrol_bot, vmap_top;				/* Reference to external variables.	*/

	struct save_screen *ss_ptr;							/* Point to the the save area.		*/
	int x;										/* A local, working variable.		*/

	vdefer(RESTORE);								/* Be sure to bring screen up to date.	*/
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

	x = sizeof(vlin_atr);								/* Size of the data to store.		*/
	ss_ptr->xlin_atr = (int *) malloc(x);						/* Allocate some memory.		*/
	if (!ss_ptr->xlin_atr)
	{
		vre("Error, vpushscr() unable to obtain memory to store screen maps");	/* Dis is a baaad error, boz.		*/
		exit(0);								/* Unconditional exit to DCL.		*/
	}         
	memcpy(ss_ptr->xlin_atr, vlin_atr, x);						/* Copy contents to storage area.	*/

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
