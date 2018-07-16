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
#include "video.h"									/* Include video definitions.		*/
#include "vlocal.h"									/* Include video internal definitions.	*/

vpopscr()										/* A function to restore the screen and	*/
											/* associated variables.		*/
{
	extern char vchr_map[MAX_LINES_PER_SCREEN][MAX_COLUMNS_PER_LINE];		/* Reference to external variable array.*/
	extern char vatr_map[MAX_LINES_PER_SCREEN][MAX_COLUMNS_PER_LINE];		/* Reference to external variable array.*/
	extern int vcur_lin, vcur_col, vcur_atr, vchr_set;				/* Reference to external variables.	*/
	extern int vcur_set[INT_SET_TABLE_SIZE];					/* Reference to external variables.	*/
	extern int vlin_atr[MAX_LINES_PER_SCREEN];					/* Reference to external variables.	*/
	extern int vscr_atr, vrol_top, vrol_bot, vmap_top;				/* Reference to external variables.	*/
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

	x = sizeof(vlin_atr);								/* Figure out the size of the area.	*/
	memcpy(vlin_atr, ss_ptr->xlin_atr, x);						/* Copy the saved data.			*/
	free(ss_ptr->xlin_atr);								/* Free up this area.			*/

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
}
