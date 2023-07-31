/*
******************************************************************************
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
**
** NOTICE:
** Confidential, unpublished property of Shell Stream Software LLC.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of Shell Stream Software LLC
** is strictly prohibited.
** 
******************************************************************************
*/



/*						Include standard header files.							*/

#include <stdio.h>									/* Include standard I/O definitions.	*/
#include <stdlib.h>
#include <string.h>
#include "video.h"									/* Include video definitions.		*/
#include "vlocal.h"									/* Include video internal definitions.	*/
#include "vmodules.h"
#include "vdata.h"

int VL_vpopscr()										/* A function to restore the screen and	*/
											/* associated variables.		*/
{
	extern struct save_screen *VL_vscrn_stack;						/* Reference to external variable.	*/
	struct save_screen *ss_ptr;							/* Local pointer to the save area.	*/
	int x;										/* A working variable.			*/
                                                                                                                                  
	ss_ptr = VL_vscrn_stack;								/* Point to the most recently saved 	*/
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

	VL_vscrn_stack = ss_ptr->prev_ptr;							/* Get address of the new top screen.	*/
	free(ss_ptr);									/* Free up the allocated area.		*/

	VL_vrefresh(HARD_REFRESH);								/* Refresh the screen.			*/
	return(0);
}
/*
**	History:
**	$Log: vpopscr.c,v $
**	Revision 1.14  2003/06/20 15:48:03  gsl
**	VL_ globals
**	
**	Revision 1.13  2003/01/31 20:58:40  gsl
**	Fix -Wall warnings
**	
**	Revision 1.12  2003/01/31 19:25:56  gsl
**	Fix copyright header
**	
**	Revision 1.11  2002/07/15 20:16:12  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.10  1997/07/09 15:49:25  gsl
**	remove extern references.
**	remove the line attribute stuff as not used
**	
**	Revision 1.9  1996-10-11 18:16:15-04  gsl
**	drcs update
**
**
**
*/
