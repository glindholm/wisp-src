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

struct save_screen *VL_vscrn_stack;							/* Reference to external variable.	*/

/*						Push screen routine.								*/

void VL_vpushscr()										/* A function to save the screen addrs	*/
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

	ss_ptr->prev_ptr = VL_vscrn_stack;							/* Save the address of the prior screen	*/
	VL_vscrn_stack = ss_ptr;								/* Save addr. of the new top screen.	*/
}                                                                                       
/*
**	History:
**	$Log: vpushscr.c,v $
**	Revision 1.13  2003/06/20 15:48:03  gsl
**	VL_ globals
**	
**	Revision 1.12  2003/01/31 19:25:56  gsl
**	Fix copyright header
**	
**	Revision 1.11  2002/07/15 20:16:12  gsl
**	Videolib VL_ gobals
**	
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
