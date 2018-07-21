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

#include <string.h>

#include "video.h"									/* Include video definitions.		*/
#include "vlocal.h"									/* Include local definitions.		*/
#include "vmodules.h"
#include "vdata.h"

static void vmp_express();
static int vmp_x();
static int vmp_up();
static int vmp_down();

/*						Subroutine entry point.								*/

int VL_vmap(action,sl,sc,el,ec) int action,sl,sc,el,ec;					/* Perform action on screen map.	*/
{
	register int i, j;								/* Working registers.			*/

	if ((action == CLEAR) || (action == TAG_AS_OLD))
	{
		if (voptlevel() == VOP_OFF) return(FAILURE);				/* Cannot do if not tracking.		*/

		if ((action == CLEAR) && (sl+sc == 0) && (el == MAX_LINES_PER_SCREEN-1) && (ec == MAX_COLUMNS_PER_LINE-1))
		{
			vmp_express();							/* Express clear the screen maps.	*/
		}
		else if (sl == el)							/* Action is only on one line.		*/
		{
			for (j = sc; j <= ec; j++) vmp_x(action,sl,j);			/* Do the requested action.		*/
		}
		else									/* Action is over multiple lines.	*/
		{
			for (i = sl; i <= el; i++)					/* Loop through all lines.		*/
			{
				if (i == sl)						/* Is this the first line.		*/
				{
					for (j = sc; j < MAX_COLUMNS_PER_LINE; j++) vmp_x(action,i,j);
				}
				else if (i == el)					/* Is this the last line.		*/
				{
					for (j = 0; j <= ec; j++) vmp_x(action,i,j);
				}
				else							/* No, then do the full line.		*/
				{
					for (j = 0; j < MAX_COLUMNS_PER_LINE; j++) vmp_x(action,i,j);
				}
			}
		}
	}

	else if (action == SCROLL_UP) vmp_up(sl,el);					/* Scroll the map up.			*/

	else if (action == SCROLL_DOWN) vmp_down(sl,el);				/* Scroll the map down.			*/

	else
	{
		vre("vmap(%d)-Invalid parameter value",action);				/* Report the occurance.		*/
		return(FAILURE);							/* Oops, invalid parameter.		*/
	}

	return(SUCCESS);								/* Return to the caller.		*/
}


/*				Do the requested action on a particular cell.							*/

static int vmp_x(action,i,j) int action,i,j;							/* Perform the requested action.	*/
{
	register int k;									/* Working registers.			*/

	k = vml(i);									/* Get the table index.			*/
	if (action == CLEAR)
	{
		vchr_map[k][j] = ' ';							/* Set the character map to space.	*/
		vatr_map[k][j] = 0;							/* Set the attribute map to null.	*/
		vmap_cng[k][j] = 0;							/* And the screen matches the map.	*/
	}
	if (action == TAG_AS_OLD)
	{
		if (!VL_visible(vchr_map[k][j],vatr_map[k][j])) vmap_cng[k][j] = 0;	/* If not visible, all is ok...		*/
		else vmap_cng[k][j] = VMAP_CNG_OLDDATA;					/*    else flag as old data.		*/
	}
	return(SUCCESS);								/* Return to the caller.		*/
}


/*					Scroll the map up.									*/

static int vmp_up(sl,el) int sl,el;								/* From start line to end line.		*/
{
	register int i,k0,k1;								/* Working registers.			*/

	if ((sl == 0) && (el == MAX_LINES_PER_SCREEN-1))				/* Is this a full screen scroll?	*/
	{
		memset(&vchr_map[vmap_top][0],' ',VL_vscr_wid);				/* Fill with spaces.			*/
		memset(&vatr_map[vmap_top][0],'\0',VL_vscr_wid);				/* No attributes set on the new line.	*/
		memset(&vmap_cng[vmap_top][0],'\0',VL_vscr_wid);				/* No changes on this character.	*/
		vmap_top = vmap_top + 1;						/* Adjust the virtual map top.		*/
		if (vmap_top == MAX_LINES_PER_SCREEN) vmap_top = 0;			/* Wrap around if necessary.		*/
	}

	else										/* Not full screen, do the hard way.	*/
	{
		k0 = vml(sl);								/* get initial value			*/
		for (i = sl+1; i <= el; i++)						/* Loop through each line.		*/
		{
			k1 = vml(i);							/* Get index into the table.		*/

			memcpy(&vchr_map[k0][0],&vchr_map[k1][0],VL_vscr_wid);		/* Shuffle the bytes.			*/
			memcpy(&vatr_map[k0][0],&vatr_map[k1][0],VL_vscr_wid);
			memcpy(&vmap_cng[k0][0],&vmap_cng[k1][0],VL_vscr_wid);

			k0 = k1;
		}
											/* Now blank the bottom line.		*/
		memset(&vchr_map[k0][0],' ',VL_vscr_wid);					/* Fill with spaces.			*/
		memset(&vatr_map[k0][0],'\0',VL_vscr_wid);					/* No attributes set on the new line.	*/
		memset(&vmap_cng[k0][0],'\0',VL_vscr_wid);					/* No changes on this character.	*/
	}

	return(SUCCESS);								/* What could possibly go wrong?	*/
}

/*					Scroll the map down.									*/

static int vmp_down(sl,el) int sl,el;								/* From start line to end line.		*/
{
	register int i,k0,k1;								/* Working registers.			*/

	if ((sl == 0) && (el == MAX_LINES_PER_SCREEN-1))				/* Is this a full screen scroll?	*/
	{
		vmap_top = vmap_top - 1;						/* Adjust the virtual map top.		*/
		if (vmap_top < 0) vmap_top = MAX_LINES_PER_SCREEN - 1;			/* Wrap around if necessary.		*/
		memset(&vchr_map[vmap_top][0],' ',VL_vscr_wid);				/* Fill with spaces.			*/
		memset(&vatr_map[vmap_top][0],'\0',VL_vscr_wid);				/* No attributes set on the new line.	*/
		memset(&vmap_cng[vmap_top][0],'\0',VL_vscr_wid);				/* No changes on this character.	*/
	}

	else										/* Not full screen, do the hard way.	*/
	{
		k0 = vml(el);								/* get initial value			*/
		for (i = el-1; i >= sl; i--)						/* Loop through each line.		*/
		{
			k1 = vml(i);							/* Get index into the table.		*/

			memcpy(&vchr_map[k0][0],&vchr_map[k1][0],VL_vscr_wid);		/* Shuffle the bytes.			*/
			memcpy(&vatr_map[k0][0],&vatr_map[k1][0],VL_vscr_wid);
			memcpy(&vmap_cng[k0][0],&vmap_cng[k1][0],VL_vscr_wid);

			k0 = k1;
		}
											/* Now blank the bottom line.		*/
		memset(&vchr_map[k0][0],' ',VL_vscr_wid);					/* Fill with spaces.			*/
		memset(&vatr_map[k0][0],'\0',VL_vscr_wid);					/* No attributes set on the new line.	*/
		memset(&vmap_cng[k0][0],'\0',VL_vscr_wid);					/* No changes on this character.	*/
	}

	return(SUCCESS);								/* What could possibly go wrong?	*/
}

static void vmp_express()
{
	memset(vchr_map, ' ',MAX_LINES_PER_SCREEN*MAX_COLUMNS_PER_LINE);		/* Clear the maps...			*/
	memset(vatr_map,'\0',MAX_LINES_PER_SCREEN*MAX_COLUMNS_PER_LINE);
	memset(vmap_cng,'\0',MAX_LINES_PER_SCREEN*MAX_COLUMNS_PER_LINE);
}
/*
**	History:
**	$Log: vmap.c,v $
**	Revision 1.16  2003/06/20 15:48:03  gsl
**	VL_ globals
**	
**	Revision 1.15  2003/01/31 20:58:40  gsl
**	Fix -Wall warnings
**	
**	Revision 1.14  2003/01/31 19:25:56  gsl
**	Fix copyright header
**	
**	Revision 1.13  2002/07/16 13:40:22  gsl
**	VL_ globals
**	
**	Revision 1.12  2002/07/15 20:16:10  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.11  1998/10/13 18:51:14  gsl
**	Change to use VMAP_CNG_OLDDATA
**	
**	Revision 1.10  1997-07-08 17:16:11-04  gsl
**	Removed the line attribure logic as double width never used
**	and not supported on must terminals.
**	Included vdata.h and removed lots of externs
**
**	Revision 1.9  1996-10-11 18:16:11-04  gsl
**	drcs update
**
**
**
*/
