/*
******************************************************************************
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** $Id:$
**
** NOTICE:
** Confidential, unpublished property of NeoMedia Technologies, Inc.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of NeoMedia 
** Technologies, Inc. is strictly prohibited.
** 
** CVS
** $Source:$
** $Author: gsl $
** $Date:$
** $Revision:$
******************************************************************************
*/

			/************************************************************************/
			/*	      VIDEO - Video Interactive Development Environment		*/
			/*			Copyright (c) 1988, 1989, 1990			*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/************************************************************************/

/*						Include standard header files.							*/

#include <string.h>

#include "video.h"									/* Include video definitions.		*/
#include "vlocal.h"									/* Include local definitions.		*/
#include "vdata.h"									/* Include keyboard control definitions.*/
#include "vcap.h"
#include "vmodules.h"
#include "vraw.h"

static int vcrlfx();

/*						Subroutine entry point.								*/

int VL_vnewline(enum e_vlf direction)							/* Generate a CR/LF.			*/
{
	return(vcrlfx(direction,TRUE));							/* Call CR/LF execute routine.		*/
}


int VL_vlinefeed(enum e_vlf direction)							/* Generate a linefeed			*/
{
	return(vcrlfx(direction,FALSE));						/* Call Video CR/LF eXecute routine.	*/
}


static int vcrlfx(enum e_vlf direction,int do_cr)					/* Move up or down a line.		*/
											/* Is it up or down? With a CR too?	*/
{
	register int i;									/* Return status.			*/
	int save_pure;									/* Location to save CR/LF pure state.	*/

	switch(direction)
	{
		case VLF_FORWARD:							/* Newline or LF in forward direction.	*/
		{
			save_pure = VL_vb_pure;						/* Remember the I/O pure mode.		*/
			if (do_cr)  VL_vb_pure = FALSE;					/* Linefeeds generate new lines.	*/
			else 	    VL_vb_pure = TRUE;					/* Let linefeeds go through unscathed.	*/
			vprint("\n");							/* Send one through now.		*/
			VL_vb_pure = save_pure;						/* Set purity back to what it was.	*/
			break;								/* We're all done.			*/
		}

		case VLF_REVERSE:							/* Newline or LF in reverse direction.	*/
		{
			int noscroll_avail, cmpos;
			enum e_vop svo;
			char temp[MAX_COLUMNS_PER_LINE+1];

			VL_vbuffering_start();						/* Logical buffering.			*/
			vdefer_restore();						/* Must restore from deferred mode.	*/

#ifdef DIRECTVID
			if (vrawdirectio())
			{
				noscroll_avail = 1;
			}
			else
#endif
			{
				if (!(noscroll_avail = VL_vcapnull(rvrsidx_esc, "SCROLL_REVERSE",0))) /* test if defined.		*/
				{							/* YES! it is defined.			*/
					vcontrol(rvrsidx_esc);				/* Send out reverse index command.	*/
					if (do_cr)					/* A newline format?			*/
					{
						vprint("\r");				/* Add a CR if needed.			*/
						vcur_col = 0;				/* Record occurence of a CR		*/
					}
				}
			}

			vcur_lin--;							/* up 1 line				*/
			if (vcur_lin < 0)						/* Did we hit the top of screen?	*/
			{
				vcur_lin = 0;						/* Set current line = top of screen     */
				vdefer_restore();					/* Make sure screen is updated		*/
				VL_vmap(SCROLL_DOWN,0,0,MAX_LINES_PER_SCREEN-1,0);		/* Scroll whole screen			*/
				if (noscroll_avail)
				{
					svo = voptimize(VOP_TRACKING_ONLY);		/* Turn optimization down.		*/
					cmpos = vmap_top;
					for (i = MAX_LINES_PER_SCREEN-1; i >= 0 ; i--)
					{
						vmove(i,0);
						if (cmpos == 0) cmpos = MAX_LINES_PER_SCREEN;
						memcpy(temp,vchr_map[cmpos-1],VL_vscr_wid);
						temp[VL_vscr_wid] = '\0';
						vprint("%s",temp);
						cmpos--;
					}
					vmove(0,0);
					voptimize(svo);					/* Set back to what it was.		*/
				}
			}
			else if (vcur_lin == (vrol_top - 1))				/* Did we hit the top of the scroll reg	*/
			{
				vcur_lin = vrol_top;					/* Set current line = top of scroll reg */
				vdefer_restore();					/* Make sure screen is updated		*/
				VL_vmap(SCROLL_DOWN,vrol_top,0,vrol_bot,0);		/* Scroll just the scrolling region	*/
				if (noscroll_avail)
				{
					svo = voptimize(VOP_TRACKING_ONLY);		/* Turn optimization down.		*/
					for (i = vrol_bot; i >= vrol_top ; i--)
					{
						vmove(i,0);
						memcpy(temp,vchr_map[i],MAX_COLUMNS_PER_LINE);
						temp[MAX_COLUMNS_PER_LINE] = '\0';
						vprint("%s",temp);
					}
					vmove(0,0);
					voptimize(svo);					/* Set back to what it was.		*/
				}
			}
			VL_vbuffering_end();						/* Back to automatic buffering.		*/
			break;
		}

		default:								/* Oops, invalid function.		*/
		{
			vre("Invalid direction specified in vnewline() or vlinefeed()");
			return(FAILURE);						/* Let the caller know we failed.	*/
		}
	}
	return(SUCCESS);
}
/*
**	History:
**	$Log: vnewline.c,v $
**	Revision 1.18  2003/01/31 19:25:56  gsl
**	Fix copyright header
**	
**	Revision 1.17  2002/07/17 21:06:03  gsl
**	VL_ globals
**	
**	Revision 1.16  2002/07/16 13:40:21  gsl
**	VL_ globals
**	
**	Revision 1.15  2002/07/15 20:16:11  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.14  1997/07/12 21:36:35  gsl
**	Fix to handle reverse scroll with COSTAR on WIN32.
**	Corrected optimization to not use VOP_OFF
**	
**	Revision 1.13  1997-07-09 11:48:35-04  gsl
**	change to use the new video.h interfaces
**
**	Revision 1.12  1997-01-09 19:32:17-05  gsl
**	Corrected the reverse scrollomg logic for NT and MSDS
**
**	Revision 1.11  1996-10-11 15:16:13-07  gsl
**	drcs update
**
**
**
*/
