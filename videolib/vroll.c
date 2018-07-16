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

#include "video.h"									/* Include video definitions.		*/
#include "vlocal.h"									/* Include local definitions.		*/
#include "vdata.h"									/* Include keyboard control definitions.*/
#include "vcap.h"
#include "vmodules.h"
#include "vraw.h"

static int vrol_do();

/*						Subroutine entry point.								*/

int vroll(int top, int bottom)								/* Set top and bottom of scroll area.	*/
{
	register int ret;								/* Return code.				*/

	ret = OPTIMIZED;								/* Assume we will be optimized.		*/

	if (top > bottom)
	{
		vre("vroll(%d,%d)-The line number of the scroll area top must be less than the bottom.",top,bottom);
		ret = FAILURE;
	}

	else if ((top < 0) || (bottom >= MAX_LINES_PER_SCREEN))
	{
		vre("vroll-area(%d,%d)-Invalid top or bottom value specified.",top,bottom);
		ret = FAILURE;
	}

	else if ((!VL_vrol_op) || (voptlevel() <= VOP_DATA_ONLY))				/* Do the action regardless?		*/
	{
		ret = vrol_do(top,bottom);						/* Do the action.			*/
		VL_vrol_op = TRUE;								/* Off with the one-shot.		*/
	}

	else if (voptlevel() <= VOP_DEFER_MOTION_ONLY)					/* Should we attempt to optimize?	*/
	{
		if ((top != vrol_top) || (bottom != vrol_bot)) ret = vrol_do(top,bottom);	/* Do the action if necessary.	*/
	}

	else if ((top != vrol_top) || (bottom != vrol_bot)) vdefer_save();		/* Deferred mode so make change later.	*/

	vrol_top = top;									/* Remember the state now.		*/
	vrol_bot = bottom;
	return(ret);
}

static int vrol_do(int top, int bottom)							/* Actually set the scroll region.	*/
{
#ifdef unix
	char *tparm();
#define PARMFUNC tparm
#else
	char *VL_vcparm();
#define PARMFUNC VL_vcparm
#endif
	
	vdefer_restore();								/* Must restore before actual output.	*/

#ifdef DIRECTVID
	if (vrawdirectio())
	{
		vrawsetscroll(top,bottom);						/* Set scroll region in vrawdos.c.	*/
	}
	else
#endif
	{
		if ( !(top == 0 && bottom == MAX_LINES_PER_SCREEN-1) )
		{
			VL_vcapnull(scrarea_esc,"CHANGE_SCROLL_REGION",1);			/* test if defined			*/
		}
		vcontrol(PARMFUNC(scrarea_esc,top,bottom));				/* Select the scroll area.		*/
		VL_vha();								/* Adjust for unwanted home move.	*/
	}

	return(SUCCESS);
}


int VL_vscroll_frwd_avail(void)
{
	static int frwd_scroll_avail = -1;

	if ( -1 == frwd_scroll_avail )
	{
#ifdef DIRECTVID
		if (vrawdirectio())
		{
			frwd_scroll_avail = 0;						/* Can't use forward scroll		*/
		}
		else
#endif
		if ( VL_vcapvalue(VCAP_LINES) && *((int *)VL_vcapvalue(VCAP_LINES)) == 25 )		/* is there a 25th line			*/
		{
			frwd_scroll_avail = 0;						/* Can't use forward scroll		*/
		}
		else
		{
			frwd_scroll_avail = 1;						/* Can use forward scroll		*/
		}
	}

	return frwd_scroll_avail;
}

int VL_vscroll_rvrs_avail(void)
{
	static int rvrs_scroll_avail = -1;

	if ( -1 == rvrs_scroll_avail )
	{
#ifdef DIRECTVID
		if (vrawdirectio())
		{
			rvrs_scroll_avail = 0;						/* Can't use forward scroll		*/
		}
		else
#endif
		if (VL_vcapnull(rvrsidx_esc, "SCROLL_REVERSE",0)) 
		{
			rvrs_scroll_avail = 0;						/* There is no reverse scroll		*/
		}
		else
		{
			rvrs_scroll_avail = 1;						/* There is reverse scroll		*/
		}
	}
	
	return rvrs_scroll_avail;
}

/*
**	History:
**	$Log: vroll.c,v $
**	Revision 1.17  2003/06/20 15:04:28  gsl
**	VL_ globals
**	
**	Revision 1.16  2003/01/31 19:25:56  gsl
**	Fix copyright header
**	
**	Revision 1.15  2003/01/28 21:39:52  gsl
**	Change vcap.h defines to make unique BELL -> VCAP_BELL etc.
**	
**	Revision 1.14  2002/07/15 20:56:40  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.13  2002/07/15 20:16:14  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.12  1997/07/12 21:48:56  gsl
**	fix prototype
**	
**	Revision 1.11  1997-07-09 12:11:42-04  gsl
**	Change to use new video.h interfaces
**	Fix the directio logic
**	Add support for COSTAR with WIN32
**	Add routines to determine in scrolling is available
**
**	Revision 1.10  1996-10-11 18:16:19-04  gsl
**	drcs update
**
**
**
*/
