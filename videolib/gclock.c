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
/*           VIDEO - Video Interactive Development Environment          */
/*                          Copyright (c) 1987                          */
/*      An unpublished work by Greg L. Adams.  All rights reserved.     */
/************************************************************************/

#include <stdio.h>
#ifdef unix
#include <sys/types.h>
#endif
#include <time.h>
#include "video.h"
#include "vlocal.h"
#include "vdata.h"
#include "vintdef.h"
#include "vmodules.h"

int gclock(void)
{
	char *ctime();
	time_t time_data;
	int i,k;
	int active;
	int row = 10;
	int col = 24;
	int rows = 3;
	int cols = 30;
	unsigned char *save, *vsss();
	char c;
	int emode,cmode;

	VL_vdetpos(0, &row, &col, rows, cols);
	active = TRUE;

	if (vscr_atr & LIGHT) 
	{ 
		emode = VMODE_BOLD; 
		cmode = VMODE_REVERSE|VMODE_BOLD; 
	}
	else 
	{ 
		emode = VMODE_REVERSE; 
		cmode = VMODE_CLEAR|VMODE_BOLD; 
	}

	while(active)
	{
		save = vsss(row,col,rows,cols);

		vset_cursor_off();
		vtext(emode,row,col,"          Good Clock          ");
		vtext(emode,row+1,col,"  ");
		VL_vmode(cmode);
		vprint("                          ");
		vtext(emode,row+1,col+28,"  ");
		vtext(emode,row+2,col,"                              ");

		for (i = 0; !(c = vcheck()); i++)
		{
			time_data = time(NULL);
			vtext(cmode,row+1,col+3,"%s",ctime(&time_data));
			VL_vwait(0,20);
		}

		if (c)
		{
			VL_vpushc(c);
			k = vgetm();
			if (k == up_arrow_key)
			{
				if ((row = row-2) < 1) row = 1;
			}
			else if (k == down_arrow_key)
			{
				if ((row = row+2) > 21) row = 21;
			}
			else if (k == left_arrow_key)
			{
				if ((col = col-4) < 0) col = 0;
			}
			else if (k == right_arrow_key)
			{
				if ((col = col+4) > 50) col = 50;
			}
			else if (k == fn4_key) vcut(ctime(&time_data));
			else active = FALSE;
		}
		else active = FALSE;
	
		vrss(save);
	}
	return(SUCCESS);
}
/*
**	History:
**	$Log: gclock.c,v $
**	Revision 1.14  2003/06/27 15:54:03  gsl
**	fix EDE API
**	
**	Revision 1.13  2003/01/31 19:25:57  gsl
**	Fix copyright header
**	
**	Revision 1.12  2002/07/17 21:06:00  gsl
**	VL_ globals
**	
**	Revision 1.11  2002/07/15 20:16:06  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.10  1997/07/08 20:17:28  gsl
**	Change to use new video.h interface
**	
**	Revision 1.9  1996-10-11 18:15:55-04  gsl
**	drcs update
**
**
**
*/
