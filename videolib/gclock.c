static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
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

int gclock()
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

	vdetpos(0, &row, &col, rows, cols);
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
		vmode(cmode);
		vprint("                          ");
		vtext(emode,row+1,col+28,"  ");
		vtext(emode,row+2,col,"                              ");

		for (i = 0; !(c = vcheck()); i++)
		{
			time_data = time(NULL);
			vtext(cmode,row+1,col+3,"%s",ctime(&time_data));
			vwait(0,20);
		}

		if (c)
		{
			vpushc(c);
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
**	Revision 1.10  1997-07-08 16:17:28-04  gsl
**	Change to use new video.h interface
**
**	Revision 1.9  1996-10-11 18:15:55-04  gsl
**	drcs update
**
**
**
*/
