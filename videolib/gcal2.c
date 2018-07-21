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
/*	     VIDEO - Video Interactive Development Environment		*/
/*			    Copyright (c) 1991				*/
/*	An unpublished work by Greg L. Adams.  All rights reserved.	*/
/************************************************************************/

#include <stdio.h>
#include <string.h>
#include <time.h>
#include "video.h"
#include "vlocal.h"
#include "vdata.h"
#include "vintdef.h"
#include "vmenu.h"
#include "vmodules.h"

#define WIDTH  32
#define LENGTH 16

static int row, col, rows, cols;
static int emode,cmode;
static unsigned char *save;
static int line, column;

static int givehelp();
static int showtab();
static int move();
static int nextcol();
static int home();

int VL_gcal2(this_date,tab) char *this_date; char tab[LENGTH][WIDTH+1];
{
	register int i, j, k;
	int active;
	unsigned char c;

	VL_vbuffering_start();
	row = 2;
	col = 20;
	rows = LENGTH+3;
	cols = WIDTH+2;
	VL_vdetpos(1,&row,&col,rows,cols);
	save = vsss(row,col,rows,cols);
	if (vscr_atr & LIGHT) { emode = VMODE_BOLD;    cmode = VMODE_REVERSE|VMODE_BOLD; }
	else                  { emode = VMODE_REVERSE; cmode = VMODE_CLEAR|VMODE_BOLD;   }

	line = 0;
	column = 6;

	showtab(this_date,tab);

	active = TRUE;
	while(active)
	{
		k = vgetm();

		if ((k >= ' ') && (k < 0177))
		{
			VL_vmode(emode);
			c = k;
			vputc(c);
			tab[line][column] = c;
			nextcol();
		}

		else if ((k == fn1_key) || (k == fn16_key)) active = FALSE;

		else if (k == help_key) givehelp();

		else if (k == delete_key)
		{
			if ((column == 0) || (column == 6))
			{
				if (line > 0)
				{
					line--;
					column = WIDTH-1;
					vmove(row+2+line, col+1+column);
					VL_vmode(emode);
					vputc(' ');
					vslew(0,-1);
					tab[line][column] = ' ';
				}
				else vbell();
			}
			else if (column > 0)
			{
				vslew(0,-1);
				VL_vmode(emode);
				vputc(' ');
				vslew(0,-1);
				column--;
				tab[line][column] = ' ';
			}
			else vbell();
		}

		else if ((k == return_key) || (k == tab_key))
		{
			if (column < 6) column = 0;
			else column = 6;
			if (++line >= LENGTH) line = 0;
			vmove(row+2+line, col+1+column);
		}

		else if (k == fn2_key)
		{
			for (i = 0; i < LENGTH; i++)
			{
				for (j = 6; j < WIDTH; j++) tab[i][j] = ' ';
			}
			showtab(this_date,tab);
			home();
		}

		else if (k == fn3_key) VL_vpaste(0);

		else if (k == fn4_key)
		{
			if (column < 6) vcut(&tab[line][0]);
			else vcut(&tab[line][6]);
		}

		else if (k == home_key) home();

		else if (k == fn5_key)
		{
			for (i = LENGTH-1; i > line; i--)
			{
				strcpy(&tab[i][6],&tab[i-1][6]);
			}
			for (j = 6; j < WIDTH; j++) tab[line][j] = ' ';
			showtab(this_date,tab);
		}

		else if (k == fn6_key)
		{
			for (i = line; i < LENGTH-1; i++)
			{
				strcpy(&tab[i][6],&tab[i+1][6]);
			}
			for (j = 6; j < WIDTH; j++) tab[LENGTH-1][j] = ' ';
			showtab(this_date,tab);
		}

		else if (k == insert_key)
		{
			if (column < 6) vbell();
			else
			{
				for (i = WIDTH-1; i > column; i--)
				{
					tab[line][i] = tab[line][i-1];
				}
				tab[line][column] = ' ';
				showtab(this_date,tab);
			}
		}

		else if (k == remove_key)
		{
			if (column < 6) vbell();
			else
			{
				for (i = column; i < WIDTH-1; i++)
				{
					tab[line][i] = tab[line][i+1];
				}
				tab[line][WIDTH-1] = ' ';
				showtab(this_date,tab);
			}
		}

		else if (k == up_arrow_key)
		{
			if (line == 0)
			{
				i = row;
				row = row - 2;
				if (row < 1) row = 1;
				if (row == i) vbell();
				else move(this_date,tab);
			}
			else
			{
				vslew(-1,0);
				line = line - 1;
			}
		}

		else if (k == down_arrow_key)
		{
			if (line == LENGTH-1)
			{
				i = row;
				row = row + 2;
				if (row > MAX_LINES_PER_SCREEN-rows)
				{
					row = MAX_LINES_PER_SCREEN-rows;
				}
				if (row == i) vbell();
				else move(this_date,tab);
			}
			else
			{
				line = line + 1;
				vslew(1,0);
			}
		}

		else if (k == left_arrow_key)
		{
			if (column == 0)
			{
				i = col;
				col = col - 4;
				if (col < 0) col = 0;
				if (col == i) vbell();
				else move(this_date,tab);
			}
			else
			{
				column--;
				vslew(0,-1);
			}
		}

		else if (k == right_arrow_key)
		{
			if (column == WIDTH-1)
			{
				i = col;
				col = col + 4;
				if (col > VL_vscr_wid-cols) col = VL_vscr_wid-cols;
				if (col == i) vbell();
				else move(this_date,tab);
			}
			else
			{
				vslew(0,1);
				column++;
			}
		}

		else vbell();
	}

	k = FALSE;
	for (i = 0; (i < LENGTH) && !k; i++)
	{
		for (j = 6; j < WIDTH; j++)
		{
			if (tab[i][j] != ' ') k = TRUE;
		}
	}

	vrss(save);
	vset_cursor_off();
	VL_vbuffering_end();
	return(k);
}

static int givehelp()
{
	struct video_menu help;
	int4 key;

	VL_vmenuinit(&help,DISPLAY_ONLY_MENU,VMODE_REVERSE,0,0,0);
	VL_vmenuitem(&help,"Good Calendar Help - Page 2 - General",0,NULL);
	VL_vmenuitem(&help,"",0,NULL);
	VL_vmenuitem(&help,"PF1 returns to monthly calendar.",0,NULL);
	VL_vmenuitem(&help,"PF2 clears the page.",0,NULL);
	VL_vmenuitem(&help,"PF3 will paste into the calendar.",0,NULL);
	VL_vmenuitem(&help,"PF4 will cut from the calendar.",0,NULL);
	VL_vmenuitem(&help,"PF5 inserts a blank line.",0,NULL);
	VL_vmenuitem(&help,"PF6 deletes the current line.",0,NULL);
	VL_vmenuitem(&help,"HOME (FIND) moves to top of page.",0,NULL);
	VL_vmenuitem(&help,"DELETE, INSERT and REMOVE are for editing.",0,NULL);
	VL_vmenuitem(&help,"Arrow keys move the cursor and the window.",0,NULL);
	VL_vmenuitem(&help,"",0,NULL);
	VL_vmenuitem(&help,"Depress any key to exit...",0,NULL);

	key = VL_vmenugo(&help);
	return(SUCCESS);
}

static int showtab(this_date,tab) char *this_date; char tab[LENGTH][WIDTH+1];
{
	register int i, j;

	VL_vbuffering_start();

	vset_cursor_off();
	i = row;

	vtext(emode, i++, col, " Good Calendar    %s ",this_date);
	vmove(i++,col); vline(HORIZONTAL, WIDTH+2);

	for (j = 0; j < LENGTH; j++)
	{
		vtext(emode, i++, col+1, "%s", tab[j]);
	}

	VL_vgrid(row+1,col,rows-1,cols,0,0);

	vmove(row+2+line, col+1+column);
	VL_vmode(CLEAR);
	vcharset(DEFAULT);
	vset_cursor_on();
	VL_vbuffering_end();
	return(SUCCESS);
}

static int move(this_date,tab) char *this_date; char tab[LENGTH][WIDTH+1];
{
	vrss(save);
	save = vsss(row,col,rows,cols);
	showtab(this_date,tab);
	return(SUCCESS);
}

static int nextcol()
{
	if (++column >= WIDTH)
	{
		column = 6;
		if (++line >= LENGTH) line = 0;
		vmove(row+2+line,col+1+column);
	}
	return(SUCCESS);
}

static int home()
{
	line = 0;
	column = 6;
	vmove(row+2+line, col+1+column);
	return(SUCCESS);
}
/*
**	History:
**	$Log: gcal2.c,v $
**	Revision 1.17  2010/02/10 03:52:33  gsl
**	fix warnings for redefined functions
**	
**	Revision 1.16  2003/06/23 15:28:04  gsl
**	VL_ global symbols
**	
**	Revision 1.15  2003/01/31 19:25:57  gsl
**	Fix copyright header
**	
**	Revision 1.14  2002/07/17 21:05:59  gsl
**	VL_ globals
**	
**	Revision 1.13  2002/07/16 13:40:23  gsl
**	VL_ globals
**	
**	Revision 1.12  2002/07/15 20:16:05  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.11  2002/07/15 17:52:53  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.10  1997/07/08 20:13:58  gsl
**	Change to use new video.h defines
**	
**	Revision 1.9  1996-10-11 18:15:53-04  gsl
**	drcs update
**
**
**
*/
