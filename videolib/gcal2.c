static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
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

int gcal2(this_date,tab) char *this_date; char tab[LENGTH][WIDTH+1];
{
	register int i, j, k;
	unsigned char *vsss();
	int active;
	unsigned char c;

	vbuffering_start();
	row = 2;
	col = 20;
	rows = LENGTH+3;
	cols = WIDTH+2;
	vdetpos(1,&row,&col,rows,cols);
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
			vmode(emode);
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
					vmode(emode);
					vputc(' ');
					vslew(0,-1);
					tab[line][column] = ' ';
				}
				else vbell();
			}
			else if (column > 0)
			{
				vslew(0,-1);
				vmode(emode);
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

		else if (k == fn3_key) vpaste(0);

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
				if (col > vscr_wid-cols) col = vscr_wid-cols;
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
	vbuffering_end();
	return(k);
}

static int givehelp()
{
	struct video_menu help;
	int4 key;

	vmenuinit(&help,DISPLAY_ONLY_MENU,VMODE_REVERSE,0,0,0);
	vmenuitem(&help,"Good Calendar Help - Page 2 - General",0,NULL);
	vmenuitem(&help,"",0,NULL);
	vmenuitem(&help,"PF1 returns to monthly calendar.",0,NULL);
	vmenuitem(&help,"PF2 clears the page.",0,NULL);
	vmenuitem(&help,"PF3 will paste into the calendar.",0,NULL);
	vmenuitem(&help,"PF4 will cut from the calendar.",0,NULL);
	vmenuitem(&help,"PF5 inserts a blank line.",0,NULL);
	vmenuitem(&help,"PF6 deletes the current line.",0,NULL);
	vmenuitem(&help,"HOME (FIND) moves to top of page.",0,NULL);
	vmenuitem(&help,"DELETE, INSERT and REMOVE are for editing.",0,NULL);
	vmenuitem(&help,"Arrow keys move the cursor and the window.",0,NULL);
	vmenuitem(&help,"",0,NULL);
	vmenuitem(&help,"Depress any key to exit...",0,NULL);

	key = vmenugo(&help);
	return(SUCCESS);
}

static int showtab(this_date,tab) char *this_date; char tab[LENGTH][WIDTH+1];
{
	register int i, j;

	vbuffering_start();

	vset_cursor_off();
	i = row;

	vtext(emode, i++, col, " Good Calendar    %s ",this_date);
	vmove(i++,col); vline(HORIZONTAL, WIDTH+2);

	for (j = 0; j < LENGTH; j++)
	{
		vtext(emode, i++, col+1, "%s", tab[j]);
	}

	vgrid(row+1,col,rows-1,cols,0,0);

	vmove(row+2+line, col+1+column);
	vmode(CLEAR);
	vcharset(DEFAULT);
	vset_cursor_on();
	vbuffering_end();
	return(SUCCESS);
}

static int move(this_date,tab) char *this_date; char tab[LENGTH][WIDTH+1];
{
	unsigned char *vsss();

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
**	Revision 1.10  1997/07/08 20:13:58  gsl
**	Change to use new video.h defines
**	
**	Revision 1.9  1996-10-11 18:15:53-04  gsl
**	drcs update
**
**
**
*/
