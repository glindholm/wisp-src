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
/*	     VIDEO - Video Interactive Development Environment		*/
/*			    Copyright (c) 1991				*/
/*	An unpublished work by Greg L. Adams.  All rights reserved.	*/
/************************************************************************/

#include <stdio.h>
#include <time.h>
#include <string.h>
#include "video.h"
#include "vlocal.h"
#include "vdata.h"
#include "vintdef.h"
#include "vmenu.h"
#include "vmodules.h"

#define PAGES  10
#define WIDTH  32
#define LENGTH 16

static int row, col, rows, cols;
static int emode,cmode;
static unsigned char *save;
static char pad[PAGES][LENGTH][WIDTH+1];
static int newpad;
static int page, line, column;

static int givehelp();
static int showpad();
static int move();
static int nextcol();

int gnotepad(void)
{
	register int i, j, k;
	unsigned char *vsss();
	int active;
	unsigned char c;
	FILE *pf;

	VL_vbuffering_start();
	row = 4;
	col = 20;
	rows = LENGTH+3;
	cols = WIDTH+2;
	VL_vdetpos(0, &row, &col, rows, cols);
	save = vsss(row,col,rows,cols);
	if (vscr_atr & LIGHT) { emode = VMODE_BOLD;    cmode = VMODE_REVERSE|VMODE_BOLD; }
	else                  { emode = VMODE_REVERSE; cmode = VMODE_CLEAR  |VMODE_BOLD;   }

	for (i = 0; i < PAGES; i++)
	{
		for (j = 0; j < LENGTH; j++)
		{
			for (k = 0; k < WIDTH; k++) pad[i][j][k] = ' ';
			pad[i][j][WIDTH] = CHAR_NULL;
		}
	}

	if ((pf = VL_vopenf("pad","r")) == NULL) newpad = TRUE;
	else
	{
		newpad = FALSE;
		for (i = 0; i < PAGES; i++)
		{
			for (j = 0; j < LENGTH; j++)
			{
				fgets(pad[i][j], WIDTH*2, pf); 
				pad[i][j][WIDTH] = CHAR_NULL;
			}
		}
		fclose(pf);
	}

	page = 0;
	line = 0;
	column = 0;
	showpad();

	active = TRUE;
	while(active)
	{
		k = vgetm();

		if (k == fn16_key) active = FALSE;

		else if (k == help_key) givehelp();

		else if (k == up_arrow_key)
		{
			if (line == 0)
			{
				i = row;
				row = row - 2;
				if (row < 1) row = 1;
				if (row == i) vbell();
				else move();
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
				else move();
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
				else move();
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
				else move();
			}
			else
			{
				vslew(0,1);
				column++;
			}
		}

		else if (k == fn1_key)
		{
			if (++page == PAGES) page = 0;
			showpad();
		}

		else if (k == fn2_key)
		{
			for (i = 0; i < LENGTH; i++)
			{
				for (j = 0; j < WIDTH; j++) pad[page][i][j] = ' ';
			}
			showpad();
		}

		else if ((k >= ' ') && (k < 0177))
		{
			VL_vmode(emode);
			c = k;
			vputc(c);
			pad[page][line][column] = c;
			nextcol();
		}

		else if (k == delete_key)
		{
			if (column > 0)
			{
				vslew(0,-1);
				VL_vmode(emode);
				vputc(' ');
				vslew(0,-1);
				column--;
				pad[page][line][column] = ' ';
			}
			else if ((column == 0) && (line > 0))
			{
				line--;
				column = WIDTH-1;
				vslew(-1,WIDTH-1);
				VL_vmode(emode);
				vputc(' ');
				vslew(0,-1);
				pad[page][line][column] = ' ';
			}
			else vbell();
		}

		else if (k == return_key)
		{
			column = 0;
			if (++line >= LENGTH) line = 0;
			vmove(row+2+line, col+1+column);
		}

		else if (k == home_key)
		{
			column = 0;
			line = 0;
			vmove(row+2, col+1);
			page = 0;
			showpad();
		}

		else if (k == fn5_key)
		{
			for (i = LENGTH-1; i > line; i--)
			{
				strcpy(pad[page][i],pad[page][i-1]);
			}
			for (j = 0; j < WIDTH; j++) pad[page][line][j] = ' ';
			showpad();
		}

		else if (k == fn6_key)
		{
			for (i = line; i < LENGTH-1; i++)
			{
				strcpy(pad[page][i],pad[page][i+1]);
			}
			for (j = 0; j < WIDTH; j++) pad[page][LENGTH-1][j] = ' ';
			showpad();
		}

		else if (k == insert_key)
		{
			for (i = WIDTH-1; i > column; i--)
			{
				pad[page][line][i] = pad[page][line][i-1];
			}
			pad[page][line][column] = ' ';
			showpad();
		}

		else if (k == remove_key)
		{
			for (i = column; i < WIDTH-1; i++)
			{
				pad[page][line][i] = pad[page][line][i+1];
			}
			pad[page][line][WIDTH-1] = ' ';
			showpad();
		}

		else if (k == fn3_key) VL_vpaste(0);

		else if (k == fn4_key) vcut(pad[page][line]);

		else vbell();
	}

	vrss(save);
	VL_vbuffering_end();

	if (newpad) pf = VL_vopenf("pad","w");
	else pf = VL_vopenf("pad","r+");
	for (i = 0; i < PAGES; i++)
	{
		for (j = 0; j < LENGTH; j++) fprintf(pf, "%s\n", pad[i][j]);
	}
	fclose(pf);

	return(SUCCESS);
}

static int givehelp(void)
{
	struct video_menu help;
	int4 key;

	VL_vmenuinit(&help,DISPLAY_ONLY_MENU,VMODE_REVERSE,0,0,0);
	VL_vmenuitem(&help,"Good Notepad Help - Page 1 - General",0,NULL);
	VL_vmenuitem(&help,"",0,NULL);
	VL_vmenuitem(&help,"PF1 moves to the next page.",0,NULL);
	VL_vmenuitem(&help,"PF2 clears the current page.",0,NULL);
	VL_vmenuitem(&help,"PF3 pastes into the current line.",0,NULL);
	VL_vmenuitem(&help,"PF4 copies the current line.",0,NULL);
	VL_vmenuitem(&help,"PF5 inserts a blank line.",0,NULL);
	VL_vmenuitem(&help,"PF6 deletes the current line.",0,NULL);
	VL_vmenuitem(&help,"HOME (FIND) selects page 0.",0,NULL);
	VL_vmenuitem(&help,"DELETE, INSERT and REMOVE are for editing.",0,NULL);
	VL_vmenuitem(&help,"Arrow keys move the cursor and the window.",0,NULL);
	VL_vmenuitem(&help,"",0,NULL);
	VL_vmenuitem(&help,"Depress any key to exit...",0,NULL);

	key = VL_vmenugo(&help);
	return(SUCCESS);
}

static int showpad()
{
	register int i, j;

	VL_vbuffering_start();

	i = row;

	vtext(emode, i++, col, " Good Notepad  ----- Page %d ----- ",page);
	vgrid(i++,col,rows-1,cols,0,0);

	for (j = 0; j < LENGTH; j++)
	{
		vtext(emode, i++, col+1, "%s", pad[page][j]);
	}

	vmove(row+2+line, col+1+column);
	VL_vmode(CLEAR);
	vcharset(DEFAULT);
	VL_vbuffering_end();
	return(SUCCESS);
}

static int move()
{
	unsigned char *vsss();

	vrss(save);
	save = vsss(row,col,rows,cols);
	showpad();
	return(SUCCESS);
}

static int nextcol()
{
	if (++column >= WIDTH)
	{
		column = 0;
		if (++line >= LENGTH) line = 0;
		vmove(row+2+line,col+1+column);
	}
	return(SUCCESS);
}
/*
**	History:
**	$Log: gnotepad.c,v $
**	Revision 1.17  2003/06/27 15:54:03  gsl
**	fix EDE API
**	
**	Revision 1.16  2003/06/23 15:28:04  gsl
**	VL_ global symbols
**	
**	Revision 1.15  2003/01/31 19:25:57  gsl
**	Fix copyright header
**	
**	Revision 1.14  2002/07/17 21:06:00  gsl
**	VL_ globals
**	
**	Revision 1.13  2002/07/16 13:40:23  gsl
**	VL_ globals
**	
**	Revision 1.12  2002/07/15 20:56:37  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.11  2002/07/15 20:16:06  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.10  1997/07/08 20:18:14  gsl
**	Change to use new video.h interface
**	
**	Revision 1.9  1996-10-11 18:15:55-04  gsl
**	drcs update
**
**
**
*/
