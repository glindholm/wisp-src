/************************************************************************/
/*	     VIDEO - Video Interactive Development Environment		*/
/*			    Copyright (c) 1991				*/
/*	An unpublished work by Greg L. Adams.  All rights reserved.	*/
/************************************************************************/

#include <stdio.h>
#include <time.h>
#include "video.h"
#include "vlocal.h"
#include "vdata.h"
#include "vintdef.h"
#include "vmenu.h"

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

int gnotepad()
{
	register int i, j, k;
	unsigned char *vsss();
	int active;
	unsigned char c;
	FILE *pf, *vopenf();

	vbuffering(LOGICAL);
	row = 4;
	col = 20;
	rows = LENGTH+3;
	cols = WIDTH+2;
	vdetpos(0, &row, &col, rows, cols);
	save = vsss(row,col,rows,cols);
	if (vscr_atr & LIGHT) { emode = BOLD;    cmode = REVERSE|BOLD; }
	else                  { emode = REVERSE; cmode = CLEAR|BOLD;   }

	for (i = 0; i < PAGES; i++)
	{
		for (j = 0; j < LENGTH; j++)
		{
			for (k = 0; k < WIDTH; k++) pad[i][j][k] = ' ';
			pad[i][j][WIDTH] = CHAR_NULL;
		}
	}

	if ((pf = vopenf("pad","r")) == NULL) newpad = TRUE;
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
				if (col > vscr_wid-cols) col = vscr_wid-cols;
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
			vmode(emode);
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
				vmode(emode);
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
				vmode(emode);
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

		else if (k == fn3_key) vpaste(0);

		else if (k == fn4_key) vcut(pad[page][line]);

		else vbell();
	}

	vrss(save);
	vbuffering(AUTOMATIC);

	if (newpad) pf = vopenf("pad","w");
	else pf = vopenf("pad","r+");
	for (i = 0; i < PAGES; i++)
	{
		for (j = 0; j < LENGTH; j++) fprintf(pf, "%s\n", pad[i][j]);
	}
	fclose(pf);

	return(SUCCESS);
}

static int givehelp(m) int m;
{
	struct video_menu help;
	register int key;

	vmenuinit(&help,DISPLAY_ONLY_MENU,REVERSE,0,0,0);
	vmenuitem(&help,"Good Notepad Help - Page 1 - General",0,NULL);
	vmenuitem(&help,"",0,NULL);
	vmenuitem(&help,"PF1 moves to the next page.",0,NULL);
	vmenuitem(&help,"PF2 clears the current page.",0,NULL);
	vmenuitem(&help,"PF3 pastes into the current line.",0,NULL);
	vmenuitem(&help,"PF4 copies the current line.",0,NULL);
	vmenuitem(&help,"PF5 inserts a blank line.",0,NULL);
	vmenuitem(&help,"PF6 deletes the current line.",0,NULL);
	vmenuitem(&help,"HOME (FIND) selects page 0.",0,NULL);
	vmenuitem(&help,"DELETE, INSERT and REMOVE are for editing.",0,NULL);
	vmenuitem(&help,"Arrow keys move the cursor and the window.",0,NULL);
	vmenuitem(&help,"",0,NULL);
	vmenuitem(&help,"Depress any key to exit...",0,NULL);

	key = vmenugo(&help);
	return(SUCCESS);
}

static int showpad()
{
	register int i, j, k;

	vbuffering(LOGICAL);

	i = row;

	vtext(emode, i++, col, " Good Notepad  ----- Page %d ----- ",page);
	vgrid(i++,col,rows-1,cols,0,0);

	for (j = 0; j < LENGTH; j++)
	{
		vtext(emode, i++, col+1, "%s", pad[page][j]);
	}

	vmove(row+2+line, col+1+column);
	vmode(CLEAR);
	vcharset(DEFAULT);
	vbuffering(AUTOMATIC);
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
