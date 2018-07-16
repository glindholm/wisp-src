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
			/*			    Copyright (c) 1987				*/
			/*	An unpublished work by Greg L. Adams.  All rights reserved.	*/
			/************************************************************************/

			/************************************************************************/
			/*	LAZER_PUZZLE - A fun and semi-exciting little Lazer ditty by:	*/
			/*		       Suzette Boron  August 1989.			*/
			/************************************************************************/

#ifdef unix
#include <sys/types.h>
#endif
#include <time.h>
#include "video.h"
#include "vlocal.h"
#include "vdata.h"
#include "vintdef.h"
#include "vmenu.h"
#include "vmodules.h"

static unsigned char *save;								/* Save location.			*/
static int move_hole = FALSE;								/* Move the button, not the hole.	*/
static int board[4][4];									/* Define the puzzle board as global.	*/
static int top,left;									/* Define starting position for grid.	*/
static int cblrow = 3;									/* Current position ob blank button.	*/
static int cblcol = 3;									/* Current position of blank button.	*/
static int total_moves;									/* Define for cumulation of moves.	*/
static int rseed;									/* Value used for randomising board.	*/
static int emode, cmode, umode;								/* Display mode flags.			*/
static int game_active, game_over;							/* Define tracking variables.		*/
static int row = 10;									/* Define puzzle default location.	*/
static int col = 24;
static int rows = 11;
static int cols = 17;

static int display_grid();
static int scramble();
static int solved();
static int mb();
static int givehelp();
static int random();
static int button();
static int move_button();
static int move();

/*						Subroutine Entry Point								*/

int gpuzzle(void)
{
	unsigned char *vsss();
	register int i, j, k;
	int active;

	VL_vdetpos(0, &row, &col, rows, cols);
	save = vsss(row,col,rows,cols);
	VL_vbuffering_start();
	if (vscr_atr & LIGHT) 
	{ 
		emode = VMODE_BOLD; 
		cmode = VMODE_REVERSE|VMODE_BOLD; 
		umode = VMODE_CLEAR; 
	}
	else 
	{ 
		emode = VMODE_REVERSE; 
		cmode = VMODE_CLEAR|VMODE_BOLD; 
		umode = VMODE_REVERSE|VMODE_BOLD; 
	}

	active = TRUE;
	while(active)
	{
		for (i = 0; i < 4; i++)							/* Initialize the board in order.	*/
		{
			for (j = 0; j < 4; j++) board[i][j] = (i*4)+j;
		}
		cblrow = 3;								/* Indicate where the hole is.		*/
		cblcol = 3;
		scramble();								/* Mix up the numbers on board.		*/
		total_moves = 0;							/* Initialize number of moves to zero.	*/
		display_grid();								/* Display the board grid.		*/
		vset_cursor_off();							/* Do not display cursor.		*/

		game_active = TRUE;							/* Initialize local parameters.		*/
		game_over = FALSE;

		while (game_active && !game_over)
		{
			vmove(23,0);							/* Move the cursor out of the way.	*/
			k = vgetm();							/* Get a meta-character.		*/

			if      (k == '2') k = down_arrow_key;				/* Convert keypad to arrows.		*/
			else if (k == '4') k = left_arrow_key;
			else if (k == '6') k = right_arrow_key;
			else if (k == '8') k = up_arrow_key;

			if (move_hole)							/* Move the hole?			*/
			{
				if (k == up_arrow_key) k = down_arrow_key;		/* Yes, then setup to move the button.	*/
				else if (k == down_arrow_key) k = up_arrow_key;
				else if (k == right_arrow_key) k = left_arrow_key;
				else if (k == left_arrow_key) k = right_arrow_key;
			}

			     if (k == up_arrow_key   ) mb(k, -1,  0);			/* Processing for UP arrow key.		*/
			else if (k == down_arrow_key ) mb(k,  1,  0);			/* Processing for DOWN arrow key.	*/
			else if (k == left_arrow_key ) mb(k,  0, -1);			/* Processing for LEFT arrow key.	*/
			else if (k == right_arrow_key) mb(k,  0,  1);			/* Processing for RIGHT arrow key.	*/
			else if (k == help_key) givehelp();				/* Help requested?			*/
			else if ((k == 'Q') || (k == 'q') || (k == fn16_key))		/* Time to quit?			*/
			{
				active = FALSE;
				game_active = FALSE;
			}
			else if (k == fn2_key) game_active = FALSE;			/* Rescramble?				*/
			if (game_active && solved())					/* Check if the game was just won? 	*/
			{
				for (i = 0; i < 10; i++) vbell();			/* Let the bells ring.			*/
				vtext(emode|VMODE_BLINK, row+10, col, "   Winner: %3d   ", total_moves);
				game_active = FALSE;					/* Now this game is done.		*/
				vmove(23,0);						/* Hide the cursor.			*/
				k = vgetm();						/* Wait for a character.		*/
				if (k == fn16_key) active = FALSE;			/* Totally done?			*/
			}
		}
	}
	vrss(save);
	VL_vbuffering_end();
	return(SUCCESS);
}

static int display_grid()
{
	int i, j;
	int cell;									/* Define a temp cell position.		*/

	vset_cursor_off();								/* Do not display cursor.		*/
	top = row + 1;									/* Assign starting row of grid.		*/
	left = col;									/* Assign starting column of grid.	*/

	VL_vmode(emode);									/* Turn on proper mode.			*/

	vtext(emode, row, col, "   Good Puzzle   ");
	vgrid(top,left,9,17,2,4);							/* Draw the board grid.			*/

	for (i = 0; i < 4; i++)								/* Display buttons on grid.		*/
	{
		for (j = 0; j < 4; j++)
		{
			cell = board[i][j];
			button((i*2) + top + 1, left + 1 + (4*j), cell);		/* Call routine to display current pos.	*/
		}
	}
	vtext(emode, row+10, col, "   Moves:  %3d   ", total_moves);
	vmove(23,0);
	return 0;	
}

static int scramble()									/* This routine will move backwards	*/
{											/*  from the solved config. so that all	*/
	int i;										/*  random starting configurations	*/
	time_t x;

	time(&x);									/* Use time as random seed.		*/
	rseed = (int) x;

	for (i = 0; i < 100; i++)
	{
		x = random(3);								/* Start with current blank position.	*/
		switch(x)								/* Random function returns 0-3.		*/
		{
			case 0:
			{
				if (cblcol) move_button(right_arrow_key);
				else 
				{
					if (cblrow) move_button(down_arrow_key);
					else move_button(up_arrow_key);
				}
				break;
			}
			case 1:
			{
				if (cblrow) move_button(down_arrow_key);
				else
				{
					if (cblcol == 3) move_button(right_arrow_key);
					else move_button(left_arrow_key);
				}
				break;
			}
			case 2:
			{
				if (cblcol == 3) move_button(right_arrow_key);
				else move_button(left_arrow_key);
				break;
			}
			case 3:
			{
				if (cblrow == 3) move_button(down_arrow_key);
				else move_button(up_arrow_key);
				break;
			}
		}
	}		
	return 0;
}

static int random(x)									/* Random number generator of		*/
int x;											/*  range 0-3.				*/
{
	long int overflow = 2147483647;
	long int trimmer = 211327; 
	x++;
	rseed = rseed * 9821;
	if (rseed < 0) rseed = (int) ((long) rseed + overflow);
	rseed = (int) ((long) rseed + trimmer);
	if (rseed < 0) rseed = (int) ((long) rseed + overflow);
	x = (int) ((long) rseed / ( overflow / (long) x ));

	return(x);
}

static int button(x,y,n) int x,y,n;
{
	char *table = "ABCDEFGHIJKLMNOP";						/* Letters to be displayed.		*/

	if (n < 15) vtext(umode, x, y," %c ",table[n]);					/* Print a letter.			*/
	else vtext(cmode, x, y, "   ");
	vmove(23,0);
	return(SUCCESS);

}

static int solved()									/* Routine to test if the puzzle has	*/
{											/*  been solved.			*/
	int i, j;									/* Working registers.			*/
	int ret;									/* Working return variable.		*/

	ret = TRUE;									/* Game not over.			*/
	for (i = 0; i < 4; i++)								/* For rows 1-4.			*/
	{
		for (j = 0; j < 4; j++)							/* For colums 1-4.			*/
		{
			if (i == 3 && j == 3 && board[i][j] == 16);			/* If blank is in correct position AND	*/
			 	 							/*  all other numbers are in order.	*/
			else if ( board[i][j] != (i*4) + j ) ret = FALSE;		/* Test valuse of positon.		*/
		}
	}

	return(ret);
}

static int move_button(inp) int inp;
{												/* Rows & Columns start with 0,	*/
	int moved;										/* Flag if we moved.		*/

	moved = FALSE;										/* Assume we didn't move.	*/
	if (inp == right_arrow_key)								/*  so board has 0,1,2 and 3.	*/
	{
		if (cblcol)									/* Current blank col is not 0.	*/
		{
			board[cblrow][cblcol] = board[cblrow][cblcol-1];			/* Copy letter into blank cell.	*/
			cblcol--;								/* Adjust new blank col. value.	*/
			moved = TRUE;								/* We moved.			*/
		}
	}
	else if (inp == left_arrow_key)
	{
		if (cblcol < 3)									/* Current blank col is < 3.	*/
		{
			board[cblrow][cblcol] = board[cblrow][cblcol+1];			/* Copy letter into blank cell.	*/
			cblcol++;								/* Adjust new blank col value.	*/
			moved = TRUE;								/* We moved.			*/
		}
	}
	else if (inp == down_arrow_key)
	{
		if (cblrow)									/* Current blank row is not 0.	*/
		{
			board[cblrow][cblcol] = board[cblrow-1][cblcol];			/* Copy letter into blank cell.	*/
			cblrow--;								/* Adjust new blank row value.	*/
			moved = TRUE;								/* We moved.			*/
		}
	}
	else if (inp == up_arrow_key)
	{
		if (cblrow < 3)									/* Current blank row is < 3.	*/
		{
			board[cblrow][cblcol] = board[cblrow+1][cblcol];			/* Copy letter into blank cell.	*/
			cblrow++;								/* Adjust new blank row value.	*/
			moved = TRUE;								/* We moved.			*/
		}
	}
	board[cblrow][cblcol] = 16;								/* Copy new blank cell.		*/
	return(moved);										/* Return if we moved.		*/
}

static int mb(k,i,j) int k, i, j;
{
	if (move_button(k))
	{
		button( top + 1 + ((cblrow+i) * 2), left + 1 + (4 * (cblcol+j)), board[cblrow+i][cblcol+j]);
		button( top + 1 +  (cblrow*2),      left + 1 + (4 *  cblcol),    board[cblrow][cblcol]);
		total_moves++;
		vtext(emode, row+10, col, "   Moves:  %3d   ", total_moves);
		vmove(23,0);
	}
	else
	{
		if (k == right_arrow_key) move(0, 8);
		else if (k == left_arrow_key) move(0, -8);
		else if (k == up_arrow_key) move(-4, 0);
		else if (k == down_arrow_key) move(4, 0);
	}
	return(SUCCESS);
}

static int move(nrows, ncols) int nrows, ncols;
{
	unsigned char *vsss();
	int orow, ocol;

	orow = row;
	ocol = col;

	row = row + nrows;
	col = col + ncols;

	while (row < 1) row++;
	while (col < 0) col++;
	while (row+rows > MAX_LINES_PER_SCREEN-1) row--;
	while (col+cols > VL_vscr_wid-1) col--;

	if ((orow == row) && (ocol == col)) vbell();
	else
	{
		vrss(save);
		save = vsss(row,col,rows,cols);
		display_grid();
	}

	return(SUCCESS);
}

static int givehelp(void)
{
	struct video_menu help;
	int4 key;

	VL_vmenuinit(&help,DISPLAY_ONLY_MENU,VMODE_REVERSE,0,0,0);
	VL_vmenuitem(&help,"Good Puzzle Help - Page 1 - General", 0, NULL);
	VL_vmenuitem(&help,"", 0, NULL);
	VL_vmenuitem(&help,"PF2 will rescramble the puzzle.", 0, NULL);
	VL_vmenuitem(&help,"Arrow keys move the tile into the hole.", 0, NULL);
	VL_vmenuitem(&help,"If a tile cannot be moved, then the whole", 0, NULL);
	VL_vmenuitem(&help,"   puzzle is moved on the screen.", 0, NULL);
	VL_vmenuitem(&help,"",0,NULL);
	VL_vmenuitem(&help,"Good luck and have fun!", 0, NULL);
	VL_vmenuitem(&help,"Depress any key to exit...", 0, NULL);

	key = VL_vmenugo(&help);
	vset_cursor_off();
	return(SUCCESS);
}
/*
**	History:
**	$Log: gpuzzle.c,v $
**	Revision 1.16  2003/06/27 15:54:03  gsl
**	fix EDE API
**	
**	Revision 1.15  2003/06/23 15:28:04  gsl
**	VL_ global symbols
**	
**	Revision 1.14  2003/01/31 19:25:57  gsl
**	Fix copyright header
**	
**	Revision 1.13  2002/07/17 21:06:00  gsl
**	VL_ globals
**	
**	Revision 1.12  2002/07/16 13:40:23  gsl
**	VL_ globals
**	
**	Revision 1.11  2002/07/15 20:16:06  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.10  1997/07/08 20:18:53  gsl
**	change to use new video.h interface
**	
**	Revision 1.9  1996-10-11 18:15:56-04  gsl
**	drcs update
**
**
**
*/
