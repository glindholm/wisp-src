/*
******************************************************************************
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
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
******************************************************************************
*/

/************************************************************************/
/*	     VIDEO - Video Interactive Development Environment		*/
/*			    Copyright (c) 1987				*/
/*	An unpublished work by Greg L. Adams.  All rights reserved.	*/
/************************************************************************/

#include <stdio.h>
#include <math.h>
#include <errno.h>
#include <string.h>
#include "video.h"
#include "vlocal.h"
#include "vdata.h"
#include "vintdef.h"
#include "vmenu.h"
#include "vmodules.h"

static int overkey();
static int funcalc();

#define DISPLAY_SIZE 15
#define REG_SIZE     64

#define INIT_REG_DISPLAY "              0"

#define RIGHT 0
#define LEFT 1

#define LIMIT   30.0

#define NULL_OP  0
#define ADD      1
#define SUBTRACT 2
#define MULTIPLY 3
#define DIVIDE   4

#define PRIMARY	 1
#define	FUNCTION 0

static int row, col, rows, cols;
static char reg_display[REG_SIZE];
static int emode, cmode, last;
static int digits, period, xpend, ypend;
static int operation;
static int error;
static int mode;
static unsigned char *save;

static double decimal, base;
static double reg_x, reg_y;

static int showcalc();
static int position();
static int display();
static int show_reg_display();
static int shuffle();
static int newdisp();
static int do_operation();
static int standard_op();
static int clear();
static int arrow();
static int givehelp();
static int overflow();

int gcalc(void)
{
	int k;
	int active;

	reg_x = 0.0;
	reg_y = 0.0;
	base = 10.0;
	decimal = 0.0;
	
	row = 4;
	col = 24;
	rows = 13;
	cols = 19;
	VL_vdetpos(0, &row, &col, rows, cols);

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

	save = vsss(row,col,rows,cols);
	clear();
	showcalc(PRIMARY);

	active = TRUE;
	while(active)
	{
		if (error)
		{
			clear();
			position();
		}

		k = vgetm();

		if ((k >= '0') && (k <= '9'))
		{
			if (xpend) newdisp();

			if ((reg_display[DISPLAY_SIZE-2] == ' ') &&
			    (reg_display[DISPLAY_SIZE-1] == '0') &&
			    (k == '0'))
			{
				show_reg_display();
				last = k;
				position();
			}

			else if (digits++ < DISPLAY_SIZE)
			{
				if (period)
				{
					reg_x += ((double)(k-'0'))/decimal;
					decimal *= base;
				}
				else reg_x = (reg_x * base) + (double)(k - '0');
				shuffle(LEFT);
				reg_display[DISPLAY_SIZE-1] = (char) k;
				show_reg_display();
				last = k;
				position();
			}
			else
			{
				digits = DISPLAY_SIZE;
				vbell();
			}
		}

		else if (k == '.')
		{
			if (xpend) newdisp();

			if (!period && (digits++ < DISPLAY_SIZE))
			{
				period = TRUE;
				decimal = base;
				shuffle(LEFT);
				reg_display[DISPLAY_SIZE-1] = (char) k;
				show_reg_display();
				last = k;
				position();
			}
			else
			{
				digits = DISPLAY_SIZE;
				vbell();
			}
		}

		else if ((k == '+') || (k == ','))
		{
			standard_op(k);
			operation = ADD;
			position();
		}

		else if (k == '-')
		{
			standard_op(k);
			operation = SUBTRACT;
			position();
		}

		else if ((k == '*') || (k == fn4_key))
		{
			standard_op(k);
			operation = MULTIPLY;
			position();
		}

		else if ((k == '/') || (k == fn3_key))
		{
			standard_op(k);
			operation = DIVIDE;
			position();
		}

		else if ((k == '=') || (k == return_key))
		{
			do_operation();
			display();
			ypend = TRUE;
			xpend = TRUE;
			last = k;
			position();
		}

		else if ((k == 'C') || (k == 'c') || (k == fn2_key))
		{
			if (!mode) overkey(PRIMARY);
			clear();
			show_reg_display();
			position();
		}

		else if (arrow(k));

		else if ((k == 'f') || (k == 'F') || (k == fn1_key))
		{
			overkey(FUNCTION);
			last = k;
			position();
			active = funcalc();
		}

		else if ((k == 'H') || (k == 'h') || (k == help_key))
		{
			givehelp(mode);
			last = 'F';
			position();
		}

		else if ((k == fn16_key) || (k == 'X') || (k == 'x') || (k == 'E') || (k == 'e')) active = FALSE;

		else if (k == ' ');

		else vbell();
	}
	vrss(save);
	return(SUCCESS);
}

static int showcalc(m) int m;
{
	int i, j, k;

	mode = m;

	VL_vbuffering_start();

	vtext(emode,row,col,"  Good Calculator  ");
	vtext(emode,row+1,col," ");
	VL_vmode(cmode);  vprint("                 ");
	VL_vmode(emode);  vprint(" ");
	i = row+2;
		  vtext(emode,i++,col,"                   ");
	if (mode) vtext(emode,i++,col,"   F   C   /   *   ");
	else      vtext(emode,i++,col,"   F   C  pst cut  ");
		  vtext(emode,i++,col,"                   ");
	if (mode) vtext(emode,i++,col,"   7   8   9   -   ");
	else      vtext(emode,i++,col,"  sin cos tan chs  ");
		  vtext(emode,i++,col,"                   ");
	if (mode) vtext(emode,i++,col,"   4   5   6   +   ");
	else      vtext(emode,i++,col,"  asn acs atn inv  ");
		  vtext(emode,i++,col,"                   ");
	if (mode) vtext(emode,i++,col,"   1   2   3       ");
	else      vtext(emode,i++,col,"  exp ln  log      ");
	if (mode) vtext(emode,i++,col,"               =   ");
	else      vtext(emode,i++,col,"                   ");
	if (mode) vtext(emode,i++,col,"     0     .       ");
	else      vtext(emode,i++,col,"   EXIT   pi       ");
		  vtext(emode,i++,col,"                   ");

	for (j = col; j < col+cols; j = j + 4)
	{
		vmove(row+2,j+1);
		k = 11;
		if (j == col+4) k = 9;
		vline(VERTICAL,k);
	}

	i = row + 2;
	vmove(i++,col+1); vline(HORIZONTAL,17);
	i++;
	vmove(i++,col+1); vline(HORIZONTAL,17);
	i++;
	vmove(i++,col+1); vline(HORIZONTAL,17);
	i++;
	vmove(i++,col+1); vline(HORIZONTAL,17);
	i++;
	vmove(i++,col+1); vline(HORIZONTAL,13);
	i++;
	vmove(i++,col+1); vline(HORIZONTAL,17);

	VL_vmode(CLEAR);
	vcharset(DEFAULT);

	show_reg_display();
	position();

	VL_vbuffering_end();
	return(SUCCESS);
}

static int move()
{
	vrss(save);
	save = vsss(row,col,rows,cols);
	showcalc(mode);
	return(SUCCESS);
}

static int position()
{
	if      ((last == 'F') || (last == fn1_key)) vmove(row+3,col+3);
	else if  (last == 'f')			     vmove(row+3,col+3);
	else if ((last == 'C') || (last == fn2_key)) vmove(row+3,col+7);
	else if  (last == 'c')			     vmove(row+3,col+7);
	else if ((last == '/') || (last == fn3_key)) vmove(row+3,col+11);
	else if ((last == '*') || (last == fn4_key)) vmove(row+3,col+15);

	else if (last == '7') vmove(row+5,col+3);
	else if (last == '8') vmove(row+5,col+7);
	else if (last == '9') vmove(row+5,col+11);
	else if (last == '-') vmove(row+5,col+15);

	else if (last == '4') vmove(row+7,col+3);
	else if (last == '5') vmove(row+7,col+7);
	else if (last == '6') vmove(row+7,col+11);
	else if ((last == ',') || (last == '+')) vmove(row+7,col+15);

	else if (last == '1') vmove(row+9,col+3);
	else if (last == '2') vmove(row+9,col+7);
	else if (last == '3') vmove(row+9,col+11);

	else if ((last == return_key) || (last == '=')) vmove(row+10,col+15);
	else if (last == '0') vmove(row+11,col+5);
	else if (last == '.') vmove(row+11,col+11);

	return(SUCCESS);
}

static int display()
{
	memset(reg_display,(char)0,sizeof(reg_display));
	sprintf(reg_display,"%15.15g",reg_x);

	if (strstr(reg_display,"e") != NULL)
	{
		if (reg_x >= 0.0) sprintf(reg_display,"%15.9e",reg_x);
		else sprintf(reg_display,"%15.8e",reg_x);
	}
	else if (strstr(reg_display,".") != NULL)
	{
		while (reg_display[DISPLAY_SIZE-1] == '0') shuffle(RIGHT);
		if ((reg_display[DISPLAY_SIZE-1] == '.') && !period) shuffle(RIGHT);
	}
	reg_display[DISPLAY_SIZE] = CHAR_NULL;
	show_reg_display();
	return 0;
}

static int show_reg_display()
{
	int i;
	double	dtemp;

	dtemp = reg_x;

	if (dtemp >= 1.0)
	{
		/*
		**	Clear leading zeros
		*/
		i = 0;
		while(i < DISPLAY_SIZE && reg_display[i] == ' ')
		{
			i++;
			if (reg_display[i] == '0')
			{
				reg_display[i] = ' ';
			}
		}
	}
	vtext(cmode,row+1,col+2,reg_display);
	VL_vmode(CLEAR);
	return(SUCCESS);
}

static int shuffle(direction) int direction;
{
	int i;

	if (direction == LEFT)
	{
		for (i = 0; i < DISPLAY_SIZE-1; i++) reg_display[i] = reg_display[i+1];
	}
	else
	{
		for (i = DISPLAY_SIZE-1; i > 0; i--) reg_display[i] = reg_display[i-1];
		reg_display[0] = ' ';
	}
	reg_display[DISPLAY_SIZE] = (char)0;
	return(SUCCESS);
}

static int newdisp()
{
	reg_x = 0.0;
	strcpy(reg_display,INIT_REG_DISPLAY);
	xpend = FALSE;
	period = FALSE;
	decimal = 0.0;
	digits = 0;
	return(SUCCESS);
}

static int do_operation()
{
	double lx, ly, ax, ay;

	if (operation != NULL_OP)
	{
		if (reg_x >= 0) lx = log10(reg_x);
		else lx = log10(-reg_x);
		if (reg_y >= 0) ly = log10(reg_y);
		else ly = log10(-reg_y);
		if (lx >= 0.0) ax = lx;
		else ax = -lx;
		if (ly >= 0.0) ay = ly;
		else ay = -ly;

		switch (operation)
		{
			case ADD:
			{
				if ((ax > LIMIT) || (ay > LIMIT)) overflow();
				else reg_x += reg_y;
				break;
			}
			case SUBTRACT:
			{
				if ((ax > LIMIT) || (ay > LIMIT)) overflow();
				else reg_x = reg_y - reg_x;
				break;
			}
			case MULTIPLY:
			{
				if ((lx + ly) > LIMIT) overflow();
				else if ((lx + ly) < -LIMIT) overflow();
				else reg_x *= reg_y;
				break;
			}
			case DIVIDE:
			{
				if (reg_x == 0.0) overflow();
				else if ((ly - lx) >  LIMIT) overflow();
				else if ((ly - lx) < -LIMIT) overflow();
				else reg_x = reg_y / reg_x;
				break;
			}
		}
	}
	return(SUCCESS);
}

static int standard_op(k) int k;
{
	if (ypend)
	{
		ypend = FALSE;
		reg_y = reg_x;
	}
	if (!xpend) do_operation();
	display();
	reg_y = reg_x;
	xpend = TRUE;
	last = k;
	return 0;
}

static int clear()
{
	reg_y = 0.0;
	reg_x = 0.0;
	strcpy(reg_display,INIT_REG_DISPLAY);
	xpend = FALSE;
	ypend = FALSE;
	operation = NULL_OP;
	digits = 0;
	decimal = 0.0;
	period = FALSE;
	last = 'C';
	error = FALSE;
	return(SUCCESS);
}

static int overflow()
{
	char c;

	vbell();
	vtext(cmode,row+1,col+2,"  * OVERFLOW * ");
	clear();
	position();
	c = vgetc();
	VL_vpushc(c);
	error = TRUE;
	return(SUCCESS);
}

static int overkey(int m)
{
	mode = m;

	VL_vbuffering_start();
	if (mode == PRIMARY)
	{
		vtext(emode,row+3,col+10," / ");
		vtext(emode,row+3,col+14," * ");

		vtext(emode,row+5,col+2," 7 ");
		vtext(emode,row+5,col+6," 8 ");
		vtext(emode,row+5,col+10," 9 ");
		vtext(emode,row+5,col+14," - ");

		vtext(emode,row+7,col+2," 4 ");
		vtext(emode,row+7,col+6," 5 ");
		vtext(emode,row+7,col+10," 6 ");
		vtext(emode,row+7,col+14," + ");

		vtext(emode,row+9,col+2," 1 ");
		vtext(emode,row+9,col+6," 2 ");
		vtext(emode,row+9,col+10," 3 ");

		vtext(emode,row+10,col+14," = ");

		vtext(emode,row+11,col+3,"  0  ");
		vtext(emode,row+11,col+10," . ");
	}
	else
	{
		vtext(emode,row+3,col+10,"pst");
		vtext(emode,row+3,col+14,"cut");

		vtext(emode,row+5,col+2,"sin");
		vtext(emode,row+5,col+6,"cos");
		vtext(emode,row+5,col+10,"tan");
		vtext(emode,row+5,col+14,"chs");

		vtext(emode,row+7,col+2,"asn");
		vtext(emode,row+7,col+6,"acs");
		vtext(emode,row+7,col+10,"atn");
		vtext(emode,row+7,col+14,"inv");

		vtext(emode,row+9,col+2,"exp");
		vtext(emode,row+9,col+6,"ln ");
		vtext(emode,row+9,col+10,"log");

		vtext(emode,row+10,col+14,"sqt");

		vtext(emode,row+11,col+3," EXIT ");
		vtext(emode,row+11,col+10,"pi ");
	}
	VL_vbuffering_end();
	position();
	return(SUCCESS);
}

static int arrow(k) int k;
{
	int i;

	if (k == up_arrow_key)
	{
		i = row;
		row = row - 2;
		if (row < 1) row = 1;
		if (row == i) vbell();
		else move();
		return(SUCCESS);
	}
	else if (k == down_arrow_key)
	{
		i = row;
		row = row + 2;
		if (row > MAX_LINES_PER_SCREEN-rows)
		{
			row = MAX_LINES_PER_SCREEN-rows;
		}
		if (row == i) vbell();
		else move();
		return(SUCCESS);
	}
	else if (k == left_arrow_key)
	{
		i = col;
		col = col - 4;
		if (col < 0) col = 0;
		if (col == i) vbell();
		else move();
		return(SUCCESS);
	}
	else if (k == right_arrow_key)
	{
		i = col;
		col = col + 4;
		if (col > VL_vscr_wid-cols) col = VL_vscr_wid-cols;
		if (col == i) vbell();
		else move();
		return(SUCCESS);
	}
	return(FAILURE);
}

static int funcalc()
{
	int i, k;

	last = 'F';
	errno = 0;

funa:	k = vgetm();

	if ((k == '0') || (k == fn16_key)) return(FAILURE);
	else if (k == ' ') goto funa;
	else if (k == '-') reg_x = -reg_x;
	else if ((k == '+') || (k == ','))
	{
		if (reg_x == 0.0) errno = 1;
		else reg_x = 1 / reg_x;
	}
	else if (k == '7') reg_x = sin(reg_x);
	else if (k == '8') reg_x = cos(reg_x);
	else if (k == '9') reg_x = tan(reg_x);
	else if (k == '4') reg_x = asin(reg_x);
	else if (k == '5') reg_x = acos(reg_x);
	else if (k == '6') reg_x = atan(reg_x);
	else if (k == '1') reg_x = exp(reg_x);
	else if (k == '2') reg_x = log(reg_x);
	else if (k == '3') reg_x = log10(reg_x);
	else if (k == '.') reg_x = 2.0 * acos(0.0);
	else if ((k == '=') || (k == return_key)) reg_x = sqrt(reg_x);

	else if ((k == '*') || (k == fn4_key))
	{
		i = 0;
		while (i < DISPLAY_SIZE && reg_display[i] == ' ') i++;
		vcut(&reg_display[i]);
		goto xit;
	}

	else if ((k == '/') || (k == fn3_key))
	{
		if (VL_vpaste(0))
		{
			reg_x = 0.0;
			strcpy(reg_display,INIT_REG_DISPLAY);
			xpend = FALSE;
			digits = 0;
			decimal = 0.0;
			period = FALSE;
			error = FALSE;
		}
		else vbell();
		goto xit;
	}

	else if ((k == 'C') || (k == 'c') || (k == fn2_key))
	{
		clear();
		show_reg_display();
		overkey(PRIMARY);
		last = k;
		position();
		return(SUCCESS);
	}

	else if (arrow(k)) goto funa;

	else if (k == help_key)
	{
		givehelp(mode);
		last = 'F';
		position();
		goto funa;
	}

	else if ((k == 'f') || (k == 'F') || (k == fn2_key)) goto xit;

	else { vbell(); goto xit; }

	xpend = TRUE;
	if (errno) overflow();
	else display();
xit:	overkey(PRIMARY);
	last = 'F';
	position();
	return(SUCCESS);
}

static int givehelp(m) int m;
{
	struct video_menu help;
	int4 key;


	VL_vmenuinit(&help,DISPLAY_ONLY_MENU,VMODE_REVERSE,0,0,0);
	VL_vmenuitem(&help,"Calculator Help - Page 1 - General",0,NULL);
	VL_vmenuitem(&help,"",0,NULL);
	VL_vmenuitem(&help,"This calculator operates like most hand-held",0,NULL);
	VL_vmenuitem(&help,"models. The screen layout matches the keypad",0,NULL);
	VL_vmenuitem(&help,"of most terminal keyboards although it might",0,NULL);
	VL_vmenuitem(&help,"not be perfectly one to one.  It should work",0,NULL);
	VL_vmenuitem(&help,"with any key layout. In general numeric keys",0,NULL);
	VL_vmenuitem(&help,"(0 1 etc.) are used for digits,  + - * / are",0,NULL);
	VL_vmenuitem(&help,"used for add, subtract etc. and = for equals.",0,NULL);
	VL_vmenuitem(&help,"",0,NULL);
	VL_vmenuitem(&help,"For more information, depress the help key.",0,NULL);
	VL_vmenuitem(&help,"Any other key will exit help..",0,NULL);

	key = VL_vmenugo(&help);
	if (key != help_key) return(SUCCESS);

	VL_vmenuinit(&help,DISPLAY_ONLY_MENU,VMODE_REVERSE,0,0,0);
	VL_vmenuitem(&help,"Good Calculator Help - Page 2 - Keys",0,NULL);
	VL_vmenuitem(&help,"",0,NULL);
	VL_vmenuitem(&help,"0 1 2 etc. are used for digits.",0,NULL);
	VL_vmenuitem(&help,"+ - * / are used for add, subtract, etc.",0,NULL);
	VL_vmenuitem(&help,"PF3 and PF4 double for * and /",0,NULL);
	VL_vmenuitem(&help,"= and the return key are used for equals.",0,NULL);
	VL_vmenuitem(&help,"PF1 or the F key selects function mode.",0,NULL);
	VL_vmenuitem(&help,"PF2 or the C key clear the display.",0,NULL);
	VL_vmenuitem(&help,"PF16 is used to exit the calculator.",0,NULL);
	VL_vmenuitem(&help,"The arrow keys move the calculator about.",0,NULL);
	VL_vmenuitem(&help,"",0,NULL);
	VL_vmenuitem(&help,"For more information, depress the help key.",0,NULL);
	VL_vmenuitem(&help,"Any other key will exit help.",0,NULL);

	key = VL_vmenugo(&help);
	if (key != help_key) return(SUCCESS);
	VL_vmenuinit(&help,DISPLAY_ONLY_MENU,VMODE_REVERSE,0,0,0);
	VL_vmenuitem(&help,"Good Calculator Help - Page 3 - Functions",0,NULL);
	VL_vmenuitem(&help,"",0,NULL);
	VL_vmenuitem(&help,"PF1 or the F key selects function mode.",0,NULL);
	VL_vmenuitem(&help,"When selected, the screen will be changed",0,NULL);
	VL_vmenuitem(&help,"to display what the functions are.  Thses",0,NULL);
	VL_vmenuitem(&help,"include sin, cos etc. and are obtained by",0,NULL);
	VL_vmenuitem(&help,"depressing the corresponding key.",0,NULL);
	VL_vmenuitem(&help,"For example, sin corresponds to 7.",0,NULL);
	VL_vmenuitem(&help,"",0,NULL);
	VL_vmenuitem(&help,"For more information, depress the help key.",0,NULL);
	VL_vmenuitem(&help,"Any other key will exit help.",0,NULL);

	key = VL_vmenugo(&help);
	if (key != help_key) return(SUCCESS);

	VL_vmenuinit(&help,DISPLAY_ONLY_MENU,VMODE_REVERSE,0,0,0);
	VL_vmenuitem(&help,"Good Calculator Help - Page 4 - Cut & Paste",0,NULL);
	VL_vmenuitem(&help,"",0,NULL);
	VL_vmenuitem(&help,"In function mode, the calculator value can",0,NULL);
	VL_vmenuitem(&help,"be cut out for use in another program,  or",0,NULL);
	VL_vmenuitem(&help,"for later use by the calculator.",0,NULL);
	VL_vmenuitem(&help,"Similarily, a previously cut value can be",0,NULL);
	VL_vmenuitem(&help,"imported from another program.",0,NULL);
	VL_vmenuitem(&help,"",0,NULL);
	VL_vmenuitem(&help,"This is the last page of help.",0,NULL);
	VL_vmenuitem(&help,"Depress any key to exit...",0,NULL);

	key = VL_vmenugo(&help);
	return(SUCCESS);
}

/*
**	History:
**	$Log: gcalc.c,v $
**	Revision 1.21  2010/02/10 14:50:17  gsl
**	fix warnings
**	
**	Revision 1.20  2010/02/10 03:52:33  gsl
**	fix warnings for redefined functions
**	
**	Revision 1.19  2003/06/27 15:54:03  gsl
**	fix EDE API
**	
**	Revision 1.18  2003/06/23 15:28:04  gsl
**	VL_ global symbols
**	
**	Revision 1.17  2003/01/31 20:18:47  gsl
**	Fix -Wall warnings
**	
**	Revision 1.16  2003/01/31 19:25:57  gsl
**	Fix copyright header
**	
**	Revision 1.15  2002/07/17 21:05:59  gsl
**	VL_ globals
**	
**	Revision 1.14  2002/07/16 13:40:23  gsl
**	VL_ globals
**	
**	Revision 1.13  2002/07/15 20:16:05  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.12  2002/07/15 17:52:53  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.11  1997/07/08 20:15:37  gsl
**	Change to use new video.h defines
**	
**	Revision 1.10  1997-01-08 16:36:42-05  gsl
**	Change strpos() calls to strstr()
**
**	Revision 1.9  1996-10-11 15:15:54-07  gsl
**	drcs update
**
**
**
*/
