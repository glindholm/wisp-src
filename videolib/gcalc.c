/************************************************************************/
/*	     VIDEO - Video Interactive Development Environment		*/
/*			    Copyright (c) 1987				*/
/*	An unpublished work by Greg L. Adams.  All rights reserved.	*/
/************************************************************************/

#include <stdio.h>
#include <math.h>
#include <errno.h>
#include "video.h"
#include "vlocal.h"
#include "vdata.h"
#include "vmenu.h"

#define DISPLAY_SIZE 15
#define REG_SIZE     64

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
static char xreg[REG_SIZE];
static int emode, cmode, last;
static int digits, period, xpend, ypend;
static double decimal;
static double x, y;
static int operation;
static int error;
static int mode;
static unsigned char *save;

int gcalc()
{
	register int i, j, k;
	int active;
	unsigned char *vsss();
	char c;

	row = 4;
	col = 24;
	rows = 13;
	cols = 19;
	vdetpos(0, &row, &col, rows, cols);

	if (vscr_atr & LIGHT)
	{
		emode = BOLD;
		cmode = REVERSE|BOLD;
	}
	else
	{
		emode = REVERSE;
		cmode = CLEAR|BOLD;
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

			if ((xreg[DISPLAY_SIZE-2] == ' ') &&
			    (xreg[DISPLAY_SIZE-1] == '0') &&
			    (k == '0'))
			{
				show();
				last = k;
				position();
			}

			else if (digits++ < DISPLAY_SIZE)
			{
				if (period)
				{
					x = x + ((double)(k-'0'))/decimal;
					decimal = decimal * 10.0;
				}
				else x = (x * 10.) + (double)(k - '0');
				shuffle(LEFT);
				xreg[DISPLAY_SIZE-1] = (char) k;
				show();
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
				decimal = 10.0;
				shuffle(LEFT);
				xreg[DISPLAY_SIZE-1] = (char) k;
				show();
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
			show();
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

		else if (k == help_key)
		{
			givehelp(mode);
			last = 'F';
			position();
		}

		else if (k == fn16_key) active = FALSE;

		else if (k == ' ');

		else vbell();
	}
	vrss(save);
	return(SUCCESS);
}

static int showcalc(m) int m;
{
	register int i, j, k;

	mode = m;

	vbuffering(LOGICAL);

	vtext(emode,row,col,"  Good Calculator  ");
	vtext(emode,row+1,col," ");
	vmode(cmode);  vprint("                 ");
	vmode(emode);  vprint(" ");
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

	vmode(CLEAR);
	vcharset(DEFAULT);

	show();
	position();

	vbuffering(AUTOMATIC);
	return(SUCCESS);
}

static int move()
{
	unsigned char *vsss();

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
	register int i;

	for (i = 0; i < REG_SIZE; i++) xreg[i] = CHAR_NULL;
	sprintf(xreg,"%15.15g",x);

	if (strpos(xreg,"e") >= 0)
	{
		if (x >= 0.0) sprintf(xreg,"%15.9e",x);
		else sprintf(xreg,"%15.8e",x);
	}
	else if (strpos(xreg,".") >= 0)
	{
		while (xreg[DISPLAY_SIZE-1] == '0') shuffle(RIGHT);
		if ((xreg[DISPLAY_SIZE-1] == '.') && !period) shuffle(RIGHT);
	}
	xreg[DISPLAY_SIZE] = CHAR_NULL;
	show();
}

show()
{
	register int i;
/*	vmove(21,0); vprint("%15.15f",x); verase(TO_EOL);
	vmove(22,0); vprint("%15.15e",x); verase(TO_EOL);
	vmove(23,0); vprint("%15.15g",x); verase(TO_EOL);
*/
	if (x >= 1.0)
	{
again:		i = 0;
		while(xreg[i++] == ' ')
		if (xreg[i] == '0')
		{
			xreg[i] = ' ';
			goto again;
		}
	}
	vtext(cmode,row+1,col+2,xreg);
	vmode(CLEAR);
	return(SUCCESS);
}

static int shuffle(direction) int direction;
{
	register int i;

	if (direction == LEFT)
	{
		for (i = 0; i < DISPLAY_SIZE-1; i++) xreg[i] = xreg[i+1];
		xreg[DISPLAY_SIZE] = CHAR_NULL;
	}
	else
	{
		for (i = DISPLAY_SIZE-1; i > 0; i--) xreg[i] = xreg[i-1];
		xreg[0] = ' ';
	}
	return(SUCCESS);
}

static int newdisp()
{
	x = 0.0;
	strcpy(xreg,"              0");
	xpend = FALSE;
	period = FALSE;
	decimal = 0.0;
	digits = 0;
	return(SUCCESS);
}

static int do_operation()
{
	double lx, ly, ax, ay, log10();

	if (operation != NULL_OP)
	{
		if (x >= 0) lx = log10(x);
		else lx = log10(-x);
		if (y >= 0) ly = log10(y);
		else ly = log10(-y);
		if (lx >= 0.0) ax = lx;
		else ax = -lx;
		if (ly >= 0.0) ay = ly;
		else ay = -ly;
	}

	switch (operation)
	{
		case ADD:
		{
			if ((ax > LIMIT) || (ay > LIMIT)) overflow();
			else x = y + x;
			break;
		}
		case SUBTRACT:
		{
			if ((ax > LIMIT) || (ay > LIMIT)) overflow();
			else x = y - x;
			break;
		}
		case MULTIPLY:
		{
			if ((lx + ly) > LIMIT) overflow();
			else if ((lx + ly) < -LIMIT) overflow();
			else x = y * x;
			break;
		}
		case DIVIDE:
		{
			if (x == 0.0) overflow();
			else if ((ly - lx) >  LIMIT) overflow();
			else if ((ly - lx) < -LIMIT) overflow();
			else x = y / x;
			break;
		}
	}
	return(SUCCESS);
}

static int standard_op(k) int k;
{
	if (ypend)
	{
		ypend = FALSE;
		y = x;
	}
	if (!xpend) do_operation();
	display();
	y = x;
	xpend = TRUE;
	last = k;
}

static int clear()
{
	y = 0.0;
	x = 0.0;
	strcpy(xreg,"              0");
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
	unsigned char c, vgetc();

	vbell();
	vtext(cmode,row+1,col+2,"  * OVERFLOW * ");
	clear();
	position();
	c = vgetc();
	vpushc(c);
	error = TRUE;
	return(SUCCESS);
}

overkey(m) int m;
{
	mode = m;

	vbuffering(LOGICAL);
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
	position();
	return(SUCCESS);
}

static int arrow(k) int k;
{
	register int i, j;

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
		if (col > vscr_wid-cols) col = vscr_wid-cols;
		if (col == i) vbell();
		else move();
		return(SUCCESS);
	}
	return(FAILURE);
}

funcalc()
{
	register int i, j, k;

	last = 'F';
	errno = 0;

funa:	k = vgetm();

	if ((k == '0') || (k == fn16_key)) return(FAILURE);
	else if (k == ' ') goto funa;
	else if (k == '-') x = -x;
	else if ((k == '+') || (k == ','))
	{
		if (x == 0.0) errno = 1;
		else x = 1 / x;
	}
	else if (k == '7') x = sin(x);
	else if (k == '8') x = cos(x);
	else if (k == '9') x = tan(x);
	else if (k == '4') x = asin(x);
	else if (k == '5') x = acos(x);
	else if (k == '6') x = atan(x);
	else if (k == '1') x = exp(x);
	else if (k == '2') x = log(x);
	else if (k == '3') x = log10(x);
	else if (k == '.') x = 2.0 * acos(0.0);
	else if ((k == '=') || (k == return_key)) x = sqrt(x);

	else if ((k == '*') || (k == fn4_key))
	{
		i = 0;
		while (xreg[i] == ' ') i++;
		vcut(&xreg[i]);
		goto xit;
	}

	else if ((k == '/') || (k == fn3_key))
	{
		if (vpaste(0))
		{
			x = 0.0;
			strcpy(xreg,"              0");
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
		show();
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
	register int key;


	vmenuinit(&help,DISPLAY_ONLY_MENU,REVERSE,0,0,0);
	vmenuitem(&help,"Calculator Help - Page 1 - General",0,NULL);
	vmenuitem(&help,"",0,NULL);
	vmenuitem(&help,"This calculator operates like most hand-held",0,NULL);
	vmenuitem(&help,"models. The screen layout matches the keypad",0,NULL);
	vmenuitem(&help,"of most terminal keyboards although it might",0,NULL);
	vmenuitem(&help,"not be perfectly one to one.  It should work",0,NULL);
	vmenuitem(&help,"with any key layout. In general numeric keys",0,NULL);
	vmenuitem(&help,"(0 1 etc.) are used for digits,  + - * / are",0,NULL);
	vmenuitem(&help,"used for add, subtract etc. and = for equals.",0,NULL);
	vmenuitem(&help,"",0,NULL);
	vmenuitem(&help,"For more information, depress the help key.",0,NULL);
	vmenuitem(&help,"Any other key will exit help..",0,NULL);

	key = vmenugo(&help);
	if (key != help_key) return(SUCCESS);

	vmenuinit(&help,DISPLAY_ONLY_MENU,REVERSE,0,0,0);
	vmenuitem(&help,"Good Calculator Help - Page 2 - Keys",0,NULL);
	vmenuitem(&help,"",0,NULL);
	vmenuitem(&help,"0 1 2 etc. are used for digits.",0,NULL);
	vmenuitem(&help,"+ - * / are used for add, subtract, etc.",0,NULL);
	vmenuitem(&help,"PF3 and PF4 double for * and /",0,NULL);
	vmenuitem(&help,"= and the return key are used for equals.",0,NULL);
	vmenuitem(&help,"PF1 or the F key selects function mode.",0,NULL);
	vmenuitem(&help,"PF2 or the C key clear the display.",0,NULL);
	vmenuitem(&help,"PF16 is used to exit the calculator.",0,NULL);
	vmenuitem(&help,"The arrow keys move the calculator about.",0,NULL);
	vmenuitem(&help,"",0,NULL);
	vmenuitem(&help,"For more information, depress the help key.",0,NULL);
	vmenuitem(&help,"Any other key will exit help.",0,NULL);

	key = vmenugo(&help);
	if (key != help_key) return(SUCCESS);
	vmenuinit(&help,DISPLAY_ONLY_MENU,REVERSE,0,0,0);
	vmenuitem(&help,"Good Calculator Help - Page 3 - Functions",0,NULL);
	vmenuitem(&help,"",0,NULL);
	vmenuitem(&help,"PF1 or the F key selects function mode.",0,NULL);
	vmenuitem(&help,"When selected, the screen will be changed",0,NULL);
	vmenuitem(&help,"to display what the functions are.  Thses",0,NULL);
	vmenuitem(&help,"include sin, cos etc. and are obtained by",0,NULL);
	vmenuitem(&help,"depressing the corresponding key.",0,NULL);
	vmenuitem(&help,"For example, sin corresponds to 7.",0,NULL);
	vmenuitem(&help,"",0,NULL);
	vmenuitem(&help,"For more information, depress the help key.",0,NULL);
	vmenuitem(&help,"Any other key will exit help.",0,NULL);

	key = vmenugo(&help);
	if (key != help_key) return(SUCCESS);

	vmenuinit(&help,DISPLAY_ONLY_MENU,REVERSE,0,0,0);
	vmenuitem(&help,"Good Calculator Help - Page 4 - Cut & Paste",0,NULL);
	vmenuitem(&help,"",0,NULL);
	vmenuitem(&help,"In function mode, the calculator value can",0,NULL);
	vmenuitem(&help,"be cut out for use in another program,  or",0,NULL);
	vmenuitem(&help,"for later use by the calculator.",0,NULL);
	vmenuitem(&help,"Similarily, a previously cut value can be",0,NULL);
	vmenuitem(&help,"imported from another program.",0,NULL);
	vmenuitem(&help,"",0,NULL);
	vmenuitem(&help,"This is the last page of help.",0,NULL);
	vmenuitem(&help,"Depress any key to exit...",0,NULL);

	key = vmenugo(&help);
	return(SUCCESS);
}

