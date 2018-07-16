/************************************************************************/
/*	     VIDEO - Video Interactive Development Environment		*/
/*			    Copyright (c) 1987				*/
/*	An unpublished work by Greg L. Adams.  All rights reserved.	*/
/************************************************************************/

#include <stdio.h>

#ifndef VMS	/* unix and MSDOS */
#include <malloc.h>
#include <sys/types.h>
#endif

#ifndef unix	/* VMS and MSDOS */
#include <stdlib.h>
#endif

#include <time.h>
#include "video.h"
#include "vlocal.h"
#include "vdata.h"
#include "vmenu.h"

#define MAX_APT 256
#define WIDTH 32
#define LENGTH 16
#define PAGE (i*LENGTH*(WIDTH+1))
#define ITEM (j*(WIDTH+1))

static int row, col, rows, cols;
static int emode, cmode, umode;
static unsigned char *save, *save2, *save3;
static int date, leap;
static time_t bintime;
static struct tm *tval;
static int rowpos, colpos;
static int lines = 5;
static char *mo[12] = { "Jan","Feb","Mar","Apr","May","Jun",
			"Jul","Aug","Sep","Oct","Nov","Dec" };
static int dim[12] = { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
static char this_date[32];
static char apt[MAX_APT][16];
static char *tab;
static int newcal;
static char temp[WIDTH*2];

#define JAN  0
#define FEB  1
#define DEC 11

int gcalend()
{
	register int i, j, k;
	unsigned char *vsss();
	int active;
	char c;
	FILE *fp, *vopenf();
	char *fgets();
	int last, found, freepage;

	i = MAX_APT * LENGTH * (WIDTH+1);
	if ((tab = malloc(i)) == NULL)
	{
		vre("gcalend-F-Internal error, unable to allocate memory");
		return(FAILURE);
	}

	for (i = 0; i < MAX_APT; i++)
	{
		strcpy(apt[i],"               ");
		apt[i][15] = CHAR_NULL;
		initpage(tab+PAGE);
	}

	if ((fp = vopenf("cal","r")) == NULL) newcal = TRUE;
	else
	{
		newcal = FALSE;
		fgets(temp, 20, fp);
		last = FALSE;
		for (i = 0; ((i < MAX_APT) && !last); i++)
		{
			if (fgets(temp, 20, fp) == NULL) last = TRUE;
			else
			{
				temp[15] = CHAR_NULL;
				strcpy(apt[i], temp);
				for (j = 0; j < LENGTH; j++)
				{
					fgets(temp, WIDTH*2, fp);
					temp[WIDTH] = CHAR_NULL;
					strcpy(tab+PAGE+ITEM, temp);
				}
			}
		}
		fclose(fp);
	}

	vbuffering(LOGICAL);
	row = 4;
	col = 20;
	rows = 15;
	cols = 38;
	vdetpos(0,&row,&col,rows,cols);
	save2 = vsss(row+rows-2,col,2,cols);
	save = vsss(row,col,rows-2,cols);

	if (vscr_atr & LIGHT)
	{
		emode = BOLD;
		cmode = REVERSE|BOLD;
		umode = UNDERSCORE;
	}
	else
	{
		emode = REVERSE;
		cmode = BOLD;
		umode = REVERSE|BOLD|UNDERSCORE;
	}

	time(&bintime);
	initpos();
	showcalend();

	active = TRUE;
	while(active)
	{
		k = vgetm();

		if (k == fn16_key) active = FALSE;

		else if (k == help_key) givehelp();

		else if ((k == return_key) || (k == enter_key))
		{
			getnow();
			found = FALSE;
			freepage = -1;
			for (i = 0; ((i < MAX_APT) && !found); i++)
			{
			    if (strpos(this_date, apt[i]) >= 0) found = TRUE;
			    if ((apt[i][0] == ' ') && (freepage < 0)) freepage = i;
			}

			if (!found) i = freepage;
			else --i;

			if (i < 0) vbell();
			else if (gcal2(this_date,tab+PAGE)) strcpy(apt[i], this_date);
			else strcpy(apt[i],"               ");
		}

		else if (k == fn2_key)
		{
			if ((tval->tm_year == 90) && (tval->tm_mon == JAN))
			{
				vbell();
			}
			else
			{
				position(emode);
				lastmonth();
				showagain();
			}
		}

		else if (k == fn1_key)
		{
			if ((tval->tm_year == 116) && (tval->tm_mon == DEC))
			{
				vbell();
			}
			else
			{
				position(emode);
				nextmonth();
				showagain();
			}
		}

		else if (k == up_arrow_key)
		{
			if (row+rowpos > row+3)
			{
				date = date - 7;
				if (date < 1)
				{
					date = date + 7;
					goto ua1;
				}
				else
				{
					date = date + 7;
					position(emode);
					rowpos = rowpos - 2;
					date = date - 7;
					bintime = bintime - (7*24*60*60);
					position(cmode);
				}
			}
			else
			{
ua1:				i = row;
				row = row - 2;
				if (row < 1) row = 1;
				if (row == i) vbell();
				else move();
			}
		}

		else if (k == down_arrow_key)
		{
			i = 5;
			if (lines == 6) i = 3;
			if (row+rowpos < row + rows - i)
			{
				date = date + 7;
				if (date > dim[tval->tm_mon])
				{
					date = date - 7;
					goto da1;
				}
				else
				{
					date = date - 7;
					position(emode);
					rowpos = rowpos + 2;
					date = date + 7;
					bintime = bintime + (7*24*60*60);
					position(cmode);
				}
			}
			else
			{
da1:				i = row;
				row = row + 2;
				if (row > MAX_LINES_PER_SCREEN-rows)
				{
					row = MAX_LINES_PER_SCREEN-rows;
				}
				if (row == i) vbell();
				else move();
			}
		}

		else if (k == left_arrow_key)
		{
			if (col+colpos > col+5)
			{
				date--;
				if (date < 1)
				{
					date++;
					goto la1;
				}
				else
				{
					date++;
					position(emode);
					colpos = colpos - 5;
					date--;
					bintime = bintime - (24*60*60);
					position(cmode);
				}
			}
			else
			{
la1:				i = col;
				col = col - 4;
				if (col < 0) col = 0;
				if (col == i) vbell();
				else move();
			}
		}

		else if (k == right_arrow_key)
		{
			if (col+colpos < col+cols-6)
			{
				date++;
				if (date > dim[tval->tm_mon])
				{
					date--;
					goto ra1;
				}
				else
				{
					date--;
					position(emode);
					colpos = colpos + 5;
					date++;
					bintime = bintime + (24*60*60);
					position(cmode);
				}
			}
			else
			{
ra1:				i = col;
				col = col + 4;
				if (col > vscr_wid-cols) col = vscr_wid-cols;
				if (col == i) vbell();
				else move();
			}
		}

		else if ((k == tab_key) || (k == ' '))
		{
			date++;
			if (date > dim[tval->tm_mon])
			{
				date--;
				position(emode);
				while(date != 1)
				{
					bintime = bintime - (24*60*60);
					date--;
				}
				initpos();
				position(cmode);
			}
			else
			{
				date--;
				position(emode);
				date++;
				bintime = bintime + (24*60*60);
				initpos();
				position(cmode);
			}
		}

		else if (k == home_key)
		{
			time(&bintime);
			showagain();
		}
		else vbell();
	}

	vrss(save);
	vrss(save2);
	vbuffering(AUTOMATIC);

	if (newcal) fp = vopenf("cal","w");
	else fp = vopenf("cal","w");
	time(&bintime);
	getnow();
	fprintf(fp, "%s\n", this_date);
	for (i = 0; i < MAX_APT; i++)
	{
		if (apt[i][0] != ' ')
		{
			fprintf(fp, "%s\n", apt[i]);
			for (j = 0; j < LENGTH; j++)
			{
				fprintf(fp, "%s\n", tab+PAGE+ITEM);
			}
		}
	}
	fclose(fp);
	free(tab);
	return(SUCCESS);
}

static int givehelp()
{
	struct video_menu help;
	register int key;

	vmenuinit(&help,DISPLAY_ONLY_MENU,REVERSE,0,0,0);
	vmenuitem(&help,"Good Calendar Help - Page 1 - General",0,0);
	vmenuitem(&help,"",0,0);
	vmenuitem(&help,"Use TAB, SPACE or arrow keys to select a date.",0,0);
	vmenuitem(&help,"Use PF1 to move to the next month.",0,0);
	vmenuitem(&help,"Use PF2 to move to the previous month.",0,0);
	vmenuitem(&help,"Use PF16 to exit.",0,0);
	vmenuitem(&help,"Use RETURN or ENTER to open the current date.",0,0);
	vmenuitem(&help,"Use HOME (FIND) to return to today's date.",0,0);
	vmenuitem(&help,"The arrow keys will also move the window",0,0);
	vmenuitem(&help,"    when the cursor is on an outside date. ",0,0);
	vmenuitem(&help,"",0,0);

	vmenuitem(&help,"Depress any key to exit...",0,0);

	key = vmenugo(&help);
	vset(CURSOR,OFF);
	return(SUCCESS);
}

static int showcalend()
{
	register int i, j, k, m, y;
	char wstr[156];

	vbuffering(LOGICAL);
	vset(CURSOR,OFF);

	tval = localtime(&bintime);
	setleap();
	setwstr(wstr);
	m = fdom();
	outmonth();
	getnow();

	i = row+2;
	vtext(emode,i++,col,"                                      ");
	vmove(i++,col);
	vmode(emode);
	vprint("  ");
	k = 0;
	j = 0;
	while (j++ != m) vprint("     ");
	for (j = m; j < 7; j++)
	{
		if (in_use(&wstr[k])) vmode(umode);
		vputc(' ');
		vputc(wstr[k++]);
		vputc(wstr[k++]);
		vputc(wstr[k++]);
		vmode(emode);
		vputc(' ');
	}
	vputc(' ');

	for (m = 0; m < (lines-1); m++)
	{
		vtext(emode,i++,col,"                                      ");

		vmove(i++,col);
		vmode(emode);
		vprint("  ");
		for (j = 0; j < 7; j++)
		{
			if (in_use(&wstr[k])) vmode(umode);
			vputc(' ');
			vputc(wstr[k++]);
			vputc(wstr[k++]);
			vputc(wstr[k++]);
			vmode(emode);
			vputc(' ');
		}
		vputc(' ');
	}

	vtext(emode,i++,col,"                                      ");

	for (j = col; j < col+cols; j = j + 5)
	{
		vmove(row+2,j+1);
		if (lines == 6) vline(VERTICAL,rows-2);
		else vline(VERTICAL,rows-4);
	}

	j = 6 - lines;
	for (i = row + 2; i < row+rows-j; i = i + 2)
	{
		vmove(i,col+1); vline(HORIZONTAL,36);
	}

	position(cmode);
	vmode(CLEAR);
	vcharset(DEFAULT);
	vbuffering(AUTOMATIC);
	return(SUCCESS);
}

static int position(m) int m;
{
	if ((date < 1) || (date > dim[tval->tm_mon]))
	{
		vtext(m,row+rowpos,col+colpos,"    ");
	}
	else
	{
		if (m == cmode) vtext(m,row+rowpos,col+colpos," %2d ",date);
		else
		{
			getnow();
			if (in_use(&this_date[8])) m = umode;
			vtext(m,row+rowpos,col+colpos," %2d ",date);
		}
	}
	vslew(0,-4);
	return(0);
}

static int move()
{
	unsigned char *vsss();

	vrss(save);
	vrss(save2);
	save2 = vsss(row+rows-2,col,2,cols);
	save = vsss(row,col,rows-2,cols);
	showcalend();
	return(SUCCESS);
}

static int fdom()
{
	time_t temp;
	int wday;

	temp = bintime;
	while (tval->tm_mday != 1)
	{
		temp = temp - (24*60*60);
		tval = localtime(&temp);
	}
	wday = tval->tm_wday;
	tval = localtime(&bintime);
	return(wday);
}

static int initpos()
{
	register int j, k, m;

	tval = localtime(&bintime);
	date = tval->tm_mday;
	k = fdom() + tval->tm_mday;
	for (j = 0; k > 7; j++) k = k - 7;

	rowpos = 3 + j*2;
	colpos = 2 + (k-1)*5;

	j = fdom();
	k = dim[tval->tm_mon];

	m = lines;
	lines = 5;
	if ((k == 31) && (j >= 5)) lines = 6;
	if ((k == 30) && (j == 6)) lines = 6;

	if (m == lines) return(FAILURE);
	return(SUCCESS);
}

static int nextmonth()
{
	do
	{
		bintime = bintime + (24*60*60);
		tval = localtime(&bintime);
	} while (tval->tm_mday != 1);

	return(SUCCESS);
}

static int lastmonth()
{
	while (tval->tm_mday != 1)
	{
		bintime = bintime - (24*60*60);
		tval = localtime(&bintime);
	}
	bintime = bintime - (24*60*60);

	return(SUCCESS);
}

static int showdates()
{
	register int i, j, k, m, y;
	char wstr[156];

	vbuffering(LOGICAL);
	vset(CURSOR,OFF);

	tval = localtime(&bintime);
	setleap();
	setwstr(wstr);
	m = fdom();
	outmonth();
	getnow();

	vmode(emode);
	i = row+3;
	vmove(i++,col+2);
	k = 0;
	j = 0;
	while (j++ != m)
	{
		vprint("    ");
		vslew(0,1);
	}

	for (j = m; j < 7; j++)
	{
		if (in_use(&wstr[k])) vmode(umode);
		vputc(' ');
		vputc(wstr[k++]);
		vputc(wstr[k++]);
		vputc(wstr[k++]);
		vmode(emode);
		vslew(0,1);
	}
	i++;

	for (m = 0; m < (lines-1); m++)
	{
		vmove(i++,col+2);
		for (j = 0; j < 7; j++)
		{
			if (in_use(&wstr[k])) vmode(emode|BOLD);
			vputc(' ');
			vputc(wstr[k++]);
			vputc(wstr[k++]);
			vputc(wstr[k++]);
			vmode(emode);
			vslew(0,1);
		}
		i++;
	}

	position(cmode);
	vmode(CLEAR);
	vcharset(DEFAULT);
	vbuffering(AUTOMATIC);
	return(SUCCESS);
}

static int setleap()
{
	leap = 0;
	if ((tval->tm_year ==  92) || (tval->tm_year ==  96)) leap = 1;
	if ((tval->tm_year == 104) || (tval->tm_year == 108)) leap = 1;
	if ((tval->tm_year == 112) || (tval->tm_year == 116)) leap = 1;
	if (leap) dim[FEB] = 29;
	return(SUCCESS);
}

static int setwstr(wstr) char *wstr;
{
	register int m;
	strcpy(&wstr[00]," 1  2  3  4  5  6  7  8  9 10 11 12 13 14 ");
	strcpy(&wstr[42],"15 16 17 18 19 20 21 22 23 24 25 26 27 ");
	m = dim[tval->tm_mon];
	if      (m == 31) strcpy(&wstr[81], "28 29 30 31 ");
	else if (m == 30) strcpy(&wstr[81], "28 29 30    ");
	else if (m == 29) strcpy(&wstr[81], "28 29       ");
	else if (m == 28) strcpy(&wstr[81], "28          ");
	strcpy(&wstr[93],"                                                 ");
	return(SUCCESS);
}

static int outmonth()
{
	register int i, y;

	if ((y = tval->tm_year) > 99) y = y - 100;
	i = row;
	if (y < 10) vtext(emode,i++,col,"  Good Calendar   ----- %s 0%d ----- ", mo[tval->tm_mon], y);
	else vtext(emode,i++,col,"  Good Calendar   ----- %s %d -----  ", mo[tval->tm_mon], y);
	
	vtext(emode,i++,col,"  Sun  Mon  Tues  Wed  Ths  Fri  Sat  ");
	return(SUCCESS);
}

static int showagain()
{
	unsigned char *vsss();
	if (initpos())
	{
		vrss(save2);
		save2 = vsss(row+rows-2,col,2,cols);
		showcalend();
	}
	else showdates();
	return(SUCCESS);
}

static int initpage(page) char page[LENGTH][WIDTH+1];
{
	register int i;

	for (i = 0; i < LENGTH; i++)
	{
	 if (i+6 < 10) sprintf(page[i],"0%d:00                           ",i+6);
	 else           sprintf(page[i],"%d:00                           ",i+6);
	}
}

static int getnow()
{
	register int i;
	strcpy(this_date,ctime(&bintime));
	for (i = 11; i < 11+5; i++) this_date[i] = this_date[i+9];
	this_date[15] = CHAR_NULL;
	return(SUCCESS);
}

static int in_use(wstr) char *wstr;
{
	int ret;
	register int i;

	ret = FALSE;
	tval = localtime(&bintime);

	if ((i = tval->tm_year) > 99) i = i - 100;
	if (i < 10) sprintf(temp,"0%d",i);
	else sprintf(temp,"%d",i);

	for (i = 0; (i < MAX_APT) && !ret; i++)
	{
		if ((mo[tval->tm_mon][0] == apt[i][4]) &&
		    (mo[tval->tm_mon][1] == apt[i][5]) &&
		    (mo[tval->tm_mon][2] == apt[i][6]) &&
		    (temp[0] == apt[i][13]) && (temp[1] == apt[i][14]))
		{
			if ((wstr[0] == apt[i][8]) && (wstr[1] == apt[i][9])) ret = TRUE;
		}
	}
	return(ret);
}
