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
/*           VIDEO - Video Interactive Development Environment          */
/*                          Copyright (c) 1987                          */
/*      An unpublished work by Greg L. Adams.  All rights reserved.     */
/************************************************************************/

#include <stdio.h>

#include <malloc.h>
#include <sys/types.h>

#ifndef NOSTDLIB
#include <stdlib.h>
#endif

#include <time.h>
#include <string.h>
#include "video.h"
#include "vlocal.h"
#include "vdata.h"
#include "vintdef.h"
#include "vmenu.h"
#include "vmodules.h"

#define SECSINWEEK 604800L /* (7*24*60*60) */
#define SECSINDAY  86400L  /*   (24*60*60) */

#if defined(WIN32)
#define MAX_APT 64U
#else
#define MAX_APT 256
#endif
#define WIDTH 32
#define LENGTH 16
#define PAGE (i*LENGTH*(WIDTH+1))
#define ITEM (j*(WIDTH+1))

static int row, col, rows, cols;
static int emode, cmode, umode;
static unsigned char *save, *save2;
static int date, leap;
static time_t bintime;
static struct tm *tval;
static int rowpos, colpos;
static int lines = 5;
static char *mo[12] = { "Jan","Feb","Mar","Apr","May","Jun",
			"Jul","Aug","Sep","Oct","Nov","Dec" };
static int dim[12] = { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
static char this_date[32] = { "" };
static char apt[MAX_APT][16];
static char *tab;
static int newcal;
static char temp[WIDTH*2];

static int givehelp();
static int showcalend();
static int position();
static int move();
static int initpos();
static int nextmonth();
static int lastmonth();
static int setleap();
static int setwstr();
static int outmonth();
static int showagain();
static int initpage(char page[LENGTH][WIDTH+1]);
static int getnow();
static int in_use();
static int fdom();

#define JAN  0
#define FEB  1
#define DEC 11

int gcalend(void)
{
	unsigned int i, j;
	int k;
	int active;
	FILE *fp;
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
		initpage((char (*)[WIDTH+1])(tab+PAGE));
	}

	if ((fp = VL_vopenf("cal","r")) == NULL) newcal = TRUE;
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
					fgets(temp, (int)WIDTH*2, fp);
					temp[WIDTH] = CHAR_NULL;
					strcpy(tab+PAGE+ITEM, temp);
				}
			}
		}
		fclose(fp);
	}

	VL_vbuffering_start();
	row = 4;
	col = 20;
	rows = 15;
	cols = 38;
	VL_vdetpos(0,&row,&col,rows,cols);
	save2 = vsss(row+rows-2,col,2,cols);
	save = vsss(row,col,rows-2,cols);

	if (vscr_atr & LIGHT)
	{
		emode = VMODE_BOLD;
		cmode = VMODE_REVERSE|VMODE_BOLD;
		umode = VMODE_UNDERSCORE;
	}
	else
	{
		emode = VMODE_REVERSE;
		cmode = VMODE_BOLD;
		umode = VMODE_REVERSE|VMODE_BOLD|VMODE_UNDERSCORE;
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
			    if (strstr(this_date, apt[i]) != NULL) found = TRUE;
			    if ((apt[i][0] == ' ') && (freepage < 0)) freepage = i;
			}

			if (!found) i = freepage;
			else --i;

			if (i > MAX_APT) vbell(); /* wrap around */
			else if (VL_gcal2(this_date,tab+PAGE)) strcpy(apt[i], this_date);
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
					bintime = bintime - SECSINWEEK;
					position(cmode);
				}
			}
			else
			{
ua1:                            i = row;
				row = row - 2;
				if (row < 1) row = 1;
				if (row == (int) i) vbell();
				else move();
			}
		}

		else if (k == down_arrow_key)
		{
			i = 5;
			if (lines == 6) i = 3;
			if (row+rowpos < row + rows - (int) i)
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
					bintime = bintime + SECSINWEEK;
					position(cmode);
				}
			}
			else
			{
da1:                            i = row;
				row = row + 2;
				if (row > MAX_LINES_PER_SCREEN-rows)
				{
					row = MAX_LINES_PER_SCREEN-rows;
				}
				if (row == (int) i) vbell();
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
					bintime = bintime - SECSINDAY;
					position(cmode);
				}
			}
			else
			{
la1:                            i = col;
				col = col - 4;
				if (col < 0) col = 0;
				if (col == (int) i) vbell();
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
					bintime = bintime + SECSINDAY;
					position(cmode);
				}
			}
			else
			{
ra1:                            i = col;
				col = col + 4;
				if (col > VL_vscr_wid-cols) col = VL_vscr_wid-cols;
				if (col == (int) i) vbell();
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
					bintime = bintime - SECSINDAY;
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
				bintime = bintime + SECSINDAY;
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
	VL_vbuffering_end();

	if (newcal) fp = VL_vopenf("cal","w");
	else fp = VL_vopenf("cal","w");

	if (!fp)
	{
		return FAILURE;
	}

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
	int4 key;

	VL_vmenuinit(&help,DISPLAY_ONLY_MENU,VMODE_REVERSE,0,0,0);
	VL_vmenuitem(&help,"Good Calendar Help - Page 1 - General",0,0);
	VL_vmenuitem(&help,"",0,0);
	VL_vmenuitem(&help,"Use TAB, SPACE or arrow keys to select a date.",0,0);
	VL_vmenuitem(&help,"Use PF1 to move to the next month.",0,0);
	VL_vmenuitem(&help,"Use PF2 to move to the previous month.",0,0);
	VL_vmenuitem(&help,"Use PF16 to exit.",0,0);
	VL_vmenuitem(&help,"Use RETURN or ENTER to open the current date.",0,0);
	VL_vmenuitem(&help,"Use HOME (FIND) to return to today's date.",0,0);
	VL_vmenuitem(&help,"The arrow keys will also move the window",0,0);
	VL_vmenuitem(&help,"    when the cursor is on an outside date. ",0,0);
	VL_vmenuitem(&help,"",0,0);

	VL_vmenuitem(&help,"Depress any key to exit...",0,0);

	key = VL_vmenugo(&help);
	vset_cursor_off();
	return(SUCCESS);
}

static int showcalend()
{
	register int i, j, k, m;
	char wstr[156];

	VL_vbuffering_start();
	vset_cursor_off();

	tval = localtime(&bintime);
	setleap();
	setwstr(wstr);
	m = fdom();
	outmonth();
	getnow();

	i = row+2;
	vtext(emode,i++,col,"                                      ");
	vmove(i++,col);
	VL_vmode(emode);
	vprint("  ");
	k = 0;
	j = 0;
	while (j++ != m) vprint("     ");
	for (j = m; j < 7; j++)
	{
		if (in_use(&wstr[k])) VL_vmode(umode);
		vputc(' ');
		vputc(wstr[k++]);
		vputc(wstr[k++]);
		vputc(wstr[k++]);
		VL_vmode(emode);
		vputc(' ');
	}
	vputc(' ');

	for (m = 0; m < (lines-1); m++)
	{
		vtext(emode,i++,col,"                                      ");

		vmove(i++,col);
		VL_vmode(emode);
		vprint("  ");
		for (j = 0; j < 7; j++)
		{
			if (in_use(&wstr[k])) VL_vmode(umode);
			vputc(' ');
			vputc(wstr[k++]);
			vputc(wstr[k++]);
			vputc(wstr[k++]);
			VL_vmode(emode);
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
	VL_vmode(CLEAR);
	vcharset(DEFAULT);
	VL_vbuffering_end();
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
		temp = temp - SECSINDAY;
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
		bintime = bintime + SECSINDAY;
		tval = localtime(&bintime);
	} while (tval->tm_mday != 1);

	return(SUCCESS);
}

static int lastmonth()
{
	while (tval->tm_mday != 1)
	{
		bintime = bintime - SECSINDAY;
		tval = localtime(&bintime);
	}
	bintime = bintime - SECSINDAY;

	return(SUCCESS);
}

static int showdates()
{
	register int i, j, k, m;
	char wstr[156];

	VL_vbuffering_start();
	vset_cursor_off();

	tval = localtime(&bintime);
	setleap();
	setwstr(wstr);
	m = fdom();
	outmonth();
	getnow();

	VL_vmode(emode);
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
		if (in_use(&wstr[k])) VL_vmode(umode);
		vputc(' ');
		vputc(wstr[k++]);
		vputc(wstr[k++]);
		vputc(wstr[k++]);
		VL_vmode(emode);
		vslew(0,1);
	}
	i++;

	for (m = 0; m < (lines-1); m++)
	{
		vmove(i++,col+2);
		for (j = 0; j < 7; j++)
		{
			if (in_use(&wstr[k])) VL_vmode(emode|BOLD);
			vputc(' ');
			vputc(wstr[k++]);
			vputc(wstr[k++]);
			vputc(wstr[k++]);
			VL_vmode(emode);
			vslew(0,1);
		}
		i++;
	}

	position(cmode);
	VL_vmode(CLEAR);
	vcharset(DEFAULT);
	VL_vbuffering_end();
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
	if (initpos())
	{
		vrss(save2);
		save2 = vsss(row+rows-2,col,2,cols);
		showcalend();
	}
	else showdates();
	return(SUCCESS);
}

static int initpage(char page[LENGTH][WIDTH+1])
{
	register int i;

	for (i = 0; i < LENGTH; i++)
	{
	 if (i+6 < 10) sprintf(page[i],"0%d:00                           ",i+6);
	 else           sprintf(page[i],"%d:00                           ",i+6);
	}
	return 0;
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
/*
**	History:
**	$Log: gcalend.c,v $
**	Revision 1.27  2011/10/28 01:42:03  gsl
**	fix warnings
**	
**	Revision 1.26  2011/10/20 01:02:47  gsl
**	mark warnings
**	
**	Revision 1.25  2010/02/10 14:50:17  gsl
**	fix warnings
**	
**	Revision 1.24  2010/02/10 03:52:33  gsl
**	fix warnings for redefined functions
**	
**	Revision 1.23  2003/06/27 15:54:03  gsl
**	fix EDE API
**	
**	Revision 1.22  2003/06/23 15:28:04  gsl
**	VL_ global symbols
**	
**	Revision 1.21  2003/01/31 20:18:47  gsl
**	Fix -Wall warnings
**	
**	Revision 1.20  2003/01/31 19:25:57  gsl
**	Fix copyright header
**	
**	Revision 1.19  2002/07/18 21:04:20  gsl
**	Remove MSDOS code
**	
**	Revision 1.18  2002/07/17 21:06:00  gsl
**	VL_ globals
**	
**	Revision 1.17  2002/07/16 13:40:23  gsl
**	VL_ globals
**	
**	Revision 1.16  2002/07/15 20:56:37  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.15  2002/07/15 20:16:06  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.14  2002/07/15 17:52:53  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.13  2002/06/26 01:42:49  gsl
**	Remove VMS code
**	
**	Revision 1.12  1998/05/21 15:44:14  gsl
**	Don't use the fp file handle unless it is valid.
**	
**	Revision 1.11  1997-07-08 16:16:21-04  gsl
**	Change to use new video.h defines
**
**	Revision 1.10  1997-01-08 16:37:13-05  gsl
**	Change strpos() to strstr()
**
**	Revision 1.9  1996-10-11 15:15:54-07  gsl
**	drcs update
**
**
**
*/
