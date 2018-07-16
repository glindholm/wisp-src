/*
 * Module:  c2vwrap.c
 * Program: IDSIprint
 * Purpose: translate curses calls into video calls
 * 
 * $Log: c2vwrap.c,v $
 * Revision 1.2  1991/04/19  00:47:04  jockc
 * *** empty log message ***
 *
 * Revision 1.1  1991/04/18  23:48:49  jockc
 * Initial revision
 *
 *
 */
static char copyright[]="Copyright 1991 International Digital Scientific, Inc. All Rights Reserved.";
static char rcsid[]="$Id:$";

#include <stdio.h>
#include <v/vlocal.h>
#include <v/video.h>
#include <v/vcap.h>
#include <v/vdata.h>

initscr()
{
	vstate(0);
	verase(FULL_SCREEN);
}
mvaddstr(r,c,s)
int r,c;
char *s;
{
	vmove(r,c);
	vprint(s);
}
move(row,col)
int row,col;
{
	vmove(row,col);
}
refresh()
{
	/*vrefresh(FULL_SCREEN);*/
}
getch()
{
	return vgetm();
}
cbreak()
{
}
raw()
{
}
erase()
{
	verase(FULL_SCREEN);
}

noecho()
{
}
endwin()
{
	vexit();
}
attron(x)
int x;
{
	vmode(x);
}
attroff(x)
int x;
{
	vmode(CLEAR);
}

