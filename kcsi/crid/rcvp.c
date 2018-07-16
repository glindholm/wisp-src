static char copyright[]="Copyright (c) 1988-1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
#include <stdio.h>
#include "dtype.h"
#include "kcsifunc.h"

static char sccsid[]="@(#)rcvp.c	1.2 3/23/93";

static char rcvtbuf[512];
static int rcvtidx;

static void as_hex(data,len)
char *data;
int len;
{
	while(len--)
		{
		sprintf(&rcvtbuf[rcvtidx],"%02X ",*data++);
		rcvtidx += 3;
		}
}

static void as_char(data,len)
char *data;
int len;
{
	int ch;

	while(len--)
		{
		ch = *data++;
		ch &= 0xff;
		if(( ch < ' ') || ( ch > '~'))
			ch = '.';
		sprintf(&rcvtbuf[rcvtidx++],"%c",ch);
		}
	
}

static void print_one(dt)
DTYPE *dt;
{
	char *cvt_type();

	sprintf(&rcvtbuf[rcvtidx],"%s ",cvt_type(dt));
	rcvtidx = strlen(rcvtbuf);
	strcat(rcvtbuf,"<");
	++rcvtidx;
	as_char(dt->_base + dt->_pos, dt->_len);
	strcat(rcvtbuf,"> ");
	rcvtidx += 2;
	as_hex(dt->_base + dt->_pos,dt->_len);
	sprintf(&rcvtbuf[rcvtidx],"dec=%d ",dt->_dec);
	crid_str(rcvtbuf);
}
static void print_pair(dest,src)
DTYPE *dest,*src;
{
	strcpy(rcvtbuf,"fr ");
	rcvtidx = 3;
	print_one(src);
	strcpy(rcvtbuf,"to ");
	rcvtidx = 3;
	print_one(dest);
}

void rcvt_pair(DTYPE *dest,DTYPE *src)
{
	print_pair(dest,src);
}

/*
**	History:
**	$Log: rcvp.c,v $
**	Revision 1.2  1996-09-17 19:45:44-04  gsl
**	drcs update
**
**
**
*/
