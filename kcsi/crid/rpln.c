static char copyright[]="Copyright (c) 1988-1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
/*----
This is set up as a temporary solution .
RPTPLN was a cobol program called from C. this worked under LPI
COBOL, but does not work for ACUCOBOL, as all 'C' is part of the
RUN TIME.
------*/

#include <stdio.h>
#ifdef 	KCSI_VAX
#include <types.h>
#include <stat.h>
#else
#include <sys/types.h>
#include <sys/stat.h>
#endif /* KCSI_VAX */
#include <errno.h>

#include "kwisp.h"
#include "intdef.h"
#include "kcsifunc.h"

/*----
io codes are
OO = Open
FF = Form Feed
WW = print a line
CC = Close
DD = Display
KK = Close and keep
First letter makes io unique
------*/

static char sccsid[]="@(#)rpln.c	1.5 4/10/93";

static char tpname[100];
static FILE *tpf;
static int printer_file_is_open;


static void open_printer_file(char *p_file,char *p_lib,char *p_vol)
{
	int4 mode;

	memset(p_lib,' ',8);
	memset(p_vol,' ',6);
	mode = WISP_OUTPUT + WISP_PRINTFILE + WISP_PRNAME;
	strcpy(p_file,"##REPO  ");
	wfopen(&mode,p_vol,p_lib,p_file,tpname,"PRINT   ");
#ifdef KCSI_VAX
	wdellock(&mode, tpname);
#endif
	strunc(tpname);
	tpf = fopen(tpname,"w");
	printer_file_is_open = 1;

}
static void form_feed()
{
	fprintf(tpf,"%c",0x0c);
}
static void print_a_line(char *rec)
{
	strunc(rec);
	fprintf(tpf,"%s\n",rec);
}
static void close_printer_file(p_file,p_lib,p_vol,mode)
char *p_file,*p_lib,*p_vol,*mode;
{
	char user_mode[2];

	if(!(printer_file_is_open))
		return;
	fclose(tpf);
	if(*mode)
		{
		wargs(2L);
		EXTRACT("PM",user_mode);
		if(user_mode[0] == 'S')
			{
			SET("PM",mode);
			}
		}
	wfclose(tpname);
	if(*mode)
		SET("PM",user_mode);
	
}
static void keep_printer_file(p_file,p_lib,p_vol)
char *p_file,*p_lib,*p_vol;
{
	close_printer_file(p_file,p_lib,p_vol,"H");
}
void display_file()
{
	link_display(tpname);
}

void rptpln(char *p_io,char *p_record,char *p_file,char *p_lib,char *p_vol)
{

	switch(*p_io) 
		{
		case 'O':
			open_printer_file(p_file,p_lib,p_vol);
			break;
		case 'F':
			form_feed();
			break;
		case 'W':
			print_a_line(p_record);
			break;
		case 'C':
			close_printer_file(p_file,p_lib,p_vol,"");
			break;
		case 'D':
			display_file();
			break;
		case 'K':
			keep_printer_file(p_file,p_lib,p_vol);
			break;
		}
}

/*
**	History:
**	$Log: rpln.c,v $
**	Revision 1.4  1997-10-23 13:56:09-04  gsl
**	Change to use link_display()
**
**	Revision 1.3  1996-09-17 19:45:49-04  gsl
**	drcs update
**
**
**
*/
