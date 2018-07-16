static char copyright[]="Copyright (c) 1988-1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
/*----
This is set up as a temporary solution .
RPTPLN was a cobol program called from C. this worked under LPI
COBOL, but does not work for ACUCOBOL, as all 'C' is part of the
RUN TIME.
------*/

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
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
	mode = IS_OUTPUT + IS_PRINTFILE;
	strcpy(p_file,"##REPO  ");
	wfopen2(&mode,p_vol,p_lib,p_file,tpname,"REPORT  ","PRINT   ");
	KCSI_strunc(tpname);
	tpf = fopen(tpname,"w");
	printer_file_is_open = 1;

}
static void form_feed()
{
	fprintf(tpf,"%c",0x0c);
}
static void print_a_line(char *rec)
{
	KCSI_strunc(rec);
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
		WL_set_va_count(2);
		EXTRACT("PM",user_mode);	/* Save current PM */
		SET("PM",mode);			/* Change PM */
		}
	WFCLOSE(tpname);
	if(*mode)
		SET("PM",user_mode);		/* Restore PM */
	
}
static void keep_printer_file(p_file,p_lib,p_vol)
char *p_file,*p_lib,*p_vol;
{
	close_printer_file(p_file,p_lib,p_vol,"K");
}
static void display_file()
{
	WL_link_display(tpname);
}

void KCSI_rptpln(char *p_io,char *p_record,char *p_file,char *p_lib,char *p_vol)
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
**	Revision 1.5.2.1  2002/11/12 15:56:35  gsl
**	Sync with $HEAD Combined KCSI 4.0.00
**	
**	Revision 1.15  2002/10/24 14:20:34  gsl
**	Make globals unique
**	
**	Revision 1.14  2002/10/23 20:39:06  gsl
**	make global name unique
**	
**	Revision 1.13  2002/10/17 17:17:22  gsl
**	Removed VAX VMS code
**	
**	Revision 1.12  2002/07/29 14:47:21  gsl
**	wfopen2 ->WFOPEN2
**	wfopen3 ->WFOPEN3
**	
**	Revision 1.11  2002/07/25 15:20:25  gsl
**	Globals
**	
**	Revision 1.10  2002/07/23 20:49:51  gsl
**	globals
**	
**	Revision 1.9  2002/07/23 02:57:52  gsl
**	wfclose -> WFCLOSE
**	
**	Revision 1.8  2002/07/10 21:06:25  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.7  2002/06/21 20:48:15  gsl
**	Rework the IS_xxx bit flags and now include from wcommon.h instead of duplicate
**	definitions.
**	
**	Revision 1.6  2002/06/21 03:48:40  gsl
**	Remove VAX
**	
**	Revision 1.5  2002/03/28 17:16:37  gsl
**	FIx the logic for closing a print file in KEEP mode
**	
**	Revision 1.4  1997-10-23 13:56:09-04  gsl
**	Change to use link_display()
**
**	Revision 1.3  1996-09-17 19:45:49-04  gsl
**	drcs update
**
**
**
*/
