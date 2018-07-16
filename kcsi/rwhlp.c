/*
******************************************************************************
**
** KCSI - King Computer Services Inc.
**
** $Id:$
**
** 
** CVS
** $Source:$
** $Author: gsl $
** $Date:$
** $Revision:$
******************************************************************************
*/


#include "gp.h"
#include "kcsifunc.h"



void rpt_full_gp_text(char *text[])
{
	int idx;

	for(idx = 0; idx < 16; ++idx)
		{
		GPCTEXT(text[idx],idx + 9,2);
		}
}

static char *text1[]={
"SUM ONLY OPTION",
"  Sum Only = YES - Only control break and total lines are printed.",
"  Sum Only = NO  - All detail lines are printed.",
" ",
"COUNT OPTION",
"The Count Option can be used to perform one of two separate functions.",
"  Count = YES  - The number of records selected for the report are printed",
"                 at the end of the report.",
"  Count = 001 to 999  - Printing is terminated after the specified number",
"                        of lines have been printed. This can be used for ",
"                        'top ten' reports, etc.",
"  Count = NO  - Neither of the above functions are performed.",
" ",
" ",
"                         **  Press (ENTER) to Continue  **",
" "
};

static void help1()
{
	WL_wpload();
	GPSETUP();
	GPSTD("HELP1   ","REPORT");
	rpt_full_gp_text(text1);
	GPENTER();
	GP_display_and_read();
}

static char *text2[]={
" ",
"CHANGE DATA FILES",
"This option allows the user to respecify the data files to be reported on",
"during the print option without having to change the Report Definition.",
" ",
" ",
"Options:",
"  YES - A screen will appear allowing a new data file to be entered.",
"  NO  -  The data file specified by the Report Definition is used.",
" ",
/*
"WISPSORT",
"  YES - Uses WISPSORT for sorting. WISPSORT is faster, but does not keep",
"        duplicates in order.",
*/
" ",
" ",
" ",
" ",
"                         **  Press (ENTER) to Continue  **",
" "
};

static char *text3[]={
"SELECT LINES      and  PRINT LINE SPACING",
"The Select Lines option is used to select which of the defined print lines",
"are to be printed, and the Line Spacing option designates the number of",
"blank lines to follow the corresponding print line on the report. It",
"is not necessary to print all lines, but you must print at least 1.",
"Select Lines is used by entering up to 3 of the possible report print line",
"numbers (1 2 or 3) in the order they are to be printed. Line Spacing is",
"controlled by entering the number of blank lines to follow the print line",
"directly below its corresponding select lines entry.",
"      Example:      Select Lines    = 31",
"                Print Line Spacing  = 02",
"will print a report with print line number 3 followed by print line 1",
"followed by 2 blank lines before the next print line number 3.",
" ",
"                         **  Press (ENTER) to Continue  **",
" "
};

static void help2()
{
	WL_wpload();
	GPSETUP();
	GPSTD("HELP2   ","REPORT");
	rpt_full_gp_text(text2);
	GPENTER();
	GP_display_and_read();
}

static void help3()
{
	WL_wpload();
	GPSETUP();
	GPSTD("HELP3   ","REPORT");
	rpt_full_gp_text(text3);
	GPENTER();
	GP_display_and_read();
}

void KCSI_rptphlp()
{
	help1();
	help2();
	help3();
}

/*
**	History:
**	$Log: rwhlp.c,v $
**	Revision 1.8  2003/02/04 19:19:08  gsl
**	fix header
**	
**	Revision 1.7  2002/10/24 14:20:33  gsl
**	Make globals unique
**	
**	Revision 1.6  2002/10/23 20:39:06  gsl
**	make global name unique
**	
**	Revision 1.5  2002/07/25 15:20:24  gsl
**	Globals
**	
**	Revision 1.4  2002/07/10 21:06:26  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.3  2001/11/05 19:46:01  gsl
**	change ENTER to (ENTER)
**	
**	Revision 1.2  1996-09-17 19:45:51-04  gsl
**	drcs update
**
**
**
*/
