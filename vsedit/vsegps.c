/*
******************************************************************************
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
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


/*----
This file contains the various GETPARM screens used by vse.
it displays the getparm as a system_name style getparm,
otherwise it uses a std wang style input.
It is passed an error message.
It returns the value of the pressed pfkey
------*/
#include <stdio.h>
#include <ctype.h>
#include "vsegps.h"
#include "vseglb.h"
#include "vsegp.h"
#include "vsedscr.h"
#include "vsedfnd.h"
#include "vsedit.h"
#include "vsemov.h"
#include "vseutl.h"
#include "wisplib.h"

static char file_info[80];
static char modified[30];
static char line_cnt[8];
       
static VSESCR_FLDS(no_file_created) = {
{LEN(0)	 ROW(1)	COL(2)	VALUE(VSE_COPYRIGHT)},
{LEN(78) ROW(2)	COL(2)	FROM(file_info)},      
{LEN(0)	 ROW(3)	COL(2)	VALUE("There are")},
{LEN(7)  ROW(3)	COL(12)	FROM(line_cnt)},      
{LEN(0)	 ROW(3)	COL(21)	VALUE("lines in the edited text.")},
{LEN(29) ROW(3)	COL(49)	FROM(modified)},      
{LEN(0)	 ROW(5)	COL(2)	VALUE("\224WARNING\204 - No file has been created on this run.")},
{LEN(0)  ROW(7)	COL(2)	FROM("Press (16) to exit the editor without saving the edited text.")},
{LEN(0)	 ROW(8)	COL(2)	VALUE("Press (1) to return to the special command menu.")},
{LASTITEM}
};
       
static VSESCR_FLDS(restart_flds) = {
{LEN(0)	 ROW(1)	COL(2)	VALUE(VSE_COPYRIGHT)},
{LEN(78) ROW(2)	COL(2)	FROM(file_info)},      
{LEN(0)	 ROW(3)	COL(2)	VALUE("There are")},
{LEN(7)  ROW(3)	COL(12)	FROM(line_cnt)},      
{LEN(0)	 ROW(3)	COL(21)	VALUE("lines in the edited text.")},
{LEN(29) ROW(3)	COL(49)	FROM(modified)},
{LEN(0)  ROW(5) COL(32) ULINEVALUE("RESTART COMMAND")},
{LEN(0)	 ROW(7)	COL(2)	VALUE("\224WARNING\204 - No file has been created on this run.")},
{LEN(0)  ROW(9)	 COL(2)	 FROM("Press (ENTER) to edit another file.")},
{LEN(0)	 ROW(10) COL(2)	VALUE("Press (1) to return to the special command menu.")},
{LASTITEM}
};

/*
**	Routine:	vse_output()
**
**	Function:	Issue a OUTPUT getparm and validate the result.
**
**	Description:	The library and volume default to the input values.
**			The file starts off as blank.  It puts up the GETPARM and validates
**			the result.  If a file is specified that already exists it makes
**			you press PF3 to override.
**
**	Arguments:
**	file		The returned wang style file name
**	library		The returned wang style library name
**	volume		The returned wang style volume name
**	sysname		The returned native filepath (untruncated)
**
**	Globals:
**	vse_gp_input_library
**	vse_gp_input_volume
**	(plus the getparm vars)
**
**	Return:
**	0		Got a name.
**	1		Exit was requested
**
**	Warnings:	None
**
**	History:	
**	03/11/94	Modified by gsl
**
*/
int vse_output(char *file, char *library, char *volume, char *sysname, int4 *start_line, int4 *end_line)
{
	int4	pick;
	int	message_code = 0;
	int	rc;
	char	message_field[80];
	char	start_field[17], end_field[17];

	memset(file,' ',VSE_FILENAME_LEN+1);
	memcpy(library,vse_gp_input_library,VSE_LIBNAME_LEN+1);
	memcpy(volume,vse_gp_input_volume,VSE_VOLNAME_LEN+1);
	memset(sysname,' ',VSE_SYSNAME_LEN+1);
	CLEAR_STRING(start_field);
	CLEAR_STRING(end_field);
	memcpy(start_field,"ALL",3);

	gppfkeys=GP_PF_01|GP_PF_05|GP_PF_03;
	WL_wswap(&gppfkeys);

	for(;;)
	{
		untrunc(sysname,VSE_SYSNAME_LEN);
		
		GPSETUP();
		switch(message_code)
		{
		case 0:
			GPSTD("OUTPUT  ","EDITOR",2);
			GPMSG("Fill in the information below to create a new file of the edited text.");
			GPMSG("Press (1) to exit the create command.");
			break;

		case 1: /* FILE EXISTS */
			GPRES("OUTPUT  ","EDITOR",4);
			GPMSG("\224WARNING\214- The file specified below already exists.");
			GPMSG("           Use (3) if you wish to scratch the existing file and continue.");
			GPMSG("           Otherwise, please specify another file name.");
			GPMSG("           Use (1) to return to the special command menu.");
			break;

		case 2: /* INVALID RANGE */
			GPRES("OUTPUT  ","EDITOR",1);
			GPMSG(message_field);
			break;

		case 3: /* NO FILENAME */
			GPRES("OUTPUT  ","EDITOR",1);
			GPMSG("\224SORRY\214-   The file information is incomplete.");
			break;
		}
		GPCTEXT("Please specify:",12,3);
		if(vse_native)
		{
			GPSYSNAME(sysname,14);
			GPCTEXT("(Press (5) for GENERIC entry mode.)",24,22);
		}
		else
		{
			GPFILE(file,14,5);
			GPLIB(library,14,28);
			GPVOL(volume,14,51);
			GPCTEXT("IN",14,25);
			GPCTEXT("ON",14,48);
			GPCTEXT("(Press (5) for NATIVE  entry mode.)",24,22);
		}

		GPCTEXT("Range:    First line to be copied?",17,3);
		GPCTEXT("          Last line to be copied?",18,3);
		GPKW("START   ",start_field,16,17,52,"U");
		GPKW("END     ",end_field,16,18,52,"U");

		GPENTER();
		GPPFS(&gppfkeys);
		
		switch(pick = display_and_read_gp())
		{
		case 1:
			return 1;
		case 5:
			vse_native = !vse_native;
			continue;
		}

		message_code = 0;
	
		if(!vse_native)
		{
			translate_name(file, library, volume, sysname);
		}

		if (' ' == sysname[0])
		{
			message_code = 3;
			continue;
		}

		if ((rc = validate_range(start_field,start_line,end_field,end_line)))
		{
			strcpy(message_field,vse_err(rc));
			message_code = 2;
			continue;
		}

		trunc(sysname);
		if(exists(sysname) && pick != 3)
		{
			message_code = 1;
		}
		else
		{
			WL_makepath(sysname);
			return 0;
		}
	}
}

/*
**	Routine:	vse_renumber_gp()
**
**	Function:	Issue the RENUMBER getparm and validate fields.
**
**	Description:	{Full detailed description}...
**
**	Arguments:
**	number_field	The NUMBER field
**	number		The returned number
**	incr_field	The INCR field
**	incr		The returned incr
**	start_field	The START field
**	start		The returned start
**	end_field	The END field
**	end		The returned end
**	resp		Respecify flag
**
**	Globals:	None
**
**	Return:
**	0		Got valid values
**	1		PF1 was press to terminate without a renumber.
**
**	Warnings:	None
**
**	History:	
**	03/14/94	Written by GSL
**
*/
int vse_renumber_gp(char number_field[7], int4 *number, char incr_field[7], int4 *incr, 
		char start_field[17], int4 *start, char end_field[17], int4 *end, int resp)
{
	char	message_field[80];

	for(;;)
	{
		gppfkeys=GP_PF_01;
		WL_wswap(&gppfkeys);
		GPSETUP();
		if (resp==0)
		{
			GPSTD("RENUMBER","EDITOR",3);
		}
		else
		{
			GPRES("RENUMBER","EDITOR",3);
		}

		strcpy(message_field,vse_err(resp));
		GPMSG(message_field);

		GPMSG(" ");
		GPMSG("                          Renumber Command");
		GPCTEXT("To renumber a portion of the edited file, fill in the information below",11,2);
		GPCTEXT("and press (ENTER). Press (1) to exit without renumbering the file.",12,2);
		GPCTEXT("Renumbering information:",14,2);
		GPCTEXT("New starting line number:",16,6);
		GPCTEXT("Increment to be used:",17,6);
		GPCTEXT("Range of lines to be renumbered:",19,2);
		GPCTEXT("(Note: Specify line numbers as they are before renumbering)",20,2);
		GPCTEXT("Start of range:",22,6);
		GPCTEXT("End of range:",23,6);
	
		if (resp==RESP_NUMBER || resp==RESP_NUMINCR)
		{
			GPRESP("NUMBER  ",number_field,6,16,33,"N");
		}
		else
		{
			GPKW("NUMBER  ",number_field,6,16,33,"N");
		}

		if (resp==RESP_INCR || resp==RESP_NUMINCR)
		{
			GPRESP("INCR    ",incr_field,6,17,33,"N");
		}
		else
		{
			GPKW("INCR    ",incr_field,6,17,33,"N");
		}

		if (resp==RESP_START || resp==RESP_RANGE)
		{
			GPRESP("START   ",start_field,16,22,33,"A");
		}
		else
		{
			GPKW("START   ",start_field,16,22,33,"A");
		}

		if (resp==RESP_END || resp== RESP_RANGE)
		{
			GPRESP("END     ",end_field,16,23,33,"A");
		}
		else
		{
			GPKW("END     ",end_field,16,23,33,"A");
		}
		GPENTER();
		GPPFS(&gppfkeys);
	
		if (display_and_read_gp())
		{
			/*
			**	PF1 = No renumber
			*/
			return 1;
		}

		resp = validate_numincr(number_field, number, incr_field, incr);

		if (!resp)
		{
			resp = validate_range(start_field, start, end_field, end);

			if (!resp)
			{
				return 0;
			}
		}
	}
}

/*
**	Routine:	vse_badnums_gp()
**
**	Function:	Issue the BADNUMS renumber getparm.
**
**	Description:	{Full detailed description}...
**
**	Arguments:
**	number_field	The NUMBER field
**	incr_field	The INCR field
**	err_msg		Error message
**
**	Globals:	None
**
**	Return:
**	0		Got valid values
**	1		PF1 was press to terminate without a renumber.
**
**	Warnings:	None
**
**	History:	
**	03/15/94	Written by GSL
**
*/
int vse_badnums_gp(char number_field[7], char incr_field[7], char *err_msg)
{
	char	file_info[256];

	gppfkeys=GP_PF_01;
	WL_wswap(&gppfkeys);
	GPSETUP();
	GPRES("BADNUMS ","EDITOR",1);
	if (strlen(err_msg))
	{
		GPMSG(err_msg);
	}
	else
	{
		GPMSG(" ");
	}

	sprintf(file_info,"Input File:\204%-65s",vse_sysname);

	GPCTEXT(file_info,10,2);
	GPCTEXT("File has invalid or unordered sequence numbers, it must be renumbered.",11,2);

	GPCTEXT("Please specify the starting line number and increment below and press (ENTER).",13,2);
	GPCTEXT("Press (1) to select another input file or language.",14,2);

	GPCTEXT("New starting line number:",16,6);
	GPCTEXT("Increment to be used:",17,6);
	
	GPKW("NUMBER  ",number_field,6,16,33,"N");
	GPKW("INCR    ",incr_field,6,17,33,"N");

	GPENTER();
	GPPFS(&gppfkeys);
	
	if (display_and_read_gp())
	{
		/*
		**	PF1 = No renumber
		*/
		return 1;
	}

	return 0;
}

/*
**	Routine:	vse_longline_gp()
**
**	Function:	Issue the LONGLINE warning getparm.
**
**	Description:	{Full detailed description}...
**
**	Arguments:
**	sysname		The file name.
**
**	Globals:	None
**
**	Return:
**	0		Continue
**	1		PF1 was press to terminate.
**
**	Warnings:	None
**
**	History:	
**	03/15/94	Written by GSL
**
*/
int vse_longline_gp(char *sysname)
{
	char	file_info[256];


	gppfkeys=GP_PF_01;
	WL_wswap(&gppfkeys);
	GPSETUP();
	GPRES("LONGLINE","EDITOR",1);
	GPMSG(" ");

	sprintf(file_info,"Input File:\204%-65s",sysname);

	GPCTEXT(file_info,10,2);
	GPCTEXT("File has long lines that must be split to continue.",11,2);

	GPCTEXT("       \204Spliting of long lines may cause unexpected problems.",13,2);

	GPCTEXT("Press (ENTER) to split the lines and continue.",15,2);
	GPCTEXT("Press (1)     to select another input file or language.",16,2);


	GPENTER();
	GPPFS(&gppfkeys);
	
	if (display_and_read_gp())
	{
		/*
		**	PF1 = Abort
		*/
		return 1;
	}

	return 0;
}

/*
**	Routine:	vse_readonly_gp()
**
**	Function:	Issue the READONLY warning getparm.
**
**	Description:	{Full detailed description}...
**
**	Arguments:	None
**
**	Globals:	None
**
**	Return:
**	0		Continue
**	1		PF1 was press to terminate.
**
**	Warnings:	None
**
**	History:	
**	03/18/94	Written by GSL
**
*/
int vse_readonly_gp(void)
{
	char	file_info[256];


	gppfkeys=GP_PF_01;
	WL_wswap(&gppfkeys);
	GPSETUP();
	GPRES("READONLY","EDITOR",1);
	GPMSG(" ");

	sprintf(file_info,"Input File:\204%-65s",vse_sysname);

	GPCTEXT(file_info,10,2);
	GPCTEXT("File is READ ONLY and can not be modified.",11,2);

	GPCTEXT("       \204In READ ONLY mode the REPLACE command will be disabled.",13,2);

	GPCTEXT("Press (ENTER) to continue with REPLACE command disabled.",15,2);
	GPCTEXT("Press (1)     to select another input file or language.",16,2);


	GPENTER();
	GPPFS(&gppfkeys);
	
	if (display_and_read_gp())
	{
		/*
		**	PF1 = Abort
		*/
		return 1;
	}

	return 0;
}

/*
**	Routine:	vse_defaults_gp()
**
**	Function:	Issue the DEAFULTS getparm and set the global values.
**
**	Description:	{Full detailed description}...
**
**	Arguments:
**	hidden		Flag for a hidden getparm.
**
**	Globals:
**	vse_gp_defaults_*
**	vse_tab_setting
**
**	Return:
**	0		OK
**	1		Abort without changing globals
**
**	Warnings:	None
**
**	History:	
**	03/18/94	Written by GSL
**
*/
int vse_defaults_gp(int hidden)
{
	char 	tabs[30],mode[6],cse[6],showmods[4];
	char 	tabstr[81];
	char	err_field[80];
	int 	i;
	int	resp = 0;
	int	first_tab_bad = 0;
	
	strcpy(tabs,vse_gp_defaults_tabs);
	strcpy(mode,vse_gp_defaults_mode);
	strcpy(cse,vse_gp_defaults_case);
	strcpy(showmods,vse_gp_defaults_showmods);

	tabs[2] = tabs[5] = tabs[8] = tabs[11] = tabs[14] = tabs[17] = tabs[20] = tabs[23] = tabs[26] = '\202';
	tabs[29] = (char)0;

	for(;;)
	{
		GPSETUP();
		gppfkeys=GP_PF_01;
		WL_wswap(&gppfkeys);

		if (0==resp)
		{
			if (hidden)
			{
				GPHIDDEN("DEFAULTS","EDITOR",2);
			}
			else
			{
				GPSTD("DEFAULTS","EDITOR",2);
			}
			GPMSG("To alter the current editor defaults, modify in place and press (ENTER),");
			GPMSG("or press (1) to exit without changing the defaults.");
		}
		else
		{
			strcpy(err_field,vse_err(resp));
			GPRES("DEFAULTS","EDITOR",1);
			GPMSG(err_field);
		}

		GPCTEXT("(Set to \"UPLOW\" to enable lower case input)",11,26);
		GPCTEXT("(Set to \"ANY\" For case-insensitive searching)",12,26);
		GPCTEXT("(Set to \"YES\" to view modification codes)", 13, 26 );

		/*
		**	TABS are handled special.  The field has embedded FACs before each tab slot.
		*/
		if (!first_tab_bad)
		{
			GPKW("TABS     ",tabs,29,10,6,"N");
		}
		else
		{
			GPRESP("TABS     ",tabs,29,10,6,"N");
		}

		if (resp!=RESP_MODE)
		{
			GPKW("MODE     ",mode,5,11,6,"A");
		}
		else
		{
			GPRESP("MODE     ",mode,5,11,6,"A");
		}

		if (resp!=RESP_CASE)
		{
			GPKW("CASE     ",cse,5,12,6,"A");
		}
		else
		{
			GPRESP("CASE     ",cse,5,12,6,"A");
		}

		if ( resp != RESP_SHOWMODS )
		{
			GPKW("SHOWMODS",showmods,3,13,6,"A");
		}
		else
		{
			GPRESP("SHOWMODS",showmods,3,13,6,"A");
		}

		GPENTER();
		GPPFS(&gppfkeys);
	
		if (display_and_read_gp())
		{
			return 1;
		}

		resp = 0;

		/*
		**	Reset all the tabs to normal
		*/
		tabs[2] = tabs[5] = tabs[8] = tabs[11] = tabs[14] = tabs[17] = tabs[20] = tabs[23] = tabs[26] = '\202';
		first_tab_bad = 0;

		if (!resp)
		{
			memset(tabstr,' ',80);
			tabstr[80]=0;

			for (i=0; i<30; i += 3)
			{
				if ((' ' != tabs[i] && !isdigit((int)tabs[i])) ||
				    (' ' != tabs[i+1] && !isdigit((int)tabs[i+1])) )
				{
					resp = RESP_TABS;
					if ( 0==i )
					{
						first_tab_bad = 1;
					}
					else
					{
						tabs[i-1] = '\222';
					}
				}
				else
				{
					int	col, pos;
					/*
					**	If any bad tabs then 0 is returned which will be caught in the next step.
					*/
					col = atoi(&tabs[i]);

				        pos = col + VSE_FIRST_SCREEN_COL - vse_edit_start_col - 1;
					if (pos>6 && pos<80)
					{
						tabstr[pos]='X';
					}
				}
			}
		}

		if (!resp)
		{
			if (memcmp(mode,"UPPER",5) && memcmp(mode,"UPLOW",5) )
			{
				resp = RESP_MODE;
			}
		}

		if (!resp)
		{
			if (memcmp(cse,"ANY  ",5) && memcmp(cse,"EXACT",5))
			{
				resp = RESP_CASE;
			}
		}

		if (!resp)
		{
			if (memcmp(showmods,"YES",3) && memcmp(showmods,"NO ",3))
			{
				resp = RESP_SHOWMODS;
			}
		}

		if (!resp)
		{
			break;
		}
	}

	tabs[2] = tabs[5] = tabs[8] = tabs[11] = tabs[14] = tabs[17] = tabs[20] = tabs[23] = tabs[26] = ' ';
	tabs[29] = (char)0;
	strcpy(vse_gp_defaults_tabs,tabs);
	strcpy((char*)vse_tab_setting,tabstr);

	memcpy(vse_gp_defaults_mode,mode,5);
	vse_gp_defaults_mode[5] = (char)0;

	memcpy(vse_gp_defaults_case,cse,5);
	vse_gp_defaults_case[5] = (char)0;

	memcpy(vse_gp_defaults_showmods,showmods,3);
	vse_gp_defaults_showmods[3] = (char)0;

	return 0;
}

int vse_options_gp(int hidden)
{
	char 	renumber[4];		/* Should we renumber upon replace */
	char 	number[7];		/* Start line number */
	char 	incr[7];		/* Increment of line number */
	char 	modcode[9];		/* Modcode */
	char	modcode_desc[80];
	int	resp = 0;
	int4	options_number, options_incr;
	char	err_field[81];

	strcpy( renumber, vse_gp_options_renumber );
	strcpy( number, vse_gp_options_number );
	strcpy( incr, vse_gp_options_incr );
	strcpy( modcode, vse_gp_options_modcode );

	if (vse_mod_width)
	{
		sprintf( modcode_desc, "columns %02d to %02d of added and changed lines?", 
			vse_mod_start_col, vse_mod_end_col );
	}

	for (;;)
	{
		GPSETUP();
		gppfkeys=GP_PF_01;
		WL_wswap(&gppfkeys);

		if (0==resp)
		{
			if (hidden)
			{
				GPHIDDEN("OPTIONS ","EDITOR",0);
			}
			else
			{
				GPSTD("OPTIONS ","EDITOR",0);
			}
		}
		else
		{
			strcpy(err_field,vse_err(resp));
			GPRES("OPTIONS ","EDITOR",1);
			GPMSG(err_field);
		}

		GPCTEXT( "To change the operating environment of the editor, please specify the", 9, 2 );
		GPCTEXT( "desired options below and press (ENTER).", 10, 2 );
		GPCTEXT( "Renumber the input file before every replace?", 12, 2 );
		GPCTEXT( "(YES/NO)", 12, 70 );

		if (RESP_RENUMBER == resp)
		{
			GPRESP( "RENUMBER", renumber, 3, 12, 54, "A" );
		}
		else
		{
			GPKW( "RENUMBER", renumber, 3, 12, 54, "A" );
		}

		GPCTEXT( "Default starting line number", 14, 2 );
		if (RESP_NUMBER == resp)
		{
			GPRESP( "NUMBER  ", number, 6, 14, 54, "N" );
		}
		else
		{
			GPKW( "NUMBER  ", number, 6, 14, 54, "N" );
		}

		GPCTEXT( "and increment to be used for renumbering?", 15, 2 );
		if (RESP_INCR == resp)
		{
			GPRESP( "INCR    ", incr, 6, 15, 54, "N" );
		}
		else
		{
			GPKW( "INCR    ", incr, 6, 15, 54, "N" );
		}

		if (vse_mod_width)
		{
			GPCTEXT( "Modification code to be placed in", 17, 2 );
			GPCTEXT( modcode_desc, 18, 2 );
			GPKW( "MODCODE ", modcode, vse_mod_width, 18, 54, "C" );
		}

		GPCTEXT( "Press (1) to return to the Set Command menu.", 24, 18 );

		GPENTER();
		GPPFS(&gppfkeys);

		if (display_and_read_gp())
		{
			return 1;
		}

		resp = 0;

		if (!resp)
		{
			if (memcmp(renumber,"YES",3) && memcmp(renumber,"NO ",3))
			{
				resp = RESP_RENUMBER;
			}
		}

		if (!resp)
		{
			resp = validate_numincr(number,&options_number,incr,&options_incr);
		}

		if (!resp)
		{
			break;
		}
	}

	memcpy( vse_gp_options_renumber, renumber, 3 );
	vse_gp_options_renumber[3] = (char)0;

	memcpy( vse_gp_options_number, number, 6 );
	vse_gp_options_number[6] = (char)0;

	memcpy( vse_gp_options_incr, incr, 6 );
	vse_gp_options_incr[6] = (char)0;

	if (vse_mod_width)
	{
		strncpy( vse_gp_options_modcode, modcode, vse_mod_width );
		vse_gp_options_modcode[vse_mod_width] = (char)0;
	}

	vse_options_number = options_number;
	vse_options_incr = options_incr;

	return 0;
}

int vse_no_file_created(int rest)
{
	int ret;
	int cnt;
	TEXT *txt;
	
	for(cnt=0, txt=text_first; txt; txt=txt->next, ++cnt);
	CLEAR_FIELD(file_info);
	CLEAR_FIELD(modified);
	if (vse_new_file)
	{
		strcpy(file_info,"There is no current input file. ");
	}
	else
	{
		sprintf(file_info,"Input file is %s",vse_sysname);
	}

	if (vse_file_changed)
	{
		strcpy(modified,"(The file has been modified.)");
	}

	sprintf(line_cnt,"%7d",cnt);
	vsescr_init(ed_scr);
	if (rest)
	{
		strcpy(ed_pfs,"0001X");	
		vsescr(restart_flds,ed_scr);
	}
	else
	{
		strcpy(ed_pfs,"0116X");	
		vsescr(no_file_created,ed_scr);
	}		
	memcpy(ed_oa,vse_default_oa,4);
	
	d_and_r_ed(TAB_TO_FIELD);
	int4_from_str2(ed_pfcode,&ret);
	return ret;
}

/*
**	Routine:	vse_xcopy_gp()
**
**	Function:	Issue a XCOPY getparm and validate the result.
**
**	Description:	Issue the getparm and validate the MODCODE field and that a filename
**			was specified.   Also handle switching between native and non-native
**			modes.
**
**	Arguments:
**	file_field	The wang style file name
**	library_field	The wang style library name
**	volume_field	The wang style volume name
**	sysname_field	The native filepath (untruncated)
**	start_field	The START field
**	end_field	The END field
**	target_field	The TARGET field
**	modcode_field	The MODCODE field
**	message_field	The error message field
**	message_code	The error message flag
**	resp		The respecify flag
**
**	Globals:	None
**
**	Return:
**	0		Got a name.
**	1		PF1 was requested
**
**	Warnings:	None
**
**	History:	
**	03/21/94	Written by gsl
**
*/
int vse_xcopy_gp(char *file_field, char *library_field, char *volume_field, char *sysname_field, 
		 char *start_field, char *end_field, char *target_field, char *modcode_field,
		 char *message_field, int message_code, int resp)
{
	int4	pick;

	gppfkeys=GP_PF_01|GP_PF_05;
	WL_wswap(&gppfkeys);

	for(;;)
	{		
		GPSETUP();

		switch(message_code)
		{
		case 0:
			GPSTD("XCOPY   ","EDITOR",0);
			break;
		case 1:
			GPRES("XCOPY   ","EDITOR",1);
			GPMSG(message_field);
			break;
		case 2: 
			GPRES("XCOPY   ","EDITOR",2);
			GPMSG("\224SORRY\204- The file you specified has invalid line numbers.");
			GPMSG("        \204The source language may be different or file needs renumbering.");
			break;

		case 3: /* NO FILENAME */
			GPRES("XCOPY   ","EDITOR",1);
			GPMSG("\224SORRY\204- The file information is incomplete.");
			break;
		}

		GPCTEXTU("External Copy Command",9,30);

		GPCTEXT("To copy a portion of another file into the present work file, fill in the",11,2);
		GPCTEXT("information below and press (ENTER).  Press (1) to return to the Special",12,2);
		GPCTEXT("Command Menu.",13,2);

		if(vse_native)
		{
			GPSYSNAME(sysname_field,15);
			GPCTEXT("(Press (5) for GENERIC entry mode.)",24,22);
		}
		else
		{
			GPFILE(file_field,   15,2);
			GPLIB(library_field, 15,28);
			GPVOL(volume_field,  15,51);
			GPCTEXT("IN",  15,25);
			GPCTEXT("ON",  15,48);
			GPCTEXT("(Press (5) for NATIVE  entry mode.)",24,22);
		}

		GPCTEXT("(Range of lines in the file specified above)",17,34);

		if (RESP_START == resp)
		{
			GPRESP("START   ",start_field,16,17,2,"U");
		}
		else
		{
			GPKW("START   ",start_field,16,17,2,"U");
		}
		if (RESP_END == resp)
		{
			GPRESP("END     ",end_field,16,18,2,"U");
		}
		else
		{
			GPKW("END     ",end_field,16,18,2,"U");
		}

		GPCTEXT("(Target line in the present work file)",20,34);
		if (RESP_TARGET == resp)
		{
			GPRESP("TARGET  ",target_field,16,20,2,"U");
		}
		else
		{
			GPKW("TARGET  ",target_field,16,20,2,"U");
		}


		if (vse_mod_width)
		{
			GPCTEXT("Retain the modification codes of the external file?",22,2);
			if (RESP_KEEPMC == resp)
			{
				GPRESP("MODCODE ",modcode_field,3,22,57,"U");
			}
			else
			{
				GPKW("MODCODE ",modcode_field,3,22,57,"U");
			}
			GPCTEXT("(YES/NO)",22,72);
		}

		GPENTER();
		GPPFS(&gppfkeys);
		
		switch(pick = display_and_read_gp())
		{
		case 1:
			return 1;
		case 5:
			vse_native = !vse_native;
			continue;
		}

		message_code = 0;
		resp = 0;

		if(!vse_native)
		{
			translate_name(file_field, library_field, volume_field, sysname_field);
		}

		if (' ' == sysname_field[0])
		{
			message_code = 3;
		}

		if (!message_code)
		{
			if (vse_mod_width && memcmp(modcode_field,"YES",3) && memcmp(modcode_field,"NO ",3))
			{
				resp = RESP_KEEPMC;
				strcpy(message_field,vse_err(RESP_KEEPMC));
				message_code = 1;
			}
		}

		if (!message_code)
		{
			return 0;
		}
	}
}


/*
**	Routine:	vse_file_changed_gp()
**
**	Function:	Issue the CHANGED getparm.
**
**	Description:	Warn user that file has changed and he should save it before compiling.
**
**	Arguments:
**	sysname		The file name.
**
**	Globals:	None
**
**	Return:
**	0		Continue
**	1		PF1 was press to terminate.
**
**	Warnings:	None
**
**	History:	
**	03/28/94	Written by GSL
**
*/
int vse_file_changed_gp(char *sysname)
{
	char	file_info[256];


	gppfkeys=GP_PF_01;
	WL_wswap(&gppfkeys);
	GPSETUP();
	GPRES("CHANGED ","EDITOR",1);
	GPMSG(" ");

	sprintf(file_info,"Input File:\204%-65s",sysname);

	GPCTEXT(file_info,10,2);
	/*	12345678901234567890123456789012345678901234567890123456789012345678901234567890 */
	GPCTEXT("The requested command uses the file on disk before current modifications",12,2);
	GPCTEXT("were made.",13,2);
	GPCTEXT("It is recommended you save this file before continuing.",14,2);

	GPCTEXT("       \204FILE HAS BEEN MODIFIED.",16,2);

	GPCTEXT("Press (ENTER) to continue with unmodified disk file.",18,2);
	GPCTEXT("Press (1)     to return to Special Command Menu.",19,2);


	GPENTER();
	GPPFS(&gppfkeys);
	
	if (display_and_read_gp())
	{
		/*
		**	PF1 = Abort
		*/
		return 1;
	}

	return 0;
}
/*
**	History:
**	$Log: vsegps.c,v $
**	Revision 1.14  2003/02/05 21:47:54  gsl
**	fix -Wall warnings
**	
**	Revision 1.13  2003/02/04 18:57:00  gsl
**	fix copyright header
**	
**	Revision 1.12  2003/02/04 18:29:13  gsl
**	fix -Wall warnings
**	
**	Revision 1.11  2002/08/01 16:00:54  gsl
**	type warnings
**	
**	Revision 1.10  2002/07/12 17:17:07  gsl
**	Global unique WL_ changes
**	
**	Revision 1.9  2002/07/10 04:27:40  gsl
**	Rename global routines with WL_ to make unique
**	
**	Revision 1.8  1996/09/03 22:24:06  gsl
**	drcs update
**	
**
**
*/
