/*
******************************************************************************
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
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


/*
**	File:		vsedit.c
**
**	Purpose:	?
**
**	Routines:	
**
**	vsedit()	The main entry point once the filename is known.
**	init_cobol()	Initialize for COBOL
**	init_c()	Initialize for C
**	init_text()	Initialize for TEXT
**	init_shell()	Initialize for SHELL
**	init_basic()	Initialize for BASIC
**	convert_tabs()
**	convert_cols()
**	int4_from_str2()
**	vse_menus()
**	load_file()	Loads the file
**	init_lastnum()
**	load_lines()
**	next_lineno()
**	vse_badnums()
**	save_file()
**	save_lines()
**	myfgets()	Reads the file and messages the input.
*/

#include <stdio.h>
#include <ctype.h>
#ifdef unix
#include <fcntl.h>
#endif

#include "idsistd.h"

#include "vsedit.h"
#include "vseglb.h"
#include "vsebasic.h"
#include "vsedfnd.h"
#include "vsegps.h"
#include "vsemov.h"
#include "vsetxt.h"
#include "vseutl.h"
#include "vwang.h"
#include "wperson.h"

static int nativecharmap = -1;		/* Flag to do automatic CHARMAP translation to native charset */


static void restart_all(void);
static void init_cobol(void);
static void init_basic(void);
static void init_proc(void);
static void init_c(void);
static void init_text(void);
static void init_shell(void);
static void convert_tabs(void);
static void vse_menus(void);
static int load_file(void);
static void init_lastnum(void);
static int load_lines(FILE *ff);
static int vse_badnums(void);
static int4 save_lines(FILE *ff, int4 start, int4 end);
static int check_lang(char *lang_string, int *type, char *ext);


/*----
Passed values are a system file name and an
upper case language, at the moment, COBOL C SHELL and TEXT are all
valid. The fields should be null terminated
------*/
int vsedit(char *name)
{  
	/* First time thru set nativecharmap flag */
	if (-1 == nativecharmap)
	{
		nativecharmap = 0;
		if (WL_get_wisp_option("NATIVECHARMAP"))
		{
			nativecharmap = 1;
			/*
			**	If using NATIVECHARMP then ensure the charmap is loaded
			*/
			vwang_load_charmap(0);
		}
	}

	restart_all();

        strcpy(vse_sysname,name);

        init_lastnum();
        vse_menu=EDIT_MENU;
        vse_lines = 0;
        vse_loading_menu();

	if (is_read_only())
	{
		if (vse_readonly_gp())
		{
			return(0);
		}
	}

	switch( load_file() )
	{
	case 0:
		vse_defaults_gp(1);
		vse_options_gp(1);
		vse_menus();
		return(0);

	case 1:
		return(0);

	case -1:
		return(1);
	}

	return(0);
}

static void restart_all(void)
{
	vse_file_changed = 0;
	vse_save_row=VSE_FIRST_SCREEN_ROW;
	vse_save_col=VSE_FIRST_SCREEN_COL;

	myfgets(NULL,0,NULL);	/* reset */

	restart_find();

	set_errfile_active(0);

	set_wang_style(!vse_native);
}

/*----
Set up options for each of the possible languages.
------*/

#define MODE_UPPER	"UPPER"
#define MODE_UPLOW	"UPLOW"

#define CASE_ANY	"ANY  "
#define CASE_EXACT	"EXACT"

#define	COBOL_TAB_STRING	"08 12 16 20 24 28 32 36 40 72"
#define BASIC_TAB_STRING	"10 20 30 40 50 60 70 72      "
#define PROC_TAB_STRING		"10 16 36 71                  "
#define	TEXT_TAB_STRING		"09 17 25 33 41 49 57 65 72   "

/*					            1         2         3         4         5         6         7         8 */
/*					  123456 78901234567890123456789012345678901234567890123456789012345678901234567890 */
#define DEFAULT_COB_TAB_STRING  	"         X   X   X   X   X   X   X   X   X                               X      "
#define DEFAULT_BASIC_TAB_STRING	"           X         X         X         X         X         X         X X      "

/*						          1         2         3         4         5         6         7         8 */
/*					  xxxxxx 12345678901234567890123456789012345678901234567890123456789012345678901234567890 */
#define DEFAULT_PROC_TAB_STRING		"                 X     X                   X                                  X "
#define DEFAULT_TAB_STRING		"                X       X       X       X       X       X       X       X      X" 

static void init_cobol(void)
{
	strcpy((char*)vse_tab_setting,DEFAULT_COB_TAB_STRING);

        strcpy(vse_gp_defaults_tabs,COBOL_TAB_STRING);
        strcpy(vse_gp_defaults_mode,MODE_UPPER);
        strcpy(vse_gp_defaults_case,CASE_EXACT);
        strcpy(vse_gp_defaults_columns,"07 72");

	vse_line_width     = 80;
        vse_edit_start_col =  7;
	vse_edit_end_col   = 72;
	vse_edit_width     = 66; /* vse_edit_end_col - vse_edit_start_col + 1; */
        vse_mod_start_col  = 73;
	vse_mod_end_col    = 80;
	vse_mod_width      =  8; /* vse_mod_end_col - vse_mod_start_col + 1; */
        vse_num_start_col  =  1;
}
static void init_basic(void)
{
	init_hash();
	strcpy((char*)vse_tab_setting,DEFAULT_BASIC_TAB_STRING);

        strcpy(vse_gp_defaults_tabs,BASIC_TAB_STRING);
        strcpy(vse_gp_defaults_mode,MODE_UPPER);
        strcpy(vse_gp_defaults_case,CASE_EXACT);
        strcpy(vse_gp_defaults_columns,"07 72");

	vse_line_width     = 80;
        vse_edit_start_col =  7;
	vse_edit_end_col   = 72;
	vse_edit_width     = 66; /* vse_edit_end_col - vse_edit_start_col + 1; */
        vse_mod_start_col  = 73;
	vse_mod_end_col    = 80;
	vse_mod_width      =  8; /* vse_mod_end_col - vse_mod_start_col + 1; */
        vse_num_start_col  =  1;
}
static void init_proc(void)
{
	strcpy((char*)vse_tab_setting,DEFAULT_PROC_TAB_STRING);

        strcpy(vse_gp_defaults_tabs,PROC_TAB_STRING);
        strcpy(vse_gp_defaults_mode,MODE_UPLOW);
        strcpy(vse_gp_defaults_case,CASE_ANY);
        strcpy(vse_gp_defaults_columns,"01 71");

	vse_line_width     = 80;
        vse_edit_start_col =  1;
	vse_edit_end_col   = 71;
	vse_edit_width     = 71; /* vse_edit_end_col - vse_edit_start_col + 1; */
        vse_mod_start_col  = 72;
	vse_mod_end_col    = 74;
	vse_mod_width      =  3; /* vse_mod_end_col - vse_mod_start_col + 1; */
        vse_num_start_col  = 75;
}
static void init_c(void)
{
        strcpy(vse_gp_defaults_mode,MODE_UPLOW);
        strcpy(vse_gp_defaults_case,CASE_ANY);
        strcpy(vse_gp_defaults_columns,"01 72");

	vse_line_width     = 72;
        vse_edit_start_col =  1;
	vse_edit_end_col   = 72;
	vse_edit_width     = 72; /* vse_edit_end_col - vse_edit_start_col + 1; */
        vse_mod_start_col  = 0;
	vse_mod_end_col    = 0;
	vse_mod_width      = 0;
        vse_num_start_col  = 0;
}
static void init_text(void)
{
        strcpy(vse_gp_defaults_mode,MODE_UPLOW);
        strcpy(vse_gp_defaults_case,CASE_ANY);
        strcpy(vse_gp_defaults_columns,"01 72");

	vse_line_width     = 72;
        vse_edit_start_col =  1;
	vse_edit_end_col   = 72;
	vse_edit_width     = 72; /* vse_edit_end_col - vse_edit_start_col + 1; */
        vse_mod_start_col  = 0;
	vse_mod_end_col    = 0;
	vse_mod_width      = 0;
        vse_num_start_col  = 0;
}
static void init_shell(void)
{
        strcpy(vse_gp_defaults_mode,MODE_UPLOW);
        strcpy(vse_gp_defaults_case,CASE_ANY);
        strcpy(vse_gp_defaults_columns,"01 72");

	vse_line_width     = 72;
        vse_edit_start_col =  1;
	vse_edit_end_col   = 72;
	vse_edit_width     = 72; /* vse_edit_end_col - vse_edit_start_col + 1; */
        vse_mod_start_col  = 0;
	vse_mod_end_col    = 0;
	vse_mod_width      = 0;
        vse_num_start_col  = 0;
}

/*----
Convert all tab entries to values
------*/
static void convert_tabs(void)
{
        int idx;
        for(idx=0; idx < NO_OF_TABS; ++idx)
                {
                int4_from_str2(&vse_gp_defaults_tabs[idx *3],&vse_tab[idx]);
                }
}

/*---
Convert next 2 bytes of str to int4 pointed to by value
------*/
void int4_from_str2(char *str, int4 *value)
{
        char buf[3];
        memcpy(buf,str,2);
        buf[2] = 0;
        *value = 0;
        *value = ATOI4(buf);
}

static void vse_menus(void)
{	
        scr_first = text_first;
        for(;;)
                {
                if(vse_menu == EDIT_MENU)
                        {
                        vse_standard_menu();
                        }
                else
                        {
                        vse_special_menu();
                        if( (vse_special_pick == 16) ||
                            (vse_special_pick == 4)   )
                                break;
                        }
                }
}

/*
**	Routine:	load_file()
**
**	Function:	Loads the input file.
**
**	Description:	{Full detailed description}...
**
**	Arguments:	None
**
**	Globals:	Lots
**
**	Return:
**	0		File loaded OK
**	1		Load was aborted
**	-1		Error while loading
**
**	Warnings:	None
**
**	History:	
**	03/15/94	Modified by GSL
**
*/
static int load_file(void)
{
        FILE *ff;
        int rc;
        if(vse_new_file)
                return(0);
        ff = fopen(vse_sysname,"r");
	if (ff)
	{
	        if ((rc = load_lines(ff)))
                {
                        free_text();
			if ( lang_type() == LANG_BASIC )
			{
				free_linenum();
			}
                }

	        fclose(ff);
	}
	else
	{
		rc = -1;
	}
        return(rc);
}
static int4 lastnum;
static void init_lastnum(void)
{
        lastnum = 0;
}

/*
**	Routine:	load_txt_line()
**
**	Function:	Loads one line from the file into a text struct.
**
**	Description:	Loads a single line from the open file.
**			It uses the global language constants to extract
**			the line number and modcode plus editable portion
**			of the text.
**			On EOF a NULL txtptr is returned.
**
**			An invalid linenumber will be returned as -1.
**
**	Arguments:
**	ff		The open file pointer.
**	txtptr		The text struct to return
**
**	Globals:	Language specific constants
**
**	Return:
**	0		OK
**	-1		out_of_space
**
**	Warnings:	This does not validate line numbering.
**
**	History:	
**	03/22/94	Written by GSL
**
*/
int load_txt_line(FILE *ff, TEXT **txtptr)
{
        char 	buf[256];
	char 	*mod_code;
	long	linenumber;
	int	len;

	*txtptr = NULL;

	if ( -1 == myfgets( buf, (int) vse_line_width, ff ) )
	{
		return(0);
	}

	len = strlen(buf);

	/*
	**	Extract the linenumber
	*/
	linenumber = -1;
	if(vse_num_start_col && len >= vse_num_start_col+6-1)
	{
		char	*num_ptr;
		long	num_calc;
		int	idx;
		int	the_digit;

/*		sscanf(&buf[vse_num_start_col - 1],"%06ld",&linenumber); */

		num_calc = 0;
		num_ptr = &buf[vse_num_start_col - 1];

		for( idx=0; idx < 6; idx++)
		{
			if (isdigit((int)num_ptr[idx]))
			{
				/*
				**	If a digit then convert it from a character to a number.
				*/
				the_digit = num_ptr[idx] - '0';
			}
			else
			{
				/*
				**	INVALID number
				*/
				num_calc = -1;
				break;
			}

			num_calc = (num_calc*10) + the_digit;
		}

		linenumber = num_calc;
	}

	/*
	**	Extract the modecode
	*/
	if (vse_mod_width && len >= vse_mod_start_col)
	{
		mod_code = (char *) calloc( vse_mod_width+1, sizeof( char ) );
		memcpy( mod_code, &buf[vse_mod_start_col-1], vse_mod_width );
		mod_code[vse_mod_width] = (char)0;
	}
	else
	{
		mod_code = NULL;
	}

	/*
	**	Truncate off trailing linenumbers and modcodes
	*/
	buf[vse_edit_start_col + vse_edit_width - 1] = (char)0;
	vse_trunc(buf);
	len = strlen(buf);

	/*
	**	Create the new text line.
	*/
	if (len >= vse_edit_start_col)
	{
                *txtptr = new_text(&buf[vse_edit_start_col - 1]);
	}
	else
	{
		*txtptr = new_text("");
	}
	if(!*txtptr)
	{
		out_of_space();
		return(-1);
	}

	/*
	**	Assign the line number and modecode
	*/
	(*txtptr)->lineno = linenumber;
	(*txtptr)->modfld = mod_code;

	return(0);
}

/*
**	Routine:	load_lines()
**
**	Function:	Loads the lines of the file into the text list.
**
**	Description:	Handles line numbers, modcodes, renumbering, split lines.
**
**	Arguments:
**	ff		The open file pointer.
**
**	Globals:	Lots and lots of em.
**
**	Return:
**	0		File is loaded
**	1		File load aborted.
**	-1		Error in loading.
**
**	Warnings:	None
**
**	History:	
**	03/15/94	Modified by GSL
**
*/
static int load_lines(FILE *ff)
{
        TEXT 	*txt;
	int	linecount;
	int	needs_renumbering;
	int	invalid_numbers;
	int	rc;

	linecount = 0;
        lastnum = 0;
	needs_renumbering = 0;
	invalid_numbers = 0;

        vse_lines = 0;

	for(;;)
        {
		if ((rc = load_txt_line(ff,&txt)))
		{
			return rc;
		}
		if (!txt)
		{
			break;
		}

		linecount += 1;

		/*
		**	Assign the line number and check if renumbering is required.
		*/
		if (!vse_num_start_col)
		{
			/*
			**	For un-numbered language types use relative numbering.
			*/
			txt->lineno = lastnum += 1;
		}
		else if (-1 == txt->lineno)
		{
			/*
			**	An INVALID line number was found so replace in with
			**	the lastnum incremented.
			**
			**	This is an attempt to avoid a full renumbering of the file.
			*/
			txt->lineno = lastnum += 1;
			invalid_numbers = 1;
		}
		else if (needs_renumbering || txt->lineno <= lastnum)
		{
			txt->lineno = lastnum = 0;
		}
		else
		{
			lastnum = txt->lineno;
		}

		if (lastnum < 1 || lastnum > 999999)
		{
			needs_renumbering = 1;
		}

		/*
		**	Append this line.
		*/
                append_text(txt);

		if (lang_type() == LANG_BASIC)
		{
			find_linenum( txt );
		}
        }

	if (0 == linecount)
	{
		return 0;
	}

	/*
	**	The file is now loaded.
	**	Check if renumbering is needed or warn if lines were split.
	*/

	if (was_line_too_long())
	{
		if (vse_longline_gp(vse_sysname))
		{
			return(1);
		}
		vse_file_changed = 1;
	}

	if (needs_renumbering)
	{
		char	number_field[7], incr_field[7];
		int4	number, incr;
		char	gp_error[80];

		gp_error[0] = (char)0;
		sprintf(number_field,"%-6d",vse_options_number);
		sprintf(  incr_field,"%-6d",vse_options_incr);

		while(needs_renumbering)
		{
			if (vse_badnums_gp(number_field, incr_field, gp_error))
			{
				return(1);
			}

			if ((rc = validate_numincr(number_field, &number, incr_field, &incr)))
			{
				strcpy(gp_error, vse_err(rc));
			}
			else
			{
				if ((number + (incr * (linecount - 1))) > 999999)
				{
					strcpy(gp_error, vse_err(RESP_NUMINCR));
				}
				else
				{
					do_renumber(text_first,text_last,number,incr);
					needs_renumbering = 0;
				}
			}
		}
	}

        return(0);
}

/*----
Returns the next line number
The second condition (num <= lastnum) is actually an error in the input
file. WANG handles this with a BADNUMS getparm asking the user if
the file should be numbered, or allowing a PFKey to respecify
the input language. In this example, In this example vse_badnums() stub
is set to return true so the renumber goes ahead.
------*/
int4 next_lineno(int4 num)
{
	if(!vse_num_start_col)
	{
                return(lastnum += vse_options_incr);
        }

        if(num <= lastnum)
        {
                if(vse_badnums())
                        return(lastnum += vse_options_incr);
                else
                        return(-1);
        }
        return(lastnum = num);
}
/*----
This stub routine should return 1 if the user wants to proceed
and let the loader renumber the file.
or zero if the user wants to abort
------*/
static int vse_badnums(void)
{
        return(1);
}
/*----
Some sort of can't load message goes here
------*/
void out_of_space(void)
{
}

/*
**	Routine:	save_file()
**
**	Function:	Save the edit buffer to a file.
**
**	Description:	{Full detailed description}...
**
**	Arguments:
**	sysname		The native filepath (truncated)
**	start		The starting line number to save.
**	end		The ending line number to save.
**
**	Globals:	None
**
**	Return:
**	0-n		The record count
**	-1		Save failed
**
**	Warnings:	None
**
**	History:	
**	03/11/94	Modified by GSL
**
*/
int4 save_file(char *sysname, int4 start, int4 end)
{
        FILE 	*ff;
        int4 	cnt;

        ff = fopen(sysname,"w");
        if(!ff)
	{
		return(-1);
	}
        cnt = save_lines(ff,start,end);
        fclose(ff);
        return(cnt);
}

static int4 save_lines(FILE *ff, int4 start, int4 end)
{
	int4	cnt;
        TEXT 	*txt;
	char	outbuf[256];

	cnt = 0;
        txt = text_first;
        while(txt)
        {
		if (txt->lineno > end)
		{
			break;
		}

		if (txt->lineno >= start)
		{
			cnt++;

                	if(vse_num_start_col)
                	{
				if (lang_type()==LANG_BASIC || lang_type()==LANG_COBOL)
				{
					if (txt->modfld)
					{
						sprintf(outbuf, "%06ld%-66s%s", (long)txt->lineno, txt->text, txt->modfld );
					}
					else
					{
						sprintf(outbuf, "%06ld%s", (long)txt->lineno, txt->text );
					}
				}
				else if (lang_type()==LANG_PROC)
				{
					char	*mod;

					mod = txt->modfld;
					if (!mod) mod = "   ";

					sprintf(outbuf, "%-71s%-3s%06ld",txt->text, mod, (long)txt->lineno);
				}
                	    	else if(vse_num_start_col == 1)
                	    		sprintf(outbuf, "%06ld%s",(long)txt->lineno,txt->text);
				else
					sprintf(outbuf, "%-74s%06ld",txt->text,(long)txt->lineno);
                	}
                	else
                	{
                	        sprintf(outbuf,"%s",txt->text);
                	}

			if (nativecharmap)
			{
				/*
				**	Translate wang charset into ansi charset following a vwang read.
				*/
				vwang_wang2ansi((unsigned char *)outbuf, strlen(outbuf));
			}

			fprintf(ff,"%s\n",outbuf);

		}
                txt = txt->next;
        }

        return(cnt);
}

static int myfgets_line_too_long = 0;

int was_line_too_long(void)
{
	return myfgets_line_too_long;
}

/*
**	Routine:	myfgets()
**
**	Function:	Get the next line from the file, handle spliting long lines.
**
**	Description:	This returns the next line from the file.
**			The lines are untabified, with a max strlen of size.
**			Trailing newlines are removed.
**			Long lines are split and the flag mygets_line_too_long is set.
**
**			Call with a NULL file to reset statics when switching files.
**
**	Arguments:
**	buf		The returned line.
**	size		The max strlen of buf.
**	file		The file pointer.
**
**	Globals:
**	myfgets_line_too_long
**
**	Return:
**	-1		EOF
**	0		Got a line
**
**	Warnings:	None
**
**	History:	
**	03/15/94	Modified by GSL
**
*/
int myfgets(char *buf, int size, FILE *file)
{
        static char linebuf[256];
        static int bytesleft=0, curpos=0;
        int 	bytes;

	if (!file)
	{
		myfgets_line_too_long = 0;
		bytesleft = 0;
		curpos = 0;
		return 0;
	}

	if (bytesleft)
	{
		myfgets_line_too_long = 1;
	}
	else
	{
                if (NULL==fgets(linebuf,sizeof(linebuf),file))
		{
			return -1;
		}

                vse_untabify(linebuf,sizeof(linebuf));
                vse_trunc(linebuf); /* remove trailing spaces and newlines */

                bytesleft=strlen(linebuf);
                curpos=0;

		if (nativecharmap)
		{
			/*
			**	Translate ansi charset into wang charset in preparation of vwang call.
			*/
			vwang_ansi2wang((unsigned char *)linebuf, bytesleft);
		}

	}

	if (bytesleft > size)
		bytes = size;
	else
		bytes = bytesleft;

	memcpy(buf,&linebuf[curpos],bytes);
	buf[bytes]=(char)0;
	bytesleft -= bytes;
	curpos += bytes;
	return 0;
}

static int	g_lang_type = -1;
static char	g_lang_ext[20] = "";

int init_lang(char *lang_string)
{
	int	rc = 0;
	int	type;
	char	ext[20];

	if ((rc = check_lang(lang_string, &type, ext)))
	{
		return rc;
	}

	/*
	**	The extension can change without the language changing.
	*/
	strcpy(g_lang_ext,ext);

	if (g_lang_type != type)
	{
		/*
		**	Only reset the OPTIONS and DEFAULTS if the language changes.
		*/

		g_lang_type = type;

		strcpy((char*)vse_tab_setting,DEFAULT_TAB_STRING);
	        strcpy(vse_gp_defaults_tabs,TEXT_TAB_STRING);
	        strcpy(vse_gp_defaults_showmods,"NO ");

		strcpy(vse_gp_options_modcode,"        ");

		switch(g_lang_type)
		{
		case LANG_COBOL:
			init_cobol();
			break;
		case LANG_PROC:
			init_proc();
			break;
		case LANG_SHELL:
			init_shell();
			break;
		case LANG_BASIC:
			init_basic();
			break;
		case LANG_C:
			init_c();
			break;
		case LANG_NONE:
		default:
			init_text();
			break;
		}

		convert_tabs();
	}

	return rc;
}

static int check_lang(char *lang_string, int *type, char *ext)
{
	int	rc = 0;
	char	temp[20];
	char 	the_lang_string[VSE_LANGUAGE_LEN+1];
	int	the_lang_type = LANG_NONE;
	char	*the_lang_ext;

	strncpy(the_lang_string,lang_string,VSE_LANGUAGE_LEN);
	the_lang_string[VSE_LANGUAGE_LEN] = (char)0;
	WL_upper_string(the_lang_string);
	vse_trunc(the_lang_string);

	sprintf(temp,"%s!",the_lang_string);

	/*		         1         2         3         4         5         6         7         8         9 */
	/*             0123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890 */
	switch(strpos("!WCB!COB!CBL!COBOL!WPS!PROC!PROCEDURE!BAS!BASIC!SH!SHELL!C!CC!TEXT!NONE!COM!BAT!BATCH!",temp))
	{
		case 0: /* (blank) */
			the_lang_type = LANG_NONE;
			the_lang_ext = "";
			break;
		case 1:	/* WCB */
			the_lang_type = LANG_COBOL;
			the_lang_ext = "wcb";
			break;
		case 5:	/* COB */
			the_lang_type = LANG_COBOL;
			the_lang_ext = "cob";
			break;
		case 9: /* CBL */
			the_lang_type = LANG_COBOL;
			the_lang_ext = "cbl";
			break;
		case 13: /* COBOL */
			the_lang_type = LANG_COBOL;
			the_lang_ext = "wcb";
			break;
		case 19: /* WPS */
		case 23: /* PROC */
		case 28: /* PROCEDURE */
			the_lang_type = LANG_PROC;
			the_lang_ext = "wps";
			break;
		case 38: /* BAS */
		case 42: /* BASIC */
			the_lang_type = LANG_BASIC;
			the_lang_ext = "bas";
			break;
		case 48: /* SH */
		case 51: /* SHELL */
			the_lang_type = LANG_SHELL;
			the_lang_ext = "sh";
			break;
		case 57: /* C */
		case 59: /* CC */
			the_lang_type = LANG_C;
			the_lang_ext = "c";
			break;
		case 62: /* TEXT */
		case 67: /* NONE */
			the_lang_type = LANG_NONE;
			the_lang_ext = "";
			break;
		case 72: /* COM */
			the_lang_type = LANG_NONE;
			the_lang_ext = "com";
			break;
		case 76: /* BAT */
		case 80: /* BAT */
			the_lang_type = LANG_NONE;
			the_lang_ext = "bat";
			break;
		default: /* UnKnown */
			the_lang_type = LANG_NONE;
			the_lang_ext = "";
			rc = 1;
			break;
	}

	if (type)
	{
		*type = the_lang_type;
	}
	if (ext)
	{
		strcpy(ext,the_lang_ext);
	}

	return rc;
}

int lang_type(void)
{
	return g_lang_type;
}

char *lang_ext(void)
{
	return g_lang_ext;
}

char *vse_err(int err_flag)
{
	switch(err_flag)
	{			      /*1   234567   8901234567890123456789012345678901234567890123456789012345678901234567890 */
	case 0:			return("");
	case RESP_NUMBER:	return("\224SORRY\204- Invalid NUMBER. Please respecify.");
	case RESP_INCR:		return("\224SORRY\204- Invalid INCR. Please respecify.");
	case RESP_START:	return("\224SORRY\204- Invalid START. Please respecify.");
	case RESP_END:		return("\224SORRY\204- Invalid END. Please respecify.");
	case RESP_NUMINCR:	return("\224SORRY\204- Choose smaller NUMBER or INCR.");
	case RESP_RANGE:	return("\224SORRY\204- Invalid START/END range. Please respecify.");
	case RESP_EMPTY:	return("\224SORRY\204- START/END range is empty. Please respecify.");
	case RESP_TARGET:	return("\224SORRY\204- Invalid TARGET. Please respecify.");
	case RESP_TABS:		return("\224SORRY\204- Invalid TABS. Please respecify.");
	case RESP_MODE:		return("\224SORRY\204- MODE must be UPPER or UPLOW. Please respecify.");
	case RESP_CASE:		return("\224SORRY\204- CASE must be EXACT or ANY. Please respecify.");
	case RESP_SHOWMODS:	return("\224SORRY\204- SHOWMODS must be YES or NO. Please respecify.");
	case RESP_RENUMBER:	return("\224SORRY\204- RENUMBER must be YES or NO. Please respecify.");
	case RESP_KEEPMC:	return("\224SORRY\204- MODCODE must be YES or NO. Please respecify.");
	default:		return("\244ERROR");
	}
}

static int g_wang_style = 1;

int wang_style_work_file(void)
{
	return g_wang_style;
}

int set_wang_style(int style)
{
	return g_wang_style = style;
}
/*
**	History:
**	$Log: vsedit.c,v $
**	Revision 1.22  2010/01/10 00:36:15  gsl
**	refactor utils to add vse_ prefix to avoid conflicts with trunc
**	vse_trunc
**	
**	Revision 1.21  2003/02/05 21:47:53  gsl
**	fix -Wall warnings
**	
**	Revision 1.20  2003/02/04 18:57:00  gsl
**	fix copyright header
**	
**	Revision 1.19  2003/02/04 18:29:13  gsl
**	fix -Wall warnings
**	
**	Revision 1.18  2002/08/01 16:00:54  gsl
**	type warnings
**	
**	Revision 1.17  2002/07/11 14:34:00  gsl
**	Fix WL_ unique globals
**	
**	Revision 1.16  2002/07/10 21:06:39  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.15  1997/12/19 21:51:38  gsl
**	fix warnings
**	
**	Revision 1.14  1997-12-18 20:15:54-05  gsl
**	Fix CHARMAP translation
**
**	Revision 1.13  1997-12-18 09:16:24-05  gsl
**	add missing includes
**
**	Revision 1.12  1997-12-17 21:42:33-05  gsl
**	Add support for NATIVECHARMAP option
**
**	Revision 1.11  1996-09-03 18:24:02-04  gsl
**	drcs update
**
**
**
*/
