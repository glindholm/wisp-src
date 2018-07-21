/*
******************************************************************************
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
** NOTICE:
** Confidential, unpublished property of Shell Stream Software LLC.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of Shell Stream Software LLC
** is strictly prohibited.
** 
******************************************************************************
*/


#include <stdio.h>
#include "idsisubs.h"
#include "paths.h"
#include "vwang.h"
#include "wisplib.h"
#include "vssubs.h"
#include "wfiledis.h"

#include "vseglb.h"
#include "vsescr.h"
#include "vsedit.h"
#include "vsegps.h"
#include "vsemov.h"
#include "vsetxt.h"
#include "vseutl.h"
#include "vsedscr.h"
#include "vsebasic.h"


static unsigned char spc_scr[1924];
static char spc_pfs[65];
static char set_pfs[]="010203X";
static unsigned char spc_func[]={03};
static unsigned char spc_lines[]={24};
static char spc_pfcode[3];
static char set_pfcode[3];
static unsigned char spc_status[3];

static VSESCR_FLDS(spc_flds) = {
{LEN(0)	ROW(1)	COL(2)	VALUE(VSE_COPYRIGHT)},
{LEN(0)	ROW(2)	COL(2)	FROM(vse_file_message)},
{LEN(0)	ROW(3)	COL(2)	FROM(vse_lines_message)},
{LEN(0)	ROW(4)	COL(2)	BRIGHT(vse_stat_message)},
{LEN(0)	ROW(6)	COL(2)	VALUE("Please select the desired function and press the appropriate PF key:")},
{LEN(0)	ROW(8)	COL(2)	VALUE(" (1) Display       - Resume text editing")},
{LEN(0)	ROW(9)	COL(2)	VALUE(" (2) Set           - Set workstation defaults")},
{LEN(0)	ROW(10)	COL(2)	VALUE(" (3) Menu          - Activate the normal menu")},
{LEN(0)	ROW(11)	COL(2)	VALUE(" (4) Restart       - Edit another file")},
{LEN(0)	ROW(12)	COL(2)	VALUE(" (5) Create        - Create a new file from the edited text")},
{LEN(0)	ROW(15)	COL(2)	VALUE(" (8) External Copy - Copy a range of lines from another file")},
{LEN(0)	ROW(20)	COL(2)	VALUE("(13) Utilities     - Run utilities or other programs")},
{LEN(0)	ROW(23)	COL(2)	VALUE("(16) Exit          - End Editor processing")},
{LASTITEM}
};

/*
{LEN(0)	ROW(16)	COL(2)	VALUE(" (9) Run           - Compile and run program")},
{LEN(0)	ROW(22)	COL(2)	VALUE("(15) Print         - Print the edited text")},
*/

static VSESCR_FLDS(spc_replace_flds) = {
{LEN(0)	ROW(13)	COL(2)	VALUE(" (6) Replace       - Replace the input file with the edited text")},
{LASTITEM}
};

static VSESCR_FLDS(spc_renumber_flds) = {
{LEN(0)	ROW(14)	COL(2)	VALUE(" (7) Renumber      - Generate new line numbers for the edited text")},
{LASTITEM}
};

static VSESCR_FLDS(spc_wac_flds) = {
{LEN(0)	ROW(17)	COL(2)	VALUE("(10) Compile       - WISP and compile program")},
{LASTITEM}
};

static VSESCR_FLDS(spc_compile_flds) = {
{LEN(0)	ROW(17)	COL(2)	VALUE("(10) Compile       - Compile the program")},
{LASTITEM}
};

static VSESCR_FLDS(spc_errors_flds) = {
{LEN(0)	ROW(18)	COL(2)	VALUE("(11) Errors        - Display compilation errors")},
{LASTITEM}
};

static VSESCR_FLDS(spc_listing_flds) = {
{LEN(0)	ROW(19)	COL(2)	VALUE("(12) Listing       - Display COB file")},
{LASTITEM}
};

static VSESCR_FLDS(set_flds) = {
{LEN(0)	ROW(1)	COL(35)	ULINEVALUE("Set Command")},
{LEN(0) ROW(4)  COL(2) VALUE("Please select the desired function and press the appropriate PF Key.")},
{LEN(0) ROW(5)  COL(2) VALUE("Press (1) to return to the special command menu.")},
{LEN(0) ROW(7)  COL(4) ULINEVALUE("Editor Options")},
{LEN(0) ROW(8)  COL(6) VALUE("(2)  Set tabs, upper/lower case and search modes")},
{LEN(0) ROW(9)  COL(6) VALUE("(3)  Set automatic renumber modes")},
{LEN(0) ROW(10) COL(11) VALUE("and modification code")},
{LASTITEM}
};


static void vse_set_command_menu(void);
static void init_special_menu(void);
static void init_set_command_menu(void);
static void vse_special_dispatch(void);
static void vse_set_dispatch(void);
static int vse_create_file(void);
static int vse_replace_file(void);
static int vse_renumber(void);
static void vse_print(void);
static int vse_utilities(void);
static int vse_run_program(char file[8], char lib[8], char vol[6], char *message);

#ifdef unix
static int vse_compile_and_run(void);
static int vse_compile(void);
static int vse_compile_command(void);
static int vse_run_command_with_log(char *cmd, char *logfile);
static int vse_errors(void);
static int vse_listing(void);
static int vse_display(char *filename);
#endif

static int vse_build_errfile(char *sysname);
static char *vse_errfile(void);
static int is_errfile_active(void);


void vse_special_menu(void)
{
	init_special_menu();
	vwang(spc_func,spc_scr,spc_lines,spc_pfs,spc_pfcode,spc_status);
	int4_from_str2(spc_pfcode,&vse_special_pick);
	vse_special_dispatch();
}

static void vse_set_command_menu(void)
{
	init_set_command_menu();
	vwang(spc_func,spc_scr,spc_lines,set_pfs,set_pfcode,spc_status);
	int4_from_str2(set_pfcode,&vse_set_command_pick);
	vse_set_dispatch();
}

static void init_special_menu(void)
{
	vse_save_row=ed_oa[OA_CURSOR_ROW];
	vse_save_col=ed_oa[OA_COL];

	memcpy(spc_scr,vse_default_oa,sizeof(vse_default_oa));
	vse_top_messages();
	vsescr_init(spc_scr);
	vsescr(spc_flds,spc_scr);

	strcpy(spc_pfs,"01020304050813");
	if (!vse_new_file && !is_read_only())
	{
		vsescr(spc_replace_flds,spc_scr);
		strcat(spc_pfs,"06");
	}
	if (text_first)
	{
		vsescr(spc_renumber_flds,spc_scr);
		strcat(spc_pfs,"07");
	}

#ifdef unix
	if (text_first)
	{
		if (0==strcmp(lang_ext(),"wcb"))
		{
			vsescr(spc_wac_flds,spc_scr);
		}
		else
		{
			vsescr(spc_compile_flds,spc_scr);
		}
		strcat(spc_pfs,"10");
	}
	if (is_errfile_active())
	{
		vsescr(spc_errors_flds,spc_scr);
		strcat(spc_pfs,"11");
	}
	if (text_first 
		&& lang_type() == LANG_COBOL 
		&& 0==strcmp(lang_ext(),"wcb")
		&& is_errfile_active())
	{
		/*
		**	Only list COB file after a WAC has been done.
		*/
		vsescr(spc_listing_flds,spc_scr);
		strcat(spc_pfs,"12");
	}
#endif /* unix */

	strcat(spc_pfs,"16X");
}

static void init_set_command_menu(void)
{
	vse_save_row=ed_oa[OA_CURSOR_ROW];
	vse_save_col=ed_oa[OA_COL];

	memcpy(spc_scr,vse_default_oa,sizeof(vse_default_oa));
	vse_top_messages();
	vsescr_init(spc_scr);
	vsescr(set_flds,spc_scr);
}

static void vse_special_dispatch(void)
{
	switch(vse_special_pick)
		{
		case 1:
			vse_ed_scr(vse_special_pick);
			break;
		case 2:
			if( vse_num_start_col || vse_mod_start_col )
			{
				for ( ;; )
				{
					vse_set_command_menu();	
					if ( vse_set_command_pick == 1 )
						break;
				}
			}
			else
				vse_set();
			break;
		case 3:
			vse_menu = EDIT_MENU;
			break;
		case 4:
			if (vse_file_changed)
			{
				int ret;
				
				ret=vse_no_file_created(1);
				if (ret==1)
				{
					vse_special_pick=1;
					break;
				}
			}
			free_text();

			if ( lang_type() == LANG_BASIC )
				free_linenum();
			break;
		case 5:
			vse_create_file();
			break;
		case 6:
			vse_replace_file();
			break;
		case 7:
			vse_renumber();
			break;
		case 8:
			vse_xcopy();
			break;

#ifdef unix
		case 9:
			vse_compile_and_run();
			break;
		case 10:
			vse_compile();
			break;
		case 11:
			vse_errors();
			break;
		case 12:
			vse_listing();
			break;
#endif /* unix */

		case 13:
			vse_utilities();
			break;
		case 15:
			vse_print();
			break;
		case 16:
			if (vse_file_changed)
			{
				int ret;
				
				ret=vse_no_file_created(0);
				if (ret==1)
				{
					vse_special_pick=1;
					break;
				}
			}
			free_text();

			if ( lang_type() == LANG_BASIC )
				free_linenum();
			break;
		default:
			break;
		}
}

static void vse_set_dispatch(void)
{
	switch ( vse_set_command_pick )
	{
		case	1:	break;

		case	2:	vse_set();
					break;

		default	 :	vse_options_gp(0);
	}
}

static int vse_create_file(void)
{
	unsigned char 	write_func=1;
	int4	cnt,start,end;

	char file[VSE_FILENAME_LEN+1];
	char library[VSE_LIBNAME_LEN+1];
	char volume[VSE_VOLNAME_LEN+1];
	char sysname[VSE_SYSNAME_LEN+1];

	
	if (vse_output(file, library, volume, sysname, &start, &end))
	{
		return 1;
	}
	
	sprintf(vse_stat_message,"Create in progress");
	init_special_menu();
	vwang(&write_func,spc_scr,spc_lines,spc_pfs,spc_pfcode,spc_status);

	vse_trunc(sysname);
	cnt = save_file(sysname,start,end);

	if (cnt < 0)
	{
		sprintf(vse_stat_message,"\224ERROR\204- Unable to WRITE file %s", sysname);
		return -1;
	}

	sprintf(vse_stat_message,"File %s created with %7ld records",sysname,(long)cnt);

	if (vse_new_file)
	{
		if ((text_first && text_first->lineno==start && text_last->lineno==end) || (1==start && 999999==end))
		{
			/*
			**	The input name only gets set if "ALL" records get written.
			*/
		        vse_new_file = 0;
			vse_file_changed = 0;

			memcpy(vse_gp_input_file,    file,    VSE_FILENAME_LEN+1);
			memcpy(vse_gp_input_library, library, VSE_LIBNAME_LEN+1);
			memcpy(vse_gp_input_volume,  volume,  VSE_VOLNAME_LEN+1);
			strcpy(vse_sysname,sysname);
			set_wang_style(!vse_native);
		}
	}
	return 0;
}

static int vse_replace_file(void)
{
	unsigned char write_func=1;
	int4 cnt,start,end;
	char sysname[VSE_SYSNAME_LEN+1];

	strcpy(sysname,vse_sysname);

	strcpy(vse_stat_message,"Replace in progress");

	init_special_menu();
	vwang(&write_func,spc_scr,spc_lines,spc_pfs,spc_pfcode,spc_status);

	if (text_first && 0==memcmp( vse_gp_options_renumber, "YES", 3 ))
	{
		int	rc, respecify;
		int4	number, incr, start, end, count;
		char	number_field[7], incr_field[7], start_field[17], end_field[17];

		number = vse_options_number;
		incr = vse_options_incr;
		start = text_first->lineno;
		end = text_last->lineno;

		sprintf(number_field,"%-6d",number);
		sprintf(incr_field,"%-6d",incr);
		strcpy(start_field,"ALL             ");
		strcpy(end_field,  "                ");

		for(;;)
		{
			respecify = vse_renumberer(number, incr, start, end, &count);
			if (0==respecify)
			{
				break;
			}

			rc=vse_renumber_gp(number_field,&number,incr_field,&incr,start_field,&start,end_field,&end,respecify);
	
			if (rc==1)
			{
				/* 
				**	Go ahead without renumbering
				*/
				break;
			}
		}
	}

	start = 1;
	end = 999999;
	cnt = save_file(sysname,start,end);

	if (cnt < 0)
	{
		sprintf(vse_stat_message,"\224ERROR\204- Unable to WRITE file %s", sysname);
		return -1;
	}

	vse_file_changed = 0;

	sprintf(vse_stat_message,"File %s written with %7ld records",sysname,(long)cnt);

	return 0;
}

static int vse_renumber(void)
{
		int	rc, respecify;
		int4	number, incr, start, end, count;
		char	number_field[7], incr_field[7], start_field[17], end_field[17];

		respecify = 0;
		number = vse_options_number;
		incr = vse_options_incr;

		sprintf(number_field,"%-6d",number);
		sprintf(  incr_field,"%-6d",incr);
		strcpy(  start_field,"ALL             ");
		strcpy(    end_field,"                ");

		for(;;)
		{
			rc=vse_renumber_gp(number_field,&number,incr_field,&incr,start_field,&start,end_field,&end,respecify);
	
			if (rc==1)
			{
				/* 
				**	Go ahead without renumbering
				*/
				return 1;
			}

			respecify = vse_renumberer(number, incr, start, end, &count);
			if (0==respecify)
			{
				return 0;
			}

		}
}

void vse_set(void)
{
	vse_defaults_gp(0);
}
static void vse_print(void)
{
	/* Not yet implemented */	
}

/*
**	Routine:	vse_utilities()
**
**	Function:	Special Menu UTILITIES PF13
**
**	Description:	Emulates the UTILITIES menu.
**			Allows user to run a program or utility.
**			Only WANG style file specs are allowed as it uses the LINK VSSUB.
**
**	Arguments:	None
**
**	Globals:	None
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	
**	03/24/94	Written by GSL
**
*/
static int vse_utilities(void)
{
	static	char	file_field[9], library_field[9], volume_field[7], message_field[81];

	static VSESCR_FLDS(vse_utilities_flds) = {
		{LEN(0)	ROW(1)	COL(2)	VALUE(VSE_COPYRIGHT)},
		{LEN(0)	ROW(3)	COL(31)	ULINEVALUE("Utility Processing")},
		{LEN(0)	ROW(5)	COL(2)	VALUE(message_field)},
		{LEN(0)	ROW(7)	COL(2)	VALUE("Please specify the program to be run and press (ENTER).")},
		{LEN(0)	ROW(8)	COL(2)	VALUE("To run a utility program, press the appropriate PF key.")},
		{LEN(0)	ROW(9)	COL(2)	VALUE("Press (1) to return to the Special Command Menu.")},
		{LEN(0)	ROW(11)	COL(11)	VALUE("Run FILE")},
		{LEN(0)	ROW(11)	COL(20)	USING(file_field)},
		{LEN(0)	ROW(11)	COL(29)	VALUE("in LIBRARY")},
		{LEN(0)	ROW(11)	COL(41)	USING(library_field)},
		{LEN(0)	ROW(11)	COL(50)	VALUE("on VOLUME")},
		{LEN(0)	ROW(11)	COL(60)	USING(volume_field)},
		{LEN(0)	ROW(13)	COL(4)  VALUE(" (2)   WCOPY    - Copies files or libraries")},
		{LEN(0)	ROW(14)	COL(4)  VALUE(" (3)   DISPLAY  - Displays files")},
		{LEN(0)	ROW(15)	COL(4)  VALUE(" (4)   WSORT    - Sorts files")},
		{LASTITEM}
		};


	CLEAR_STRING(file_field);
	CLEAR_STRING(library_field);
	CLEAR_STRING(volume_field);
	strcpy(message_field,"");

	for(;;)
	{
		memcpy(spc_scr,vse_default_oa,sizeof(vse_default_oa));
		vsescr_init(spc_scr);
		vsescr(vse_utilities_flds,spc_scr);

		strcpy(spc_pfs,"0001020304X");

		vwang(spc_func,spc_scr,spc_lines,spc_pfs,spc_pfcode,spc_status);
		int4_from_str2(spc_pfcode,&vse_special_pick);

		strcpy(message_field,"");

		switch(vse_special_pick)
		{
		case 0:
			vseunscr(vse_utilities_flds,spc_scr);
			if (0==memcmp(file_field,"        ",8))
			{
				strcpy(message_field,"\224SORRY\204- The FILE name must be non-blank");
			}
			else
			{
				vse_run_program(file_field, library_field, volume_field, message_field);
			}
			break;
		case 1:
			return(0);
		case 2:
			vse_run_program("WCOPY   ", "        ", "      ", message_field);
			break;
		case 3:
			vse_run_program("DISPLAY ", "        ", "      ", message_field);
			break;
		case 4:
			vse_run_program("WSORT   ", "        ", "      ", message_field);
			break;
		}
	}
}

/*
**	Routine:	vse_run_program()
**
**	Function:	Run a program using LINK
**
**	Description:	If lib or vol is specified then a LINK type "P" is used,
**			otherwise a type " " is tried then if not found a type "S" is tried.
**
**	Arguments:
**	file		The Wang style file name 
**	lib		The Wang style library name 
**	vol		The Wang style volume name 
**	message		The returned message
**
**	Globals:	None
**
**	Return:		The LINK compcode.
**
**	Warnings:	None
**
**	History:	
**	03/24/94	Written by GSL
**
*/
static int vse_run_program(char file[8], char lib[8], char vol[6], char *message)
{
	int4	compcode, returncode;


	compcode = 0;								/* Initialize the comp & return codes	*/
	returncode = 0;

	if (' ' != lib[0] || ' ' != vol[0])
	{
		/*
		**	If volume or library specified then ONLY use that
		*/
		WL_set_va_count(6);						/* Set the arg count to 4		*/
		LINK2(file,8,"P",1,lib,8,vol,6,&compcode,4,&returncode,4);	/* Do the LINK				*/

		WL_wswap(&compcode);						/* Un-swap the comp & return codes	*/
		WL_wswap(&returncode);
	}
	else
	{
		WL_set_va_count(4);						/* Set the arg count to 4		*/
		LINK2(file,8," ",1,&compcode,4,&returncode,4);			/* Do the LINK				*/

		WL_wswap(&compcode);						/* Un-swap the comp & return codes	*/
		WL_wswap(&returncode);

		if ( 8 == compcode && 20 == returncode )				/* If not found ...			*/
		{
			WL_set_va_count(4);						/* Set the arg count to 4		*/
			LINK2(file,8,"S",1,&compcode,4,&returncode,4);			/* Do the LINK				*/

			WL_wswap(&compcode);						/* Un-swap the comp & return codes	*/
			WL_wswap(&returncode);
		}
	}

	if ( 0 == compcode)
	{
		sprintf(message, "\204Program %8.8s completed with return code %ld",file,(long)returncode);
	}
	else if ( 8 == compcode)
	{

		if (20 == returncode)
		{
			strcpy(message, "\224SORRY\204- Program not found");
		}
		else if (28 == returncode)
		{
			strcpy(message, "\224SORRY\204- Access denied");
		}
		else
		{
			sprintf(message, "\224SORRY\204- LINK to %8.8s failed: CompCode 8 RetCode %ld",file, (long)returncode);
		}
	}
	else if ( 16 == compcode)
	{
		sprintf(message, "\204Program %8.8s was cancelled",file);
	}
	else
	{
		strcpy(message, "\204UNKNOWN ERROR");
	}

	return((int)compcode);
}

/*
**	Routine:	vse_compile()
**
**	Function:	Special Command Menu (10) COMPILE
**
**	Description:	{Full detailed description}...
**
**	Arguments:	None
**
**	Globals:	None
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	
**	03/25/94	Written by GSL
**
*/
#ifdef unix
static int vse_compile(void)
{
	return vse_compile_command();
}

static int vse_compile_command(void)
{
#define LINE_LEN	61
	static	char	message_field[81];
	static	char	line1_field[LINE_LEN];
	static	char	line2_field[LINE_LEN];
	static	char	line3_field[LINE_LEN];
	static	char	line4_field[LINE_LEN];
	static	char	errfile_field[LINE_LEN];
	static	char	save_lang[20] = "xxx";
	char	cmd[1024];
	int	rc, err;
	unsigned char 	write_func=1;

	static VSESCR_FLDS(vse_compile_flds) = {
		{LEN(0)	ROW(1)	COL(2)	VALUE(VSE_COPYRIGHT)},
		{LEN(0)	ROW(3)	COL(36)	ULINEVALUE("Compile")},
		{LEN(0)	ROW(5)	COL(2)	BRIGHT(message_field)},
		{LEN(0)	ROW(7)	COL(2)	VALUE("Please specify the Compile command to be run and press (ENTER).")},
		{LEN(0)	ROW(9)	COL(2)	VALUE("Press (1) to return to the Special Command Menu.")},
		{LEN(0)	ROW(11)	COL(2)	VALUE("COMPILE $")},
		{LEN(0)	ROW(12)	COL(2)	VALUE("(more)  >")},
		{LEN(0)	ROW(13)	COL(2)	VALUE("(more)  >")},
		{LEN(0)	ROW(14)	COL(2)	VALUE("(more)  >")},
		{LEN(0)	ROW(16)	COL(2)	VALUE("ERRFILE :")},
		{LEN(0)	ROW(11)	COL(12)	UPLOW(line1_field)},
		{LEN(0)	ROW(12)	COL(12)	UPLOW(line2_field)},
		{LEN(0)	ROW(13)	COL(12)	UPLOW(line3_field)},
		{LEN(0)	ROW(14)	COL(12)	UPLOW(line4_field)},
		{LEN(0)	ROW(16)	COL(12)	BRIGHT(errfile_field)},
		{LASTITEM}
		};

	if (vse_new_file)
	{
		strcpy(vse_stat_message,"File must be Created before compiled");
		return 0;
	}

	if (vse_file_changed)
	{
		if (vse_file_changed_gp(vse_sysname))
		{
			return 1;
		}
	}

	CLEAR_STRING(line2_field);
	memcpy(line2_field,vse_sysname,strlen(vse_sysname));

#ifdef MSDOS 
	{
		char	*ext_ptr;

		/*
		**	For MSDOS we pass the extension as a separate arg.
		*/
		if (ext_ptr = WL_osd_ext(vse_sysname))
		{
			line2_field[ ext_ptr - vse_sysname - 1 ] = ' ';
		}
		else
		{
			line2_field[ strlen(vse_sysname) + 1 ] = '?';
		}
	}
#endif

	vse_build_errfile(vse_sysname);
	strcpy(errfile_field,vse_errfile());

	if (0 != strcmp(lang_ext(),save_lang))
	{
		strcpy(save_lang,lang_ext());

		CLEAR_STRING(line1_field);
		CLEAR_STRING(line3_field);
		CLEAR_STRING(line4_field);

		if (0==strcmp(lang_ext(),"wcb"))
		{
#ifdef unix
			memcpy(line1_field,"wac.sh",6);
			memcpy(line3_field,"../obj",6);
#endif
#ifdef MSDOS
			memcpy(line1_field,"WAC.BAT",7);
			memcpy(line3_field,"..\\obj",6);
#endif
		}
		if (0==strcmp(lang_ext(),"wps"))
		{
			memcpy(line1_field,"wproc -s",8);
		}
	}

	strcpy(message_field,"");

	for(;;)
	{
		memcpy(spc_scr,vse_default_oa,sizeof(vse_default_oa));
		vsescr_init(spc_scr);
		vsescr(vse_compile_flds,spc_scr);

		strcpy(spc_pfs,"0001X");

		vwang(spc_func,spc_scr,spc_lines,spc_pfs,spc_pfcode,spc_status);
		int4_from_str2(spc_pfcode,&vse_special_pick);

		if (1 == vse_special_pick)
		{
			return 1;
		}

		strcpy(message_field,"");
		err = 0;

		vseunscr(vse_compile_flds,spc_scr);

		strcpy(cmd,line1_field);
		vse_trunc(cmd);

		if (!cmd[0])
		{
			strcpy(message_field,"\224SORRY\204- COMPILE script must be non-blank");
			err = 1;
		}

		if (!err)
		{
#ifdef unix
			strcpy(cmd,line1_field);
			strcat(cmd,line2_field);
			strcat(cmd,line3_field);
			strcat(cmd,line4_field);
			vse_trunc(cmd);
#endif
#ifdef MSDOS
			strcpy(cmd,line1_field);
			vse_trunc(cmd);
			strcat(cmd," ");
			strcat(cmd,line2_field);
			vse_trunc(cmd);
			strcat(cmd," ");
			strcat(cmd,line3_field);
			vse_trunc(cmd);
			strcat(cmd," ");
			strcat(cmd,line4_field);
			vse_trunc(cmd);
#endif

			sprintf(vse_stat_message,"Compilation of %s in progress",vse_sysname);
			init_special_menu();
			vwang(&write_func,spc_scr,spc_lines,spc_pfs,spc_pfcode,spc_status);

			rc = vse_run_command_with_log(cmd,errfile_field);

			sprintf(vse_stat_message,"Compilation finished with Exit Code = %d",rc);

			set_errfile_active(1);

			return rc;
		}
	}

}
#endif /* unix */

/*
**	Routine:	vse_run_command_with_log()
**
**	Function:	To run a command with all output logged to a file.
**
**	Description:	{Full detailed description}...
**
**	Arguments:
**	cmd		The command to run.
**	logfile		The log file to use.
**
**	Globals:	None
**
**	Return:		The command exit code.
**
**	Warnings:	None
**
**	History:	
**	03/29/94	Written by GSL
**
*/
#ifdef unix
static int vse_run_command_with_log(char *cmd, char *logfile)
{
	int	rc;
	char	fullcmd[1024];

	rc = 0;

	sprintf(fullcmd,"%s >%s 2>&1",cmd,logfile);
	rc = WL_wsystem(fullcmd);
	WISPSYNC();
	vwang_stty_sync();
	return rc;
}
#endif

/*
**	Routine:	vse_build_errfile()
**
**	Function:	Contruct the errors filepath.
**
**	Description:	Use the sysname and replace the extension with .err
**
**	Arguments:
**	sysname		The filename to base the error file on.
**
**	Globals:
**	g_errfile	The generated error file name.
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	
**	03/28/94	Written by GSL
**
*/

static char	g_errfile[256];
static int	g_errfile_active = 0;

static int vse_build_errfile(char *sysname)
{
	if (sysname && *sysname)
	{
		buildfilepath(g_errfile,splitpath(sysname),splitname(sysname));
		strcat(g_errfile,".err");
	}
	else
	{
		g_errfile[0] = (char)0;
	}
	return 0;
}

static char *vse_errfile(void)
{
	return g_errfile;
}

static int is_errfile_active(void)
{
	return g_errfile_active;
}

int set_errfile_active(int active)
{
	return g_errfile_active = active;
}

#ifdef unix
/*
**	Routine:	vse_errors()
**
**	Function:	Special Command Menu ERRORS
**
**	Description:	Display the errors file.
**
**	Arguments:	None
**
**	Globals:	None
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	
**	03/28/94	Written by GSL
**
*/
static int vse_errors(void)
{
	/*
	**	This may need work for VMS
	*/
	if (vse_exists(vse_errfile()))
	{
		vse_display(vse_errfile());
		vse_stat_message[0] = (char)0;
	}
	else
	{
		strcpy(vse_stat_message,"No error file found");			
	}
	return 0;
}
#endif /* unix */

#ifdef unix
static int vse_compile_and_run(void)
{
	return 0;
}
#endif /* unix */

/*
**	Routine:	vse_listing()
**
**	Function:	Display the COB file after a WISP
**
**	Description:	{Full detailed description}...
**
**	Arguments:	None
**
**	Globals:
**	vse_sysname
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	
**	03/28/94	Written by GSL
**
*/
#ifdef unix
static int vse_listing(void)
{
	char	listfile[256];

	if (vse_sysname[0])
	{
		buildfilepath(listfile,splitpath(vse_sysname),splitname(vse_sysname));
		strcat(listfile,".cob");
		if (vse_exists(listfile))
		{
			vse_display(listfile);
			vse_stat_message[0] = (char)0;
		}
		else
		{
			strcpy(vse_stat_message,"No listing file found");			
		}
	}
	else
	{
		strcpy(vse_stat_message,"No file");
	}
	return 0;
}

static int vse_display(char *filename)
{
	/*	LINK to the DISPLAY utility to display the file */
	return WL_link_display(filename);
}
#endif /* unix */

/*
**	History:
**	$Log: vsespmnu.c,v $
**	Revision 1.29  2010/02/10 03:53:17  gsl
**	fix warnings
**	
**	Revision 1.28  2010/01/10 00:36:15  gsl
**	refactor utils to add vse_ prefix to avoid conflicts with trunc
**	vse_trunc
**	
**	Revision 1.27  2003/08/04 14:43:04  gsl
**	Cleanup WAC compile stuff.
**	MSDOS code had been incorrectly changed to WIN32 code. - fixed
**	Some non-working WIN32 code was removed.
**	
**	Revision 1.26  2003/02/17 22:07:18  gsl
**	move VSSUB prototypes to vssubs.h
**	
**	Revision 1.25  2003/02/05 21:47:54  gsl
**	fix -Wall warnings
**	
**	Revision 1.24  2003/02/04 18:57:00  gsl
**	fix copyright header
**	
**	Revision 1.23  2002/08/01 15:57:50  gsl
**	type warnings
**	
**	Revision 1.22  2002/08/01 15:31:11  gsl
**	type warnings
**	
**	Revision 1.21  2002/07/18 21:04:30  gsl
**	Remove MSDOS code
**	
**	Revision 1.20  2002/07/12 17:17:08  gsl
**	Global unique WL_ changes
**	
**	Revision 1.19  2002/07/11 14:33:59  gsl
**	Fix WL_ unique globals
**	
**	Revision 1.18  2002/07/10 21:06:39  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.17  2002/06/26 01:42:50  gsl
**	Remove VMS code
**	
**	Revision 1.16  1998/10/15 14:07:44  gsl
**	fix warning
**	
**	Revision 1.15  1998-05-21 09:39:36-04  gsl
**	Fix warning
**
**	Revision 1.14  1997-10-23 14:02:38-04  gsl
**	change to use link_display()
**
**	Revision 1.13  1996-11-12 18:49:24-05  gsl
**	Comment out the WISP and Compile option for Windows NT
**
**	Revision 1.12  1996-09-03 15:24:11-07  gsl
**	drcs update
**
**
**
*/
