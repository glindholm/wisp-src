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
#include <string.h>

#include "vseglb.h"
#include "vsescr.h"
#include "vsedit.h"
#include "vsedscr.h"

#include "vwang.h"

static unsigned char std_scr[1924];
static char std_pfs[]="01020304050607080910111213141516X";
static unsigned char std_func[]={03};
static unsigned char std_lines[]={24};
static char std_pfcode[3];
static unsigned char std_status[3];

static VSESCR_FLDS(std_flds) = {
{LEN(0)	ROW(1)	COL(2)	VALUE(VSE_COPYRIGHT)},
{LEN(0)	ROW(2)	COL(2)	FROM(vse_file_message)},
{LEN(0)	ROW(3)	COL(2)	FROM(vse_lines_message)},
{LEN(0)	ROW(5)	COL(2)
VALUE("Please select the desired function and press the appropriate PF key:")},
{LEN(0)	ROW(7)	COL(2)
VALUE(" (1) Disp  - Return to display mode and resume text editing")},
{LEN(0)	ROW(8)	COL(2)
VALUE(" (2) First - Display the first 20 lines in the file")},
{LEN(0)	ROW(9)	COL(2)
VALUE(" (3) Last  - Display the last 5 lines in the file")},
{LEN(0)	ROW(10)	COL(2)
VALUE(" (4) Prev  - Move the display back 15 lines")},
{LEN(0)	ROW(11)	COL(2)
VALUE(" (5) Next  - Advance the display 15 lines")},
{LEN(0)	ROW(12)	COL(2)
VALUE(" (6) Down  - Move the display back 1 line")},
{LEN(0)	ROW(13)	COL(2)
VALUE(" (7) Up    - Advance the display 1 line")},
{LEN(0)	ROW(14)	COL(2)
VALUE(" (8) Find  - Find the next occurrence of a specified text")},
{LEN(0)	ROW(15)	COL(2)
VALUE(" (9) Mod   - Allow modification of displayed lines, or add lines if at end")},
{LEN(0)	ROW(16) COL(2)
VALUE("(10) Chng  - Change occurrences of a text string within a specified range")},
{LEN(0)	ROW(17)	COL(2)
VALUE("(11) Ins   - Inset a new line after the cursor line")},
{LEN(0)	ROW(18)	COL(2)
VALUE("(12) Del   - Delete a range of lines")},
{LEN(0)	ROW(19)	COL(2)
VALUE("(13) Move  - Move a range of lines to after the cursor line")},
{LEN(0)	ROW(20)	COL(2)
VALUE("(14) Copy  - Copy a range of lines to after the cursor line")},
{LEN(0)	ROW(21)	COL(2)
VALUE("(15) Col   - Show cursor column position")},
{LEN(0)	ROW(22)	COL(2)
VALUE("(16) Menu  - Activate special menu (Set, Restart, Create, Replace, Renumber,")},
{LEN(0)	ROW(23)	COL(15)	VALUE("Exit")},
{LASTITEM}
};

int vse_initial_keypress;

static VSEFLD loading =
{LEN(0)	ROW(4)	COL(2)	BRIGHT("Loading Editor work space")};


static void init_loading_menu(void);
static void init_standard_menu(void);
static void vse_standard_dispatch(void);


void vse_loading_menu(void)
{
	init_standard_menu();
	init_loading_menu();
	vwang(std_func,std_scr,std_lines,std_pfs,std_pfcode,std_status);
}

static void init_loading_menu(void)
{
	vsefld(&loading,std_scr);
	memcpy(std_scr,vse_locked_oa,sizeof(vse_locked_oa));
}

void vse_standard_menu(void)
{
	init_standard_menu();
	vwang(std_func,std_scr,std_lines,std_pfs,std_pfcode,std_status);
	int4_from_str2(std_pfcode,&vse_edit_pick);
	vse_standard_dispatch();
}

static void init_standard_menu(void)
{
	memcpy(std_scr,vse_default_oa,sizeof(vse_default_oa));
	vse_top_messages();
	vsescr_init(std_scr);
	vsescr(std_flds,std_scr);
}

void vse_top_messages(void)
{
	sprintf(vse_lines_message,
		"There are %6ld lines in the edited text.   %s",(long)vse_lines,
		vse_file_changed?"(The file has been modified.)":"");
	if(vse_new_file)
	{
		sprintf(vse_file_message,"There is no current input file");
	}
	else if (wang_style_work_file())
	{
		sprintf(vse_file_message,"Input file is %s in %s on %s",
			vse_gp_input_file,
			vse_gp_input_library,
			vse_gp_input_volume);
	}
	else
	{
		sprintf(vse_file_message,"Input file is %s",vse_sysname);
	}
}

static void vse_standard_dispatch(void)
{
	vse_initial_keypress = vse_edit_pick;
	
	switch(vse_edit_pick)
		{
		case 2:
		case 3:
		case 4:
		case 5:
		case 6:
		case 7:
		case 8:
		case 9:
		case 11:
		case 12:
		case 10:
		case 13:
		case 14:
		case 15:
		case 1:
			vse_menu = SPECIAL_MENU;
			vse_ed_scr(vse_edit_pick);
			break;
		case 16:
			vse_menu = SPECIAL_MENU;
			CLEAR_FIELD(vse_stat_message);
			vse_initial_keypress = -1;
			break;
		}
}

/*
**	History:
**	$Log: vsestmnu.c,v $
**	Revision 1.13  2003/02/05 21:47:54  gsl
**	fix -Wall warnings
**	
**	Revision 1.12  2003/02/04 18:57:00  gsl
**	fix copyright header
**	
**	Revision 1.11  2002/08/01 15:31:11  gsl
**	type warnings
**	
**	Revision 1.10  1996/09/03 22:24:12  gsl
**	drcs update
**	
**
**
*/
