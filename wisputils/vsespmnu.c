#include <stdio.h>
#include "vseglb.h"
#include "vsescr.h"

static char spc_scr[1924];
static char spc_pfs[]="01020304050607081516X";
static char spc_func[]={03};
static char spc_lines[]={24};
static char spc_pfcode[3];
static char spc_status[3];
static char input_message[81];

static VSESCR_FLDS(spc_flds) = {
{LEN(0)	ROW(1)	COL(2)	VALUE("Integrated Program Editor")},
{LEN(0)	ROW(2)	COL(2)	FROM(vse_file_message)},
{LEN(0)	ROW(3)	COL(2)	FROM(vse_lines_message)},
{LEN(0)	ROW(6)	COL(2)
VALUE("Please select the desired function and press the appropriate PF key:")},
{LEN(0)	ROW(8)	COL(2)
VALUE(" (1) Display       - Resume text editing")},
{LEN(0)	ROW(9)	COL(2)
VALUE(" (2) Set           - Set workstation defaults")},
{LEN(0)	ROW(10)	COL(2)
VALUE(" (3) Menu          - Activate the normal menu")},
{LEN(0)	ROW(11)	COL(2)
VALUE(" (4) Restart       - Edit another file")},
{LEN(0)	ROW(12)	COL(2)
VALUE(" (5) Create        - Create a new file from the edited text")},
{LEN(0)	ROW(13)	COL(2)
VALUE(" (6) Replace       - Replace the input file with the edited text")},
{LEN(0)	ROW(14)	COL(2)
VALUE(" (7) Renumber      - Generate new line numbers for the edited text")},
{LEN(0)	ROW(15)	COL(2)
VALUE(" (8) External Copy - Copy a range of lines from another file")},
{LEN(0)	ROW(22)	COL(2)
VALUE("(15) Print         - Print the edited text")},
{LEN(0)	ROW(23)	COL(2)
VALUE("(16) Exit          - End Editor processing")},
{LASTITEM}
};

vse_special_menu()
{
	init_special_menu();
	vwang(spc_func,spc_scr,spc_lines,spc_pfs,spc_pfcode,spc_status);
	long_from_str2(spc_pfcode,&vse_special_pick);
	vse_special_dispatch();
}

init_special_menu()
{
	memcpy(spc_scr,vse_default_oa,sizeof(vse_default_oa));
	vse_top_messages();
	vsescr_init(spc_scr);
	vsescr(spc_flds,spc_scr);
}

vse_special_dispatch()
{
	switch(vse_special_pick)
		{
		case 1:
			vse_ed_scr();
			break;
		case 3:
			vse_menu = EDIT_MENU;
			break;
		case 5:
			vse_create_file();
			break;
		case 6:
			vse_old_file();
			break;
		case 2:
		case 7:
		case 8:
		case 15:
			vse_naf();
			break;
		case 4:
		case 16:
			free_text();
			break;
		default:
			break;
		}
}

vse_create_file()
{
	if(!vse_new_file)
		{
		vse_cnaf();
		return;
		}
	vse_create("");
	if(vse_create_pick == 16)
		return;
	if(!vse_native)
		build_system_name();
	trunc(vse_sysname);
	if(val_sysname_file_exists())
		return;
/* Save it and then reload it */
	save_file();
	free_text();
	load_file();
	scr_first = text_first;
}

vse_old_file()
{
	if(vse_new_file)
		{
		vse_snaf();
		return;
		}
	save_file();
	free_text();
	load_file();
	scr_first = text_first;
}

