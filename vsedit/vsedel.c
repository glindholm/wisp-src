#include <stdio.h>

#include "vseglb.h"
#include "vsescr.h"
#include "vsedscr.h"
#include "idsistd.h"

static TEXT *start_txt,*end_txt;
static int del_row;

int4 start_line,end_line;

static char start_field[17],end_field[17],emsg_field[81];

static VSESCR_FLDS(ed_del_flds) = {
{LEN(0)	ROW(1)	COL(2)	VALUE("Enter the Range to Delete and press ENTER or")},
{LEN(0)	ROW(2)	COL(2)	VALUE("(1) Exit")},
{LEN(0)	ROW(3)	COL(2)	VALUE("Start")},
{LEN(16)	ROW(3)	COL(8)	USING(start_field)},
{LEN(0)	ROW(3)	COL(25)	VALUE("End")},
{LEN(16)	ROW(3)	COL(29)	USING(end_field)},
{LEN(0)	ROW(4)	COL(2)	FROM(emsg_field)},
{LASTITEM}
};

static char save_oa[4];
static screen_error;

/*----
When user requests (12)Delete from the edit menu
1.	Extract the cursor row and save the order area 
2.	Init cursor positioning to first modifiable field 
3.	If row was 1-4 then force to the first text row on the screen 
4.	Convert row to 0 thru 19 (can insert above top line)
5.	If row is below the bottom of the file then adjust 
6.	Clear error message. Load start field with current line
	clear the end field.
7.	Load screen and pfs and display it.
8.	Pick up the pfkey and exit if it 1
9.	Unload the screen and validate the range entries
10.	If no errors, then do the delete (via dispatch)
------*/
vse_ed_del()
{
/*1.*/
	del_row = ed_oa[3];
	memcpy(save_oa,ed_oa,4);
/*2.*/
	ed_oa[3] = ed_oa[2] = 0;

/*3.*/
	if(del_row < 5)
		{
		del_row = 5;
		}
/*4.*/
	del_row -= 5;
/*5.*/
	while(del_row > -1)
		{
		if(ed_txt[del_row])
			break;
		--del_row;
		}
	init_del_fields();
/*6.*/
	for(;;)
		{
/*7.*/
		screen_error = 0;
		vse_ed_del_init();
		d_and_r_ed(TAB_TO_FIELD);
		strcpy(emsg_field,"");
/*8.*/
		int4_from_str2(ed_pfcode,&vse_edit_pick);
		if(1 == vse_edit_pick)
			break;
/*9.*/
		vseunscr(ed_del_flds,ed_scr);
		vse_ed_del_val();
/*10.*/
		if(!screen_error)
			{
			vse_ed_del_dispatch();
			break;
			}
	
		}
	memcpy(ed_oa,save_oa,4);
}

/*----
Clear error and end fields, load start field with current line no
------*/
init_del_fields()
{
	CLEAR_FIELD(end_field);
	CLEAR_FIELD(emsg_field);
	sprintf(start_field,"%06ld          ",ed_txt[del_row]->lineno);
}
/*----
Clear vwang screen, set up pfs, load the screen
------*/
vse_ed_del_init()
{
	vsescr_init(ed_scr);
/* 
   This routine from the main screen is only used to get the
   text correctly loaded
*/
	vse_ed_load_full_lines();
	strcpy(ed_pfs,"0001X");
	vsescr(ed_line_flds,ed_scr);
	vsescr(ed_del_flds,ed_scr);
	vse_ed_add_numbers();
	vse_ed_add_mod();
}
/*----
Valid entries are 
Start field = ALL
Start field = FIRST or a number
End field = LAST or a number
Start <= End
------*/

vse_ed_del_val()
{
	if(!(strcmp(start_field,"ALL             ")))
		{
		init_del_all();
		return;
		}
	if(!(strcmp(start_field,"FIRST           ")))
		{
		init_del_first();
		}
	else
		{
		start_line = 0;
		sscanf(start_field," %ld",&start_line);
		}
	if(!(strcmp(end_field,"LAST            ")))
		{
		init_del_last();
		}
	else
		{
		end_line = 0;
		sscanf(end_field," %ld",&end_line);
		if (end_line==0 && start_line != 0)
		  end_line=start_line;
		}
	if(end_line == 0)
		end_line = start_line;
	if(end_line < start_line)
		{
		screen_error = 1;
		strcpy(emsg_field,"Invalid Range");
		}
}

/*---
These convert FIRST, LAST and ALL literals into line numbers
------*/
init_del_all()
{
	init_del_first();
	init_del_last();
}
init_del_first()
{
	start_line = text_first->lineno;
}
init_del_last()
{
	end_line = text_last->lineno;
}

/*----
No real dispatching here, just call the s\delete routine
------*/
vse_ed_del_dispatch()
{
	switch(vse_edit_pick)
		{
		case 0:
			vse_ed_del_range();
			break;
		}
}
/*----
Search from the top of the text work area until end or
line number is > end line.
Delete anything within the delete request range
If the deleted field happens to be the line at the top of the
screen, then move the screen pointer forward to the next field
------*/
vse_ed_del_range()
{
	TEXT *txt,*deltxt;

	txt = text_first;

	while(txt)
		{
		deltxt = txt;
		txt = txt->next;
		if(deltxt->lineno > end_line)
			break;
		if(deltxt->lineno >= start_line)
			{
			if(deltxt == scr_first)
				scr_first = txt;

			/* If language is BASIC, delete entries in list if this line has
			   embedded line numbers. Added by CIS, 07/12/93 AJA */
			if (!(strcmp( vse_gp_input_language, BASIC_LANGUAGE )))
				delete_linenum( deltxt->lineno );

			del_text(deltxt);
			}
		}
}

