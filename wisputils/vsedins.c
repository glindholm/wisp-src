#include <stdio.h>

#include "vseglb.h"
#include "vsescr.h"
#include "vsedscr.h"

static TEXT *lastins;
static int ins_row;

vse_ed_ins()
{
	for(;;)
	{
	ins_row = ed_oa[3];

	if(ins_row < 3)
		{
		vse_edit_pick = 1;
		return;
		}
/* Convert row to -1 thru 19 (can insert above top line) */
	ins_row -= 5;
/* If row is below the bottom of the file then adjust */
	while(ins_row > -1)
		{
		if(ed_txt[ins_row])
			break;
		--ins_row;
		}
/*
	for(;;)
	{
*/
		if(vse_ed_ins_init() == -1)
			{
			vse_edit_pick = 1;
			break;
			}
		d_and_r_ed();
		ed_num_flds[ins_row].fac = DIM_FAC;
		ed_line_flds[ins_row].fac = DIM_FAC;
		long_from_str2(ed_pfcode,&vse_edit_pick);
		vse_ed_ins_dispatch();
		if(vse_edit_pick == 1) 
			break;
	}
}

vse_ed_ins_dispatch()
{
	switch(vse_edit_pick)
		{
		case 1:
			del_text(lastins);
			break;
		case 2:
			vse_naf();
			break;
		case 15:
			vse_naf();
			break;
		case 0:
			vse_ed_ins_on();
			break;
		}
}
vse_ed_ins_on()
{
	vseunscr(ed_line_flds,ed_scr);
	vse_ed_ins_unload();
}

vse_ed_ins_unload()
{
	spaceout(ed_line[ins_row],vse_text_width);
	trunc(ed_line[ins_row]);
	over_text(ed_txt[ins_row],ed_line[ins_row]);

}

vse_ed_ins_init()
{
	int fac;
	TEXT *txt,*above,*below ,*new_text();


	vsescr_init(ed_scr);
/* Insert a line as requested */
	txt = new_text("");
	if(!txt)
		{
		out_of_space();
		return(-1);
		}
/* Inserting at the top of the screen */
	if(ins_row == -1)
		{
		above = below = NULL;
		if(ed_txt[0])
			{
			below = ed_txt[0];
			above = ed_txt[0]->prev;
			}
		}
	else
		{
		above = ed_txt[ins_row];
		below = above->next;
		}
	insert_text(above,below,txt);
	if(!new_number(txt))
		{
		return(-1);
		}
	lastins = txt;
/* Inserting at the top creates a new first screen field */
	if(ins_row == -1)
		{
		scr_first = txt;
		}
/* Inserting at the bottom shuffle screen up one */
	if(ins_row == 19)
		{
		scr_first = scr_first->next;
		}
/* if we are not at the bottom, then set the true row */
	if(ins_row < 19)
		++ins_row;

/* 
   This routine from the main screen is only used to get the
   text correctly loaded
*/
	init_vse_ed_lines();
	vse_ed_ins_top();
	strcpy(ed_pfs,"00010215X");
	fac = language_case();
	ed_line_flds[ins_row].fac = fac;
	ed_num_flds[ins_row].fac = BRITE_FAC;
	vsescr(ed_line_flds,ed_scr);
	vsescr(ed_menu_flds,ed_scr);
	vse_ed_add_numbers();
	ed_oa[3] = ins_row + 5;
	if(vse_numbering)
		ed_oa[2] = 9;
	else
		ed_oa[2] = 2;

	return(0);
}

new_number(txt)
TEXT *txt;
{
	long lo,hi,diff;
	

	if(!vse_numbering)
		return(1);
	lo = 0;
	hi = 900000;
	if(txt->prev)
		lo = txt->prev->lineno;
	if(txt->next)
		hi = txt->next->lineno;
	diff = hi - lo;
	if(diff < 2)
		{
		del_text(txt);
		do_we_renumber();
		return(0);
		}
	if(diff > vse_options_number)
		diff = vse_options_number;
	else
	if(diff < 11)
		diff = 1;
	else
	if(diff < 101)
		diff = 10;
	else
	if(diff < 1001)
		diff = 100;
	txt->lineno = (diff + lo);
	
	return(1);
}

do_we_renumber()
{
	if(ask_renumber())
		{
		renumber_text();
		}
}
renumber_text()
{
	TEXT *txt;

	txt = text_first;

	init_lastnum();
	while(txt)
		{
		txt->lineno = next_lineno(0L);
		txt = txt->next;
		}
}

/*----
This routine should ask the user and then return true is use says yes
------*/
ask_renumber()
{
	return(1);
}
vse_ed_ins_top()
{
	strcpy(ed_top1,"Input New Line and press ENTER or select:");
	strcpy(ed_top2,"(1) Exit  (2) Set Tabs (15) Show Column");
	vse_ed_ins_col();
}

/*----
Column logic is the same as modifiy mode
------*/
vse_ed_ins_col()
{
	vse_ed_mod_col();
}

