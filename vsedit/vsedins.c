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

#include "idsistd.h"
#include "vseglb.h"
#include "vsescr.h"
#include "vsedscr.h"
#include "vsebasic.h"
#include "vsedfnd.h"
#include "vsedit.h"
#include "vsedmod.h"
#include "vsemov.h"
#include "vsetxt.h"
#include "vseutl.h"


static void vse_ed_ins_dispatch(void);
static void vse_ed_ins_on(void);
static void vse_ed_ins_unload(void);
static int vse_ed_ins_init(void);
static int new_number(TEXT *txt);
static void vse_ed_ins_top(void);
static void vse_ed_ins_col(void);


static TEXT *lastins;
static int ins_row;
static int vse_ins_nextline=1;
static int vse_ins_save_col;

static char number_field[7], incr_field[7], start_field[17], end_field[17];

static char renum_first_line[80];

static VSESCR_FLDS(ask_renum_flds) = {
{LEN(0) ROW(1)  COL(2) BRIGHT(renum_first_line)},
{LEN(0)	ROW(1)	COL(73)	ULINEVALUE("Renumber")},
{LEN(0) ROW(2)  COL(2) VALUE("Enter renumbering information, or press (1) to exit.")},

{LEN(0) ROW(3)  COL(2) VALUE("New starting line is ")},
{LEN(6) ROW(3)  COL(23) USING(number_field)},
{LEN(0) ROW(3)  COL(32) VALUE("Increment by ")},
{LEN(6) ROW(3)  COL(45) USING(incr_field)},

{LEN(0)  ROW(4)  COL(2) VALUE("from line ")},
{LEN(16) ROW(4)  COL(12) USING(start_field)},
{LEN(0)  ROW(4)  COL(32) VALUE("to line ")},
{LEN(16) ROW(4)  COL(40) USING(end_field)},

{LASTITEM}
};

#ifdef NOT_USED
static char max_start_field[7], min_start_field[7];
static VSESCR_FLDS(ask_renum_startl_flds) = {
{LEN(0) ROW(1)  COL(2) BRIGHT("Note")},
{LEN(0) ROW(1)  COL(7) VALUE("-  The starting line must be between ")},
{LEN(6) ROW(1)  COL(44) BRIGHT(min_start_field)},
{LEN(0) ROW(1)  COL(51) VALUE("and")},
{LEN(6) ROW(1)  COL(55) BRIGHT(max_start_field)},
{LEN(0)	ROW(1)	COL(73)	ULINEVALUE("Renumber")},
{LEN(0) ROW(2)  COL(2) VALUE("Enter renumbering information, or press (1) to exit.")},

{LEN(0) ROW(3)  COL(2) VALUE("New starting line is ")},
{LEN(6) ROW(3)  COL(23) USING(number_field)},
{LEN(0) ROW(3)  COL(32) VALUE("Increment by ")},
{LEN(6) ROW(3)  COL(45) USING(incr_field)},

{LEN(0)  ROW(4)  COL(2) VALUE("from line ")},
{LEN(16) ROW(4)  COL(12) USING(start_field)},
{LEN(0)  ROW(4)  COL(32) VALUE("to line ")},
{LEN(16) ROW(4)  COL(40) USING(end_field)},

{LASTITEM}
};
static VSESCR_FLDS(ask_renum_incre_flds) = {
{LEN(0) ROW(1)  COL(2) BRIGHT("Note")},
{LEN(0) ROW(1)  COL(7) VALUE("-  The increment must be between ")},
{LEN(6) ROW(1)  COL(40) BRIGHT(&min_start_field[0])},
{LEN(0) ROW(1)  COL(47) VALUE("and")},
{LEN(6) ROW(1)  COL(51) BRIGHT(&max_start_field[0])},
{LEN(0)	ROW(1)	COL(73)	ULINEVALUE("Renumber")},
{LEN(0) ROW(2)  COL(2) VALUE("Enter renumbering information, or press (1) to exit.")},

{LEN(0) ROW(3)  COL(2) VALUE("New starting line is ")},
{LEN(6) ROW(3)  COL(23) USING(number_field)},
{LEN(0) ROW(3)  COL(32) VALUE("Increment by ")},
{LEN(6) ROW(3)  COL(45) USING(incr_field)},

{LEN(0)  ROW(4)  COL(2) VALUE("from line ")},
{LEN(16) ROW(4)  COL(12) USING(start_field)},
{LEN(0)  ROW(4)  COL(32) VALUE("to line ")},
{LEN(16) ROW(4)  COL(40) USING(end_field)},

{LASTITEM}
};
#endif /* NOT_USED */

void vse_ed_ins(void)
{
	for(vse_ins_nextline=1,vse_edit_pick=0;;)
	{
		ins_row = ed_oa[OA_CURSOR_ROW];

		if(ins_row <= 4)
		  ed_oa[OA_CURSOR_ROW]=ins_row=4;

		ins_row -= VSE_FIRST_SCREEN_ROW;
		while(ins_row > -1)
		{
			if(ed_txt[ins_row])
			  break;
			--ins_row;
		}
		if(vse_edit_pick!= -1)
		{
			if (vse_ed_ins_init() == -1)
			{
				vse_edit_pick = 1;
				break;
			}
		}
		d_and_r_ed(TAB_NORMAL);
		int4_from_str2(ed_pfcode,&vse_edit_pick);
		if (vse_edit_pick != -1)
		{
			ed_num_flds[ins_row].fac = DIM_FAC;
			ed_line_flds[ins_row].fac = DIM_FAC;
		}
		vse_ed_ins_dispatch();
		vse_save_pos();
		if(vse_edit_pick == 1) 
		  break;
	}
}

static void vse_ed_ins_dispatch(void)
{
	switch(vse_edit_pick)
	{
	      case 1:
		del_text(lastins);
		return;
		break;
	      case 2:
		vse_set();
		break;
	      case 15:
		++show_col_flag;
		break;
	      case 0:
		vse_ins_nextline=1;
		break;
	}
	vse_ins_save_col = ed_oa[OA_COL];
	
	vse_ed_ins_on();
}

static void vse_ed_ins_on(void)
{
	vse_file_changed=1;
	vseunscr(ed_line_flds,ed_scr);
	vse_ed_ins_unload();
}

static void vse_ed_ins_unload(void)
{
	spaceout(ed_line[ins_row],vse_edit_width);
	vse_trunc(ed_line[ins_row]);
	over_text(ed_txt[ins_row],ed_line[ins_row]);

	ed_txt[ins_row]->modfld = NULL;
	add_modcode(ed_txt[ins_row]);

	if (lang_type() == LANG_BASIC)
		find_linenum( ed_txt[ins_row] );

}

static int vse_ed_ins_init(void)
{
	int fac;
	TEXT *txt,*above,*below;


	vsescr_init(ed_scr);
	if (vse_ins_nextline)
	{
		txt = new_text("");
		if(!txt)
		{
			out_of_space();
			return(-1);
		}
		if(ins_row < 0)
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
		if(ins_row == -1)
		{
			scr_first = txt;
		}
		if(ins_row == VSE_EDIT_ROWS-1)
		{
			scr_first = scr_first->next;
		}
		if(ins_row < VSE_EDIT_ROWS-1)
		  ++ins_row;
	}
/* 
   This routine from the main screen is only used to get the
   text correctly loaded
*/
	vse_ed_load_lines();
	vse_ed_ins_top();
	strcpy(ed_pfs,"00010215X");
	fac = language_case();
	ed_line_flds[ins_row].fac = fac;
	ed_num_flds[ins_row].fac = BRITE_FAC;
	if (show_col_flag)
	{
		vse_sho_col();
		show_col_flag=0;
	}
	vsescr(ed_line_flds,ed_scr);
	vsescr(ed_menu_flds,ed_scr);
	vse_ed_add_numbers();
	ed_oa[OA_CURSOR_ROW] = ins_row + VSE_FIRST_SCREEN_ROW;

	if (vse_ins_nextline)
	{
		ed_oa[OA_COL]=VSE_FIRST_SCREEN_COL;
		vse_ins_nextline=0;
	}
	else
	  ed_oa[OA_COL]=vse_ins_save_col;
	
	return(0);
}

static int new_number(TEXT *txt)
{
	int4 lo,hi,diff;

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
		ask_renumber();
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

int ask_renumber(void)
{
	int4 	number, incr, start, end, count;
	int	resp_flag = 0;

	ed_oa[OA_CURSOR_ROW] = ed_oa[OA_COL] = 0;
	sprintf(number_field,"%-6d",vse_options_number);
	sprintf(incr_field,"%-6d",vse_options_incr);
	sprintf(start_field,"%-16s","FIRST");
	sprintf(end_field,"%-16s","LAST");

	for(;;)
	{
		if (0==resp_flag)
		{
			strcpy(renum_first_line,"\224NOTE\204- The file must be renumbered to continue inserts.");
		}
		else
		{
			strcpy(renum_first_line,vse_err(resp_flag));
		}

		strcpy(ed_pfs,"0001X");
		vsescr_init(ed_scr);
		vse_ed_load_lines();
		vsescr(ask_renum_flds,ed_scr);
		vsescr(ed_line_flds,ed_scr);
		vse_ed_add_numbers();

		d_and_r_ed(TAB_TO_FIELD);
		int4_from_str2(ed_pfcode,&vse_edit_pick);
		if(1 == vse_edit_pick)
		{
			return 1;
		}
		vseunscr(ask_renum_flds,ed_scr);

		resp_flag = validate_numincr(number_field, &number, incr_field, &incr);

		if (!resp_flag)
		{
			resp_flag = validate_range(start_field, &start, end_field, &end);
		}

		if (!resp_flag)
		{
			resp_flag = vse_renumberer(number,incr,start,end,&count);
			if (!resp_flag)
			{
				return 0;
			}
		}
	}
}

static void vse_ed_ins_top(void)
{
	strcpy(ed_top1, "Input New Line and press (ENTER) or select:                             \254Insert");
	strcpy(ed_top2,"(1) Exit  (2) Set Tabs (15) Show Column");
	vse_ed_ins_col();
}

/*----
Column logic is the same as modifiy mode
------*/
static void vse_ed_ins_col(void)
{
	vse_ed_mod_col();
}

/*
**	History:
**	$Log: vsedins.c,v $
**	Revision 1.16  2010/01/10 00:36:15  gsl
**	refactor utils to add vse_ prefix to avoid conflicts with trunc
**	vse_trunc
**	
**	Revision 1.15  2003/02/20 19:29:54  gsl
**	fix -Wall warnings
**	
**	Revision 1.14  2003/02/05 21:47:53  gsl
**	fix -Wall warnings
**	
**	Revision 1.13  2003/02/04 18:57:00  gsl
**	fix copyright header
**	
**	Revision 1.12  2002/08/01 15:57:51  gsl
**	type warnings
**	
**	Revision 1.11  1996/09/03 22:24:01  gsl
**	drcs update
**	
**
**
*/
