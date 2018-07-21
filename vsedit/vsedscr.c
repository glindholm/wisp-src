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


#define	_VSEDSCR_C
#include <stdio.h>

#include "idsistd.h"
#include "vwang.h"

#include "vseglb.h"
#include "vsescr.h"
#include "vsedscr.h"
#include "vsedel.h"
#include "vsedfnd.h"
#include "vsedit.h"
#include "vsedmod.h"
#include "vsemov.h"

static unsigned char ed_func[]={03};
static unsigned char ed_lines[]={24};

static char ed_menu1[]=
  "        (2)First (3)Last (4)Prev (5)Next  (6)Down  (7)Up   (8)Find     \254Display";
static char ed_menu2[]=
  "(9)Mod (10)Chng (11)Ins (12)Del (13)Move (14)Copy (15)Col (16)Menu";

static char col_num[4];
static VSESCR_FLDS(ed_showcol_fld) = {
{LEN(0) ROW(2)  COL(72) VALUE("Column")},
{LEN(3) ROW(2)  COL(79) FROM(col_num)},
{LASTITEM}
};

static void init_line_fields(void);
static void vse_ed_scr_dispatch(void);
static void init_vse_ed_scr(void);
static void vse_ed_scr_pfs(void);
static int lines_below(int4 lines);
static void menu_off(int pf, char *menu);
static void disable_16(int pf, char *pfs);
static void spacecpy(char *dest, char *src, int len);
static void next_line(void);
static void prev_line(void);
static void next_page(void);
static void prev_page(void);
static void file_top(void);


void vse_ed_scr(int pick)
{
	extern int vse_initial_keypress;
	init_line_fields();

	memcpy(ed_oa,vse_default_oa,4);

	for(;;)
	{
		init_vse_ed_scr();
		if (vse_initial_keypress != -1)
		  ed_oa[OA_WCC] &= 0x7f;
		d_and_r_ed(TAB_TO_FIELD);
		if (vse_initial_keypress == -1)
		{
			int4_from_str2(ed_pfcode,&vse_edit_pick);
		}
		else
		{
			vse_edit_pick = vse_initial_keypress;
			vse_initial_keypress = -1;
			ed_oa[OA_WCC] |= 0x80;
			if (!strncmp(&ed_pfs[vse_edit_pick*2],"16",2))
			  vse_edit_pick=1;
		}
		vse_ed_scr_dispatch();
		if(vse_edit_pick == 16)
			break;
	}
}

void d_and_r_ed(int tabs)
{
	unsigned char settab;
	unsigned char write_all=1, read_all=2;
	
	if (tabs)
	{
		settab=13;
		vwang(&settab,vse_tab_setting,0,0,0,0);
	}
	
	memcpy(ed_scr,ed_oa,4);
	if (tabs)
	{
		vwang(&write_all,ed_scr,ed_lines,ed_pfs,ed_pfcode,ed_status);
		vwang(&read_all,ed_scr,ed_lines,ed_pfs,ed_pfcode,ed_status);
	}
	else
	{
		vwang(ed_func,ed_scr,ed_lines,ed_pfs,ed_pfcode,ed_status);
	}
	memcpy(ed_oa,ed_scr,4);

	if (tabs)
	{
		settab=14;
		vwang(&settab,0,0,0,0,0);
	}
}

static void init_line_fields(void)
{
	int idx,col;

	col = VSE_FIRST_SCREEN_COL;
	for (idx = 0; idx < VSE_EDIT_ROWS; ++idx)
		{
		ed_line_flds[idx].len = vse_edit_width;
		ed_line_flds[idx].col = col;
		}
}

static void vse_ed_scr_dispatch(void)
{
	switch(vse_edit_pick)
		{
		case 2:
			file_top();
			break;
		case 3:
			file_bottom();
			break;
		case 4:
			prev_page();
			break;
		case 5:
			next_page();
			break;
		case 7:
			next_line();
			break;
		case 6:
			prev_line();
			break;
		case 8:
			vse_save_pos();
			vse_ed_find();
			break;
		case 9:
			vse_ed_mod();
			vse_save_pos();		/* Force the cursor to stay at the current location */
			break;
		case 11:
			vse_save_pos();
			vse_ed_ins();
			break;
		case 12:
			vse_save_pos();
			vse_ed_del();
			break;
		case 16:
			CLEAR_FIELD(vse_stat_message);
			break;
		case 10:
			vse_save_pos();
			vse_ed_change();
			break;
		case 13:
			vse_save_pos();
			vse_mov_lin();
			break;
		case 14:
			vse_save_pos();
			vse_cpy_lin();
			break;
		case 15:
			++show_col_flag;
			break;
		case 1:
			break;
			
		default:
			vse_naf();
			break;
		}

}

void vse_save_pos(void)
{
	vse_save_row = ed_oa[OA_CURSOR_ROW];
	vse_save_col = ed_oa[OA_COL];
}


static void init_vse_ed_scr(void)
{
	make_all(DIM_FAC);

	vse_ed_load_full_lines();
	vsescr_init(ed_scr);
	vse_ed_scr_pfs();
	vsescr(ed_menu_flds,ed_scr);
	vsescr(ed_line_flds,ed_scr);
	if (show_col_flag)
	{
		vse_sho_col();
		show_col_flag=0;
	}
	sprintf(col_num,"%3d",ed_oa[OA_COL] + vse_edit_start_col - VSE_FIRST_SCREEN_COL);
	vse_ed_add_numbers();
	vse_ed_add_mod();
	if (vse_save_row!= -1)
	{
		ed_oa[OA_CURSOR_ROW]=vse_save_row;
	}
	else
	  ed_oa[OA_CURSOR_ROW]=VSE_FIRST_SCREEN_ROW;
	if (vse_save_col!= -1)
	{
		ed_oa[OA_COL]=vse_save_col;
	}
	else
	  ed_oa[OA_COL]=VSE_FIRST_SCREEN_COL;
}

void vse_ed_load_full_lines(void)
{
	TEXT *txt;
	int idx;

	for(idx = 0, txt = scr_first; idx < VSE_EDIT_ROWS; ++idx)
		{
		memset(ed_line[idx],0,vse_edit_width);
		memset(ed_num[idx],0,VSE_NUM_WIDTH);
		memset(ed_mod[idx],0,vse_mod_width);
		ed_txt[idx] = NULL;
		strcpy(ed_num[idx],"ADD - ");
		if(txt)
			{
			spacecpy(ed_line[idx],txt->text,(int) vse_edit_width);
			ed_txt[idx] = txt;
			sprintf(ed_num[idx],"%06ld",(long)txt->lineno);
			if ( txt->modfld )
				strcpy( ed_mod[idx], txt->modfld );
			else
				memset( ed_mod[idx], ' ', vse_mod_width );
			txt=txt->next;
			}
		}
}

void vse_ed_load_lines(void)
{
	TEXT *txt;
	int idx;

	for(idx = 0, txt = scr_first; idx < VSE_EDIT_ROWS; ++idx)
		{
		ed_line_flds[idx].len = vse_edit_width;
		memset(ed_line[idx],0,vse_edit_width);
		memset(ed_num[idx],0,VSE_NUM_WIDTH);
		ed_txt[idx] = NULL;
		strcpy(ed_num[idx],"ADD - ");
		if(txt)
			{
			spacecpy(ed_line[idx],txt->text,(int) vse_edit_width);
			ed_txt[idx] = txt;
			sprintf(ed_num[idx],"%06ld",(long)txt->lineno);
			txt=txt->next;
			}
		}
}

void vse_ed_add_numbers(void)
{
	static int kludge=0;
	int idx;
	
	if(vse_num_start_col)
		vsescr(ed_num_flds,ed_scr);
	else
	{
		kludge=1;
		for (idx=0; idx<VSE_EDIT_ROWS; ++idx)
		{
			ed_num_flds[idx].col=2;
			ed_num_flds[idx].len=VSE_NUM_WIDTH;
			ed_num_flds[idx].row=idx+VSE_FIRST_SCREEN_ROW;

			ed_line_flds[idx].len=vse_edit_width;
			ed_line_flds[idx].col=VSE_FIRST_SCREEN_COL;
		}
		vsescr(ed_num_flds,ed_scr);
	}
}

void vse_ed_add_mod(void)
{
	int 	idx;
	int	showmod_col = VSE_SCREEN_WIDTH - vse_mod_width + 1;

	if ( 0==strcmp( vse_gp_defaults_showmods, "YES" ) && vse_mod_width )
	{
		for ( idx = 0; idx < VSE_EDIT_ROWS; idx++ )
		{
			ed_mod_flds[idx].col = showmod_col;
			ed_mod_flds[idx].len = vse_mod_width;
		}
		vsescr( ed_mod_flds, ed_scr );
	}
}

static void vse_ed_scr_pfs(void)
{
	int idx;

	strcpy(ed_pfs,ALL_PFS);			 /* all pfs */
	strcpy(ed_top1,ed_menu1);		/* all menus*/
	strcpy(ed_top2,ed_menu2);		/* oh yeah */
	CLEAR_FIELD(ed_top3);
	CLEAR_FIELD(ed_top4);

	if (vse_mod_width && 0==strcmp(vse_gp_defaults_showmods, "YES") )
	{
		int	col;

		col = VSE_SCREEN_WIDTH - vse_mod_width - 1;
		ed_top4[col-1] = '\254';
		memcpy(&ed_top4[col],"Modcode ",vse_mod_width);
	}

	for(idx= 17; idx < 33; ++idx)		/*disable 17 thru 32*/
		disable_16(idx,ed_pfs);
	disable_16(0,ed_pfs);			/*Disable enter key*/

	if(scr_first == text_first)		/* If at top */
		{
		disable_16(2,ed_pfs);		/*Disable Top*/
		disable_16(4,ed_pfs);		/*Disable pg_up*/
		disable_16(6,ed_pfs);		/*Disable dn*/
		menu_off(6,ed_top1);
		menu_off(2,ed_top1);		/*Same for the menus*/
		menu_off(4,ed_top1);
		}
	if(!lines_below(vse_page_size))		/* If at bot*/
		{
		disable_16(3,ed_pfs);		/*Disable Last*/
		disable_16(5,ed_pfs);		/*Disable pg dn*/
		menu_off(3,ed_top1);
		menu_off(5,ed_top1);
		}
	if(!lines_below((int4)VSE_BOTTOM_ROWS))	/* If at bot*/
		{
		disable_16(7,ed_pfs);		/*Disable UP*/
		menu_off(7,ed_top1);
		}
	if(vse_lines == 0)				/*If buffer empty*/
		{
		disable_16(8,ed_pfs);		/* Find */
		disable_16(10,ed_pfs);		/* change */
		disable_16(12,ed_pfs);		/* del */
		disable_16(13,ed_pfs);		/* move */
		disable_16(14,ed_pfs);		/* copy*/
		menu_off(8,ed_top1);
		menu_off(10,ed_top2);
		menu_off(12, ed_top2);
		menu_off(13, ed_top2);
		menu_off(14, ed_top2);
		}

}

/*----
Are there lines left
------*/
static int lines_below(int4 lines)
{
	TEXT *txt;
	int idx;

	if(!scr_first)
		return(0);
	txt = scr_first->next;
	for(idx = 0; idx < lines; ++idx)
	{
		if(!txt)
			return(0);
		txt = txt->next;
	}
	return(1);
}
/*----
Clears a menu selection
Each selection is 9 bytes long
------*/
static void menu_off(int pf, char *menu)
{
	int idx,len;
	
	switch(pf)
	{
	      case 1:
		break;
	      case 2: idx = 8;	len = 8; break;
	      case 3: idx = 17;	len = 8; break;
	      case 4: idx = 25;	len = 8; break;
	      case 5: idx = 33; len = 8;break;
	      case 6:idx = 42;len = 8;break;
	      case 7:idx = 51;len = 8;break;
	      case 8:idx = 59;len = 8;break;
	      case 9:idx = 0;len = 7;break;
	      case 10:idx = 7;len = 8;break;
	      case 11:idx = 16;len = 8;break;
	      case 12:idx = 24;len = 8;break;
	      case 13:idx = 32;len = 8;break;
	      case 14:idx = 41;len = 8;break;
	      case 15:idx = 50;len = 8;break;
	      case 16:idx = 58;len = 8;break;	
	}
	memcpy(&menu[idx],"         ",len);
}


/*----
Disable a pfkey by dropping 16 into it's slot
------*/
static void disable_16(int pf, char *pfs)
{
	pf *= 2;
	memcpy(&pfs[pf],"16",2);
}


void make_all(int fac)
{
	int idx;

	for(idx = 0; idx < VSE_EDIT_ROWS; ++idx)
		{
		ed_line_flds[idx].fac = fac;
		}
}


static void spacecpy(char *dest, char *src, int len)
{
	while(len--)
		{
		if(*src)
			*dest = *src;
		else
			*dest = 0;
		if(*dest == ' ')
			*dest = 0;
		++dest;
		if(*src)
			++src;
		}
}

static void next_line(void)
{
	if(!scr_first)
		return;
	if(!scr_first->next)
		return;
	scr_first = scr_first->next;

	if (ed_oa[OA_CURSOR_ROW] <= VSE_FIRST_SCREEN_ROW)
	{
		vse_save_row = ed_oa[OA_CURSOR_ROW];
	}
	else if ( ed_oa[OA_CURSOR_ROW] <= 24)
	{
		vse_save_row = ed_oa[OA_CURSOR_ROW] - 1;
	}
}
static void prev_line(void)
{
	if(!scr_first->prev)
		return;
	scr_first = scr_first->prev;

	if (ed_oa[OA_CURSOR_ROW] < VSE_FIRST_SCREEN_ROW)
	{
		vse_save_row = ed_oa[OA_CURSOR_ROW];
	}
	else if (ed_oa[OA_CURSOR_ROW] < 24)
	{
		vse_save_row = ed_oa[OA_CURSOR_ROW] + 1;
	}
}

static void next_page(void)
{
	TEXT *txt;
	int idx;

	if(!scr_first)
		return;
	if(!scr_first->next)
		return;
	for(idx = 0, txt = scr_first; idx < vse_page_size; ++idx)
		{
		if(!txt->next)
			break;
		txt = txt->next;
		}
	scr_first = txt;
}

static void prev_page(void)
{
	TEXT *txt;
	int idx;

	if(!scr_first)
		return;
	if(!scr_first->prev)
		return;
	for(idx = 0, txt = scr_first; idx < vse_page_size; ++idx)
		{
		if(!txt->prev)
			break;
		txt = txt->prev;
		}
	scr_first = txt;
}

static void file_top(void)
{
	vse_save_row = VSE_FIRST_SCREEN_ROW;
	vse_save_col = VSE_FIRST_SCREEN_COL;
	scr_first = text_first;
}

void file_bottom(void)
{
	TEXT *txt;
	int idx;
	
	vse_save_row = VSE_FIRST_SCREEN_ROW;
	vse_save_col = VSE_FIRST_SCREEN_COL;

	scr_first = text_last;
	if(!scr_first) return;

	for(idx = 1, txt=scr_first; idx < VSE_BOTTOM_ROWS; ++idx)
	{
		if(!txt->prev)
			break;
		txt = txt->prev;
		vse_save_row += 1;
	}
	scr_first = txt;
}

void vse_sho_col(void)
{
	sprintf(col_num,"%-2d",(ed_oa[OA_COL]<=1)?0:(ed_oa[OA_COL] + vse_edit_start_col - VSE_FIRST_SCREEN_COL));
	vsescr(ed_showcol_fld,ed_scr);
}

/*
**	History:
**	$Log: vsedscr.c,v $
**	Revision 1.14  2003/02/05 21:47:53  gsl
**	fix -Wall warnings
**	
**	Revision 1.13  2003/02/04 18:57:00  gsl
**	fix copyright header
**	
**	Revision 1.12  2002/08/01 15:57:51  gsl
**	type warnings
**	
**	Revision 1.11  2002/08/01 15:31:11  gsl
**	type warnings
**	
**	Revision 1.10  1996/09/03 22:24:04  gsl
**	drcs update
**	
**
**
*/
