#define	_VSEDSCR_C
#include <stdio.h>

#include "idsistd.h"
#include "vseglb.h"
#include "vsescr.h"
#include "vsedscr.h"

static char ed_func[]={03};
static char ed_lines[]={24};

static char ed_menu1[]=
"        (2)First (3)Last (4)Prev (5)Next  (6)Down  (7)Up   (8)Find";

static char ed_menu2[]=
"(9)Mod (10)Chng (11)Ins (12)Del (13)Move (14)Copy (15)Col (16)Menu";

static char col_num[4];
static VSESCR_FLDS(ed_showcol_fld) = {
{LEN(0) ROW(2)  COL(72) VALUE("Column")},
{LEN(3) ROW(2)  COL(79) FROM(col_num)},
{LASTITEM}
};
static int lines_below();

vse_ed_scr()
{
	extern int vse_initial_keypress;
	init_line_fields();

	memcpy(ed_oa,vse_default_oa,4);

	for(;;)
	{
		init_vse_ed_scr();
		if (vse_initial_keypress != -1)
		  ed_oa[1] &= 0x7f;
		d_and_r_ed(TAB_TO_FIELD);
		if (vse_initial_keypress == -1)
		{
			int4_from_str2(ed_pfcode,&vse_edit_pick);
		}
		else
		{
			vse_edit_pick = vse_initial_keypress;
			vse_initial_keypress = -1;
			ed_oa[1] |= 0x80;
			if (!strncmp(&ed_pfs[vse_edit_pick*2],"16",2))
			  vse_edit_pick=1;
		}
		vse_ed_scr_dispatch();
		if(vse_edit_pick == 16)
			break;
	}
}

d_and_r_ed(tabs)
int tabs;
{
	char settab;
	char write_all=1, read_all=2;
	
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
init_line_fields()
{
	int idx,col;

/*	if(vse_numbering)
		{
		col = 9;
		}
	else
		{
		col = 2;
		}
*/ 
	col = 9;
	for (idx = 0; idx < 20; ++idx)
		{
		ed_line_flds[idx].len = vse_numbering?vse_text_width:71;
		ed_line_flds[idx].col = col;
		}
}
vse_ed_scr_dispatch()
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
			vse_ed_fnd();
			break;
		case 9:
			vse_save_pos();
			vse_ed_mod();
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
			vse_ed_srch_repl();
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
vse_save_pos()
{
	extern char ed_oa[];
	
	vse_save_row = ed_oa[3];
	vse_save_col = ed_oa[2];
}


init_vse_ed_scr()
{
	int idx;
	TEXT *txt;

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
	sprintf(col_num,"%3d",ed_oa[2] - (vse_numbering?2:9));
	vse_ed_add_numbers();
	vse_ed_add_mod();
	if (vse_save_row!= -1)
	{
		ed_oa[3]=vse_save_row;
	}
	else
	  ed_oa[3]=5;
	if (vse_save_col!= -1)
	{
		ed_oa[2]=vse_save_col;
	}
	else
	  ed_oa[2]=9;
}

vse_ed_load_full_lines()
{
	TEXT *txt;
	int idx;

	for(idx = 0, txt = scr_first; idx < 20; ++idx)
		{
		memset(ed_line[idx],0,vse_text_width);
		memset(ed_num[idx],0,6);
		memset(ed_mod[idx],0,8);
		ed_txt[idx] = NULL;
		strcpy(ed_num[idx],"ADD - ");
		if(txt)
			{
			if ( !(strcmp( vse_gp_defaults_showmod, "YES" )) )
				spacecpy( ed_line[idx], txt->text, vse_mod_text_width );
			else
				spacecpy(ed_line[idx],txt->text,vse_text_width);
			ed_txt[idx] = txt;
			sprintf(ed_num[idx],"%06ld",txt->lineno);
			if ( txt->modfld )
				sprintf( ed_mod[idx], "%.8s", txt->modfld );
			else
				memset( ed_mod[idx], ' ', 8 );
			txt=txt->next;
			}
		}
}

vse_ed_load_lines()
{
	TEXT *txt;
	int idx;

	for(idx = 0, txt = scr_first; idx < 20; ++idx)
		{
		ed_line_flds[idx].len = vse_text_width;
		memset(ed_line[idx],0,vse_text_width);
		memset(ed_num[idx],0,6);
		ed_txt[idx] = NULL;
		strcpy(ed_num[idx],"ADD - ");
		if(txt)
			{
			spacecpy(ed_line[idx],txt->text,vse_text_width);
			ed_txt[idx] = txt;
			sprintf(ed_num[idx],"%06ld",txt->lineno);
			txt=txt->next;
			}
		}
}

vse_ed_add_numbers()
{
	static int kludge=0;
	int idx;
	
	if(vse_numbering)
		vsescr(ed_num_flds,ed_scr);
	else
	{
		kludge=1;
		for (idx=0; idx<20; ++idx)
		{
			ed_num_flds[idx].col=2;
			ed_num_flds[idx].len=6;
			ed_num_flds[idx].row=idx+5;
			ed_line_flds[idx].len=71;
			ed_line_flds[idx].col=9;
		}
		vsescr(ed_num_flds,ed_scr);
	}
}

vse_ed_add_mod()

{
	int idx;

	if ( !(strcmp( vse_gp_defaults_showmod, "YES" )) )
	{
		for ( idx = 0; idx < 20; idx++ )
		{
			ed_mod_flds[idx].col = 73;
			ed_mod_flds[idx].len = 8;
			ed_line_flds[idx].len = vse_mod_text_width;
			ed_line_flds[idx].col = 9;
		}
		vsescr( ed_mod_flds, ed_scr );
	}
}

vse_ed_scr_pfs()
{
	int idx;

	strcpy(ed_pfs,ALL_PFS);			 /* all pfs */
	strcpy(ed_top1,ed_menu1);		/* all menus*/
	strcpy(ed_top2,ed_menu2);		/* oh yeah */
	CLEAR_FIELD(ed_top3);
	CLEAR_FIELD(ed_top4);

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
	if(!lines_below(5))			/* If at bot*/
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
static lines_below(lines)
int4 lines;
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
}
/*----
Clears a menu selection
Each selection is 9 bytes long
------*/
menu_off(pf,menu)
int pf;
char *menu;
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
disable_16(pf,pfs)
char *pfs;
int pf;
{
	pf *= 2;
	memcpy(&pfs[pf],"16",2);
}


make_all(fac)
int fac;
{
	int idx;

	for(idx = 0; idx < 20; ++idx)
		{
		ed_line_flds[idx].fac = fac;
		}
}


spacecpy(dest,src,len)
char *dest,*src;
int len;
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

spaceout(str,len)
char *str;
int4 len;
{
	while(len--)
		if(str[len] == 0)
			str[len] = ' ';
}

next_line()
{
	if(!scr_first)
		return;
	if(!scr_first->next)
		return;
	scr_first = scr_first->next;
}
prev_line()
{
	if(!scr_first->prev)
		return;
	scr_first = scr_first->prev;
}
next_page()
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
prev_page()
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

file_top()
{
	scr_first = text_first;
}
file_bottom()
{
	TEXT *txt;
	int idx;
	
	scr_first = text_last;

	if(!scr_first)
		return;
	for(idx = 0, txt=scr_first; idx < 5; ++idx)
		{
		if(!txt->prev)
			break;
		txt = txt->prev;
		}
	scr_first = txt;
}

vse_sho_col()
{
	extern char ed_oa[];

	sprintf(col_num,"%-2d",(ed_oa[2]<=1)?0:(ed_oa[2] - (vse_numbering?2:8)));
	vsescr(ed_showcol_fld,ed_scr);
}

