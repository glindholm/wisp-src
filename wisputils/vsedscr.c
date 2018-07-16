#define	_VSEDSCR_C
#include <stdio.h>

#include "vseglb.h"
#include "vsescr.h"
#include "vsedscr.h"

static char ed_func[]={03};
static char ed_lines[]={24};

static char ed_menu1[]=
"(1)Disp  (2)First (3)Last  (4)Prev  (5)Next  (6)Up    (7)Down  (8)Find  ";
static char ed_menu2[]=
"(9)Mod   (10)Chng (11)Ins  (12)Del  (13)Move (14)Copy (15)Col  (16)Menu ";

vse_ed_scr()
{
	init_line_fields();
	memcpy(ed_oa,vse_default_oa,4);
	for(;;)
		{
		init_vse_ed_scr();
		d_and_r_ed();
		long_from_str2(ed_pfcode,&vse_edit_pick);
		vse_ed_scr_dispatch();
		if(vse_edit_pick == 16)
			break;
		}
}

d_and_r_ed()
{
	memcpy(ed_scr,ed_oa,4);
	vwang(ed_func,ed_scr,ed_lines,ed_pfs,ed_pfcode,ed_status);
	memcpy(ed_oa,ed_scr,4);
}

init_line_fields()
{
	int idx,col;

	if(vse_numbering)
		{
		col = 9;
		}
	else
		{
		col = 2;
		}
		
	for (idx = 0; idx < 20; ++idx)
		{
		ed_line_flds[idx].len = vse_text_width;
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
		case 6:
			prev_line();
			break;
		case 7:
			next_line();
			break;
		case 8:
			vse_ed_fnd();
			break;
		case 9:
			vse_ed_mod();
			break;
		case 11:
			vse_ed_ins();
			break;
		case 12:
			vse_ed_del();
			break;
		case 16:
			break;
		case 1:
		case 10:
		case 13:
		case 14:
		case 15:
		default:
			vse_naf();
			break;
		}

}


init_vse_ed_scr()
{
	int idx;
	TEXT *txt;

	make_all(DIM_FAC);

	init_vse_ed_lines();
	vsescr_init(ed_scr);
	vse_ed_scr_pfs();
	vsescr(ed_menu_flds,ed_scr);
	vsescr(ed_line_flds,ed_scr);
	vse_ed_add_numbers();
}
init_vse_ed_lines()
{
	TEXT *txt;
	int idx;

	for(idx = 0, txt = scr_first; idx < 20; ++idx)
		{
		memset(ed_line[idx],0,vse_text_width);
		memset(ed_num[idx],0,6);
		ed_txt[idx] = NULL;
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
	if(vse_numbering)
		vsescr(ed_num_flds,ed_scr);
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
		disable_16(6,ed_pfs);		/*Disable UP*/
		menu_off(2,ed_top1);		/*Same for the menus*/
		menu_off(4,ed_top1);
		menu_off(6,ed_top1);
		}
	if(!lines_below(vse_page_size))		/* If at bot*/
		{
		disable_16(3,ed_pfs);		/*Disable Last*/
		disable_16(5,ed_pfs);		/*Disable pg dn*/
		menu_off(3,ed_top1);
		menu_off(5,ed_top1);
		}
	if(!lines_below(5L))			/* If at bot*/
		{
		disable_16(7,ed_pfs);		/*Disable dn*/
		menu_off(7,ed_top1);
		}
	if(vse_lines == 0)				/*If buffer empty*/
		{
		disable_16(8,ed_pfs);		/* Find */
		disable_16(10,ed_pfs);		/* change */
		disable_16(12,ed_pfs);		/* del */
		disable_16(13,ed_pfs);		/* move */
		disable_16(14,ed_pfs);		/* copy*/
		disable_16(15,ed_pfs);		/*print*/
		menu_off(8,ed_top1);
		menu_off(10 % 8,ed_top2);
		menu_off(12 % 8, ed_top2);
		menu_off(13 % 8, ed_top2);
		menu_off(14 % 8, ed_top2);
		menu_off(15 % 8, ed_top2);
		}

}

/*----
Are there lines left
------*/
static lines_below(lines)
long lines;
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
	--pf;
	pf *= 9;
	memcpy(&menu[pf],"         ",9);
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
long len;
{
	while(len--)
		{
		if(*str == 0)
			*str = ' ';
		++str;
		}
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

