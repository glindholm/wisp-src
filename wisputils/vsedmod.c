#include <stdio.h>

#include "vseglb.h"
#include "vsescr.h"
#include "vsedscr.h"


vse_ed_mod()
{
	int min_col,mod_col,mod_row;

	mod_row = ed_oa[3];
	mod_col = ed_oa[2];
	if(mod_row < 5)
		mod_row = 5;
	if(vse_numbering)
		min_col = 9;
	else
		min_col = 2;
	if(mod_col < min_col)
		mod_col = min_col;
	ed_oa[3] = mod_row;
	ed_oa[2] = mod_col;

	

	for(;;)
	{
		vse_ed_mod_init();
		d_and_r_ed();
		long_from_str2(ed_pfcode,&vse_edit_pick);
		vse_ed_mod_dispatch();
		if(vse_edit_pick == 1) 
			break;
		if((vse_edit_pick == 0) &&
		   (!vse_append)         )
			break;
		if(vse_append)
			{
			file_bottom();
			init_vse_ed_lines();
			}
	}
}

vse_ed_mod_dispatch()
{
	switch(vse_edit_pick)
		{
		case 2:
			vse_naf();
			break;
		case 15:
			vse_naf();
			break;
		case 0:
			vse_ed_mod_renew();
			break;
		}
}
vse_ed_mod_renew()
{
	vseunscr(ed_line_flds,ed_scr);
	vse_ed_mod_unload();
}

vse_ed_mod_unload()
{
	TEXT *txt,*new_text();
	int idx,lidx;

	vse_append = 0;
	txt = scr_first;
	for(idx = 0,lidx = 0; idx < 20; ++idx)
		{
		spaceout(ed_line[idx],vse_text_width);
		if(!(isblank(ed_line[idx],vse_text_width)))
			lidx = idx;
		}
	for(idx = 0;idx < 20; ++idx)
		{
		if(!txt)
			{
			if(lidx >= idx)
				vse_append = 1;
			else
				break;
			}
		trunc(ed_line[idx]);
		if(!vse_append)
			over_text(txt,ed_line[idx]);
		else
			{
			txt = new_text(ed_line[idx]);
			append_text(txt);
			txt->lineno = next_lineno(0L);
			}
		txt = txt->next;
		}
}

vse_ed_mod_init()
{
	int fac;

	vsescr_init(ed_scr);
	vse_ed_mod_top();
	strcpy(ed_pfs,"00010215X");
	fac = language_case();
	make_all(fac);
	vsescr(ed_menu_flds,ed_scr);
	vse_ed_add_numbers();
	vsescr(ed_line_flds,ed_scr);
}
language_case()
{
	if(strcmp(vse_gp_defaults_mode,"UPPER"))
		return(UPLOW_ENTRY_FAC);
	else
		return(UPPER_ENTRY_FAC);
}


vse_ed_mod_top()
{
	strcpy(ed_top1,"Make changes and press ENTER or select:");
	strcpy(ed_top2,"(1) Exit  (2) Set Tabs (15) Show Column");
	vse_ed_mod_col();
}
vse_ed_mod_col()
{
static char tens[]=
"         1         2         3         4         5         6         7         ";
static char ones[] = 
"1234567890123456789012345678901234567890123456789012345678901234567890123456789";

	int didx,sidx;

	CLEAR_FIELD(ed_top3);
	CLEAR_FIELD(ed_top4);
	
	if(vse_numbering)
		didx = 7;
	else
		didx =0;

	sidx = vse_column[0] - 1;
	memcpy(&ed_top3[didx],&tens[sidx],vse_text_width);
	memcpy(&ed_top4[didx],&ones[sidx],vse_text_width);
	ed_top3[vse_text_width + didx] = 0;
	ed_top4[vse_text_width + didx] = 0;
}

