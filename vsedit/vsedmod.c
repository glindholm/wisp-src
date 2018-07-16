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
		d_and_r_ed(TAB_NORMAL);
		int4_from_str2(ed_pfcode,&vse_edit_pick);
		vse_ed_mod_dispatch();
		if(vse_edit_pick == 1) 
			break;
		vse_file_changed=1;
		if((vse_edit_pick == 0) &&
		   (!vse_append)         )
			break;
		if(vse_append)
			{
			file_bottom();
			vse_ed_load_lines();
			}
	}
}

vse_ed_mod_dispatch()
{
	switch(vse_edit_pick)
		{
		case 2:
			vse_set();
			break;
		case 15:
			++show_col_flag;
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

		/* If the current line of text does not have a value for it's modfld,
		   you can remove the trailing blanks. Added by CIS: 08/12/93 AJA */
		if ( txt )
		{
			if ( !(txt->modfld) )
				trunc(ed_line[idx]);
		}
		else
		{
			if(lidx >= idx)
			{
				vse_append = 1;
				trunc( ed_line[idx] );
			}
			else
				break;
		}
		if(!vse_append)
		{

			/* If the line has been changed, copy the new line over, set the
			   modcode if necessary, and scan the line for embedded line
			   numbers. Added by CIS: 08/12/93 AJA */
			if ( strcmp( txt->text, ed_line[idx] ) )
			{
				if ( strcmp( vse_gp_options_modcode, "        " ) )
				{
					if ( !(txt->modfld) )
						txt->modfld = (char *) calloc( 9, sizeof( char ) );
					strncpy( txt->modfld, vse_gp_options_modcode, 8 );
					spaceout( ed_line[idx], vse_text_width );
				}
				over_text(txt,ed_line[idx]);

				/* If language is BASIC, look for embedded line number in
				   modified line. Added by CIS, 07/12/93 AJA */
				if (!(strcmp( vse_gp_input_language, BASIC_LANGUAGE )))
				{

					/* Delete all elements where this line has references to
					   other line numbers */
					delete_linenum( txt->lineno );

					/* Rescan the modified line looking for embedded line
					   numbers */
					find_linenum( txt );
				}
			}
		}
		else
		{
			txt = new_text(ed_line[idx]);

			/* Set the modcode if necessary */
			if ( strcmp( vse_gp_options_modcode, "        " ) )
			{
				txt->modfld = (char *) calloc( 9, sizeof( char ) );
				strncpy( txt->modfld, vse_gp_options_modcode, 8 );
				spaceout( ed_line[idx], vse_text_width );
			}
			else
				txt->modfld = NULL;
			append_text(txt);
			txt->lineno = next_lineno(0);

			/* If the language is BASIC, look for embedded line number in
			   line. Added by CIS: 08/12/93 AJA */
			if (!(strcmp( vse_gp_input_language, BASIC_LANGUAGE )))
				find_linenum( txt );
		}
		txt = txt->next;
		}
}

vse_ed_mod_init()
{
	int fac;

	vse_ed_load_lines();
	vsescr_init(ed_scr);
	vse_ed_mod_top();
	strcpy(ed_pfs,"00010215X");
	fac = language_case();
	make_all(fac);			
	if (show_col_flag)
	{
		vse_sho_col();
		show_col_flag=0;
	}
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
	char *tens, *ones;
	int didx,sidx;

	tens="         1         2         3         4         5         6         7         ";
	ones="123456789012345678901234567890123456789012345678901234567890123456789012";
	if (vse_numbering)
	{
		didx = 7;
		sidx = 6;
	}
	else
	{
		didx = 7;
		sidx = 0;
	}
	CLEAR_FIELD(ed_top3);
	CLEAR_FIELD(ed_top4);

	memcpy(&ed_top3[didx],&tens[sidx],vse_text_width);
	memcpy(&ed_top4[didx],&ones[sidx],vse_text_width);
	ed_top3[vse_text_width + didx] = 0;
	ed_top4[vse_text_width + didx] = 0;
}

