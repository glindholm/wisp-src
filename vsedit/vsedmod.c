/*
******************************************************************************
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
** $Id:$
**
** NOTICE:
** Confidential, unpublished property of NeoMedia Technologies, Inc.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of NeoMedia 
** Technologies, Inc. is strictly prohibited.
** 
** CVS
** $Source:$
** $Author: gsl $
** $Date:$
** $Revision:$
******************************************************************************
*/


#include <stdio.h>

#include "vsedmod.h"
#include "vseglb.h"
#include "vsescr.h"
#include "vsedscr.h"
#include "vsebasic.h"
#include "vsedit.h"
#include "vsetxt.h"
#include "vseutl.h"


static void vse_ed_mod_unload(void);
static void vse_ed_mod_init(void);
static void vse_ed_mod_top(void);


void vse_ed_mod(void)
{
	int mod_col,mod_row;

	mod_row = ed_oa[OA_CURSOR_ROW];
	mod_col = ed_oa[OA_COL];
	if(mod_row < VSE_FIRST_SCREEN_ROW)
		mod_row = VSE_FIRST_SCREEN_ROW;
	if(mod_col < VSE_FIRST_SCREEN_COL)
		mod_col = VSE_FIRST_SCREEN_COL;
	ed_oa[OA_CURSOR_ROW] = mod_row;
	ed_oa[OA_COL] = mod_col;

	
	vse_ed_load_lines();

	for(;;)
	{
		vse_ed_mod_init();
		d_and_r_ed(TAB_NORMAL);
		int4_from_str2(ed_pfcode,&vse_edit_pick);

		if ( 1 == vse_edit_pick )
		{
			return;
		}
		
		vseunscr(ed_line_flds,ed_scr);

		switch(vse_edit_pick)
		{
		case 2:
			vse_set();
			break;
		case 15:
			++show_col_flag;
			break;
		case 0:
			vse_ed_mod_unload();

			if(!vse_append)
			{
				return;
			}
			
			file_bottom();
			ed_oa[OA_CURSOR_ROW] = vse_save_row + 1;
			ed_oa[OA_COL] = vse_save_col;
			vse_ed_load_lines();

			break;
		}
	}
}

static void vse_ed_mod_unload(void)
{
	TEXT *txt;
	int idx,last_idx;

	vse_append = 0;
	txt = scr_first;
	for(idx = 0,last_idx = 0; idx < VSE_EDIT_ROWS; ++idx)
	{
		spaceout(ed_line[idx],vse_edit_width);
		if(!(isblankstr(ed_line[idx],vse_edit_width)))
			last_idx = idx;
		vse_trunc(ed_line[idx]);
	}
	for(idx = 0;idx < VSE_EDIT_ROWS; ++idx)
	{
		if ( !txt )
		{
			if(last_idx >= idx)
			{
				vse_append = 1;
			}
			else
			{
				break;
			}
		}

		if(!vse_append)
		{
			/*
			**	Check to see if the line was modified.
			**	Both fields are (had better be) truncated.
			*/
			if ( 0 != strcmp( txt->text, ed_line[idx] ) )
			{
				vse_file_changed = 1;
				add_modcode(txt);
				over_text(txt,ed_line[idx]);

				if (lang_type() == LANG_BASIC)
				{
					delete_linenum( txt->lineno );
					find_linenum( txt );
				}
			}
		}
		else
		{
			vse_file_changed = 1;
			txt = new_text(ed_line[idx]);

			txt->modfld = NULL;
			add_modcode(txt);

			append_text(txt);
			txt->lineno = next_lineno(0);

			/* If the language is BASIC, look for embedded line number in
			   line. Added by CIS: 08/12/93 AJA */
			if (lang_type() == LANG_BASIC)
				find_linenum( txt );
		}
		txt = txt->next;
	}
}

static void vse_ed_mod_init(void)
{
	int fac;

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

int mode_upper(void)
{
	return (0==strcmp(vse_gp_defaults_mode,"UPPER")) ? 1 : 0;
}

int language_case(void)
{
	if(mode_upper())
		return(UPPER_ENTRY_FAC);
	else
		return(UPLOW_ENTRY_FAC);
}


static void vse_ed_mod_top(void)
{
	strcpy(ed_top1,   "Make changes and press (ENTER) or select:                               \254Modify");
	strcpy(ed_top2,   "(1) Exit  (2) Set Tabs (15) Show Column");
	vse_ed_mod_col();
}

void vse_ed_mod_col(void)
{
	char *tens, *ones;
	int didx,sidx;

	tens="         1         2         3         4         5         6         7         ";
	ones="123456789012345678901234567890123456789012345678901234567890123456789012";

	didx = VSE_NUM_WIDTH+1;
	sidx = vse_edit_start_col - 1;

	CLEAR_FIELD(ed_top3);
	CLEAR_FIELD(ed_top4);

	memcpy(&ed_top3[didx],&tens[sidx],vse_edit_width);
	memcpy(&ed_top4[didx],&ones[sidx],vse_edit_width);
	ed_top3[vse_edit_width + didx] = 0;
	ed_top4[vse_edit_width + didx] = 0;
}

/*
**	Routine:	add_modcode()
**
**	Function:	Add a modcode to the line.
**
**	Description:	This add the modcode to the given line if applicable.
**
**	Arguments:
**	txt		The line to add the modcode to.
**
**	Globals:
**	vse_gp_options_modcode
**			The modcode
**	vse_mod_width	The modcode width
**
**	Return:
**	1		Modcode was added.
**	0		Not added.
**
**	Warnings:	None
**
**	History:	
**	03/09/94	Written by GSL
**
*/
int add_modcode(TEXT *txt)
{
	if ( vse_mod_width && 0 != memcmp( vse_gp_options_modcode, "        ", vse_mod_width ) )
	{
		if ( !(txt->modfld) )
		{
			txt->modfld = (char *) calloc( vse_mod_width+1, sizeof( char ) );
		}
		strncpy( txt->modfld, vse_gp_options_modcode, vse_mod_width );
		txt->modfld[vse_mod_width] = (char)0;
		return 1;
	}
	return 0;
}

void spaceout(char *str, int4 len)
{
	while(len--)
		if(str[len] == 0)
			str[len] = ' ';
}
/*
**	History:
**	$Log: vsedmod.c,v $
**	Revision 1.14  2010/01/10 00:36:15  gsl
**	refactor utils to add vse_ prefix to avoid conflicts with trunc
**	vse_trunc
**	
**	Revision 1.13  2003/02/04 18:57:00  gsl
**	fix copyright header
**	
**	Revision 1.12  1996/09/03 22:24:03  gsl
**	drcs update
**	
**
**
*/
