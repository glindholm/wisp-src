static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*	      Copyright (c) 1988,1989,1990,1991,1992,1993,1994		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

#include <stdio.h>
#include <string.h>

#include "idsistd.h"
#include "vsedel.h"
#include "vseglb.h"
#include "vsescr.h"
#include "vsedscr.h"
#include "vsedfnd.h"
#include "vsedit.h"
#include "vsetxt.h"
#include "vsebasic.h"


static char start_field[17],end_field[17],emsg_field[81];

static VSESCR_FLDS(ed_del_flds) = {
{LEN(0)	 ROW(1)	COL(2)	VALUE("Press (ENTER) to delete the specified range of lines.")},
{LEN(0)	 ROW(1)	COL(75)	ULINEVALUE("Delete")},
{LEN(0)	 ROW(2)	COL(2)	VALUE("Press (1) to return to display mode.")},
{LEN(0)	 ROW(3)	COL(2)	FROM(emsg_field)},

{LEN(0)	 ROW(4)	COL(2)	VALUE("Start =")},
{LEN(16) ROW(4)	COL(10)	USING(start_field)},
{LEN(0)	 ROW(4)	COL(29)	VALUE("End   =")},
{LEN(16) ROW(4)	COL(37)	USING(end_field)},

{LASTITEM}
};

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
int vse_ed_del(void)
{
	int	rc;
	int4 	start_line,end_line;

	rc = get_line_and_index((int)(ed_oa[OA_CURSOR_ROW]),&start_line);
	if (rc < 0)
	{
		start_line = 0;
		CLEAR_FIELD(start_field);
	}
	else
	{
		sprintf(start_field,"%06ld          ",(long)start_line);
	}

	ed_oa[OA_CURSOR_ROW] = ed_oa[OA_COL] = 0;

	CLEAR_FIELD(end_field);
	CLEAR_FIELD(emsg_field);

	for(;;)
	{
		/*
		**	Setup the screen
		*/
		vsescr_init(ed_scr);
		vse_ed_load_full_lines();
		strcpy(ed_pfs,"0001X");
		vsescr(ed_line_flds,ed_scr);
		vsescr(ed_del_flds,ed_scr);
		vse_ed_add_numbers();
		vse_ed_add_mod();

		d_and_r_ed(TAB_TO_FIELD);

		int4_from_str2(ed_pfcode,&vse_edit_pick);
		if(1 == vse_edit_pick)
		{
			/*
			**	PF1 to abort
			*/
			return 1;
		}

		vseunscr(ed_del_flds,ed_scr);

		if (rc = validate_range(start_field, &start_line, end_field, &end_line))
		{
			strcpy(emsg_field,vse_err(rc));
		}

		if(!rc)
		{
			if (rc = vse_ed_del_range(start_line,end_line))
			{
				strcpy(emsg_field,vse_err(rc));
			}
			else
			{
				return 0;
			}
		}
	
	}
}

/*----
Search from the top of the text work area until end or
line number is > end line.
Delete anything within the delete request range
If the deleted field happens to be the line at the top of the
screen, then move the screen pointer forward to the next field
------*/
int vse_ed_del_range(int4 start_line, int4 end_line)
{
	TEXT	*txt,*deltxt;
	int	did_delete = 0;

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

			if (lang_type() == LANG_BASIC)
				delete_linenum( deltxt->lineno );

			del_text(deltxt);
			did_delete = 1;
			vse_file_changed = 1;
		}
	}

	return (did_delete)?0:RESP_RANGE;
}

/*
**	History:
**	$Log: vsedel.c,v $
**	Revision 1.11  1996/09/03 22:24:00  gsl
**	drcs update
**	
**
**
*/
