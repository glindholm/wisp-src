#include <stdio.h>

#include "vseglb.h"
#include "vsescr.h"
#include "vsedscr.h"
#include "idsistd.h"

static TEXT *lastins;
static int ins_row;
int start_renum, inc_renum, startl_renum, endl_renum;
static int vse_ins_nextline=1;
static int vse_ins_save_col;

static char max_start_field[6], min_start_field[6];
static char start_renum_field[6],inc_renum_field[6],startl_renum_field[17],endl_renum_field[17];
static VSESCR_FLDS(ask_renum_flds) = {
{LEN(0) ROW(1)  COL(2) BRIGHT("Note")},
{LEN(0) ROW(1)  COL(7) VALUE("-  The file must be renumbered to continue inserts.")},
{LEN(0) ROW(2)  COL(2) VALUE("Enter renumbering information, or press PF1 to exit.")},
{LEN(0) ROW(3)  COL(2) VALUE("New starting line is ")},
{LEN(5) ROW(3)  COL(23) USING(start_renum_field)},
{LEN(0) ROW(3)  COL(30) VALUE("Increment by ")},
{LEN(5) ROW(3)  COL(43) USING(inc_renum_field)},
{LEN(0) ROW(4)  COL(2) VALUE("from line ")},
{LEN(16) ROW(4)  COL(12) USING(startl_renum_field)},
{LEN(0) ROW(4)  COL(30) VALUE("to line ")},
{LEN(16) ROW(4)  COL(38) USING(endl_renum_field)},
{LASTITEM}
};
static VSESCR_FLDS(ask_renum_startl_flds) = {
{LEN(0) ROW(1)  COL(2) BRIGHT("Note")},
{LEN(0) ROW(1)  COL(7) VALUE("-  The starting line must be between ")},
{LEN(5) ROW(1)  COL(44) BRIGHT(min_start_field)},
{LEN(0) ROW(1)  COL(50) VALUE("and")},
{LEN(5) ROW(1)  COL(54) BRIGHT(max_start_field)},
{LEN(0) ROW(2)  COL(2) VALUE("Enter renumbering information, or press PF1 to exit.")},
{LEN(0) ROW(3)  COL(2) VALUE("New starting line is ")},
{LEN(5) ROW(3)  COL(23) USING(start_renum_field)},
{LEN(0) ROW(3)  COL(30) VALUE("Increment by ")},
{LEN(5) ROW(3)  COL(43) USING(inc_renum_field)},
{LEN(0) ROW(4)  COL(2) VALUE("from line ")},
{LEN(16) ROW(4)  COL(12) USING(startl_renum_field)},
{LEN(0) ROW(4)  COL(30) VALUE("to line ")},
{LEN(16) ROW(4)  COL(38) USING(endl_renum_field)},
{LASTITEM}
};
static VSESCR_FLDS(ask_renum_incre_flds) = {
{LEN(0) ROW(1)  COL(2) BRIGHT("Note")},
{LEN(0) ROW(1)  COL(7) VALUE("-  The increment must be between ")},
{LEN(5) ROW(1)  COL(40) BRIGHT(&min_start_field[0])},
{LEN(0) ROW(1)  COL(56) VALUE("and")},
{LEN(5) ROW(1)  COL(50) BRIGHT(&max_start_field[0])},
{LEN(0) ROW(2)  COL(2) VALUE("Enter renumbering information, or press PF1 to exit.")},
{LEN(0) ROW(3)  COL(2) VALUE("New starting line is ")},
{LEN(5) ROW(3)  COL(23) USING(start_renum_field)},
{LEN(0) ROW(3)  COL(30) VALUE("Increment by ")},
{LEN(5) ROW(3)  COL(43) USING(inc_renum_field)},
{LEN(0) ROW(4)  COL(2) VALUE("from line ")},
{LEN(16) ROW(4)  COL(12) USING(startl_renum_field)},
{LEN(0) ROW(4)  COL(30) VALUE("to line ")},
{LEN(16) ROW(4)  COL(38) USING(endl_renum_field)},
{LASTITEM}
};


vse_ed_ins()
{
	for(vse_ins_nextline=1,vse_edit_pick=0;;)
	{
		ins_row = ed_oa[3];

		if(ins_row <= 4)
		  ed_oa[3]=ins_row=4;

		ins_row -= 5;             
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

vse_ed_ins_dispatch()
{
	extern char ed_oa[];
	
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
	vse_ins_save_col = ed_oa[2];
	
	vse_ed_ins_on();
}
vse_ed_ins_on()
{
	vse_file_changed=1;
	vseunscr(ed_line_flds,ed_scr);
	vse_ed_ins_unload();
}

vse_ed_ins_unload()
{
	spaceout(ed_line[ins_row],vse_text_width);
	if ( !(strcmp( vse_gp_options_modcode, "        " )) )
		trunc(ed_line[ins_row]);
	over_text(ed_txt[ins_row],ed_line[ins_row]);

	/* Add the modcode if necessary. Added by CIS: 08/12/93 AJA */
	if ( strcmp( vse_gp_options_modcode, "        " ) )
	{
		ed_txt[ins_row]->modfld = (char *) calloc( 9, sizeof( char ) );
		strncpy( ed_txt[ins_row]->modfld, vse_gp_options_modcode, 8 );
	}
	else
		ed_txt[ins_row]->modfld = NULL;

	/* If language is BASIC, look for embedded line number in added BASIC
	   line. Added by CIS, 07/13/93 AJA */
	if (!(strcmp( vse_gp_input_language, BASIC_LANGUAGE )))
		find_linenum( ed_txt[ins_row] );

}

vse_ed_ins_init()
{
	int fac;
	TEXT *txt,*above,*below ,*new_text();


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
		if(ins_row == 19)
		{
			scr_first = scr_first->next;
		}
		if(ins_row < 19)
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
	ed_oa[3] = ins_row + 5;
/*	if(vse_numbering)
		ed_oa[2] = 9;
	else
		ed_oa[2] = 2;*/
	if (vse_ins_nextline)
	{
		ed_oa[2]=9;
		vse_ins_nextline=0;
	}
	else
	  ed_oa[2]=vse_ins_save_col;
	
	return(0);
}

new_number(txt)
TEXT *txt;
{
	int4 lo,hi,diff;
	

/*	if(!vse_numbering)
		return(1);*/
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
	start_renum=inc_renum=startl_renum=endl_renum= -1;
	
	if(ask_renumber(100))
		{
		renumber_text();
		}
}
/* rewrite */
renumber_text()
{
	TEXT *txt;
	int lineno;
	
	for (txt = text_first; txt && txt->lineno != startl_renum; txt = txt->next)
	  ;

	if (!txt)
	  return;
	
	lineno = start_renum;
	for(;;)
	{
		/* If language is BASIC, look for references to old line number in
		   list and change those to the new line number in text and list.
		   Added by CIS, 07/12/93 AJA */
		if (!(strcmp( vse_gp_input_language, BASIC_LANGUAGE )))
			update_linenum( txt->lineno, lineno );
		txt->lineno = lineno;
		lineno += inc_renum;
		txt = txt->next;
		if (txt==NULL)
		  break;
		if (txt->prev->lineno == endl_renum)
		  break;
	}

	/* After renumbering is complete, update the branch values in the linked
	   list of embedded BASIC line numbers. Added by CIS, 07/13/93 AJA */
	if (!(strcmp( vse_gp_input_language, BASIC_LANGUAGE )))
		update_branch();
}

ask_renumber(lcnt)
int lcnt;
{
	TEXT *txt;
	
	CLEAR_FIELD(start_renum_field);	
	CLEAR_FIELD(inc_renum_field);
	CLEAR_FIELD(startl_renum_field);
	CLEAR_FIELD(endl_renum_field);
	if (start_renum >=0) 
	  sprintf(start_renum_field,"%-5d",start_renum);
	if (inc_renum >=0) 
	  sprintf(inc_renum_field,"%-5d",inc_renum);

	if (startl_renum >=0) 
	  sprintf(startl_renum_field,"%-16d",startl_renum);
	else
	  sprintf(startl_renum_field,"%-16s","FIRST");
	
	if (endl_renum >=0) 
	  sprintf(endl_renum_field,"%-16d",endl_renum);
	else
	  sprintf(endl_renum_field,"%-16s","LAST");

	strcpy(ed_pfs,"0001X");
	vsescr_init(ed_scr);
	vse_ed_load_lines();
	vsescr(ask_renum_flds,ed_scr);
	vsescr(ed_line_flds,ed_scr);
	vse_ed_add_numbers();
      bad:	
	d_and_r_ed(TAB_TO_FIELD);
	int4_from_str2(ed_pfcode,&vse_edit_pick);
	vseunscr(ask_renum_flds,ed_scr);
	
	sscanf(start_renum_field,"%5d",&start_renum);
	if (start_renum<=0) 
	  start_renum=100;
	sscanf(inc_renum_field,"%5d",&inc_renum);
	if (inc_renum<=0) 
	  inc_renum=100;
	if (!strncmp(startl_renum_field,"FIRST",5))
	{
		startl_renum = text_first->lineno;
	}
	else
	  sscanf(startl_renum_field,"%6d",&startl_renum);
	if (!strncmp(endl_renum_field,"LAST",4))
	{
		endl_renum = text_last->lineno;
	}
	else
	{
		sscanf(endl_renum_field,"%6d",&endl_renum);
		if (endl_renum==0 && startl_renum != 0)
		  endl_renum=startl_renum;
	}

	for (txt = text_first; txt && txt->lineno != startl_renum; txt=txt->next)
	  ;
	if (txt && txt->lineno == startl_renum)
	{
		int min_inc;
		
		if (!txt->prev || !txt->next)
		  ;
		else
		if ((start_renum <= txt->prev->lineno) ||
		    (start_renum >= txt->next->lineno))
		{
			sprintf(min_start_field,"%-5d",txt->prev->lineno+1);
			sprintf(max_start_field,"%-5d",txt->next->lineno-1);			
			vsescr_init(ed_scr);
			vse_ed_load_lines();
			vsescr(ask_renum_startl_flds,ed_scr);
			vsescr(ed_line_flds,ed_scr);
			vse_ed_add_numbers();
			goto bad;
		}
		else
		if ( (min_inc=(txt->next->lineno - txt->prev->lineno) 
		      / lcnt ) > inc_renum)
		{
			sprintf(min_start_field,"%-5d",1);
			sprintf(max_start_field,"%-5d",min_inc);			
			vsescr_init(ed_scr);
			vse_ed_load_lines();
			vsescr(ask_renum_incre_flds,ed_scr);
			vsescr(ed_line_flds,ed_scr);
			vse_ed_add_numbers();
			goto bad;

		}
	}
	if(1 == vse_edit_pick)
	  return 0;
	else 
	  return 1;
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

