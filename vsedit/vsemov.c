#include <stdio.h>

#include "vseglb.h"
#include "vsescr.h"
#include "vsedscr.h"
#include "idsistd.h"

static int mov_row;

int4 dest_line;
extern int4 start_line, end_line;

static char start_field[17],end_field[17],dest_field[17],emsg_field[81];

static VSESCR_FLDS(ed_mov_flds) = {
{LEN(0)	ROW(1)	COL(2)	VALUE("Enter the Range to Move and press ENTER or")},
{LEN(0)	ROW(2)	COL(2)	VALUE("(1) Exit")},
{LEN(0)	ROW(3)	COL(2)	VALUE("Start")},
{LEN(16)	ROW(3)	COL(8)	USING(start_field)},
{LEN(0)	ROW(3)	COL(25)	VALUE("End")},
{LEN(16)	ROW(3)	COL(29)	USING(end_field)},
{LEN(0)	ROW(3)	COL(46)	VALUE("Target")},
{LEN(16)	ROW(3)	COL(53)	USING(dest_field)},
{LEN(0)	ROW(4)	COL(2)	FROM(emsg_field)},
{LASTITEM}
};
static VSESCR_FLDS(ed_cpy_flds) = {
{LEN(0)	ROW(1)	COL(2)	VALUE("Enter the Range to Copy and press ENTER or")},
{LEN(0)	ROW(2)	COL(2)	VALUE("(1) Exit")},
{LEN(0)	ROW(3)	COL(2)	VALUE("Start")},
{LEN(16)	ROW(3)	COL(8)	USING(start_field)},
{LEN(0)	ROW(3)	COL(25)	VALUE("End")},
{LEN(16)	ROW(3)	COL(29)	USING(end_field)},
{LEN(0)	ROW(3)	COL(46)	VALUE("Target")},
{LEN(16)	ROW(3)	COL(53)	USING(dest_field)},
{LEN(0)	ROW(4)	COL(2)	FROM(emsg_field)},
{LASTITEM}
};


static char save_oa[4];
static screen_error;

#define VSE_LINE_MOVE 1
#define VSE_LINE_COPY 2

vse_mov_lin()
{
	vse_mov_cpy(VSE_LINE_MOVE);
}
vse_cpy_lin()
{
	vse_mov_cpy(VSE_LINE_COPY);
}
vse_mov_cpy(type) int type;
{
	int valid;
	
	mov_row = ed_oa[3];
	memcpy(save_oa,ed_oa,4);
	ed_oa[3] = ed_oa[2] = 0;
	if(mov_row < 5)
		{
		mov_row = 5;
		}
	mov_row -= 5;
	while(mov_row > -1)
		{
		if(ed_txt[mov_row])
			break;
		--mov_row;
		}
	init_mov_fields();
	
	for(;;)
		{
		screen_error = 0;
		vse_ed_mov_init(type);
		d_and_r_ed(TAB_TO_FIELD);
		strcpy(emsg_field,"");
		int4_from_str2(ed_pfcode,&vse_edit_pick);
		if(1 == vse_edit_pick)
			break;
		vse_file_changed=1;
		if (type==VSE_LINE_MOVE)
		  vseunscr(ed_mov_flds,ed_scr);
		else
		  vseunscr(ed_cpy_flds,ed_scr);
		vse_ed_mov_val();
		if(!screen_error)
			{
			  vse_ed_mov_dispatch(type);
			  break;
			}
		}
	memcpy(ed_oa,save_oa,4);
}

/*----
Clear error and end fields, load start field with current line no
------*/
init_mov_fields()
{
	CLEAR_FIELD(end_field);
	CLEAR_FIELD(emsg_field);
	CLEAR_FIELD(start_field);
	sprintf(dest_field,"%06ld          ",ed_txt[mov_row]->lineno);
}
/*----
Clear vwang screen, set up pfs, load the screen
------*/
vse_ed_mov_init(type)int type;
{
	vsescr_init(ed_scr);
/* 
   This routine from the main screen is only used to get the
   text correctly loaded
*/
	vse_ed_load_full_lines();
	strcpy(ed_pfs,"0001X");
	vsescr(ed_line_flds,ed_scr);
	type==VSE_LINE_MOVE? vsescr(ed_mov_flds,ed_scr) :vsescr(ed_cpy_flds,ed_scr) ;
	  
	vse_ed_add_numbers();
	vse_ed_add_mod();
}
/*----
Valid entries are 
Start field = ALL
Start field = FIRST or a number
End field = LAST or a number
Start <= End
------*/

vse_ed_mov_val()
{
	if(!(strcmp(start_field,"ALL             ")))
	{
		init_mov_all();
		return;
	}
	if(!(strcmp(start_field,"FIRST           ")))
	{
		init_mov_first();
	}
	else
	{
		start_line = ATOI4(start_field);
	}
	if(!(strcmp(end_field,"LAST            ")))
	{
		init_mov_last();
	}
	else
	{
		end_line = ATOI4(end_field);
	}
	if(end_line == 0)
	{
		end_line = start_line;
	}

	/* If the starting line number is > then the last line number of the file,
	   an error should be displayed. Added by CIS: 07/23/93 AJA */
	if( start_line > text_last->lineno )
	{
		screen_error = 1;
		strcpy(emsg_field,"Invalid Range");
	}
	if(end_line < start_line)
	{
		screen_error = 1;
		strcpy(emsg_field,"Invalid Range");
	}
	if(!(strncmp(dest_field,"LAST",4)))
	{
		dest_line = text_last->lineno;
	}
	else if (!(strncmp(dest_field,"FIRST",5)))
	{	
		dest_line = text_first->lineno;
	}
	else
	{
		dest_line = ATOI4(dest_field);
	}
	if (dest_line < end_line && dest_line >= start_line)
	{
		screen_error = 1;
		strcpy(emsg_field,"Invalid Range");
	}
}

/*---
These convert FIRST, LAST and ALL literals into line numbers
------*/
init_mov_all()
{
	init_mov_first();
	init_mov_last();
}
init_mov_first()
{
	start_line = text_first->lineno;
}
init_mov_last()
{
	end_line = text_last->lineno;
}

/*----
No real dispatching here, just call the move routine
------*/
vse_ed_mov_dispatch(type) int type;
{
	switch(vse_edit_pick)
		{
		case 0:
			vse_ed_mov_range (type);
			break;
		}
}
/*----
Search from the top of the text work area until end or
line number is > end line.
Delete anything within the delete request range
If the deleted field happens to be the line at the top of the
screen, then mov the screen pointer forward to the next field
------*/
vse_ed_mov_range(type) int type;
{
	TEXT *txt=NULL,*start=NULL,*end=NULL,*dest=NULL,*last_txt=NULL,*p;
	TEXT *ostart,*oend;
	
	int diff, num,  inc;
	
	/* Corrected logic. start should equal the first line with a line number
	   greater than or equal to start_line, end should equal the first line
	   that is less than or equal to end_line, and dest should equal the first
	   line that is closest to dest_line. Added by CIS: 07/22/93 AJA */
	for (txt = text_first, num=0;
	     txt && (!start || !end || !dest) ;
	     txt = txt->next, (start && !end)?++num:num)
	{
		if ( !start )
		{
			if (txt->lineno >= start_line)
				start=txt;
		}
		if ( !end )
		{
			if (txt->lineno == end_line)
				end=txt;
			else if ( txt->lineno > end_line )
				end = txt->prev;
		}
		if ( !dest )
		{
			if (txt->lineno == dest_line)
				dest=txt;
			else if ( txt->lineno > dest_line )
				dest = txt->prev;
		}
		last_txt = txt;
	}

	/* If the end of the text came before end or dest was set, set them to
	   last_txt which is the last line of text. Added by CIS: 07/22/93 AJA */
	if ( !end )
		end = last_txt;
	if ( !dest )
		dest = last_txt;
	if (dest->next && (num >= dest->next->lineno - dest->lineno))
	{
		extern int start_renum, inc_renum, startl_renum, endl_renum;

		start_renum = 100;
		inc_renum = 100;
		startl_renum= -1;
		endl_renum= -1;
		if (ask_renumber(num))
		  renumber_text();
		else
			return;
	}
	ostart=start;
	oend=end;
	copy_block(&start,&end,type);
	end->next = dest->next;
	if (end->next)
	  end->next->prev = end;
	dest->next = start;
	start->prev = dest;
	if (text_last==dest)
	  text_last=end;
	renumber_range(start,end,-1,-1,-1);
	if (type == VSE_LINE_MOVE)
	{
		start_line = ostart->lineno;
		end_line = oend->lineno;
		vse_ed_del_range();
	}
}
ver()
{
#if 0
	for (txt=text_first; txt ; txt=txt->next)
	{
		if (txt==text_last)
		  vre_window("reached text last");
	}
	for (txt=text_last; txt ; txt=txt->prev)
	{
		if (txt==text_first)
		  vre_window("back to text first");
	}
#endif
}

copy_block(st,end,type)
TEXT **st,**end;
int  type;
{
	TEXT *p;
	TEXT *newst, *newp;
	void *calloc();
	char *mystrdup();
	int notdone;
	
	for (notdone=1, p = *st, newst=newp=NULL; notdone; p=p->next)
	{
		if (newst==NULL)
		{
			newp = newst = (TEXT*) calloc(sizeof(TEXT),1);
		}
		else
		{
			newp->next = (TEXT*) calloc(sizeof(TEXT),1);
			newp->next->prev = newp;
			newp=newp->next;
		}

		/* If moving text, copy the line number from p so that when renumbering
		   occurs and the language is BASIC, the lines will renumber correctly.
		   If you are copying text, set the new line number for the duplicate
		   lines to 0 so that when renumbering in BASIC you do not renumber the
		   references to point to the new lines but the old ones.
		   Modified by CIS: 08/05/93 AJA */
		if ( type == VSE_LINE_MOVE )
			newp->lineno = p->lineno;
		else
			newp->lineno = 0;
		newp->text = mystrdup(p->text);
		if ( p->modfld )
		{
			newp->modfld = (char *) calloc( 9, sizeof( char ) );
			strncpy( newp->modfld, p->modfld, 8 );
		}
		else
			newp->modfld = NULL;

		/* If language is BASIC, look for embedded line number in the new copy
		   of the BASIC line. Added by CIS, 08/03/93 AJA */
		if ( !(strcmp( vse_gp_input_language, BASIC_LANGUAGE )) )
			find_linenum( newp );
		if (p== *end)
		  notdone=0;
	}
	*st = newst;
	*end = newp;
}
char *mystrdup(str)
char *str;
{
	void *malloc();
	static char *x;
	x=(char*)malloc(strlen(str)+1);
	strcpy(x,str);
	return x;
}
renumber_range(st_txt,end_txt,gp_start,gp_cnt,gp_incr)
TEXT 
  *st_txt, 
  *end_txt;
int 
  gp_start,
  gp_cnt,
  gp_incr;
{
	int stnum,stline,endline,diff,inc,num,i,maxinc;
	TEXT *p;

	stline = st_txt->prev? st_txt->prev->lineno : st_txt->lineno;
	endline = end_txt->next?end_txt->next->lineno:999999;

	if (st_txt==end_txt)
	{
		if (gp_start > 0)
		{
			st_txt->lineno = gp_start;
			return 0;
		}
	}
	
	diff=endline-((gp_start>0)?gp_start:stline);           /* difference;  (range we can renumber in) */
	if (diff <= 0)                 /* if the difference is zero or less, renumber everything */
	{
		diff=999999;
	}
	if (gp_cnt != -1)		       /* if caller provided a line count, use it */
	  num=gp_cnt;
	else
	{
		for (num=0, p=st_txt; p!=end_txt->next; p=p->next)
		  ++num;
	}
	if (++num > diff) return -1;     /* cannot renumber, not enough numbers */
	inc= maxinc = diff / num;
	if (inc >=100) inc=100;
	else if (inc <100 && inc >=50) inc=50;
	else if (inc <50 && inc >=10) inc=10;
	else if (inc <10 && inc >=5) inc=5;
	else if (inc <5 && inc >=2) inc=2;
	else if (inc <2 ) inc=1;

	if (gp_incr != -1 && inc > gp_incr)	       /* if caller provided increment, use it */
	  inc = gp_incr;
	if (gp_incr != -1 && inc < gp_incr && end_txt==text_last)
	  inc = gp_incr;
	else if (gp_incr != -1 && inc < gp_incr && gp_incr<= maxinc && end_txt!=text_last)
	  inc = gp_incr;
	else if (gp_incr != -1 && maxinc < gp_incr && end_txt!=text_last)
	{
		vse_renum_range_low = 1;
		vse_renum_range_hi = maxinc;
		return RENUM_INVAL_INCR;
	}

	stnum=(gp_start != -1)?gp_start:(st_txt->prev?(st_txt->prev->lineno)+inc:st_txt->lineno);
	if ((stnum + inc*(num-1)) > (end_txt->next?end_txt->next->lineno:999999))
	{
		vse_renum_range_low=vse_renum_range_hi= -1;
		return RENUM_BAD_RANGE;
	}

	for (p=st_txt,num=stnum;
	     p && p!=end_txt->next; 
	     p=p->next, num+=inc)
	{
	
		/* If language is BASIC, look for references to old line number in
		   list and change those to the new line number in text and list.
		   Added by CIS, 07/12/93 AJA */
		if (!(strcmp( vse_gp_input_language, BASIC_LANGUAGE )))
			update_linenum( p->lineno, num );
	  	p->lineno = num;
	}

	/* After renumbering is complete, update the branch values in the linked
	   list of embedded BASIC line numbers. Added by CIS, 07/13/93 AJA */
	if (!(strcmp( vse_gp_input_language, BASIC_LANGUAGE )))
		update_branch();
	return 0;
}

