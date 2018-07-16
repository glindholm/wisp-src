#include <stdio.h>
#include "vseglb.h"
#include "vsescr.h"

static char spc_scr[1924];
static char spc_pfs[]="01020304050607081516X";
static char set_pfs[]="010203X";
static char spc_func[]={03};
static char spc_lines[]={24};
static char spc_pfcode[3];
static char set_pfcode[3];
static char spc_status[3];
static char input_message[81];

static VSESCR_FLDS(spc_flds) = {
{LEN(0)	ROW(1)	COL(2)	VALUE("Integrated Program Editor")},
{LEN(0)	ROW(2)	COL(2)	FROM(vse_file_message)},
{LEN(0)	ROW(3)	COL(2)	FROM(vse_lines_message)},
{LEN(0)	ROW(4)	COL(2)	BRIGHT(vse_stat_message)},
{LEN(0)	ROW(6)	COL(2)
VALUE("Please select the desired function and press the appropriate PF key:")},
{LEN(0)	ROW(8)	COL(2)
VALUE(" (1) Display       - Resume text editing")},
{LEN(0)	ROW(9)	COL(2)
VALUE(" (2) Set           - Set workstation defaults")},
{LEN(0)	ROW(10)	COL(2)
VALUE(" (3) Menu          - Activate the normal menu")},
{LEN(0)	ROW(11)	COL(2)
VALUE(" (4) Restart       - Edit another file")},
{LEN(0)	ROW(12)	COL(2)
VALUE(" (5) Create        - Create a new file from the edited text")},
{LEN(0)	ROW(13)	COL(2)
VALUE(" (6) Replace       - Replace the input file with the edited text")},
{LEN(0)	ROW(14)	COL(2)
VALUE(" (7) Renumber      - Generate new line numbers for the edited text")},
/*{LEN(0)	ROW(15)	COL(2)
VALUE(" (8) External Copy - Copy a range of lines from another file")},*/
/*{LEN(0)	ROW(22)	COL(2)
VALUE("(15) Print         - Print the edited text")},*/
{LEN(0)	ROW(23)	COL(2)
VALUE("(16) Exit          - End Editor processing")},
{LASTITEM}
};

/* Use these fields to bring up a screen so that the screen to set mod codes
   can be selected. Added by CIS: 08/11/93 AJA */
static VSESCR_FLDS(set_flds) = {
{LEN(0)	ROW(1)	COL(35)	ULINEVALUE("SET COMMAND")},
{LEN(0) ROW(4)  COL(2) VALUE("PLEASE SELECT THE DESIRED FUNCTION AND PRESS THE APPROPRIATE PF KEY.")},
{LEN(0) ROW(5) COL(2) VALUE("PRESS PF1 TO RETURN TO SPECIAL MENU")},
{LEN(0) ROW(7) COL(4) ULINEVALUE("EDITOR OPTIONS")},
{LEN(0) ROW(8) COL(6) VALUE("(2)  SET TABS, UPPER/LOWER CASE AND SEARCH MODES")},
{LEN(0) ROW(9) COL(6) VALUE("(3)  SET AUTOMATIC RENUMBER MODES")},
{LEN(0) ROW(10) COL(11) VALUE("AND MODIFICATION CODE")},
{LASTITEM}
};

vse_special_menu()
{
	init_special_menu();
	vwang(spc_func,spc_scr,spc_lines,spc_pfs,spc_pfcode,spc_status);
	int4_from_str2(spc_pfcode,&vse_special_pick);
	vse_special_dispatch();
}

vse_set_command_menu()
{
	init_set_command_menu();
	vwang(spc_func,spc_scr,spc_lines,set_pfs,set_pfcode,spc_status);
	int4_from_str2(set_pfcode,&vse_set_command_pick);
	vse_set_dispatch();
}

init_special_menu()
{
	extern char ed_oa[];
	
	vse_save_row=ed_oa[3];
	vse_save_col=ed_oa[2];

	memcpy(spc_scr,vse_default_oa,sizeof(vse_default_oa));
	vse_top_messages();
	vsescr_init(spc_scr);
	vsescr(spc_flds,spc_scr);
}

init_set_command_menu()
{
	extern char ed_oa[];
	
	vse_save_row=ed_oa[3];
	vse_save_col=ed_oa[2];

	memcpy(spc_scr,vse_default_oa,sizeof(vse_default_oa));
	vse_top_messages();
	vsescr_init(spc_scr);
	vsescr(set_flds,spc_scr);
}

vse_special_dispatch()
{
	switch(vse_special_pick)
		{
		case 1:
			vse_ed_scr();
			break;
		case 3:
			vse_menu = EDIT_MENU;
			break;
		case 5:
			vse_create_file();
			break;
		case 6:
			vse_old_file();
			break;
		case 7:
			vse_renum();
			break;
		case 2:
			if( !(strcmp(vse_gp_input_language,COBOL_LANGUAGE)) ||
				!(strcmp( vse_gp_input_language, BASIC_LANGUAGE )) )
			{

				/* Allow the user the oportunity to set mod code and default
				   renumbering parameters. Added by CIS: 08/11/93 AJA */
				for ( ;; )
				{
					vse_set_command_menu();	
					if ( vse_set_command_pick == 1 )
						break;
				}
			}
			else
				vse_set();
			break;
		case 8:
			vse_copy_file();
			break;
		case 15:
			vse_print();
			break;
		case 4:
			if (vse_file_changed)
			{
				int ret;
				
				ret=vse_no_file_created(1);
				if (ret==1)
				{
					vse_special_pick=1;
					break;
				}
			}
			free_text();

			/* If BASIC, free up list of line numbers.
			   Added by CIS, 07/13/93 AJA */
			if ( !(strcmp( vse_gp_input_language, BASIC_LANGUAGE )) )
				free_linenum();
			vse_restart();
			break;
		case 16:
			if (vse_file_changed)
			{
				int ret;
				
				ret=vse_no_file_created(0);
				if (ret==1)
				{
					vse_special_pick=1;
					break;
				}
			}
			free_text();

			/* If BASIC, free up list of line numbers.
			   Added by CIS, 07/13/93 AJA */
			if ( !(strcmp( vse_gp_input_language, BASIC_LANGUAGE )) )
				free_linenum();
			break;
		default:
			break;
		}
}

vse_set_dispatch()
{
	switch ( vse_set_command_pick )
	{
		case	1:	break;

		case	2:	vse_set();
					break;

		default	 :	vse_set_options_gp();
	}
}

vse_create_file()
{
	char write_func=1;
	int exists=0,cnt;
	TEXT *txt;
	
	for(cnt=0, txt=text_first; txt; txt=txt->next, ++cnt);
      create_top:
	vse_create(exists);
	if(vse_create_pick == 16 || vse_create_pick==1)
		return;
	if(!vse_native)
		build_system_name();
	trunc(vse_sysname);
	if(val_sysname_file_exists() && vse_create_pick!=3)
	{
		exists=1;
		goto create_top;
	}
/* Save it and then reload it */
	makepath(vse_sysname);
	
	CLEAR_FIELD(vse_stat_message);
	sprintf(vse_stat_message,"Create in progress");
	init_special_menu();
	vwang(&write_func,spc_scr,spc_lines,spc_pfs,spc_pfcode,spc_status);

	save_file();
	free_text();

	/* If BASIC, free up list of line numbers.
	   Added by CIS, 07/13/93 AJA */
	if ( !(strcmp( vse_gp_input_language, BASIC_LANGUAGE )) )
		free_linenum();
	load_file();

	if(!vse_native)
		build_system_name();

	CLEAR_FIELD(vse_stat_message);
	trunc(vse_sysname);
	sprintf(vse_stat_message,"File %s created with %7ld records",vse_sysname,cnt);
	untrunc(vse_sysname,80);

	scr_first = text_first;
}

vse_old_file()
{
	char write_func=1;
	int cnt;
	TEXT *txt;
	char write_func1;
	
	if (strlen(vse_sysname)==0 || isspaces(vse_sysname))
	{
		vse_create_file();
		return;
	}	
	for(cnt=0, txt=text_first; txt; txt=txt->next, ++cnt);

	CLEAR_FIELD(vse_stat_message);
	sprintf(vse_stat_message,"Replace in progress");
	init_special_menu();
	vwang(&write_func,spc_scr,spc_lines,spc_pfs,spc_pfcode,spc_status);

	/* If automatic renumbering before replace is specified, do it.
	   Added by CIS: 08/12/93 AJA */
	if ( !(strncmp( vse_gp_options_renumber, "YES", 3 )) )
		renumber_range( text_first, text_last, vse_options_number, cnt,
			vse_options_incr );
	save_file();
	free_text();

	/* If BASIC, free up list of line numbers.
	   Added by CIS, 07/13/93 AJA */
	if ( !(strcmp( vse_gp_input_language, BASIC_LANGUAGE )) )
		free_linenum();
	load_file();

	CLEAR_FIELD(vse_stat_message);
	trunc(vse_sysname);
	sprintf(vse_stat_message,"File %s written with %7ld records",vse_sysname,cnt);
	untrunc(vse_sysname,80);

	scr_first = text_first;
}
vse_renum()
{
	int startnum,incr,stline,endline;
	TEXT *st, *end, *txt;
	int resp,ret;

	resp=0;
	
      top_renum:

	ret=vse_renum_gp(&startnum,&incr,&stline,&endline,resp);

	if (ret==1) 
	  return;
	
	for (txt = text_first, st=NULL, end=NULL;
	     txt && (!st || !end) ;
	     txt = txt->next)
	{
		if (txt->lineno == stline)
		  st=txt;
		if (txt->lineno == endline)
		  end=txt;
	}
	if (!st)
	{
		resp=RESP_RANGE;
		goto top_renum;
	}
	if (!end) 
	{
		resp=RESP_RANGE;
		goto top_renum;
	}		
	if (incr <= 0)
	{
		resp=RESP_INCR;
		goto top_renum;
	}
	if ((startnum > text_last->lineno) &&
	    !((st==text_first) && (end==text_last)))
	{
		resp=RESP_STARTNUM;
		vse_renum_range_low = vse_renum_range_hi = -1;
		goto top_renum;
	}
	if ((startnum < (st->prev ? st->prev->lineno: 0)) &&
	    !((st==text_first) && (end==text_last)))
	{
		int num;
		TEXT *p;
		
		resp=RESP_STARTNUM;
		vse_renum_range_low = st->prev->lineno+1;
		if (st==end)
		  num=1;
		else
		  for (num=0, p=st->next; p!=end; p=p->next)
		    ++num;
		vse_renum_range_hi = (end->next==NULL)? 999999-num : end->next->lineno-num;
		goto top_renum;
	}
	ret=renumber_range(st,end,startnum,-1,incr);
	if (ret == RENUM_INVAL_INCR)
	{
		resp=RESP_INCR;
		goto top_renum;
	}
	if (ret == RENUM_BAD_RANGE)
	{
		resp=RESP_RANGE;
		goto top_renum;
	}
}
vse_set()
{
	int tabs[10],mode,cse,i,pos;
	char tabstr[81];
	int ret, resp;
	
	memset(tabstr,' ',80);
	tabstr[80]=0;
	resp=0;
	
      top_set:
	
	ret = vse_settabs_gp(tabs,&mode,&cse,resp);
	if (ret==1)
	  return;
	if (ret <0)
	{
		resp = -ret;
		goto top_set;
	}
	for (i=0; i<10; ++i)
	{       pos = tabs[i] + ((vse_numbering)?2:7);
		if (pos<80)
		  tabstr[pos]='X';
	}

	strncpy(vse_tab_setting,tabstr,80);
	vse_tab_setting[80]=0;
	if (mode==1)
	{
		strcpy(vse_gp_defaults_mode,"UPPER");
	}
	else
	{
		strcpy(vse_gp_defaults_mode,"UPLOW");
	}
	if (cse==1)
	{
		strcpy(vse_gp_defaults_case,"ANY  ");
	}
	else
	{
		strcpy(vse_gp_defaults_case,"EXACT");
	}
}

vse_restart()
{
	vse();
	vexit();
}
vse_copy_file()
{
	
}
vse_print()
{
	
}

