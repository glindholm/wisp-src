/*----
This file contains the various GETPARM screens used by vse.
it displays the getparm as a system_name style getparm,
otherwise it uses a std wang style input.
It is passed an error message.
It returns the value of the pressed pfkey
------*/
#include <stdio.h>
#include "vseglb.h"
#include "vsegp.h"
#include "vsedscr.h"

static char logo[]="Program Development Editor v 1.00";

/* Option BASIC added by CIS 07/09/93 AJA */
static char lang_opts[]="(COBOL,C,TEXT,SHELL,BASIC)";

static char fill_in[]="Fill in the information below to create a new file of the edited text.";
static char to_exit[]="Press PF1 to exit the create command.";
static char specify[]="Please specify:";

static char source[]="Source files . . . . . . . . . .";
static char go_native[]="(Press PF5 to switch to native mode";
static char go_vs[]=    "(Press PF5 to switch to   VS   mode";
static char end_editor[]="Press PF16 to end Editor Processing)";

char vse_out_file[9];
char vse_out_lib[9];
char vse_out_vol[7];
char vse_out_ext[4];

static char file_info[80];
static char modified[30];
static char line_cnt[8];
static char what_happens[80];
       
static VSESCR_FLDS(no_file_created) = {
{LEN(0)	 ROW(1)	COL(2)	VALUE("Copyright 1992 IDSI - Integrated Program Editor")},
{LEN(78) ROW(2)	COL(2)	FROM(file_info)},      
{LEN(0)	 ROW(3)	COL(2)	VALUE("There are")},
{LEN(7) ROW(3)	COL(12)	FROM(line_cnt)},      
{LEN(0)	 ROW(3)	COL(21)	VALUE("lines in the edited text.")},
{LEN(29) ROW(3)	COL(49)	FROM(modified)},      
{LEN(0)	 ROW(5)	COL(2)	VALUE("WARNING - No file has been created on this run.")},
{LEN(79) ROW(7)	COL(2)	FROM(what_happens)},
{LEN(0)	 ROW(8)	COL(2)	VALUE("Press PF1 to return to the special menu.")},
{LASTITEM}
};

vse_create(exists)
int exists;
{
	wpload();

	do
	{
		if (!exists)
		{
			CLEAR_FIELD(vse_out_file);
			CLEAR_FIELD(vse_out_lib);
			CLEAR_FIELD(vse_out_vol);
			CLEAR_FIELD(vse_out_ext);
			CLEAR_FIELD(vse_sysname);

			gppfkeys=GP_PF_01|GP_PF_05;
		}
		else
		  gppfkeys=GP_PF_01|GP_PF_05|GP_PF_03;
		
		wswap(&gppfkeys);
		
		GPSETUP();
		if (!exists)
		{
			GPSTD("OUTPUT  ","EDITOR",2);
			GPMSG(fill_in);
			GPMSG(to_exit);
		}
		else
		{
			GPRES("OUTPUT  ","EDITOR",4);
			GPMSG("Warning - The file specified below already exists.");
			GPMSG("          Use PF3 if you wish to scratch the existing file and continue.");
			GPMSG("          Otherwise, please specify another file name.");
			GPMSG("          Use PF1 to return to the special command menu.");
		}
		GPCTEXT(specify,12,3);
		if(vse_native)
		{
			GPSYSNAME(vse_sysname,17);
		}
		else
		{
			GPFILE(vse_out_file,15,5);
			GPLIB(vse_out_lib,15,28);
			GPVOL(vse_out_vol,15,51);
			GPEXT(vse_out_ext,16,5);
			GPCTEXT("IN",15,25);
			GPCTEXT("ON",15,48);
		}
		GPENTER();
		GPPFS(&gppfkeys);
		
		vse_create_pick = display_and_read_gp();
		if (vse_create_pick == 5)
		  vse_native = 1 - vse_native;
	} while (vse_create_pick == 5);
	
	memcpy(vse_gp_input_file,vse_out_file,8);
	memcpy(vse_gp_input_ext,vse_out_ext,3);
	memcpy(vse_gp_input_library,vse_out_lib,8);
	memcpy(vse_gp_input_volume,vse_out_vol,6);
	return vse_create_pick;
}
vse_renum_gp(n,i,s,e,resp)
int *n,*i,*s,*e;
int resp;
{
	static char numb[7],incr[7];
	static char start[17],end[17];
	int ret;
	static int first=1;

	if (first && resp==0)
	{
		sprintf(numb,"%-6d",100);
		sprintf(incr,"%-6d",100);
		strcpy(start,"ALL             ");
		strcpy(end,"                ");
		first=0;
	}
	gppfkeys=GP_PF_01;
	wswap(&gppfkeys);
	GPSETUP();
	if (resp==0)
	{
		GPSTD("RENUMBER","EDITOR",3);
	}
	else
	{
		GPRES("RENUMBER","EDITOR",3);
	}
	switch(resp)
	{
	      case 0:
		GPMSG(" ");
		break;
	      case RESP_STARTNUM:
	      {
		      char message[80];
		      sprintf(message," Sorry - Starting number must be between %d and %d.",
			      vse_renum_range_low,vse_renum_range_hi);			 
		      GPMSG(message);
		      break;
	      }
	      case RESP_INCR:
	      {
		      char message[80];
		      sprintf(message," Sorry - Increment must be between %d and %d.",
			      vse_renum_range_low,vse_renum_range_hi);			 
		      GPMSG(message);
		      break;
	      }
	      case RESP_RANGE:
	      case 4:
		GPMSG(" Sorry - Invalid range.");
		break;
	}
	GPMSG(" ");
	GPMSG("                          Renumber Command");
	GPCTEXT("To renumber a portion of the edited file, fill in the information below",11,2);
	GPCTEXT("and press ENTER.  Press PF1 to exit without renumbering the file.",12,2);
	GPCTEXT("Renumbering information:",14,2);
	GPCTEXT("New starting line number:",16,6);
	GPCTEXT("Increment to be used:",17,6);
	GPCTEXT("Range of lines to be renumbered:",19,2);
	GPCTEXT("(Note: Specify line numbers as they are before renumbering)",20,2);
	GPCTEXT("Start of range:",22,6);
	GPCTEXT("End of range:",23,6);
	
	if (resp!=RESP_STARTNUM)
	{
		GPKW("NUMBER  ",numb,6,16,33,"N");
	}
	else
	{
		GPRESP("NUMBER  ",numb,6,16,33,"N");
	}
	if (resp!=RESP_INCR)
	{
		GPKW("INCR    ",incr,6,17,33,"N");
	}
	else
	{
		GPRESP("INCR    ",incr,6,17,33,"N");
	}
	if (resp!=RESP_RANGE)
	{
		GPKW("START   ",start,16,22,33,"A");
	}
	else
	{
		GPRESP("START   ",start,16,22,33,"A");
	}
	if (resp!=RESP_RANGE)
	{
		GPKW("END     ",end,16,23,33,"A");
	}
	else
	{
		GPRESP("END     ",end,16,23,33,"A");
	}
	GPENTER();
	GPPFS(&gppfkeys);
	
	ret=display_and_read_gp();
	*n=atoi(numb);
	*i=atoi(incr);
	if (!text_first) 
	{
		*s =0;
		*e =0;
	}
	else
	{
		*s = atoi(start);
		*e = atoi(end);
		if (*s && ! *e)
		{
			*e = *s;
		}
	}
	if (!strncmp(start,"ALL",3))
	{
		*s = text_first->lineno;
		*e = text_last->lineno;
	}
	if (!strncmp(start,"FIRST",5))
	{
		*s = text_first->lineno;
		*e = atoi(end);
	}
	if (!strncmp(end,"LAST",4))
	{
		*s = atoi(start);
		*e = text_last->lineno;
	}
	return ret;
}
vse_settabs_gp(tablist,md,cs,resp)
int *tablist,*md,*cs;
int resp;
{
	static char tabs[30],mode[6],cse[6],show[4];
	char tabcopy[30];
	int ret,i;
	char *p, *strtok();
	
	if (resp==0)
	{
		strcpy(tabs,vse_gp_defaults_tabs);
		strcpy(mode,vse_gp_defaults_mode);
		strcpy(cse,vse_gp_defaults_case);
		strcpy( show, vse_gp_defaults_showmod );
	}
	GPSETUP();
	gppfkeys=GP_PF_01;
	wswap(&gppfkeys);
	GPSTD("DEFAULTS","EDITOR",2);
	GPMSG("To alter the current editor defaults, modify in place and press enter,");
	GPMSG("or press PF1 to exit without changing the defaults.");
	GPCTEXT("(Set to \"UPLOW\" to enable lower case input)",11,26);
	GPCTEXT("(Set to \"ANY\" For case-insensitive searching)",12,26);
	GPCTEXT( "(Set to \"YES\" to view modification codes)", 13, 26 );

	if (resp!=1)
	{
		GPKW("TABS     ",tabs,29,10,6,"N");
	}
	else
	{
		GPRESP("TABS     ",tabs,29,10,6,"N");
	}
	if (resp!=2)
	{
		GPKW("MODE     ",mode,5,11,6,"A");
	}
	else
	{
		GPRESP("MODE     ",mode,5,11,6,"A");
	}
	if (resp!=3)
	{
		GPKW("CASE     ",cse,5,12,6,"A");
	}
	else
	{
		GPRESP("CASE     ",cse,5,12,6,"A");
	}
	if ( resp != 4 )
	{
		GPKW("MODCODES ",show,3,13,6,"A");
	}
	else
	{
		GPRESP("MODCODES ",show,3,13,6,"A");
	}
	GPENTER();
	GPPFS(&gppfkeys);
	
	ret=display_and_read_gp();

	if ( ret != 1 )
		strncpy( vse_gp_defaults_showmod, show, 3 );

	if (!strncmp(mode,"UPPER",5))
	  *md = 1;
	else if (!strncmp(mode,"UPLOW",5))
	  *md = 0;
	else 
	  return -2;
	
	if (!strncmp(cse,"ANY",3))
	  *cs = 1;
	else if (!strncmp(cse,"EXACT",5))
	  *cs = 0;
	else
	  return -3;

	strcpy(tabcopy,tabs);
	strcpy(vse_gp_defaults_tabs,tabs);
	*tablist = atoi(tabcopy);
	for (i=1, p=strtok(tabcopy," "); i<10 && p; ++i, p=strtok(0," "))
	{
		if (atoi(p)==0)
		  return -1;
		*++tablist = atoi(p);
	}
	return ret;
}

/* vse_set_options_gp will bring up a GETPARM screen so the user can enter
   options for renumbering on replace, start line number, increment, and
   modcode. Added by CIS: 08/12/93 AJA */
int vse_set_options_gp()

{
	char renum[4];		/* Should we renumber upon replace */
	char start[7];		/* Start line number */
	char inc[7];		/* Increment of line number */
	char modcode[9];	/* Modcode */

	int ret;

	strncpy( renum, vse_gp_options_renumber, 3 );
	strncpy( start, vse_gp_options_number, 6 );
	strncpy( inc, vse_gp_options_incr, 6 );
	strncpy( modcode, vse_gp_options_modcode, 8 );

	GPSETUP();
	gppfkeys=GP_PF_01;
	wswap(&gppfkeys);
	GPSTD("OPTIONS","EDITOR",0);

	GPCTEXT( "TO CHANGE THE OPERATING ENVIRONMENT OF THE EDITOR, PLEASE SPECIFY THE", 9, 2 );
	GPCTEXT( "DESIRED OPTIONS BELOW AND PRESS ENTER.", 10, 2 );
	GPCTEXT( "RENUMBER THE INPUT FILE BEFORE EVERY REPLACE?", 12, 2 );
	GPCTEXT( "(YES/NO)", 12, 70 );
	GPCTEXT( "DEFAULT STARTING LINE NUMBER", 14, 2 );
	GPCTEXT( "AND INCREMENT TO BE USED FOR RENUMBERING?", 15, 2 );
	GPCTEXT( "MODIFICATION CODE TO BE PLACED IN", 17, 2 );
	GPCTEXT( "COLUMNS 73 TO 80 OF ADDED AND CHANGED LINES?", 18, 2 );
	GPCTEXT( "PRESS PF1 TO RETURN TO THE SET COMMAND MENU.", 24, 18 );

	GPKW( "RENUMBER", renum, 3, 12, 54, "A" );
	GPKW( "NUMBER  ", start, 6, 14, 54, "N" );
	GPKW( "INCR    ", inc, 6, 15, 54, "N" );
	GPKW( "MODCODE ", modcode, 8, 18, 54, "C" );

	GPENTER();
	GPPFS(&gppfkeys);

	ret=display_and_read_gp();

	if ( ret != 1 )
	{

		/* Unload the variables */
		strncpy( vse_gp_options_renumber, renum, 3 );
		strncpy( vse_gp_options_number, start, 6 );
		strncpy( vse_gp_options_incr, inc, 6 );
		strncpy( vse_gp_options_modcode, modcode, 8 );

		if ( !(strncmp( vse_gp_options_renumber, "YES", 3 )) )
		{
			vse_options_number = atoi( vse_gp_options_number );
			vse_options_incr = atoi( vse_gp_options_incr );
		}
	}
}

vse_no_file_created(rest)
int rest;
{
	int ret;
	int cnt;
	TEXT *txt;
	
	for(cnt=0, txt=text_first; txt; txt=txt->next, ++cnt);
	CLEAR_FIELD(file_info);
	CLEAR_FIELD(modified);
	CLEAR_FIELD(what_happens);
	if (strlen(vse_sysname) && !isspaces(vse_sysname))
	{
		sprintf(file_info,"The current input file is %s",vse_sysname);
	}
	else
	{
		strcpy(file_info,"There is no current input file. ");
	}
	if (vse_file_changed)
	{
		strcpy(modified,"(The file has been modified.)");
	}
	if (rest)
	{
		strcpy(what_happens,"Press PF16 to restart the editor without saving the edited text.");
	}
	else
	{
		strcpy(what_happens,"Press PF16 to exit the editor without saving the edited text.");
	}		
	sprintf(line_cnt,"%7ld",cnt);
	vsescr_init(ed_scr);
	strcpy(ed_pfs,"0116X");
	
	vsescr(no_file_created,ed_scr);
	ed_oa[2]=ed_oa[3]=0;
	
	d_and_r_ed(TAB_TO_FIELD);
	int4_from_str2(ed_pfcode,&ret);
	return ret;
}
#if 0
isspaces(p)
char *p;
{
	while (*p)
	{
		if (*p != ' ')
		  return 0;
	}
	if (*p == (char)0)
	  return 1;
}
#endif
