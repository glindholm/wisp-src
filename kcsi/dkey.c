static char copyright[]="Copyright (c) 1988-1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
#include <stdio.h>
#include <string.h>
#include "kcsio.h"
#include "dbsc.h"
#include "dglb.h"
#include "shrthand.h"
#include "iocode.h"
#include "cobioblk.h"
#include "dtype.h"
#include "dmnt.h"
#include "kcsifunc.h"

static char sccsid[]="@(#)dkey.c	1.4 1/30/93";

static void enter_key_init(char *idx,char *mode);
static int enter_key_entry(char *idx,char *mode);
static void init_enter_fields();
static void init_enter_facs(char *idx);
static void init_enter_footers(char *mode);
static void init_enter_pfs(char *mode);
static void val_key_fields(char *idx);
static void val_rel_key_fields();
static void val_keyed_key_fields(char *idx);
static void init_key_message_field(char *mode);
static void move_key_fld(FIELD *fld,int key);
static void start_this_key(char *idx);
static void start_this_record_key(char *idx);

/*----
			KEY ENTRY LOGIC
------*/
/*----
For key entry, delete must always be by primary key.
Note mode is passed as 'A', 'B','C' for Add Change Delete.
Odd as 'C' is Delete, 'B' is change.
------*/
int dte_enter_the_key(char *idx,char *mode)
{
	int pf;

	while(1)
		{
/* If the file is relative, force the primary key */
		if(dte_file_is_relative())
			memcpy(idx,"017",IDX_LEN);
/* In delete mode, force the primary key */
		if(*mode == 'C')
			memcpy(idx,"017",IDX_LEN);
/* If the idx is zero then ask for a new index */
		if(Memeq(idx,"000",IDX_LEN))
			{
			DTEKEY(mode,idx);
			}
/* F is an illegal flag and should not occur here (see DTEMNU) */
		if(*mode == 'F')
			return(16);
		else
			{
			enter_key_init(idx,mode);
			pf = enter_key_entry(idx,mode);
/* PF 7 is change paths so set the idx to zero */
			if(pf == 7)
				memcpy(idx,"000",IDX_LEN);
/* Any key other than 7 causes an exit */
			if(pf != 7)
				break;
			}
		}
	return(pf);
}

static void enter_key_init(char *idx,char *mode)
{
	init_enter_fields();
	init_enter_facs(idx);
	init_enter_footers(mode);
	init_enter_pfs(mode);
	init_key_message_field(mode);
}

static int enter_key_entry(char *idx,char *mode)
{
	dte_screen_error = 1;
	while(dte_screen_error)
		{
		dte_screen_error = 0;
		dte_space_out((char*)dte_crt_file_status,2);
		memcpy(dte_on_pfkeys,"X",1);
		memcpy(dte_crt_record,dte_order_area,4);

		WSCREEN(dte_main_scr,
			dte_dnr_altered,
			dte_crt_record,
			dte_full_screen,
			dte_pfkeys,
			dte_on_pfkeys,
			dte_pfkey_code,
			dte_crt_file_status);

		dte_init_message_field();

		if(Memeq(dte_pfkey_code,"16",2))	/*Exit*/
			return(16);
		if(Memeq(dte_pfkey_code,"09",2))	/*Change to Add Mode*/
			return(9);
		if(Memeq(dte_pfkey_code,"07",2))	/*Change Path if not*/
			{				/*rel Shouldn't happen*/
			if(dte_file_is_relative())		/*as 7 now not enabled*/
				dte_screen_error = 1;	/*for rel files.      */
			else
				return(7);
			}
		if(Memeq(dte_pfkey_code,"02",2))	/*Find*/
			{
			dte_load_first_record(idx);
			if(!dte_screen_error)
				return(0);
			}
		if(Memeq(dte_pfkey_code,"04",2))	/*Previous*/
			{
			dte_load_previous_record(idx);
			if(!dte_screen_error)
			    return(0);
			}
		if(Memeq(dte_pfkey_code,"05",2))	/*Next*/
			{
			dte_load_next_record(idx);
			if(!dte_screen_error)
				return(0);
			}
		if(Memeq(dte_pfkey_code,"06",2))	/*Start partial key*/
			{
			if(dte_file_is_relative())		/* relfiles 26-Mar-1990*/
				dte_screen_error = 1;	/* relfiles 26-Mar-1990*/
			else
				{
				start_this_key(idx);
				if(!dte_screen_error)
					return(0);
				}
			}
		val_key_fields(idx);			/* Enter */
		if(!dte_screen_error)
			{
			return(0);
			}
		}
	return 0;
}

/*----
			KEY ENTRY SUPPORT
------*/
/*----
Should be nothing to do after the screen is built.
------*/
static void init_enter_fields()
{
	KCSI_clear_all_fields();
}
/*----
Again all okay based on the screen.
------*/
static void init_enter_facs(char *idx)
{
	dte_hide_all_fields();
	dte_hide_all_prompts();
	if(dte_file_is_relative())
		{
		dte_display_rel_prompt();
		dte_unprotect_rel_field();
		}
	else
		{
		dte_display_idx_prompts(idx);
		dte_unprotect_idx_fields(idx);
		}
}


/*----
key pfs and messages change for mode and file type.
------*/
static char del_key_footer[]=
    "(ENTER)Find (2)First (4)Prev (5)Next (6)by Inexact Key (16)Exit";
static char del_key_pfs[]="000204050616X";
static char chg_idx_footer[]=
    "(ENTER)Find (2)1st (4)Prev (5)Next (6)Inexact Key (7)Path (9)Add (16)Exit";
static char chg_idx_pfs[]="0002040506070916X";
static char chg_rel_footer[]=
    "(ENTER)Find (2)1st (4)Prev (5)Next (9)Add (16)Exit";
static char chg_rel_pfs[]="000204050916X";

static void init_enter_footers(char *mode)
{
	if(*mode == 'C')    /*delete mode */
		{
		dte_load_footer(del_key_footer);
		}
	else		    /* change mode */
	if(dte_file_is_relative())
		{
		dte_load_footer(chg_rel_footer);
		}
	else
		{
		dte_load_footer(chg_idx_footer);
		}


}
static void init_enter_pfs(char *mode)
{
	if(*mode == 'C')	/* delete mode */
		memcpy(dte_pfkeys,del_key_pfs,strlen(del_key_pfs));
	else
	if(dte_file_is_relative())	/* change mode */
		memcpy(dte_pfkeys,chg_rel_pfs,strlen(chg_rel_pfs));
	else
		memcpy(dte_pfkeys,chg_idx_pfs,strlen(chg_idx_pfs));
	
}

static void val_key_fields(char *idx)
{
	if(dte_file_is_relative())
		val_rel_key_fields();
	else
		val_keyed_key_fields(idx);
}

static void val_rel_key_fields()
{
	static char rnf[]="Error - Record Not Found";

	dte_read_rel_key_record();
	if(memcmp(&dte_dio_block[STATUS_POS],"00",STATUS_LEN))
		{
		KCSI_make_error_message(rnf);
		dte_screen_error = 1;
		}
	else
		{
		dte_rel_rec_num_to_fld();
		}

}

void dte_read_rel_key_record()
{
	char ufb[1];

	memcpy(dte_rel_rec_num,dte_relative_record.fld,REL_KEY_LEN);
	dte_rel_rec_num[REL_KEY_LEN] = 0;
	memcpy(&dte_dio_block[REL_KEY_POS],dte_rel_rec_num,REL_KEY_LEN);
	memcpy(&dte_dio_block[IO_KEY_POS],"00",2);
	memcpy(&dte_dio_block[IO_POS],READ_KEYED,IO_LEN);
	KCSIO(dte_dio_block,ufb,dte_record);
}

/*---- relfiles 26-Mar-1990
Extract the isam record number into the display field for this record.
------*/

void dte_rel_rec_num_to_fld()
{

    memcpy(dte_rel_rec_num,&dte_dio_block[REL_KEY_POS],REL_KEY_LEN);
    dte_rel_rec_num[REL_KEY_LEN] = 0;
    memcpy(dte_relative_record.fld,&dte_dio_block[REL_KEY_POS],REL_KEY_LEN);
}


static void val_keyed_key_fields(char *idx)
{
	int i,key;
	char ufb[1];
	static char rnf[]="Error - Record Not Found";

	key = dte_key_from_idx(idx);
	for(i = 0; dtefld[i].name[0] > ' '; ++i)
		{
		if(dtefld[i].frow)
			move_key_fld(&dtefld[i],key);
		}
	memcpy(&dte_dio_block[IO_KEY_POS],&idx[1],IO_KEY_LEN);
	if(Memeq(&dte_dio_block[IO_KEY_POS],"17",2))
		memcpy(&dte_dio_block[IO_KEY_POS],"00",2);
		
	memcpy(&dte_dio_block[IO_POS],READ_KEYED,IO_LEN);
	KCSIO(dte_dio_block,ufb,dte_record);
	if(memcmp(&dte_dio_block[STATUS_POS],"00",STATUS_LEN))
		{
		KCSI_make_error_message(rnf);
		dte_screen_error = 1;
		}
}

static void init_key_message_field(char *mode)
{
	static char chg_msg[]="Enter the key of the record to Change";
	static char del_msg[]="Enter the key of the record to Delete";

	if(*mode == 'B')
		KCSI_make_error_message(chg_msg);
	else
		KCSI_make_error_message(del_msg);
}

static void move_key_fld(FIELD *fld,int key)
{
	if(dte_is_this_key(fld,key))
		dte_move_one_to_record(fld);
}

void dte_load_first_record(char *idx)
{
	dte_init_record();	/* Blank out the dte_record */
	if(dte_file_is_relative())		    
	{			    
		memcpy(dte_relative_record.fld,"1       ", REL_KEY_LEN);    
		val_rel_key_fields(idx);
		if (!dte_screen_error)
		{
			dte_move_to_screen();
		}
		else
		{
			/* 
			** There is not a record 1 so 
			** clear the error message and 
			** try reading next record 
			*/
			dte_screen_error = 0;
			dte_init_message_field();
			dte_load_next_record(idx);
		}

	}			    
	else
	{
		start_this_record_key(idx);
	}
}

static void start_this_key(char *idx)
{
	int key,i;

	key = dte_key_from_idx(idx);

	for(i = 0; dtefld[i].name[0] > ' '; ++i)
		{
		if(dtefld[i].frow)
			{
			move_key_fld(&dtefld[i],key);
			}
		}
/* KCSIO uses 0 for the primary key */
	if(key == -1)
		key = 0;
	start_this_record_key(idx);
}
static void start_this_record_key(char *idx)
{
	char ufb[1];
	static char record_not_found[]="Error - Record Not Found";

	memcpy(&dte_dio_block[IO_KEY_POS],&idx[1],IO_KEY_LEN);
	if(Memeq(&dte_dio_block[IO_KEY_POS],"17",2))
		memcpy(&dte_dio_block[IO_KEY_POS],"00",2);
	memcpy(&dte_dio_block[IO_POS],START_NLT_KEYED,IO_LEN);
	KCSIO(dte_dio_block,ufb,dte_record);
	if(memcmp(&dte_dio_block[STATUS_POS],"00",STATUS_LEN))	/*IO no ok*/
		{
		dte_screen_error = 1;
		KCSI_make_error_message(record_not_found);
		return;
		}
	dte_load_next_record(idx);
}

void dte_load_next_record(char *idx)
{
	static char last_record[]="Error - Last Record Already Displayed.";
	char ufb[1];

	memcpy(&dte_dio_block[IO_POS],READ_NEXT_RECORD,IO_LEN);
	KCSIO(dte_dio_block,ufb,dte_record);
	if(memcmp(&dte_dio_block[STATUS_POS],"00",STATUS_LEN))	/*IO no ok*/
		{
		dte_screen_error = 1;
		KCSI_make_error_message(last_record);
		return;
		}
	else
		dte_move_to_screen();
	if(dte_file_is_relative())		    /* relfiles 26-Mar-1990*/
		dte_rel_rec_num_to_fld();	    /* relfiles 26-Mar-1990*/
}

/*----
Added to load a previous record from the current pointer.
------*/
void dte_load_previous_record(char *idx)
{
	static char first_record[]="Error - First Record Already Displayed.";
	char ufb[1];

	memcpy(&dte_dio_block[IO_POS],READ_PREVIOUS_RECORD,IO_LEN);
	KCSIO(dte_dio_block,ufb,dte_record);
	if(memcmp(&dte_dio_block[STATUS_POS],"00",STATUS_LEN))	/*IO no ok*/
		{
		dte_screen_error = 1;
		KCSI_make_error_message(first_record);
		return;
		}
	else
		dte_move_to_screen();

	if(dte_file_is_relative())		    /* relfiles 26-Mar-1990*/
		dte_rel_rec_num_to_fld();	    /* relfiles 26-Mar-1990*/
}
/*
**	History:
**	$Log: dkey.c,v $
**	Revision 1.4.2.1  2002/11/12 15:56:23  gsl
**	Sync with $HEAD Combined KCSI 4.0.00
**	
**	Revision 1.16  2002/10/24 15:48:33  gsl
**	Make globals unique
**	
**	Revision 1.15  2002/10/24 14:20:39  gsl
**	Make globals unique
**	
**	Revision 1.14  2002/10/23 21:07:27  gsl
**	make global name unique
**	
**	Revision 1.13  2002/10/23 20:39:09  gsl
**	make global name unique
**	
**	Revision 1.12  2002/10/17 17:17:17  gsl
**	Removed VAX VMS code
**	
**	Revision 1.11  2002/09/03 18:08:47  gsl
**	load_first_record() for relative file if no record 1
**	clear the error message then
**	try a dte_load_next_record
**	
**	Revision 1.10  2002/09/03 18:02:33  gsl
**	load_first_record() for relative file if no record 1 try a dte_load_next_record
**	
**	Revision 1.9  2002/09/03 17:58:11  gsl
**	load_first_record() for relative file if no record 1
**	
**	Revision 1.8  2002/09/03 17:49:46  gsl
**	load_first_record() for relative file if no record 1
**	
**	Revision 1.7  2002/08/01 16:49:54  gsl
**	type warnings
**	
**	Revision 1.6  2002/07/26 18:19:19  gsl
**	wscreen -> WSCREEN
**	
**	Revision 1.5  2002/07/25 15:20:29  gsl
**	Globals
**	
**	Revision 1.4  1999/09/13 19:47:06  gsl
**	fix missing return code
**	
**	Revision 1.3  1996-09-17 19:45:35-04  gsl
**	drcs update
**
**
**
*/
