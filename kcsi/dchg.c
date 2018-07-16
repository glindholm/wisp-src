static char copyright[]="Copyright (c) 1988-1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
#include <stdio.h>
#include "dbsc.h"
#include "dglb.h"
#include "shrthand.h"
#include "iocode.h"
#include "cobioblk.h"
#include "dtype.h"
#include "dmnt.h"
#include "kcsifunc.h"

static char sccsid[]="@(#)dchg.c	1.4 1/30/93";

static void change_records_init(char *idx);
static int change_records_entry(char *idx);
static void init_chg_fields();
static void init_chg_facs();
static void init_chg_footers();
static void init_chg_pfs();
static void val_chg_fields();
static void change_the_record();

/*----
			CHANGE RECORDS
------*/
int dte_change_records(char *idx,char *mode)
{
	int pf;

	while(1)
		{
		pf = dte_enter_the_key(idx,mode);
		if(pf == 16)
			return(16);
		if(pf == 9)
			return(9);
		change_records_init(idx);
		pf = change_records_entry(idx);
		if(pf == 16)
			return(16);
		if(pf == 9)
			return(9);
		}
}
static void change_records_init(char *idx)
{
	init_chg_fields();
	init_chg_facs();
	init_chg_footers();
	init_chg_pfs();
	dte_init_message_field();

}
static int change_records_entry(char *idx)
{
	while(1)
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

		if(Memeq(dte_pfkey_code,"16",2))
			return(16);
		if(Memeq(dte_pfkey_code,"09",2))
			return(9);
		if(Memeq(dte_pfkey_code,"02",2))
			{
			dte_load_first_record(idx);
			}
		if(Memeq(dte_pfkey_code,"04",2))
			{
			dte_load_previous_record(idx);
			}
		if(Memeq(dte_pfkey_code,"05",2))
			{
			dte_load_next_record(idx);
			}
		if(Memeq(dte_pfkey_code,"07",2))
			{
			memcpy(idx,"000",IDX_LEN);
			return(0);
			}
		if(Memeq(dte_pfkey_code,"01",2))
			{
			return(0);
			}
		if(Memeq(dte_pfkey_code,"00",2))
			{
			val_chg_fields();
			if(!dte_screen_error)
				{
				dte_move_to_record();
				change_the_record();
				if(!dte_screen_error)
					{
					dte_load_next_record(idx);
					}
				}
			}
		}

}

/*----
			CHANGE SUPPORT
------*/
static void init_chg_fields()
{
	int i;
	FIELD *fld;

	dte_move_to_screen();

	KCSI_get_date_stamps();
	for(i = 0; dtefld[i].name[0] > ' '; ++i )
		{
		if(dtefld[i].frow)
			{
			fld = &dtefld[i];
			if(Streq(fld->val,"CM"))
				memcpy(fld->fld,dte_sys_ymdhmsh,14);
			}
		}

}
static void init_chg_facs()
{
	dte_display_all_prompts();
	dte_unprotect_all_fields();
	if(dte_file_is_relative())
		{
		dte_protect_rel_field();
		}
	else
		{
		dte_protect_key_fields();
		}
	dte_protect_nomod_fields();
}

static char chg_idx_footer[]=
    "(ENTER)Mod (1)Find (2)1st (4)Prev (5)Nxt (7)Path (9)Add (16)Exit";
static char chg_idx_pfs[]="0001020405070916X";
static char chg_rel_footer[]=
    "(ENTER)Mod (1)Find (2)1st (4)Prev (5)Nxt (9)Add (16)Exit";
static char chg_rel_pfs[]="00010204050916X";

static void init_chg_footers()
{
	if (dte_file_is_relative())
	    dte_load_footer(chg_rel_footer);
	else
	    dte_load_footer(chg_idx_footer);

}

static void init_chg_pfs()
{
    if (dte_file_is_relative())
	memcpy(dte_pfkeys,chg_rel_pfs,strlen(chg_rel_pfs));
    else
	memcpy(dte_pfkeys,chg_idx_pfs,strlen(chg_idx_pfs));

}

static void val_chg_fields()
{
	dte_val_all_fields();
}

static void change_the_record()
{
	static char ufb[1];
	static char record_on_file[]="Error - Record Already on File.";
	static char save_record[2040];

	memcpy(save_record,dte_record,2040);
	memcpy(&dte_dio_block[IO_POS],HOLD_RECORD,IO_LEN);
	KCSIO(dte_dio_block,ufb,dte_record);
	memcpy(dte_record,save_record,2040);
	memcpy(&dte_dio_block[IO_POS],REWRITE_RECORD,IO_LEN);
	KCSIO(dte_dio_block,ufb,dte_record);
	if(memcmp(&dte_dio_block[STATUS_POS],"00",STATUS_LEN))
		{
		KCSI_make_error_message(record_on_file);
		dte_screen_error = 1;
		}

}
/*
**	History:
**	$Log: dchg.c,v $
**	Revision 1.3.2.1  2002/11/12 15:56:22  gsl
**	Sync with $HEAD Combined KCSI 4.0.00
**	
**	Revision 1.10  2002/10/24 15:48:34  gsl
**	Make globals unique
**	
**	Revision 1.9  2002/10/24 14:20:40  gsl
**	Make globals unique
**	
**	Revision 1.8  2002/10/23 20:39:09  gsl
**	make global name unique
**	
**	Revision 1.7  2002/10/17 17:17:16  gsl
**	Removed VAX VMS code
**	
**	Revision 1.6  2002/08/01 16:49:54  gsl
**	type warnings
**	
**	Revision 1.5  2002/07/26 18:19:19  gsl
**	wscreen -> WSCREEN
**	
**	Revision 1.4  2002/07/25 15:20:29  gsl
**	Globals
**	
**	Revision 1.3  1996/09/17 23:45:33  gsl
**	drcs update
**	
**
**
*/
