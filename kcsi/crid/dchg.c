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
int change_records(char *idx,char *mode)
{
	int pf;

	while(1)
		{
		pf = enter_the_key(idx,mode);
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
	init_message_field();

}
static int change_records_entry(char *idx)
{
	while(1)
		{
		dte_screen_error = 0;
		space_out(dte_crt_file_status,2);
		memcpy(dte_on_pfkeys,"X",1);
		memcpy(dte_crt_record,dte_order_area,4);

		wscreen(main_scr,
			dte_dnr_altered,
			dte_crt_record,
			dte_full_screen,
			dte_pfkeys,
			dte_on_pfkeys,
			dte_pfkey_code,
			dte_crt_file_status);

		init_message_field();

		if(Memeq(dte_pfkey_code,"16",2))
			return(16);
		if(Memeq(dte_pfkey_code,"09",2))
			return(9);
		if(Memeq(dte_pfkey_code,"02",2))
			{
			load_first_record(idx);
			}
		if(Memeq(dte_pfkey_code,"04",2))
			{
			load_previous_record(idx);
			}
		if(Memeq(dte_pfkey_code,"05",2))
			{
			load_next_record(idx);
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
				move_to_record();
				change_the_record();
				if(!dte_screen_error)
					{
					load_next_record(idx);
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

	move_to_screen();

	get_date_stamps();
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
	display_all_prompts();
	unprotect_all_fields();
	if(file_is_relative())
		{
		protect_rel_field();
		}
	else
		{
		protect_key_fields();
		}
	protect_nomod_fields();
}

#ifdef KCSI_VAXCOBOL
static char chg_idx_footer[]=
    "(ENTER)Mod (1)Find (2)1st         (5)Nxt (7)Path (9)Add (16)Exit";
static char chg_idx_pfs[]="0001020005070916X";
static char chg_rel_footer[]=
    "(ENTER)Mod (1)Find (2)1st         (5)Nxt (9)Add (16)Exit";
static char chg_rel_pfs[]="00010200050916X";
#else
static char chg_idx_footer[]=
    "(ENTER)Mod (1)Find (2)1st (4)Prev (5)Nxt (7)Path (9)Add (16)Exit";
static char chg_idx_pfs[]="0001020405070916X";
static char chg_rel_footer[]=
    "(ENTER)Mod (1)Find (2)1st (4)Prev (5)Nxt (9)Add (16)Exit";
static char chg_rel_pfs[]="00010204050916X";
#endif

static void init_chg_footers()
{
/*
	dte_scr = dte_trailer;
	if (file_is_relative())
	    add_prompt(24,2,chg_rel_footer);
	else
	    add_prompt(24,2,chg_idx_footer);
	*dte_scr++ = 0xff;
	*dte_scr++ = 0xff;
*/
	if (file_is_relative())
	    load_footer(chg_rel_footer);
	else
	    load_footer(chg_idx_footer);

}

static void init_chg_pfs()
{
    if (file_is_relative())
	memcpy(dte_pfkeys,chg_rel_pfs,strlen(chg_rel_pfs));
    else
	memcpy(dte_pfkeys,chg_idx_pfs,strlen(chg_idx_pfs));

}

static void val_chg_fields()
{
	val_all_fields();
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
		make_error_message(record_on_file);
		dte_screen_error = 1;
		}

}
/*
**	History:
**	$Log: dchg.c,v $
**	Revision 1.3  1996-09-17 19:45:33-04  gsl
**	drcs update
**
**
**
*/
