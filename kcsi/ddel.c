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

static char sccsid[]="@(#)ddel.c	1.4 1/30/93";

static void delete_records_init(char *idx);
static int delete_records_entry(char *idx);
static void init_del_fields();
static void init_del_facs();
static void init_del_footers();
static void init_del_pfs();
static void delete_the_record(char *idx);

/*----
			DELETE RECORDS
------*/

int delete_records(char *idx,char *mode)
{
	int pf;

	while(1)
		{
		pf = enter_the_key(idx,mode);
		if(pf == 16)
			return(16);
		if(pf == 9)
			return(9);
		delete_records_init(idx);
		pf = delete_records_entry(idx);
		if(pf == 16)
			return(16);
		if(pf == 9)
			return(9);
		}
}
static void delete_records_init(char *idx)
{
	init_del_fields();
	init_del_facs();
	init_del_footers();
	init_del_pfs();
	init_message_field();

}
static int delete_records_entry(char *idx)
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
		if(Memeq(dte_pfkey_code,"01",2))
			{
			return(0);
			}
		if(Memeq(dte_pfkey_code,"00",2))
			{
			delete_the_record(idx);
			if(!dte_screen_error)
			     load_next_record(idx);
			}
		}

}

/*----
			DELETE SUPPORT
------*/
static void init_del_fields()
{
	move_to_screen();

}
static void init_del_facs()
{
	display_all_prompts();
	protect_all_fields();
	if(file_is_relative())
		protect_rel_field();
}

static void init_del_footers()
{
#ifdef KCSI_VAXCOBOL
	load_footer("(ENTER)Delete (1)Find (2)1st         (5)Nxt (16)Exit");
#else
	load_footer("(ENTER)Delete (1)Find (2)1st (4)Prev (5)Nxt (16)Exit");
#endif /*KCSI_VAXCOBOL */
}

#ifdef KCSI_VAX_COL
static char del_idx_pfs[]="000102000516X";
#else
static char del_idx_pfs[]="000102040516X";
#endif /* KCSI_VAXCOBOL */

static void init_del_pfs()
{
	memcpy(dte_pfkeys,del_idx_pfs,strlen(del_idx_pfs));

}

static void delete_the_record(char *idx)
{
	static char ufb[1];
	static char no_delete[]="Error - Record Not Available for Deletion";

	memcpy(&dte_dio_block[IO_POS],HOLD_RECORD,IO_LEN);
	KCSIO(dte_dio_block,ufb,dte_record);
	memcpy(&dte_dio_block[IO_POS],DELETE_RECORD,IO_LEN);
	KCSIO(dte_dio_block,ufb,dte_record);
	if(memcmp(&dte_dio_block[STATUS_POS],"00",STATUS_LEN))
		{
		make_error_message(no_delete);
		dte_screen_error = 1;
		}

}
/*
**	History:
**	$Log: ddel.c,v $
**	Revision 1.3  1996/09/17 23:45:33  gsl
**	drcs update
**	
**
**
*/
