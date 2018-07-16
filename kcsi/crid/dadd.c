static char copyright[]="Copyright (c) 1988-1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
#include <stdio.h>
#include <string.h>
#include "dbsc.h"
#include "dglb.h"
#include "shrthand.h"
#include "iocode.h"
#include "cobioblk.h"
#include "dtype.h"
#include "dmnt.h"
#include "kcsifunc.h"

static char sccsid[]="@(#)dadd.c	1.6 11/15/92";

static void add_records_init();
static int add_records_entry();
static void init_add_facs();
static void init_add_footers();
static void init_add_pfs();
static void val_add_fields();
static void val_add_rel_key();
static void add_the_record();
static int file_is_consecutive();

/*----
			ADD RECORDS
------*/
int add_records()
{
	char ufb[1];

/* If the file is consecutive, then move to the end of the file for adds. */
	if(file_is_consecutive())			    
		{					    
		memcpy(&dte_dio_block[IO_POS],START_LAST,2);
		KCSIO(dte_dio_block,ufb,dte_record);	    
		}					  
	add_records_init();
	return(add_records_entry());
}

static void add_records_init()
{
	init_add_fields();
	init_add_facs();
	init_add_footers();
	init_add_pfs();
	init_message_field();
}
static int add_records_entry()
{
	dte_screen_error = 1;
	while(dte_screen_error)
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
		if(Memeq(dte_pfkey_code,"00",2))
			{
			val_add_fields();
			if(!dte_screen_error)
				{
				move_to_record();
				add_the_record();
				if(!dte_screen_error)
					return(0);
				}
			}
		}
	return 0;
}

/*----
			ADD SUPPORT LOGIC
------*/

/*----
Init the data record and move spaces to all fields.
------*/
void init_add_fields()
{
	int i;
	FIELD *fld;


	init_dte_record();
	get_date_stamps();
	clear_all_fields();
	display_all_prompts();

	for(i = 0; dtefld[i].name[0] > ' '; ++i )
		{
		if(dtefld[i].frow)
			{
			fld = &dtefld[i];
/* Add date or time stamping */
			if(Streq(fld->val,"DF"))
				memcpy(fld->fld,dte_sys_d_mdy,6);
			if(Streq(fld->val,"GD"))
				memcpy(fld->fld,dte_sys_d_ymd,6);
			if(Streq(fld->val,"DS"))
				memcpy(fld->fld,dte_sys_jd,5);
			if(Streq(fld->val,"TS"))
				memcpy(fld->fld,dte_sys_t,8);
			if((Streq(fld->val,"CS")) || (Streq(fld->val,"CM")))
				memcpy(fld->fld,dte_sys_ymdhmsh,14);
			}
		}
}

void clear_all_fields()
{
	int i;

	space_out(rel_rec_num,relative_record.edit_len);    /*22-Mar-1990*/
	if (file_is_relative()) 			    /*09-Apr-1990*/
	    {
	    space_out(relative_record.fld,relative_record.edit_len);
	    }

	for(i = 0; dtefld[i].name[0] > ' '; ++i )
		{
		clear_one_field(&dtefld[i]);
		}
}

void clear_one_field(FIELD *fld)
{
	if(!fld->frow)
		return;
	space_out(fld->fld,fld->edit_len);

}

/*----
All fields are made modifiable for adding except accumulators,
and display only fields.
------*/
static void init_add_facs()
{
	if(file_is_relative())
		unprotect_rel_field();
	unprotect_all_fields();
	protect_nomod_fields();
}

static void init_add_footers()
{
	load_footer("(ENTER) Add record (9) Exit to Modify    (16) Exit");
}

static void init_add_pfs()
{
	memcpy(dte_pfkeys,"000916X",7);
}

/*----
Validate all fields before adding the record.
------*/
static void val_add_fields()
{
	if(file_is_relative())
		val_add_rel_key();
	val_all_fields();
}
static void val_add_rel_key()
{
	long rec_num;
	char work[101];

	static char rnf[]="Error - Record Already on File";
	static char invk[]="Error - Invalid key for relative file";

	memcpy(work,relative_record.fld,REL_KEY_LEN);
	work[REL_KEY_LEN] = 0;
	rec_num = 0;
	sscanf(work,"%ld",&rec_num);
	if(rec_num == 0)
		{
		make_error_message(invk);
		dte_screen_error = 1;
		return;
		}
	read_rel_key_record();
	if(!(memcmp(&dte_dio_block[STATUS_POS],"00",STATUS_LEN)))
		{
		make_error_message(rnf);
		dte_screen_error = 1;
		}
	else
		{
		rel_rec_num_to_fld();
		}

}

static void add_the_record()
{
	static char ufb[1];
	static char record_on_file[]="Error - Record Already on File.";

	memcpy(&dte_dio_block[IO_POS],WRITE_RECORD,IO_LEN);
	KCSIO(dte_dio_block,ufb,dte_record);
	if(memcmp(&dte_dio_block[STATUS_POS],"00",STATUS_LEN))
		{
		memcpy(dte_message,record_on_file,strlen(record_on_file));
		dte_screen_error = 1;
		}
}

/*----22-Mar-1990
Return true when the org is the requested type.
------*/
int file_is_relative()
{
	if(dte_dio_block[ORG_POS] == 'R')
		return(1);
	return(0);
}
static int file_is_consecutive()
{
	if(dte_dio_block[ORG_POS] == 'C')
		return(1);
	return(0);
}
#ifdef OLD
int file_is_indexed()
{
	if(dte_dio_block[ORG_POS] == 'I')
		return(1);
	return(0);
}
#endif /* OLD */

/*
**	History:
**	$Log: dadd.c,v $
**	Revision 1.4  1999-09-13 15:45:48-04  gsl
**	Fix missing return code
**
**	Revision 1.3  1996-09-17 19:45:31-04  gsl
**	drcs update
**
**
**
*/
