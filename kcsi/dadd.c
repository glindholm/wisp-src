/*
******************************************************************************
**
** KCSI - King Computer Services Inc.
**
** $Id:$
**
** 
** CVS
** $Source:$
** $Author: gsl $
** $Date:$
** $Revision:$
******************************************************************************
*/

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
int dte_add_records()
{
	char ufb[1];

	kcsitrace(1, "DATENTRY", "ADD", "Entry into Add records");

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
	KCSI_init_add_fields();
	init_add_facs();
	init_add_footers();
	init_add_pfs();
	dte_init_message_field();
}
static int add_records_entry()
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

		if(Memeq(dte_pfkey_code,"16",2))
			return(16);
		if(Memeq(dte_pfkey_code,"09",2))
			return(9);
		if(Memeq(dte_pfkey_code,"00",2))
			{
			val_add_fields();
			if(!dte_screen_error)
				{
				dte_move_to_record();
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
void KCSI_init_add_fields()
{
	int i;
	FIELD *fld;


	dte_init_record();
	KCSI_get_date_stamps();
	KCSI_clear_all_fields();
	dte_display_all_prompts();

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

void KCSI_clear_all_fields()
{
	int i;

	dte_space_out(dte_rel_rec_num,dte_relative_record.edit_len);    /*22-Mar-1990*/
	if (dte_file_is_relative()) 			    /*09-Apr-1990*/
	    {
	    dte_space_out(dte_relative_record.fld,dte_relative_record.edit_len);
	    }

	for(i = 0; dtefld[i].name[0] > ' '; ++i )
		{
		KCSI_clear_one_field(&dtefld[i]);
		}
}

void KCSI_clear_one_field(FIELD *fld)
{
	if(!fld->frow)
		return;
	dte_space_out(fld->fld,fld->edit_len);

}

/*----
All fields are made modifiable for adding except accumulators,
and display only fields.
------*/
static void init_add_facs()
{
	if(dte_file_is_relative())
		dte_unprotect_rel_field();
	dte_unprotect_all_fields();
	dte_protect_nomod_fields();
}

static void init_add_footers()
{
	dte_load_footer("(ENTER) Add record (9) Exit to Modify    (16) Exit");
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
	if(dte_file_is_relative())
		val_add_rel_key();
	dte_val_all_fields();
}
static void val_add_rel_key()
{
	long rec_num;
	char work[101];

	static char rnf[]="Error - Record Already on File";
	static char invk[]="Error - Invalid key for relative file";

	memcpy(work,dte_relative_record.fld,REL_KEY_LEN);
	work[REL_KEY_LEN] = 0;
	rec_num = 0;
	sscanf(work,"%ld",&rec_num);
	if(rec_num == 0)
		{
		KCSI_make_error_message(invk);
		dte_screen_error = 1;
		return;
		}
	dte_read_rel_key_record();
	if(!(memcmp(&dte_dio_block[STATUS_POS],"00",STATUS_LEN)))
		{
		KCSI_make_error_message(rnf);
		dte_screen_error = 1;
		}
	else
		{
		dte_rel_rec_num_to_fld();
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
int dte_file_is_relative()
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


/*
**	History:
**	$Log: dadd.c,v $
**	Revision 1.14  2003/04/04 19:42:02  gsl
**	Add trace
**	
**	Revision 1.13  2003/02/04 19:19:09  gsl
**	fix header
**	
**	Revision 1.12  2002/10/24 15:48:34  gsl
**	Make globals unique
**	
**	Revision 1.11  2002/10/24 14:20:41  gsl
**	Make globals unique
**	
**	Revision 1.10  2002/10/23 21:07:28  gsl
**	make global name unique
**	
**	Revision 1.9  2002/10/23 20:39:10  gsl
**	make global name unique
**	
**	Revision 1.8  2002/10/17 21:22:41  gsl
**	cleanup
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
**	Revision 1.4  1999/09/13 19:45:48  gsl
**	Fix missing return code
**	
**	Revision 1.3  1996-09-17 19:45:31-04  gsl
**	drcs update
**
**
**
*/
