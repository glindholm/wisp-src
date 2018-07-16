static char copyright[]="Copyright (c) 1988-1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
#include "kplatfrm.h"
#include <stdio.h>
#include "datcob.h"
#include "dbsc.h"
#include "kcsifunc.h"

static char sccsid[]="@(#)dglb.c	1.4 1/27/93";

#define REL_KEY_LEN	8	/*22-Mar-1990*/


/*----
Globals used by datentry
------*/

void vax_dte_globals()
{
	/* Dummy for the vax linker */
}


int	d_new_screen;
int	d_new_key;

FIELD dtefld[MAX_FIELDS];
FIELD *keys[MAX_DISPLAY_FIELDS];
FIELD wrk_field;

int field_count;
int next_row;
int next_col;
int datentry_spacing = 1;


DTYPE	cf_t1_src[]={
	{T1_INT_LEN},
	/* {T1_EXT_LEN}, */
	{T1_START_POS},
	{T1_OCCURRENCES},
	{T1_ZERO_SUPPRESS},
	{T1_SIGN},
	{T1_DOLLAR_COMMA},
	{T1_UPDATE},
	{T1_DEC},
	{T1_BIN},
	{T1_SEQ},
	{T1_DISPLAY},
	{NULL,0,0,0,0}};

DTYPE	cf_t1_dest[]={
	{(char*)&wrk_field.len,0,0,CINT,0},
	/* {(char*)&wrk_field.ext_len,0,0,CINT,0}, */
	{(char*)&wrk_field.pos,0,0,CINT,0},
	{(char*)&wrk_field.occurrences,0,0,BONE,0},
	{(char*)&wrk_field.supp,0,0,CINT,0},
	{(char*)&wrk_field.sign,0,0,CINT,0},
	{(char*)&wrk_field.dollar,0,0,CINT,0},
	{(char*)&wrk_field.noupdate,0,0,CINT,0},
	{(char*)&wrk_field.dec,0,0,CINT,0},
	{(char*)&wrk_field.bin,0,0,CINT,0},
	{(char*)&wrk_field.seq,0,0,CINT,0},
	{(char*)&wrk_field.display,0,0,CINT,0},
	{NULL,0,0,0,0}};

DTYPE	cf_t2_src[]={
	{T2_NAME},
	{T2_TYPE},
	{T2_VALIDATION},
	{T2_DEFAULT_FAC},
	{T2_TABLE_NAME},
	{T2_LO_RANGE},
	{T2_HI_RANGE},
	{T2_CUMMULATIVE_NAME},
	{NULL,0,0,0,0}};

DTYPE	cf_t2_dest[]={
	{wrk_field.name,0,0,CSTR,0},
	{(char*)&wrk_field.type,0,0,CCHR,0},
	{wrk_field.val,0,0,CSTR,0},
	{(char*)&wrk_field.fac,0,0,CCHR,0},
	{(char*)wrk_field.table,0,0,CSTR,0},
	{wrk_field.lo,0,0,CSTR,0},
	{wrk_field.hi,0,0,CSTR,0},
	{wrk_field.cumm_name,0,0,CSTR,0},
	{NULL,0,0,0,0}};

DTYPE	ch_src[]={
	{CH_SPACING},
	{NULL,0,0,0,0}};

DTYPE	ch_dest[]={
	{(char*)&datentry_spacing,0,0,CINT,0},
	{NULL,0,0,0,0}};

/*----
This start up area of the screen is the first thing copied to 
the screen area, and must contain the correct library number.
Currently 11. It is 41 bytes long.
------*/

/*
char start_scr[]="\013DATENTRYENTRY-SCREEN                    ";
int lib_vers=20;
*/

/*----
Two screens are needed.
A main screen  and a key screen A screen can occupy the screen size
plus the header bytes (start_scr see below) plus overhead bytes for
99 prompts and 99 fields. A prompt uses minimum overhead while a field
can use maximum.
------*/

char main_scr[SCR_DEF_SIZE], key_scr[SCR_DEF_SIZE];

char dte_record[2040];

char dum_fld[]={0,0,1,0,24,80,0,0,2,0,0,0,0,(char)140,0,0,0,0};
char *dte_scr,*dte_trailer,*dte_key_trailer,*dte_message,*dte_key_message;
int dte_screen_error;

char dte_on_pfkeys[67]= "X";

char dte_pfkeys[67];
char dte_pfkey_code[2];
char dte_crt_file_status[2];
char dte_crt_record[1924];
char dte_order_area[]={1,(char)161,0,0};
char dte_dnr_altered[]={7};
char dte_full_screen[]={24};
char dte_close_ws[]={4};
long dte_a_longword;
char dte_app_name[]="DATENTRY";
/*----
For date and time stamps.
------*/
char dte_sys_d_ymd[6],
	    dte_sys_d_mdy[6],
            dte_sys_t[8],
	    dte_sys_jd[5],
	    dte_sys_ymdhmsh[14];


/*----
Saviors of the passed globals.
------*/
char *dte_cio_block,*dte_dio_block,*dte_cf_t1,*dte_cf_t2,*dte_cf_hdrs;

/*----22-Mar-1990
This global is used to build a screen field for relative files.
------*/

FIELD relative_record = {
	"RECORD", 1, REL_KEY_LEN, 
	REL_KEY_LEN, /*Name occur len edit_len */
	1, 1,			/* Position (not used)and zero suppres */
	0,0,0,0,0,1,0,		/* Sign dollar noupdate dec bin seq displ*/
	'U'			/* type */
	};			/* Pos isn't really used */
				/* All else default to zero */

char rel_rec_num[REL_KEY_LEN + 1];



/*
**	History:
**	$Log: dglb.c,v $
**	Revision 1.3  1996/09/17 23:45:34  gsl
**	drcs update
**	
**
**
*/
