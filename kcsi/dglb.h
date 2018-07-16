/* Copyright (c) 1988-1996 DevTech Migrations, All rights reserved. */
/* $Id:$ */
#ifndef	DGLBH
#define	DGLBH
#include "dtype.h"
#include "dbsc.h"

extern	int d_new_screen,d_new_key;
extern DTYPE cf_t1_src[],cf_t1_dest[],cf_t2_src[],cf_t2_dest[],
	     ch_src[],ch_dest[];

extern	FIELD dtefld[],*keys[],wrk_field,relative_record;

extern int field_count,next_row,next_col,datentry_spacing;
extern char main_scr[],key_scr[];
/*
extern char start_scr[];
extern int lib_vers;
*/
#define	LIB_VERS	22

extern char dte_record[];

extern char dum_fld[];
extern char *dte_scr,*dte_trailer,
	    *dte_key_trailer,*dte_message,*dte_key_message;
extern int dte_screen_error;
extern char dte_on_pfkeys[];

extern char dte_pfkeys[];
extern char dte_pfkey_code[];
extern char dte_crt_file_status[];
extern char dte_crt_record[];
extern char dte_order_area[];
extern char dte_dnr_altered[];
extern char dte_full_screen[];
extern char dte_close_ws[];
extern long dte_a_longword;
extern char dte_app_name[];
/*----
For date and time stamps.
------*/
extern char dte_sys_d_ymd[],
	    dte_sys_d_mdy[],
            dte_sys_t[],
	    dte_sys_jd[],
	    dte_sys_ymdhmsh[];

/*----
Saviors of the passed globals.
------*/
extern char *dte_cio_block,*dte_dio_block,*dte_cf_t1,*dte_cf_t2,*dte_cf_hdrs;
extern char rel_rec_num[];

#endif
/*
**	History:
**	$Log: dglb.h,v $
**	Revision 1.3  1996/09/17 23:34:05  gsl
**	drcs update
**	
**
**
*/
