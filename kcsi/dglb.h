/*
******************************************************************************
**
** KCSI - King Computer Services Inc.
**
** 
******************************************************************************
*/

#ifndef	DGLBH
#define	DGLBH
#include "dtype.h"
#include "dbsc.h"

extern	int dte_new_screen,dte_new_key;

extern	FIELD dtefld[],dte_wrk_field,dte_relative_record;
/* extern	FIELD *keys[]; */

extern int KD_field_count;
unsigned extern char dte_main_scr[];
extern char dte_key_scr[];
/*
extern char start_scr[];
extern int lib_vers;
*/
#define	LIB_VERS	22

extern char dte_record[];

extern char *dte_scr,*dte_trailer,
	    *dte_key_trailer,*dte_message,*dte_key_message;
extern int dte_screen_error;
extern char dte_on_pfkeys[];

extern char dte_pfkeys[];
extern char dte_pfkey_code[];
extern unsigned char dte_crt_file_status[];
extern unsigned char dte_crt_record[];
extern unsigned char dte_order_area[];
extern unsigned char dte_dnr_altered[];
extern unsigned char dte_full_screen[];
extern unsigned char dte_close_ws[];
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
extern char dte_rel_rec_num[];

#endif
/*
**	History:
**	$Log: dglb.h,v $
**	Revision 1.10  2003/02/05 15:50:11  gsl
**	Fix copyright headers
**	
**	Revision 1.9  2002/10/24 15:48:33  gsl
**	Make globals unique
**	
**	Revision 1.8  2002/10/24 14:20:40  gsl
**	Make globals unique
**	
**	Revision 1.7  2002/10/23 21:07:27  gsl
**	make global name unique
**	
**	Revision 1.6  2002/10/23 20:39:09  gsl
**	make global name unique
**	
**	Revision 1.5  2002/08/01 15:41:24  gsl
**	type warnings
**	
**	Revision 1.4  2002/07/25 15:20:29  gsl
**	Globals
**	
**	Revision 1.3  1996/09/17 23:34:05  gsl
**	drcs update
**	
**
**
*/
