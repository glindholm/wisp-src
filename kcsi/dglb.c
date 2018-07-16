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
#include "datcob.h"
#include "dbsc.h"
#include "kcsifunc.h"


#define REL_KEY_LEN	8	/*22-Mar-1990*/


/*----
Globals used by datentry
------*/


int	dte_new_screen;
int	dte_new_key;

FIELD dtefld[MAX_FIELDS];
/* FIELD *keys[MAX_DISPLAY_FIELDS]; */
FIELD dte_wrk_field;

int KD_field_count;



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

unsigned char dte_main_scr[SCR_DEF_SIZE];
char dte_key_scr[SCR_DEF_SIZE];

char dte_record[2040];

char *dte_scr,*dte_trailer,*dte_key_trailer,*dte_message,*dte_key_message;
int dte_screen_error;

char dte_on_pfkeys[67]= "X";

char dte_pfkeys[67];
char dte_pfkey_code[2];
unsigned char dte_crt_file_status[2];
unsigned char dte_crt_record[1924];
unsigned char dte_order_area[]={1,(char)161,0,0};
unsigned char dte_dnr_altered[]={7};
unsigned char dte_full_screen[]={24};
unsigned char dte_close_ws[]={4};
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

FIELD dte_relative_record = {
	"RECORD", 1, REL_KEY_LEN, 
	REL_KEY_LEN, /*Name occur len edit_len */
	1, 1,			/* Position (not used)and zero suppres */
	0,0,0,0,0,1,0,		/* Sign dollar noupdate dec bin seq displ*/
	'U'			/* type */
	};			/* Pos isn't really used */
				/* All else default to zero */

char dte_rel_rec_num[REL_KEY_LEN + 1];



/*
**	History:
**	$Log: dglb.c,v $
**	Revision 1.13  2003/02/05 21:47:53  gsl
**	fix -Wall warnings
**	
**	Revision 1.12  2003/02/04 19:19:09  gsl
**	fix header
**	
**	Revision 1.11  2002/10/24 15:48:33  gsl
**	Make globals unique
**	
**	Revision 1.10  2002/10/24 14:20:40  gsl
**	Make globals unique
**	
**	Revision 1.9  2002/10/23 21:07:27  gsl
**	make global name unique
**	
**	Revision 1.8  2002/10/23 20:39:09  gsl
**	make global name unique
**	
**	Revision 1.7  2002/10/17 21:22:41  gsl
**	cleanup
**	
**	Revision 1.6  2002/10/17 17:17:17  gsl
**	Removed VAX VMS code
**	
**	Revision 1.5  2002/08/01 15:41:24  gsl
**	type warnings
**	
**	Revision 1.4  2002/07/25 15:20:29  gsl
**	Globals
**	
**	Revision 1.3  1996/09/17 23:45:34  gsl
**	drcs update
**	
**
**
*/
