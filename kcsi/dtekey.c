/*
******************************************************************************
**
** KCSI - King Computer Services Inc.
**
** 
******************************************************************************
*/

/*----
Mon Oct 26 05:51:47 1992
Generated by: cob2c dtek.cob 
Version 1.10 (c) 1991
------*/

#include "crec.h"
#include "dglb.h"
#include "dmnt.h"
#include "gp.h"
#include "intdef.h"
#include "kcsifunc.h"



static int4 gp_numeric_pfkey;
#define	gp_pfkey_32_pressed	(gp_numeric_pfkey == 32L)
#define gp_enter_key_pressed	(gp_numeric_pfkey == 0L)
static int screen_error_status = 0;
#define	entry_error	(1)
#define	screen_entry_error	(screen_error_status == entry_error)

static char mode_flag[1];

static int index_of_key;

static char altkey_lit[321];
static char keypath_path_field[9];
static int ak_idx,cidx;
static char cname[9],cdups[4];
static char work[101],format[101];

static char lit1[]= "The primary key is";

static char ek_msg[]="Enter the key path to use.";
/*static long int4[81],long4[81]; */
static char text_01[]="Enter the key path for reading the data file and press (ENTER), or";
static char text_02[]="press the PFkey relating to the desired path number.";
static char text_03[]="Use (17) to define the Primary key as the path.";
static char text_05[]="      Path   Dups         Path   Dups         Path   Dups         Path   Dups";
static char text_10[]="Press (32) Exit ";
static char message_field[80];

static int opening_procedure();
static void closing_procedure();
static void main_process();
static void compare_the_key();
static void get_the_keyname();
static void call_keypath_getparm();
static void clear_message_field();
static int init_the_literals();
static void clear_the_altkeys();
static int load_the_altkeys();
static int load_one_altkey();
static void set_up_extra_pfs();
static void set_gp_pf(uint4 *mask,int pf,int onoff);



void DTEKEY(char *mode,char *idx)
{
	mode_flag[0] = *mode;
	index_of_key = kcsi_atoilen(idx,IDX_LEN);
	if(opening_procedure())
		{
		main_process();
		closing_procedure();
		}
	else
		{
		index_of_key = 17;
		}
	*mode = mode_flag[0];
	sprintf(format,"%%0%dd",IDX_LEN);
	sprintf(work,format,index_of_key);
	memcpy(idx,work,IDX_LEN);
}
static int opening_procedure() /*tic*/
{
	return(init_the_literals());
}

static void closing_procedure() /*tic*/
{
	if( ( gp_pfkey_32_pressed ) )
		{
		mode_flag[0] = 'F';
		}
}
static void main_process() /*tic*/
{
main_process:
	screen_error_status = 0;
	call_keypath_getparm();
	clear_message_field();
	if( ( gp_enter_key_pressed ) )
		{
		compare_the_key();
		}
	else
		{
		index_of_key = gp_numeric_pfkey;
		}
	if( ( screen_entry_error ) )
		{
		memcpy(message_field,ek_msg,strlen(ek_msg));
		goto main_process;
		}
}
static void compare_the_key() /*tic*/
{
	if(keypath_path_field[0] < '!')
		{
		screen_error_status = entry_error;
		return;
		}
	
	if( !memcmp(keypath_path_field,&dte_cf_hdrs[PKEY_POS],8) )
		{
		index_of_key = 17;
		}
	else
		{
		for(ak_idx = 0; ak_idx < 16; ++ak_idx)
			{
			get_the_keyname();
			if(!memcmp(keypath_path_field,cname,8))
				{
				index_of_key = ak_idx + 1;
				break;
				}
			}
		if(ak_idx > 15)
			{
			screen_error_status = entry_error;
			}
		}
}
static void get_the_keyname() /*tic*/
{
	int pos;

	if( ( ak_idx < 8 ) )
		{
		cidx = ak_idx;
		pos = AKEY1_POS + (cidx *9);
		}
	else
		{
		cidx = ak_idx - 8;
		pos = AKEY2_POS + (cidx * 9);
		}
	memcpy(cname,&dte_cf_hdrs[pos],8);
	memcpy(cdups,(dte_cf_hdrs[pos + 8] == 'Y')?"YES":"NO ", 3);
}

static void call_keypath_getparm() /*tic*/
{

	WL_wpload();	
	GPSETUP();
	GPSTD("KEYPATH ","PATHS ");
	GPCTEXT(text_01,9,2);
	GPCTEXT(text_02,10,2);
	GPCTEXT(text_03,12,2);
	GPCTEXT(lit1,14,2);
	GPTEXT( &dte_cf_hdrs[PKEY_POS],8,14,22);
	GPKW("PATH    ",keypath_path_field,8,16,2,"A");
	GPCTEXT(text_05,18,2);
	GPTEXT(&altkey_lit[0],78,19,2);
	GPTEXT(&altkey_lit[80],78,20,2);
	GPTEXT(&altkey_lit[160],78,21,2);
	GPTEXT(&altkey_lit[240],78,22,2);
	GPCTEXT(text_10,23,2);
	GPCTEXT(message_field,24,2);
	GPENTER();
	WL_wswap(&GP_pfkeys);
	GPPFS(&GP_pfkeys);
	gp_numeric_pfkey = GP_display_and_read();
}
static void clear_message_field()
{
	memset(message_field,' ',79);
}
static int init_the_literals() /*tic*/
{
	int nkeys;

	GP_pfkeys = 0;
	clear_the_altkeys();
	clear_message_field();
	nkeys = load_the_altkeys();
	set_up_extra_pfs();
	memset(keypath_path_field,' ',8);
	if(	(index_of_key == 17)	||
		(index_of_key == 0)	)
		memcpy(keypath_path_field,&dte_cf_hdrs[PKEY_POS],8);
	else
	if(index_of_key != 0)
		{
		ak_idx = index_of_key -1;
		get_the_keyname();
		memcpy(keypath_path_field,cname,8);
		}
	return(nkeys);
}
static void clear_the_altkeys() /*tic*/
{
	memset(keypath_path_field,' ',8);
	memset(altkey_lit,' ',320);
}
static int load_the_altkeys() /*tic*/
{
	int akc;

	akc = 0;
	for(ak_idx = 0; ak_idx < 16; ++ak_idx)
		akc += load_one_altkey();
	return(akc);
}
static int load_one_altkey() /*tic*/
{
	get_the_keyname();
	if( ( cname[0] > ' '))
		{
		sprintf(&altkey_lit[ak_idx * 20],
		"(%2d) %-8s  %-3s  ",
		ak_idx + 1,
		cname,
		cdups);
		set_gp_pf((uint4*)&GP_pfkeys,ak_idx + 1, 1);
		return(1);
		}
	return(0);
}

static void set_up_extra_pfs()
{
	GP_pfkeys |= GP_PF_17;
	GP_pfkeys |= GP_PF_32;
}

static void set_gp_pf(uint4 *mask,int pf,int onoff)
{
	uint4 smask;


	smask = 0x80000000;

	smask >>= (pf - 1);

	if(onoff)
		*mask |= smask;
	else
		*mask &= ~smask;
	
}

/*
**	History:
**	$Log: dtekey.c,v $
**	Revision 1.13  2003/02/20 19:29:55  gsl
**	fix -Wall warnings
**	
**	Revision 1.12  2003/02/04 19:19:09  gsl
**	fix header
**	
**	Revision 1.11  2002/10/24 14:20:39  gsl
**	Make globals unique
**	
**	Revision 1.10  2002/07/25 15:20:28  gsl
**	Globals
**	
**	Revision 1.9  2002/07/12 17:17:01  gsl
**	Global unique WL_ changes
**	
**	Revision 1.8  2002/07/10 21:06:24  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.7  2002/06/25 18:18:34  gsl
**	Remove WISPRETURNCODE as a global, now must go thru set/get routines
**	
**	Revision 1.6  1998/11/02 21:21:50  gsl
**	Fix (ENTER) tag
**	
**	Revision 1.5  1997-10-02 09:49:13-04  gsl
**	Fix warnings
**
**	Revision 1.4  1996-09-17 19:45:36-04  gsl
**	drcs update
**
**
**
*/
