static char copyright[]="Copyright (c) 1988-1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
/*---
kv3.c

IO for vision isam files.

------*/
#include <stdio.h>
#include <ctype.h>
/*#include "iocode.h"*/
#include "kcsio.h"
#include "visn3.h"
#include "kcsifunc.h"

static char sccsid[]="@(#)kv3.c	1.16 11/13/93";

static void do_open(KCSIO_BLOCK *kfb, int mode);
static void do_start(KCSIO_BLOCK *kfb, int mode);
static int v_trans(int code);
static int v_trans2(int code);
static int kptoi(char **str);

/*----
Extract the root file data and move in the space.
------*/
void ksam_file_space(KCSIO_BLOCK *kfb)
{
	char result[11];

/* doesn't work on AIX
	i_info(kfb->_io_vector,-4,result);
*/
	strcpy(result,"1");

	kfb->_space = atol(result);
}

/*----
            <<<<<<<<<  OPEN AND CLOSE ROUTINES   >>>>>>>>>>>
------*/
/*---
Shared is the same as IO for C-ISAM. All C-ISAM opens to be compatible
with COBOL require that the file be set up at the first record.
A COBOL open sets things up for the first record. A c-isam open
does not.
------*/
void ksam_open_shared(KCSIO_BLOCK *kfb)
{
	ksam_open_io(kfb);
}
void ksam_open_input(KCSIO_BLOCK *kfb)
{
	do_open(kfb,Finput);
}
void ksam_open_io(KCSIO_BLOCK *kfb)
{
	do_open(kfb,Fio);
}
/*----
Doopen should not be used for open output
------*/
static void do_open(KCSIO_BLOCK *kfb, int mode)
{
	char l_param[512];
	sprintf(l_param,"%d,%d,%d",
		kfb->_record_len,
		kfb->_record_len,
		kfb->_altkey_count +1);
	f_errno = 0;
	kfb->_io_vector = (char *) i_open(kfb->_sys_name,mode,l_param);
	kfb->_status = v_trans(f_errno);
	kfb->_last_io_key = 0;
	kfb->_io_key = 0;
	kfb->_open_status = 1;
	if(kfb->_status == 0)
		ksam_file_info(kfb);
}
/*----
A vision open consists of building a file with all keys.
------*/
void ksam_open_output(KCSIO_BLOCK *kfb)
{
	char l_param[512];
	char k_param[512];
	char w_param[512];
	int ak,rc;
	char *ptr;
	extern char *Agetenv(const char* var);	/* Acucobol getenv() which reads A_CONFIG file */
	extern short v_make_vers;		/* ACUCOBOL FILE SYSTEM VARIABLE TO DEFINE VISION LEVEL 2,3,4 */

	sprintf(l_param,"%d,%d,%d",
		kfb->_record_len,
		kfb->_record_len,
		kfb->_altkey_count +1);
	sprintf(k_param,"1,0,%d,%d",
		kfb->_key[0].k_part[0].kp_leng,
		kfb->_key[0].k_part[0].kp_start);

	for(ak=0; ak < kfb->_altkey_count; ++ak)
		{
		sprintf(w_param,",1,%s,%d,%d",
			(kfb->_key[ak+1].k_flags & ISDUPS)?"1":"0",
			kfb->_key[ak+1].k_part[0].kp_leng,
			kfb->_key[ak+1].k_part[0].kp_start);
		strcat(k_param,w_param);
		}
	f_errno = 0;
	
	/*
	**	Default to Vision 3 files.
	**	This can be overridden by setting the V_VERSION envvar.
	**	The A_CONFIG Agetenv() doesn't currently seem to work.
	*/
	v_make_vers = 3;
	
	if ((ptr = getenv("V_VERSION")) ||
	    (ptr = Agetenv("V_VERSION"))   )
	{
		int vversion;

		if (1 == sscanf(ptr,"%d",&vversion))
		{
			if (2 == vversion || 3 == vversion || 4 == vversion)
			{
				v_make_vers = vversion;
			}
		}
	}

	/*
	**	The only valid values are '2','3', and '4'
	*/
	switch(kfb->_format)
	{
	case '2':
		v_make_vers = 2;
		break;
		
	case '3':
		v_make_vers = 3;
		break;
		
	case '4':
		v_make_vers = 4;
		break;
	}

	rc = i_make(kfb->_sys_name,"Control",NULL,l_param,k_param,NULL);
	if(rc)
		f_errno = 0;
	kfb->_status = v_trans(f_errno);
	if(f_errno)
		return;

	kfb->_io_vector = (char *) i_open(kfb->_sys_name,Foutput,l_param);
	kfb->_status = v_trans(f_errno);
	if(f_errno)
		return;
	kfb->_last_io_key = 0;
	kfb->_open_status = 1;
	ksam_file_info(kfb);
}

void ksam_close(KCSIO_BLOCK *kfb)
{

	kfb->_status = f_errno = 0;
	if(kfb->_open_status == 0)
		return;
	i_close(kfb->_io_vector);
	kfb->_status = v_trans(f_errno);
	kfb->_open_status = 0;
	ksam_file_space(kfb);
}
/*----
               <<<<<<<<  READ HOLD  >>>>>>>>>>>
------*/
/*----
A read record request assumes the primary key.
------*/
void ksam_read(KCSIO_BLOCK *kfb)
{
	kfb->_io_key = 0;
	ksam_read_keyed(kfb);
}

void ksam_hold(KCSIO_BLOCK *kfb)
{
	kfb->_io_key = 0;
	ksam_hold_keyed(kfb);
}

void ksam_read_next(KCSIO_BLOCK *kfb)
{
	f_errno = 0;
	i_next(kfb->_io_vector,kfb->_record);
	kfb->_status = v_trans2(f_errno);
}

void ksam_read_previous(KCSIO_BLOCK *kfb)
{
	f_errno = 0;
	i_previous(kfb->_io_vector,kfb->_record);
	kfb->_status = v_trans2(f_errno);
}


void ksam_hold_next(KCSIO_BLOCK *kfb)
{
	f_errno = 0;
	i_next(kfb->_io_vector,kfb->_record);
	kfb->_status = v_trans2(f_errno);
}

void ksam_read_keyed(KCSIO_BLOCK *kfb)
{
	f_errno = 0;
	i_read(kfb->_io_vector,kfb->_record,kfb->_io_key);
	kfb->_status = v_trans(f_errno);
}

void ksam_hold_keyed(KCSIO_BLOCK *kfb)
{
	f_errno = 0;
	i_read(kfb->_io_vector,kfb->_record,kfb->_io_key);
	kfb->_status = v_trans(f_errno);
}

/*----
			<<<< STARTS >>>>
------*/
/*----
Starts are vanilla, but see notes on read and hold next.
------*/
void ksam_start_eq(KCSIO_BLOCK *kfb)
{
	kfb->_io_key = 0;
	do_start(kfb,F_EQUALS);
}

void ksam_start_nlt(KCSIO_BLOCK *kfb)
{
	kfb->_io_key = 0;
	do_start(kfb,F_NOT_LESS);
}

void ksam_start_gt(KCSIO_BLOCK *kfb)
{
	kfb->_io_key = 0;
	do_start(kfb,F_GREATER);
}
void ksam_start_eq_keyed(KCSIO_BLOCK *kfb)
{
	do_start(kfb,F_EQUALS);
}

void ksam_start_nlt_keyed(KCSIO_BLOCK *kfb)
{
	do_start(kfb,F_NOT_LESS);
}

void ksam_start_gt_keyed(KCSIO_BLOCK *kfb)
{
	do_start(kfb,F_GREATER);
}

void ksam_start_last(KCSIO_BLOCK *kfb)
{
	kfb->_status = EBADARG;
}

/*----
Executes a start and sets up a new _last_io_key.
------*/
static void do_start(KCSIO_BLOCK *kfb, int mode)
{
	f_errno = 0;
	i_start(kfb->_io_vector,kfb->_record,kfb->_io_key,0,mode);
	kfb->_status = v_trans(f_errno);
	kfb->_last_io_key = kfb->_io_key;
}

/*----
               <<<<<<<< WRITE REWRITE DELETE  >>>>>>>>>>>>
------*/
/*----
Write, rewrite and delete are vanilla except that a hold should have
been issued before the rewrite or delete so these are released.
------*/
void ksam_write(KCSIO_BLOCK *kfb)
{
	f_errno = 0;
	i_write(kfb->_io_vector,kfb->_record,kfb->_record_len);
	kfb->_status = v_trans(f_errno);
	ksam_file_space(kfb);
}

void ksam_rewrite(KCSIO_BLOCK *kfb)
{
	f_errno = 0;
	i_rewrite(kfb->_io_vector, kfb->_record, kfb->_record_len);
	kfb->_status = v_trans(f_errno);
}

void ksam_delete(KCSIO_BLOCK *kfb)
{
	f_errno = 0;
	i_delete(kfb->_io_vector,kfb->_record);
	kfb->_status = v_trans(f_errno);
	ksam_file_space(kfb);
}

void ksam_unlock(KCSIO_BLOCK *kfb)
{
/* 	f_errno = 0; */
	i_unlock(kfb->_io_vector);
/*	kfb->_status = v_trans(f_errno); */
}

/*----
Get table_info (same as file_info under vision 3)
------*/
void ksam_table_info(KCSIO_BLOCK *kfb)
{
	ksam_file_info(kfb);
}

/*----
The ksam_file_info() routine pulls the data on an open file and
sets up the c structure.
------*/
void ksam_file_info(KCSIO_BLOCK *kfb)
{
	char params[512];
	char *lp;
	int ak,rc;
	
	rc = i_info(kfb->_io_vector,-1,params);
	lp = params;

	kfb->_record_len = kptoi(&lp);
	++lp;
	kptoi(&lp);
	++lp;
	kfb->_altkey_count = kptoi(&lp);
	++lp;
	--kfb->_altkey_count;

	clear_keys(kfb);

	i_info(kfb->_io_vector,0,params);
	lp = params;
	kptoi(&lp);
	++lp;
	kptoi(&lp);
	++lp;
	kfb->_key[0].k_part[0].kp_leng = kptoi(&lp);
	++lp;
	kfb->_key[0].k_part[0].kp_start = kptoi(&lp);
	++lp;
	kfb->_key[0].k_nparts = 1;
	for(ak = 0; ak < kfb->_altkey_count ; ++ak)
		{
		i_info(kfb->_io_vector,ak+1,params);
		lp = params;
		kptoi(&lp);
		++lp;
		kfb->_key[ak + 1].k_flags = (kptoi(&lp))?ISDUPS:ISNODUPS;
		++lp;
		kfb->_key[ak + 1].k_part[0].kp_leng = kptoi(&lp);
		++lp;
		kfb->_key[ak + 1].k_part[0].kp_start = kptoi(&lp);
		++lp;
		kfb->_key[ak + 1].k_nparts = 1;
		}
	ksam_file_space(kfb);
}
static int kptoi(char **str)
{
	int ch;
	int result;

	result = 0;

	while(isdigit(ch = **str))
		{
		result *= 10;
		result += (ch - '0');
		++(*str);
		}
	return(result);
}

static int v_trans2(int code)
{
	if(	(code == E_NOT_FOUND)	||
		(code == E_UNDEF_RECORD)	)
		return(EENDFILE);
	else
		return(v_trans(code));
}

static int v_trans(int code)
{
	switch(code)
		{
		case 0:
			return(0);
		case W_DUP_OK:
			return(0);
		case E_REC_LOCKED:
			return(ELOCKED);
		case E_SYS_ERR:
		case E_DISK_FULL:
			return(f_errno);
		case E_PARAM_ERR:
			return(EBADARG);
		case E_TOO_MANY_FILES:
			return(EBADFILE);
		case E_MODE_CLASH:
			return(ENOTEXCL);
		case E_BROKEN:
			return(EBADFILE);
		case E_DUPLICATE:
			return(EDUPL);
		case E_NOT_FOUND:
			return(ENOREC);
		case E_NO_MEMORY:
			return(EBADMEM);
		default:
			return(EBADFILE);
		}
}

/*
**	History:
**	$Log: kv3.c,v $
**	Revision 1.6  1998-05-18 16:09:23-04  gsl
**	Set the VISION VERSION number v_make_vers based on the _format field in kfb.
**
**	Revision 1.5  1998-04-17 09:57:05-04  gsl
**	Changed to default to Vision 3 files.
**	Was creating vision 4 files and was not obeying V-VERSION 3 in A_CONFIG.
**	You can change to Vision 2 or 4 by setting envvar V_VERSION=4.
**
**	Revision 1.4  1996-09-17 19:34:12-04  gsl
**	drcs update
**
**
**
*/
