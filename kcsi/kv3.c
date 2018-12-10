/*
******************************************************************************
**
** KCSI - King Computer Services Inc.
**
** 
******************************************************************************
*/


#ifdef KCSI_ACU

/*---
kv3.c

IO for vision isam files.

------*/
#include <stdio.h>
#include <ctype.h>
#include "kcsio.h"
#include "visn3.h"
#include "kcsifunc.h"

static void do_open(KCSIO_BLOCK *kfb, int mode);
static void do_start(KCSIO_BLOCK *kfb, int mode);
static int v_trans(int code);
static int v_trans2(int code);
static int kptoi(char **str);

static void kcsi_acu_zero_f_errno(void);
static short kcsi_acu_get_f_errno(void);

/* Initialize Vision file system */
void ksam_init()
{
	static int first = 1;

	if (first)
	{
		/*
		** The call to i_init() is not needed since KCSI is using
		** the Acucobol runtime (vs standalone executables).
		** Calling i_init() seems to be causing signal 11 errors
		** when exiting the runtime. Show up when stepping out of
		** ACULINK.
		**
		** i_init();
		*/

		first = 0;
	}
}

/*----
Extract the root file data and move in the space.
------*/
void ksam_file_space(KCSIO_BLOCK *kfb)
{
	char result[11];

	kcsitrace(1, "kv3:ksam_file_space()", "enter", "%s", kfb->_sys_name);

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
	kcsitrace(1, "kv3:ksam_open_shared()", "enter", "%s", kfb->_sys_name);
	ksam_open_io(kfb);
}
void ksam_open_input(KCSIO_BLOCK *kfb)
{
	kcsitrace(1, "kv3:ksam_open_input()", "enter", "%s", kfb->_sys_name);
	do_open(kfb,Finput);
}
void ksam_open_io(KCSIO_BLOCK *kfb)
{
	kcsitrace(1, "kv3:ksam_open_io()", "enter", "%s", kfb->_sys_name);
	do_open(kfb,Fio);
}
/*----
Doopen should not be used for open output
------*/
static void do_open(KCSIO_BLOCK *kfb, int mode)
{
	char l_param[512];
	kcsitrace(1, "kv3:do_open()", "enter", "mode=%d %s", mode, kfb->_sys_name);

	sprintf(l_param,"%d,%d,%d",
		kfb->_record_len,
		kfb->_record_len,
		kfb->_altkey_count +1);
	kcsi_acu_zero_f_errno();
	kfb->_io_vector = (char *) i_open(kfb->_sys_name,mode,l_param);
	kfb->_status = v_trans(kcsi_acu_get_f_errno());
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
	/* extern short v_make_vers;	*/	/* ACUCOBOL FILE SYSTEM VARIABLE TO DEFINE VISION LEVEL 2,3,4 */
	int vision_version;

	kcsitrace(1, "kv3:ksam_open_output()", "enter", "%s", kfb->_sys_name);

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
	kcsi_acu_zero_f_errno();
	
	/*
	**	Default to Vision 3 files.
	**	This can be overridden by setting the V_VERSION envvar.
	**	The A_CONFIG Agetenv() doesn't currently seem to work.
	*/
	vision_version = 3;
	
	if ((ptr = getenv("V_VERSION")) ||
	    (ptr = Agetenv("V_VERSION"))   )
	{
		int vversion;

		if (1 == sscanf(ptr,"%d",&vversion))
		{
			if (2 == vversion || 3 == vversion || 4 == vversion)
			{
				vision_version = vversion;
			}
		}
	}

	/*
	**	The only valid values are '2','3', and '4'
	*/
	switch(kfb->_format)
	{
	case '2':
		vision_version = 2;
		break;
		
	case '3':
		vision_version = 3;
		break;
		
	case '4':
		vision_version = 4;
		break;
	}

	/* v_make_vers = vision_version */
	rc = i_make(kfb->_sys_name,"Control",NULL,l_param,k_param,NULL);
	if(rc)
		kcsi_acu_zero_f_errno();
	kfb->_status = v_trans(kcsi_acu_get_f_errno());
	if(kcsi_acu_get_f_errno())
		return;

	kfb->_io_vector = (char *) i_open(kfb->_sys_name,Foutput,l_param);
	kfb->_status = v_trans(kcsi_acu_get_f_errno());
	if(kcsi_acu_get_f_errno())
		return;
	kfb->_last_io_key = 0;
	kfb->_open_status = 1;
	ksam_file_info(kfb);
}

void ksam_close(KCSIO_BLOCK *kfb)
{
	kcsitrace(1, "kv3:ksam_close()", "enter", "%s", kfb->_sys_name);

	kcsi_acu_zero_f_errno();
	kfb->_status = 0;
	if(kfb->_open_status == 0)
		return;
	i_close(kfb->_io_vector);
	kfb->_status = v_trans(kcsi_acu_get_f_errno());
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
	kcsitrace(1, "kv3:ksam_read()", "enter", "%s", kfb->_sys_name);
	kfb->_io_key = 0;
	ksam_read_keyed(kfb);
}

void ksam_hold(KCSIO_BLOCK *kfb)
{
	kcsitrace(1, "kv3:ksam_hold()", "enter", "%s", kfb->_sys_name);
	kfb->_io_key = 0;
	ksam_hold_keyed(kfb);
}

void ksam_read_next(KCSIO_BLOCK *kfb)
{
	kcsitrace(1, "kv3:ksam_read_next()", "enter", "%s", kfb->_sys_name);
	kcsi_acu_zero_f_errno();
	i_next(kfb->_io_vector,kfb->_record);
	kfb->_status = v_trans2(kcsi_acu_get_f_errno());

	/* Unlock the record */
	if (kfb->_status == 0)
	{
		i_unlock(kfb->_io_vector);
	}
}

void ksam_read_previous(KCSIO_BLOCK *kfb)
{
	kcsitrace(1, "kv3:ksam_read_previous()", "enter", "%s", kfb->_sys_name);
	kcsi_acu_zero_f_errno();
	i_previous(kfb->_io_vector,kfb->_record);
	kfb->_status = v_trans2(kcsi_acu_get_f_errno());

	/* Unlock the record */
	if (kfb->_status == 0)
	{
		i_unlock(kfb->_io_vector);
	}
}


void ksam_hold_next(KCSIO_BLOCK *kfb)
{
	kcsitrace(1, "kv3:ksam_hold_next()", "enter", "%s", kfb->_sys_name);
	kcsi_acu_zero_f_errno();
	i_next(kfb->_io_vector,kfb->_record);
	kfb->_status = v_trans2(kcsi_acu_get_f_errno());
}

void ksam_read_keyed(KCSIO_BLOCK *kfb)
{
	kcsitrace(1, "kv3:ksam_read_keyed()", "enter", "%s", kfb->_sys_name);
	kcsi_acu_zero_f_errno();
	i_read(kfb->_io_vector,kfb->_record,kfb->_io_key);
	kfb->_status = v_trans(kcsi_acu_get_f_errno());

	/* Unlock the record */
	if (kfb->_status == 0)
	{
		i_unlock(kfb->_io_vector);
	}
}

void ksam_hold_keyed(KCSIO_BLOCK *kfb)
{
	kcsitrace(1, "kv3:ksam_hold_kyed()", "enter", "%s", kfb->_sys_name);
	kcsi_acu_zero_f_errno();
	i_read(kfb->_io_vector,kfb->_record,kfb->_io_key);
	kfb->_status = v_trans(kcsi_acu_get_f_errno());
}

/*----
			<<<< STARTS >>>>
------*/
/*----
Starts are vanilla, but see notes on read and hold next.
------*/
void ksam_start_eq(KCSIO_BLOCK *kfb)
{
	kcsitrace(1, "kv3:ksam_start_eq()", "enter", "%s", kfb->_sys_name);
	kfb->_io_key = 0;
	do_start(kfb,F_EQUALS);
}

void ksam_start_nlt(KCSIO_BLOCK *kfb)
{
	kcsitrace(1, "kv3:ksam_start_nlt()", "enter", "%s", kfb->_sys_name);
	kfb->_io_key = 0;
	do_start(kfb,F_NOT_LESS);
}

void ksam_start_gt(KCSIO_BLOCK *kfb)
{
	kcsitrace(1, "kv3:ksam_start_gt()", "enter", "%s", kfb->_sys_name);
	kfb->_io_key = 0;
	do_start(kfb,F_GREATER);
}
void ksam_start_eq_keyed(KCSIO_BLOCK *kfb)
{
	kcsitrace(1, "kv3:ksam_start_eq_keyed()", "enter", "%s", kfb->_sys_name);
	do_start(kfb,F_EQUALS);
}

void ksam_start_nlt_keyed(KCSIO_BLOCK *kfb)
{
	kcsitrace(1, "kv3:ksam_start_nlt_keyed()", "enter", "%s", kfb->_sys_name);
	do_start(kfb,F_NOT_LESS);
}

void ksam_start_gt_keyed(KCSIO_BLOCK *kfb)
{
	kcsitrace(1, "kv3:ksam_start_gt_keyed()", "enter", "%s", kfb->_sys_name);
	do_start(kfb,F_GREATER);
}

void ksam_start_last(KCSIO_BLOCK *kfb)
{
	kcsitrace(1, "kv3:ksam_start_last()", "enter", "%s", kfb->_sys_name);
	kfb->_status = EBADARG;
}

/*----
Executes a start and sets up a new _last_io_key.
------*/
static void do_start(KCSIO_BLOCK *kfb, int mode)
{
	kcsitrace(1, "kv3:do_start()", "enter", "%s mode=%d", kfb->_sys_name, mode);
	kcsi_acu_zero_f_errno();
	i_start(kfb->_io_vector,kfb->_record,kfb->_io_key,0,mode);
	kfb->_status = v_trans(kcsi_acu_get_f_errno());
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
	kcsitrace(1, "kv3:ksam_write()", "enter", "%s", kfb->_sys_name);
	kcsi_acu_zero_f_errno();
	i_write(kfb->_io_vector,kfb->_record,kfb->_record_len);
	kfb->_status = v_trans(kcsi_acu_get_f_errno());
	ksam_file_space(kfb);
}

void ksam_rewrite(KCSIO_BLOCK *kfb)
{
	kcsitrace(1, "kv3:ksam_rewrite()", "enter", "%s", kfb->_sys_name);
	kcsi_acu_zero_f_errno();
	i_rewrite(kfb->_io_vector, kfb->_record, kfb->_record_len);
	kfb->_status = v_trans(kcsi_acu_get_f_errno());
}

void ksam_delete(KCSIO_BLOCK *kfb)
{
	kcsitrace(1, "kv3:ksam_delete()", "enter", "%s", kfb->_sys_name);
	kcsi_acu_zero_f_errno();
	i_delete(kfb->_io_vector,kfb->_record);
	kfb->_status = v_trans(kcsi_acu_get_f_errno());
	ksam_file_space(kfb);
}

void ksam_unlock(KCSIO_BLOCK *kfb)
{
	kcsitrace(1, "kv3:ksam_unlock()", "enter", "%s", kfb->_sys_name);
/* 	kcsi_acu_zero_f_errno(); */
	i_unlock(kfb->_io_vector);
/*	kfb->_status = v_trans(kcsi_acu_get_f_errno()); */
}

/*----
Get table_info (same as file_info under vision 3)
------*/
void ksam_table_info(KCSIO_BLOCK *kfb)
{
	kcsitrace(1, "kv3:ksam_table_info()", "enter", "%s", kfb->_sys_name);
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
	
	kcsitrace(1, "kv3:ksam_file_info()", "enter", "%s", kfb->_sys_name);
	rc = i_info(kfb->_io_vector,-1,params);
	lp = params;

	kfb->_record_len = kptoi(&lp);
	++lp;
	kptoi(&lp);
	++lp;
	kfb->_altkey_count = kptoi(&lp);
	++lp;
	--kfb->_altkey_count;

	KCSI_clear_keys(kfb);

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
	int rc;
	

	if(	(code == E_NOT_FOUND)	||
		(code == E_UNDEF_RECORD)	)
		rc = EENDFILE;
	else
		rc = v_trans(code);

	kcsitrace(1, "kv3:v_trans2()", "translated", "%d --> %d", code, rc);

	return rc;
}

static int v_trans(int code)
{
	int rc;
	char *msg = "";

	switch(code)
		{
		case 0:
			rc = 0;
			msg = "OK";
			break;

		case W_DUP_OK:
			rc = 0;
			msg = "Duplicate OK";
			break;

		case E_REC_LOCKED:
			rc = ELOCKED;
			msg = "Locked";
			break;			

		case E_SYS_ERR:
		case E_DISK_FULL:
			rc = kcsi_acu_get_f_errno();
			msg = "System error/Disk full";
			break;
			
		case E_PARAM_ERR:
			rc = EBADARG;
			msg = "Param error";
			break;
			
		case E_TOO_MANY_FILES:
			rc = EBADFILE;
			msg = "Too many files";
			break;
			
		case E_MODE_CLASH:
			rc = ENOTEXCL;
			msg = "Mode clash";
			break;
			
		case E_BROKEN:
			rc = EBADFILE;
			msg = "Broken file";
			break;
			
		case E_DUPLICATE:
			rc = EDUPL;
			msg = "Duplicate";
			break;
			
		case E_NOT_FOUND:
			rc = ENOREC;
			msg = "Not found";
			break;
			
		case E_NO_MEMORY:
			rc = EBADMEM;
			msg = "No memory";
			break;
			
		default:
			rc = EBADFILE;
			msg = "Unknown";
			break;
		}

	kcsitrace(1, "kv3:v_trans()", "translated", "%d --> %d %s", code, rc, msg);

	return rc;
}

/*
**	Older versions of Acucobol (5.1 and earlier) exposed
**	the f_errno variable as extern short.
**	
**	In 5.2  f_errno is replaced by (*Astdlib_f_errno())  
**	which is defined int sub.h.
*/

extern	short		*Astdlib_f_errno(); /* From ACU 5.2 sub.h */

static void kcsi_acu_zero_f_errno(void)
{
        (*Astdlib_f_errno()) = 0;
}

static short kcsi_acu_get_f_errno(void)
{
	short rc;
	
	rc = (*Astdlib_f_errno());

	kcsitrace(1, "kv3:kcsi_acu_get_f_errno()", "errno", "f_errno=%d", (int)rc);
	return rc;
}

#endif /* KCSI_ACU */

/*
**	History:
**	$Log: kv3.c,v $
**	Revision 1.18  2003/06/11 19:22:10  gsl
**	add 4.0.01 updates to fix signal problem with acu
**	
**	Revision 1.17  2003/04/04 19:43:51  gsl
**	For Acucobol READ was also locking the record.  so add an UNLOCK after
**	every READ that doesn't want a lock.
**	BUGZILLA 114
**	
**	Revision 1.16  2003/02/04 19:19:09  gsl
**	fix header
**	
**	Revision 1.15  2002/10/24 14:20:37  gsl
**	Make globals unique
**	
**	Revision 1.14  2002/10/21 16:07:05  gsl
**	Add ksam_init
**	
**	Revision 1.13  2002/10/18 20:13:04  gsl
**	Move Acucobol 5.1 f_error support from kv3.c to vscrmain.c
**	
**	Revision 1.12  2002/05/14 21:14:29  gsl
**	Fixed f_error -> f_errno
**	
**	Revision 1.11  2002-05-14 16:22:10-04  gsl
**	Use old f_errno for CREATE on Windows
**
**	Revision 1.10  2002-04-24 11:35:26-04  gsl
**	tracing
**
**	Revision 1.9  2002-03-27 15:21:16-05  gsl
**	Fix f_errno for Acucobol 5.2 to use *Astdlib_f_errno()
**
**	Revision 1.8  2001-10-25 09:50:02-04  gsl
**	fix comment
**
**	Revision 1.7  2001-09-06 10:16:13-04  gsl
**	Remove v_make_vers
**	as not in Acucobol 5.x
**
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
