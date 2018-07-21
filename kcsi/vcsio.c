/*
******************************************************************************
**
** KCSI - King Computer Services Inc.
**
** 
******************************************************************************
*/

/*---
vcsio.c
Imitates the KCSIO program as a shell into VISION calls.
the ufb is ignored as not needed by VISION though it is used in the Wang
COBOL version.

24 Mar 1990 modified the clear keys routine to call clear_one_key
intstead of clearing al internally.
This was done so that get file info could clear a single key that was
found to have no length.

Cloned from KCSI_ccsio.c for routine names.

------*/
#include <stdio.h>
#include <ctype.h>
#include <errno.h>

#ifdef unix
#include <unistd.h>
#endif

#include "iocode.h"
#include "kcsio.h"
#include "gp.h"
#include "kwisp.h"
#include "intdef.h"
#include "kcsifunc.h"
#include "visn3.h"

#ifdef WIN32
#define unlink(filename) _unlink(filename)
#endif

/*
#define	DEBUG
*/
/*----
The passed io_block is converted to the global struct to allow all routines
to access the data and to get it into a format that 'C' can live with.
The conversion is based on the type of IO being done.
For OPENs it is done for all fields.
For reads and starts the appropriate key is converted.
The record_area is copied into and out of rec for the length in the
passed io block.
------*/
/*----
Some shorthand and specialized versions of semi-standard routines
and to improve readability.
------*/
/*Copy for target length -1 and add nul. x must not be a pointer */
#define Make_c_str(x,y)   memcpy(x,y,sizeof(x)-1); x[sizeof(x)-1] = 0;
#define Cobol_status(x)   memcpy(&kio[STATUS_POS],x,2) /* Set COBOL status  */
#define Streq(x,y)        (!strcmp(x,y))               /* True when equal   */
#define IOis(y)           Streq(kfb->_io,y)            /* Test for IO type  */

/*----
This should handle an alignment problem by copying the struct
from the sys-io-block into an aligned structure.
------*/

static void gpcsio_open(KFB *kfb,char *recarea,int wangflag);
static void gpcsio_output(KFB *kfb,char *recarea,int wangflag);
static int gpcsio_unlink(char *name,char *ext);
static int gpcsio_error(KFB *kfb,int wangflag,int type);
static void ccsio_init(void);
static void cinit_for_open(KFB *kfb);
static void decide(KFB *kfb);
static void logit(KFB *kfb);
static void clear_one_key(struct keydesc *k);


/*------
		C Level IO Logic

Entry conditions.
	For any io
1.	_io filled in (see iocode.h)

	For io other than an open
1.	Legal _io_vector of an open file

	For reads
1.	an _io_key number (0 - 16)
2.	There are primary reads and starts that assume key 0 so it need not
	be filled in.
3.	A _rel_key number (physical record in the file) for non keyed
	files.

	For OPENS
1.	_sys_name filled in (without ".dat" extension)

	For OPEN OUTPUT
1.	_record_len at least one _key structure

	For FILE INFO
1.	File must be open before this call

------*/

#define	OPEN_GP		0
#define	SCRATCH_GP	1
#define	OUTPUT_GP	2

/*----
KCSI_gpcsio() is a higher level version of KCSI_ccsio(). It is passed a
kfb, a record, and a system or wang flag of 0, or 1. If the IO
request is an open, then a the result of the open is tested and
a GETPARM issued to resolve difficulties. THe flag indicates
whether to use _sys_name, or wang _name, _lib and _vol attributes
for the processing. KCSI_gpwcsio() and KCSI_gpscsio() are entries that force
the file naming type in question
------*/


void KCSI_gpcsio(kfb,recarea,wangflag)
char *recarea;
KFB *kfb;
int wangflag;
{

	if(IOis(OPEN_OUTPUT))
		gpcsio_output(kfb,recarea,wangflag);
	else	
	if(io_is_open(kfb))
		gpcsio_open(kfb,recarea,wangflag);
	else
		KCSI_ccsio(kfb,recarea);
}

void KCSI_gpwcsio(KFB *kfb,char *recarea)
{
	KCSI_gpcsio(kfb,recarea,1);
}

void KCSI_gpscsio(kfb,recarea)
char *recarea;
KFB *kfb;
{
	KCSI_gpcsio(kfb,recarea,0);
}


/*----
Non Output open.
Set up a mode flag, and do a kcsio_wfopen() if this is in Wang mode.
This sets up the name correctly.
------*/

static void gpcsio_open(KFB *kfb,char *recarea,int wangflag)
{
	int4 mode;

	mode = 0;
	if(kfb->_org[0] == 'I')
		mode += IS_INDEXED;
	if(wangflag)
		kcsio_wfopen(mode,kfb);
/*
 * 1 Truncate the name
 * 2 Try the open
 * 3 Untruncate the name
 * 4 Error GP if it fails
 * 5 Loop til open, or PF 16 pressed
 */
	while(1)
		{
		KCSI_strunc(kfb->_sys_name);
		KCSI_ccsio(kfb,recarea);
		KCSI_unstrunc(kfb->_sys_name,sizeof(kfb->_sys_name) - 1);
		if(kfb->_status == 0)
			break;
		if(gpcsio_error(kfb,wangflag,OPEN_GP) == 16)
			{
			kcsi_exit(0);
			}
		}
		
}

static void gpcsio_output(KFB *kfb,char *recarea,int wangflag)
{
	int4 mode;
	int rc;

	mode = IS_OUTPUT;
	if(kfb->_org[0] == 'I')
		mode += IS_INDEXED;
	if(wangflag)
		kcsio_wfopen(mode,kfb);
/*
 * 1. Try to delete all parts of a file
 * 2. If a delete on any part fails for other than (ENOENT), the
 *    delete has failed.
 * 3. Loop until delete succeeds or 16 exit.
 */
	while(1)
		{
		KCSI_strunc(kfb->_sys_name);
		rc = gpcsio_unlink(kfb->_sys_name,"");
		if(!rc)
			rc = gpcsio_unlink(kfb->_sys_name,".dat");
		if(!rc)
			rc = gpcsio_unlink(kfb->_sys_name,".idx");
		KCSI_unstrunc(kfb->_sys_name,sizeof(kfb->_sys_name) - 1);
		if(rc == 0)
			break;
		if(gpcsio_error(kfb,wangflag,SCRATCH_GP) == 16)
			{
			kcsi_exit(0);
			}
		}
		
/*
 * 1 Truncate the name
 * 2 Try the open
 * 3 Untruncate the name
 * 4 Error GP if it fails
 * 5 Loop til open, or PF 16 pressed
 */
	while(1)
		{
		KCSI_strunc(kfb->_sys_name);
		KCSI_ccsio(kfb,recarea);
		KCSI_unstrunc(kfb->_sys_name,sizeof(kfb->_sys_name) - 1);
		if(kfb->_status == 0)
			break;
		if(gpcsio_error(kfb,wangflag,OUTPUT_GP) == 16)
			{
			kcsi_exit(0);
			}
		}
		
}

static int gpcsio_unlink(char *name,char *ext)
{
	int rc;
	char work[91];

	sprintf(work,"%s%s",name,ext);
	rc = unlink(work);
	if (rc)
		rc = errno;
	if (rc == ENOENT)
		rc = 0;
	return(rc);
}


static int gpcsio_error(KFB *kfb,int wangflag,int type)
{
	int rc;
	int4 pf;
	char prname[9];

	GP_init_gpint();
	WL_wpload();
	GP_pfkeys = 0;

/* pfkeys for scratch getparm and an open getparm */
	if(type == SCRATCH_GP)
		GP_pfkeys=GP_PF_03|GP_PF_16;
	else	/*OPEN or OUTPUT */
		GP_pfkeys=GP_PF_16;

/*prname use one of it exists, else make one up based on open type */
	if(kfb->_prname[0] > ' ')
		sprintf(prname,"%-8s", kfb->_prname);
	else
	if(type == OPEN_GP)
		strcpy(prname,"INFILE  ");
	else
		strcpy(prname,"OUTFILE ");
	
	WL_wswap(&GP_pfkeys);
	GPSETUP();
	GPSTD(prname,"CCSIO ");
	if(type == SCRATCH_GP)
		{
		GPCTEXT("File already exists.",11,2);
		}
	else	/* OPEN or OUTPUT */
		{
		GPCTEXT("File cannot be opened.",11,2);
		}
	if(wangflag)
		{
		GPFLVUC(kfb->_name,kfb->_library,kfb->_volume,14,15);
		}
	else
		{
		GPSYSNAMEC(kfb->_sys_name,14,2);
		}
	GPCTEXT("Please rename and press (ENTER) or",16,2);
	if(type == SCRATCH_GP)
		{
		GPCTEXT("PFKey (3) to Scratch (16) to Exit", 18,2);
		}
	else	/* OPEN or OUTPUT */
		{
		GPCTEXT("PFKey (16) to Exit",18,2);
		}
	GPPFS(&GP_pfkeys);
	pf = GP_display_and_read();
	rc = pf;
	return(rc);
}


void KCSI_ccsio(KFB *kfb,char *recarea)
{
	kfb->_record = recarea;

	kfb->_status = 0;

	KCSI_strunc(kfb->_sys_name);
	ccsio_init();
	if(io_is_open(kfb))
		cinit_for_open(kfb);
	decide(kfb);
/*
 * Set _last_io to _io and store the value back in the cobol
 * area.
 */
	strcpy(kfb->_last_io,kfb->_io);
	KCSI_unstrunc(kfb->_sys_name,sizeof(kfb->_sys_name) - 1);
}

static void ccsio_init(void)
{
	static int initted = 0;

	if(initted)
	{
		return;
	}

	ksam_init();
	initted = 1;
	
}

static void cinit_for_open(KFB *kfb)
{
	kfb->_open_status = 0;
	kfb->_io_key = 0;
	kfb->_io_vector = NULL;
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
static void open_shared(KFB *kfb)
{
	kcsitrace(1, "open_shared()", "enter", "[%c] %s", kfb->_org[0], kfb->_sys_name);

	switch(kfb->_org[0])
		{
		case 'I':
			ksam_open_shared(kfb);
			break;
		case 'C':
			seq_open_shared(kfb);
			break;
		case 'B':
			brel_open_shared(kfb);
			break;
		case 'R':
		default:
#ifdef KCSI_ACU
			brel_open_shared(kfb);
#else
			rel_open_shared(kfb);
#endif
			break;
		}
	logit(kfb);
}
static void open_input(KFB *kfb)
{
	kcsitrace(1, "open_input()", "enter", "[%c] %s", kfb->_org[0], kfb->_sys_name);

	switch(kfb->_org[0])
		{
		case 'I':
			ksam_open_input(kfb);
			break;
		case 'C':
			seq_open_input(kfb);
			break;
		case 'B':
			brel_open_input(kfb);
			break;
		case 'R':
		default:
#ifdef KCSI_ACU
			brel_open_input(kfb);
#else
			rel_open_input(kfb);
#endif
			break;
		}
	logit(kfb);
}
static void open_io(KFB *kfb)
{
	kcsitrace(1, "open_io()", "enter", "[%c] %s", kfb->_org[0], kfb->_sys_name);

	switch(kfb->_org[0])
		{
		case 'I':
			ksam_open_io(kfb);
			break;
		case 'C':
			seq_open_io(kfb);
			break;
		case 'B':
			brel_open_io(kfb);
			break;
		case 'R':
		default:
#ifdef KCSI_ACU
			brel_open_io(kfb);
#else
			rel_open_io(kfb);
#endif
			break;
		}
	logit(kfb);
}
/*----
A vision open consists of building a file with all keys.
------*/
static void open_output(KFB *kfb)
{
	kcsitrace(1, "open_output()", "enter", "[%c] %s", kfb->_org[0], kfb->_sys_name);

	switch(kfb->_org[0])
		{
		case 'I':
			ksam_open_output(kfb);
			break;
		case 'C':
			seq_open_output(kfb);
			break;
		case 'B':
			brel_open_output(kfb);
			break;
		case 'R':
		default:
#ifdef KCSI_ACU
			brel_open_output(kfb);
#else
			rel_open_output(kfb);
#endif
			break;
		}
	logit(kfb);
}

static void close_file(KFB *kfb)
{
	kcsitrace(1, "close_file()", "enter", "[%.1s] %.80s", kfb->_org, kfb->_sys_name);

	switch(kfb->_org[0])
	{
		case 'I':
			ksam_close(kfb);
			break;
		case 'C':
			seq_close(kfb);
			break;
		case 'B':
			brel_close(kfb);
			break;
		case 'R':
		default:
#ifdef KCSI_ACU
			brel_close(kfb);
#else
			rel_close(kfb);
#endif
			break;
	}
	logit(kfb);
}
/*----
               <<<<<<<<  READ START HOLD  >>>>>>>>>>>
------*/
/*----
A read record request assumes the primary key.
------*/
static void read_record(KFB *kfb)
{
	kcsitrace(1, "read_record()", "enter", "[%c] %s", kfb->_org[0], kfb->_sys_name);

	switch(kfb->_org[0])
		{
		case 'I':
			ksam_read(kfb);
			break;
		case 'C':
			seq_read(kfb);
			break;
		case 'B':
			brel_read(kfb);
			break;
		case 'R':
		default:
#ifdef KCSI_ACU
			brel_read(kfb);
#else
			rel_read(kfb);
#endif
			break;
		}
	logit(kfb);
}

static void read_keyed(KFB *kfb)
{
	kcsitrace(1, "read_keyed()", "enter", "[%c] %s", kfb->_org[0], kfb->_sys_name);

	switch(kfb->_org[0])
		{
		case 'I':
			ksam_read_keyed(kfb);
			break;
		case 'C':
			seq_read_keyed(kfb);
			break;
		case 'B':
			brel_read_keyed(kfb);
			break;
		case 'R':
		default:
#ifdef KCSI_ACU
			brel_read_keyed(kfb);
#else
			rel_read_keyed(kfb);
#endif
			break;
		}
	logit(kfb);
}

static void hold_record(KFB *kfb)
{
	kcsitrace(1, "hold_record()", "enter", "[%c] %s", kfb->_org[0], kfb->_sys_name);

	switch(kfb->_org[0])
		{
		case 'I':
			ksam_hold(kfb);
			break;
		case 'C':
			seq_hold(kfb);
			break;
		case 'B':
			brel_hold(kfb);
			break;
		case 'R':
		default:
#ifdef KCSI_ACU
			brel_hold(kfb);
#else
			rel_hold(kfb);
#endif
			break;
		}
	logit(kfb);
}

static void hold_keyed(KFB *kfb)
{
	kcsitrace(1, "hold_keyed()", "enter", "[%c] %s", kfb->_org[0], kfb->_sys_name);

	switch(kfb->_org[0])
		{
		case 'I':
			ksam_hold_keyed(kfb);
			break;
		case 'C':
			seq_hold_keyed(kfb);
			break;
		case 'B':
			brel_hold_keyed(kfb);
			break;
		case 'R':
		default:
#ifdef KCSI_ACU
			brel_hold_keyed(kfb);
#else
			rel_hold_keyed(kfb);
#endif
			break;
		}
	logit(kfb);
}

/*----
Starts are vanilla, but see notes on read and hold next.
------*/
static void start_eq(KFB *kfb)
{
	kcsitrace(1, "start_eq()", "enter", "[%c] %s", kfb->_org[0], kfb->_sys_name);

	switch(kfb->_org[0])
		{
		case 'I':
			ksam_start_eq(kfb);
			break;
		case 'C':
			seq_start_eq(kfb);
			break;
		case 'B':
			brel_start_eq(kfb);
			break;
		case 'R':
		default:
#ifdef KCSI_ACU
			brel_start_eq(kfb);
#else
			rel_start_eq(kfb);
#endif
			break;
		}
	logit(kfb);
}

static void start_nlt(KFB *kfb)
{
	kcsitrace(1, "start_nlt()", "enter", "[%c] %s", kfb->_org[0], kfb->_sys_name);

	switch(kfb->_org[0])
		{
		case 'I':
			ksam_start_nlt(kfb);
			break;
		case 'C':
			seq_start_nlt(kfb);
			break;
		case 'B':
			brel_start_nlt(kfb);
			break;
		case 'R':
		default:
#ifdef KCSI_ACU
			brel_start_nlt(kfb);
#else
			rel_start_nlt(kfb);
#endif
			break;
		}
	logit(kfb);
}

static void start_gt(KFB *kfb)
{
	kcsitrace(1, "start_gt()", "enter", "[%c] %s", kfb->_org[0], kfb->_sys_name);

	switch(kfb->_org[0])
		{
		case 'I':
			ksam_start_gt(kfb);
			break;
		case 'C':
			seq_start_gt(kfb);
			break;
		case 'B':
			brel_start_gt(kfb);
			break;
		case 'R':
		default:
#ifdef KCSI_ACU
			brel_start_gt(kfb);
#else
			rel_start_gt(kfb);
#endif
			break;
		}
	logit(kfb);
}
static void start_eq_keyed(KFB *kfb)
{
	kcsitrace(1, "start_eq_keyed()", "enter", "[%c] %s", kfb->_org[0], kfb->_sys_name);

	switch(kfb->_org[0])
		{
		case 'I':
			ksam_start_eq_keyed(kfb);
			break;
		case 'C':
			seq_start_eq_keyed(kfb);
			break;
		case 'B':
			brel_start_eq_keyed(kfb);
			break;
		case 'R':
		default:
#ifdef KCSI_ACU
			brel_start_eq_keyed(kfb);
#else
			rel_start_eq_keyed(kfb);
#endif
			break;
		}
	logit(kfb);
}

static void start_nlt_keyed(KFB *kfb)
{
	kcsitrace(1, "start_nlt_keyed()", "enter", "[%c] %s", kfb->_org[0], kfb->_sys_name);

	switch(kfb->_org[0])
		{
		case 'I':
			ksam_start_nlt_keyed(kfb);
			break;
		case 'C':
			seq_start_nlt_keyed(kfb);
			break;
		case 'B':
			brel_start_nlt_keyed(kfb);
			break;
		case 'R':
		default:
#ifdef KCSI_ACU
			brel_start_nlt_keyed(kfb);
#else
			rel_start_nlt_keyed(kfb);
#endif
			break;
		}
	logit(kfb);
}

static void start_gt_keyed(KFB *kfb)
{
	kcsitrace(1, "start_gt_keyed()", "enter", "[%c] %s", kfb->_org[0], kfb->_sys_name);

	switch(kfb->_org[0])
		{
		case 'I':
			ksam_start_gt_keyed(kfb);
			break;
		case 'C':
			seq_start_gt_keyed(kfb);
			break;
		case 'B':
			brel_start_gt_keyed(kfb);
			break;
		case 'R':
		default:
#ifdef KCSI_ACU
			brel_start_gt_keyed(kfb);
#else
			rel_start_gt_keyed(kfb);
#endif
			break;
		}
	logit(kfb);
}
static void start_last(KFB *kfb)
{
	kcsitrace(1, "start_last()", "enter", "[%c] %s", kfb->_org[0], kfb->_sys_name);

	switch(kfb->_org[0])
		{
		case 'I':
			ksam_start_last(kfb);
			break;
		case 'C':
			seq_start_last(kfb);
			break;
		case 'B':
			brel_start_last(kfb);
			break;
		case 'R':
		default:
#ifdef KCSI_ACU
			brel_start_last(kfb);
#else
			rel_start_last(kfb);
#endif
			break;
		}
	logit(kfb);
}


/*----
For read next and hold next, vision presents no problem.
------*/
static void read_next_record(KFB *kfb)
{
	kcsitrace(1, "read_next_record()", "enter", "[%c] %s", kfb->_org[0], kfb->_sys_name);
	
	switch(kfb->_org[0])
		{
		case 'I':
			ksam_read_next(kfb);
			break;
		case 'C':
			seq_read_next(kfb);
			break;
		case 'B':
			brel_read_next(kfb);
			break;
		case 'R':
		default:
#ifdef KCSI_ACU
			brel_read_next(kfb);
#else
			rel_read_next(kfb);
#endif
			break;
		}
	logit(kfb);
}

static void read_previous_record(KFB *kfb)
{
	kcsitrace(1, "read_previous_record()", "enter", "[%c] %s", kfb->_org[0], kfb->_sys_name);

	switch(kfb->_org[0])
		{
		case 'I':
			ksam_read_previous(kfb);
			break;
		case 'C':
			seq_read_previous(kfb);
			break;
		case 'B':
			brel_read_previous(kfb);
			break;
		case 'R':
		default:
#ifdef KCSI_ACU
			brel_read_previous(kfb);
#else
			rel_read_previous(kfb);
#endif
			break;
		}
	logit(kfb);
}

static void hold_next_record(KFB *kfb)
{
	kcsitrace(1, "hold_next_record()", "enter", "[%c] %s", kfb->_org[0], kfb->_sys_name);

	switch(kfb->_org[0])
		{
		case 'I':
			ksam_hold_next(kfb);
			break;
		case 'C':
			seq_hold_next(kfb);
			break;
		case 'B':
			brel_hold_next(kfb);
			break;
		case 'R':
		default:
#ifdef KCSI_ACU
			brel_hold_next(kfb);
#else
			rel_hold_next(kfb);
#endif
			break;
		}
	logit(kfb);
}
/*----
               <<<<<<<< WRITE REWRITE DELETE  >>>>>>>>>>>>
------*/
/*----
Write, rewrite and delete are vanilla except that a hold should have
been issued before the rewrite or delete so these are released.
------*/
static void write_record(KFB *kfb)
{
	kcsitrace(1, "write_record()", "enter", "[%c] %s", kfb->_org[0], kfb->_sys_name);

	switch(kfb->_org[0])
		{
		case 'I':
			ksam_write(kfb);
			break;
		case 'C':
			seq_write(kfb);
			break;
		case 'B':
			brel_write(kfb);
			break;
		case 'R':
		default:
#ifdef KCSI_ACU
			brel_write(kfb);
#else
			rel_write(kfb);
#endif
			break;
		}
	logit(kfb);
}

static void rewrite_record(KFB *kfb)
{
	kcsitrace(1, "rewrite_record()", "enter", "[%c] %s", kfb->_org[0], kfb->_sys_name);

	switch(kfb->_org[0])
		{
		case 'I':
			ksam_rewrite(kfb);
			ksam_unlock(kfb);
			break;
		case 'C':
			seq_rewrite(kfb);
			seq_unlock(kfb);
			break;
		case 'B':
			brel_rewrite(kfb);
			brel_unlock(kfb);
			break;
		case 'R':
		default:
#ifdef KCSI_ACU
			brel_rewrite(kfb);
			brel_unlock(kfb);
#else
			rel_rewrite(kfb);
			rel_unlock(kfb);
#endif
			break;
		}
	logit(kfb);
}

static void delete_record(KFB *kfb)
{
	kcsitrace(1, "delete_record()", "enter", "[%c] %s", kfb->_org[0], kfb->_sys_name);

	switch(kfb->_org[0])
		{
		case 'I':
			ksam_delete(kfb);
			ksam_unlock(kfb);
			break;
		case 'C':
			seq_delete(kfb);
			seq_unlock(kfb);
			break;
		case 'B':
			brel_delete(kfb);
			brel_unlock(kfb);
			break;
		case 'R':
		default:
#ifdef KCSI_ACU
			brel_delete(kfb);
			brel_unlock(kfb);
#else
			rel_delete(kfb);
			rel_unlock(kfb);
#endif
			break;
		}
}

/*----
Clear a key structure in preparation for an open.
------*/
void KCSI_clear_keys(KFB *kfb)
{
	int i;
	for(i = 0 ; i < WANG_MAX_KEYS ; ++i)
		{
		clear_one_key(&kfb->_key[i]);	/*   24-Mar-1990*/
		}
}
/*----
Clear one key structure.
------*/
static void clear_one_key(struct keydesc *k)
{
    int j;

    k->k_flags = 0;
    k->k_nparts = 0;
    for(j = 0 ; j < NPARTS ; ++j)
	{
	k->k_part[j].kp_start = 0;
	k->k_part[j].kp_leng  = 0;
	k->k_part[j].kp_type  = 0;
	}
}

/*----
This routine is a simple init that forces a key to have
1 part and be a CHARTYPE. The position passed by COBOL uses
the COBOL base 1 offsets and must have 1 subtracted to use
'C' base 0 offsets.
------*/
void KCSI_init_a_key(struct keydesc *k,int pos,int len,int dups)
{
	if((pos == 0) ||(len == 0))
		return;
	k->k_flags = dups;
	k->k_nparts = 1;
	k->k_part[0].kp_start = pos - 1;
	k->k_part[0].kp_leng  = len;
	k->k_part[0].kp_type  = 0;
/*
 * If the length of the key part is zero
 * then it has no parts. This should only exist on the primary
 * key for relative files.
 */
	if(k->k_part[0].kp_leng  == 0)
		k->k_nparts = 0;
}

/*----
The get_file_info() routine pulls the data on an open file and
sets up the c structure.
------*/
static void get_file_info(KFB *kfb)
{
	kcsitrace(1, "get_file_info()", "enter", "[%c] %s", kfb->_org[0], kfb->_sys_name);

	switch(kfb->_org[0])
		{
		case 'I':
			ksam_file_info(kfb);
			break;
		case 'C':
			seq_file_info(kfb);
			break;
		case 'B':
			brel_file_info(kfb);
			break;
		case 'R':
		default:
#ifdef KCSI_ACU
			brel_file_info(kfb);
#else
			rel_file_info(kfb);
#endif
			break;
		}
}

/*----
The get_table_info() routine is a special version of
get_file_info() that behaves differently under 
microfocus COBOL. It pulls the data on an open file and
sets up the c structure. Assuming it is a table file.
It should only be used for keyed table files.
------*/
static void get_table_info(KFB *kfb)
{
	kcsitrace(1, "get_table_info()", "enter", "[%c] %s", kfb->_org[0], kfb->_sys_name);

	switch(kfb->_org[0])
		{
		case 'I':
			ksam_table_info(kfb);
			break;
		case 'C':
			seq_file_info(kfb);
			break;
		case 'B':
			brel_file_info(kfb);
			break;
		case 'R':
		default:
#ifdef KCSI_ACU
			brel_file_info(kfb);
#else
			rel_file_info(kfb);
#endif
			break;
		}
}

/*----
Dispatch table and dispatcher.
------*/
typedef struct _dispatch{
	char code[3];
	void (*func)(KFB *);
	}DISPATCH;

static DISPATCH dis[]={
	{OPEN_SHARED, open_shared},
	{OPEN_INPUT, open_input},
	{OPEN_OUTPUT, open_output},
	{OPEN_IO, open_io},
	{CLOSE_FILE, close_file},
	{READ_RECORD, read_record},
	{WRITE_RECORD, write_record},
	{REWRITE_RECORD, rewrite_record},
	{HOLD_RECORD, hold_record},
	{READ_NEXT_RECORD, read_next_record},
	{HOLD_NEXT_RECORD, hold_next_record},
	{DELETE_RECORD, delete_record},
	{READ_KEYED, read_keyed},
	{HOLD_KEYED, hold_keyed},
	{START_EQ, start_eq},
	{START_NLT, start_nlt},
	{START_GT, start_gt},
	{START_EQ_KEYED, start_eq_keyed},
	{START_NLT_KEYED, start_nlt_keyed},
	{START_GT_KEYED, start_gt_keyed},
	{FILE_INFO, get_file_info},
	{START_LAST, start_last},
	{READ_PREVIOUS_RECORD, read_previous_record},
	{TABLE_INFO, get_table_info},
	{"", NULL}
	};

/*----
1. Match the iocode and dispatch to routine that is located.
------*/
static void decide(KFB *kfb)
{
	int i;

	for(i=0;dis[i].code[0];++i)
		{
		if(IOis(dis[i].code))
			{
			 (*dis[i].func)(kfb);
			break;
			}
		}
}


static void logit(KFB *kfb)
{
	if(kfb->_status == 0)
		return;

	KCSI_kfberr(kfb);
}

void kcsio_wfopen(int4 mode,KFB *kfb)
{
	WFOPEN2(&mode,
			kfb->_volume,kfb->_library,kfb->_name,kfb->_sys_name,
			"KCSI    ", kfb->_prname);
	KCSI_strunc(kfb->_sys_name);
}

/* Routine added here on 03/14/95 by ZR as per MJB.  Copied from relio.c,
   and commented out in that file . Also commented out of vxrelio.c (VMS)
*/

int KCSI_e_trans(int code)
{
	if(code == 0)
		return(0);
	return(EBADF);
}
/*
**	History:
**	$Log: vcsio.c,v $
**	Revision 1.30  2011/10/29 20:09:14  gsl
**	Fix ISO routine name warnins on WIN32
**	
**	Revision 1.29  2003/06/10 16:49:50  gsl
**	fix trace in close_file()
**	
**	Revision 1.28  2003/02/05 15:23:59  gsl
**	Fix -Wall warnings
**	
**	Revision 1.27  2003/02/04 19:19:08  gsl
**	fix header
**	
**	Revision 1.26  2002/10/24 14:20:33  gsl
**	Make globals unique
**	
**	Revision 1.25  2002/10/23 20:39:05  gsl
**	make global name unique
**	
**	Revision 1.24  2002/10/21 16:07:04  gsl
**	Add ksam_init
**	
**	Revision 1.23  2002/10/17 21:22:44  gsl
**	cleanup
**	
**	Revision 1.22  2002/10/17 17:17:23  gsl
**	Removed VAX VMS code
**	
**	Revision 1.21  2002/07/29 14:47:21  gsl
**	wfopen2 ->WFOPEN2
**	wfopen3 ->WFOPEN3
**	
**	Revision 1.20  2002/07/25 15:48:42  gsl
**	Globals
**	
**	Revision 1.19  2002/07/25 15:20:23  gsl
**	Globals
**	
**	Revision 1.18  2002/07/23 20:49:51  gsl
**	globals
**	
**	Revision 1.17  2002/07/12 17:17:02  gsl
**	Global unique WL_ changes
**	
**	Revision 1.16  2002/07/10 21:06:26  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.15  2002/06/21 20:48:14  gsl
**	Rework the IS_xxx bit flags and now include from wcommon.h instead of duplicate
**	definitions.
**	
**	Revision 1.14  2002/04/23 20:49:56  gsl
**	add trace
**	
**	Revision 1.13  2002-04-22 16:57:14-04  gsl
**	Add kcsitrace
**
**	Revision 1.12  2001-11-15 19:16:21-05  gsl
**	Ifdef out old routine
**
**	Revision 1.11  1997-08-06 14:10:13-04  scass
**	Change 17 to WANG_MAX_KEYS
**
**	Revision 1.10  1996-10-02 20:13:27-04  gsl
**	Fix pfkey tag for w4w support
**
**	Revision 1.9  1996-10-02 15:11:54-07  gsl
**	Add include file
**
**	Revision 1.8  1996-10-02 09:15:08-07  gsl
**	Fix prototype
**
**	Revision 1.7  1996-09-17 16:34:21-07  gsl
**	drcs update
**
**
**
*/
