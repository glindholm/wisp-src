static char copyright[]="Copyright (c) 1988-1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
/*---
IO for LPI C-ISAM Files
------*/
#include <stdio.h>
#include <ctype.h>
#include "disam.h"
#include "iocode.h"
#include "kcsio.h"

static char sccsid[]="@(#)klpi.c	1.7 11/15/93";


/*
#define	DEBUG
*/
#define	DOS
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
#define LastIOis(y)       Streq(kfb->_last_io,y)        /* Test the last IO  */

/*----
Extract the root file data and move in the space.
------*/
ksam_file_space(KCSIO_BLOCK *kfb)
{
	struct dictinfo d;

	isindexinfo(kfb->_io_channel,&d,0);
	kfb->_space = d.di_nrecords;

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
ksam_open_shared(KCSIO_BLOCK *kfb)
{
	ksam_open_io(kfb);
}
ksam_open_input(KCSIO_BLOCK *kfb)
{
	do_open(kfb,ISINPUT + ISMANULOCK);
}
ksam_open_io(KCSIO_BLOCK *kfb)
{
	do_open(kfb,ISINOUT + ISMANULOCK);
}
/*----
Doopen should not be used for open output
------*/
static do_open(KCSIO_BLOCK *kfb, int mode)
{
	set_open(isopen(kfb->_sys_name,mode),kfb);
	if(kfb->_status == 0)
		ksam_file_info(kfb);
	if(kfb->_status == 0)
		do_start(kfb,ISFIRST);
}

ksam_unlock(KCSIO_BLOCK *kfb)
{

}

/*----
A c-isam open consists of building a file and then adding the
alternate indices.
------*/
ksam_open_output(KCSIO_BLOCK *kfb)
{
	int rc,keycount;

	rc = isbuild(kfb->_sys_name,
		kfb->_record_len,&kfb->_key[0],ISOUTPUT + ISEXCLLOCK);
	if(rc < 0)
		{
		kfb->_status = iserrno;
		logit(kfb);
		return;
		}
	kfb->_io_channel = rc;
	isclose(kfb->_io_channel);
	rc = isopen(kfb->_sys_name,ISINOUT + ISEXCLLOCK);
	if(rc < 0)
		{
		kfb->_status = iserrno;
		logit(kfb);
		return;
		}
	kfb->_io_channel = rc;
	for( keycount = 1 ; keycount <= kfb->_altkey_count ; ++keycount )
		{
		rc = isaddindex(kfb->_io_channel,&kfb->_key[keycount]);
		if (rc < 0)
			break;
		}
	isclose(kfb->_io_channel);
	if(rc > -1)
		rc = isopen(kfb->_sys_name,ISOUTPUT + ISMANULOCK);
	set_open(rc,kfb);
}

/*----
Set up some of the results from an open.
------*/
static set_open(int rc, KCSIO_BLOCK *kfb)
{
	if(rc < 0)
		{
		kfb->_status = iserrno;
		return;
		}
	else
		{
		kfb->_io_channel = rc;
		kfb->_last_io_key = 0;
		kfb->_open_status = 1;
		}

}
ksam_close(KCSIO_BLOCK *kfb)
{
	kfb->_status = iserrno = 0;

	if(kfb->_open_status == 0)
		return(iserrno);
	if(isclose(kfb->_io_channel))
		kfb->_status = iserrno;
	else
		kfb->_open_status = 0;
	return(iserrno);
}
/*----
               <<<<<<<<  READ START HOLD  >>>>>>>>>>>
------*/
/*----
A read record request assumes the primary key.
------*/
ksam_read(KCSIO_BLOCK *kfb)
{
	kfb->_io_key = 0;
	ksam_read_keyed(kfb);
}

ksam_read_keyed(KCSIO_BLOCK *kfb)
{
	do_read(kfb,ISEQUAL);
}

ksam_hold(KCSIO_BLOCK *kfb)
{
	kfb->_io_key = 0;
	ksam_hold_keyed(kfb);
}

ksam_hold_keyed(KCSIO_BLOCK *kfb)
{
	do_read(kfb,ISEQUAL + ISLOCK);
}

static do_read(KCSIO_BLOCK *kfb, int mode)
{
	int x;

	x = 0;
	if(kfb->_io_key != kfb->_last_io_key)
		x = set_new_key(kfb);

	if(!x)
		{
		x = keyedread(kfb,mode);
		}
	if(x)
		kfb->_status = iserrno;
}

static keyedread(KCSIO_BLOCK *kfb, int mode)
{
	int rc;

	rc=isread(kfb->_io_channel,kfb->_record,mode);
	return(rc);

}
/*
static relread(kfb,mode)
KFB *kfb;
int mode;
{
	int rc;

	isrecnum = kfb->_rel_key;
	rc=isread(kfb->_io_channel,kfb->_record,mode);
	return(rc);
}
*/

/*----
Establish a new key of reference. It would be easier to start with
ISCURR, change the key without changing records. D-ISAM supports this,
but C-ISAM does not.
------*/
static set_new_key(KCSIO_BLOCK *kfb)
{
	return(do_start(kfb,ISFIRST));
}
/*----
Starts are vanilla, but see notes on read and hold next.
------*/
ksam_start_eq(KCSIO_BLOCK *kfb)
{
	kfb->_io_key = 0;
	do_start(kfb,ISEQUAL);
}

ksam_start_nlt(KCSIO_BLOCK *kfb)
{
	kfb->_io_key = 0;
	do_start(kfb,ISGTEQ);
}

ksam_start_gt(KCSIO_BLOCK *kfb)
{
	kfb->_io_key = 0;
	do_start(kfb,ISGREAT);
}
ksam_start_eq_keyed(KCSIO_BLOCK *kfb)
{
	do_start(kfb,ISEQUAL);
}

ksam_start_nlt_keyed(KCSIO_BLOCK *kfb)
{
	do_start(kfb,ISGTEQ);
}

ksam_start_gt_keyed(KCSIO_BLOCK *kfb)
{
	do_start(kfb,ISGREAT);
}
ksam_start_last(KCSIO_BLOCK *kfb)
{
	do_start(kfb,ISLAST);
}

/*----
Executes a start and sets up a new _last_io_key.
------*/
static do_start(KCSIO_BLOCK *kfb, int mode)
{
	if(isstart(kfb->_io_channel,
		&kfb->_key[kfb->_io_key],0,kfb->_record,mode))
		kfb->_status = iserrno;
	else
		kfb->_last_io_key = kfb->_io_key;
}

/*----
For read next and hold next, c-isam presents a slight problem.
In COBOL a start positions the file so that a read next will pick
up the record that matches the start criteria.
A start in C-isam actually positions to a record such that
a read will pick up the record that matches. A read next will
skip over the desired record.
To compensate for this, the last io is saved. If a read next (hold next)
is issued immediately after a start, then a read current is used
otherwise a read next is used.
------*/
ksam_read_next(KCSIO_BLOCK *kfb)
{
	if(file_is_started(kfb))
		do_read(kfb,ISCURR);
	else
		do_read(kfb,ISNEXT);
}
ksam_read_previous(KCSIO_BLOCK *kfb)
{
    do_read(kfb,ISPREV);
}

ksam_hold_next(KCSIO_BLOCK *kfb)
{
	if(file_is_started(kfb))
		do_read(kfb,ISCURR + ISLOCK);
	else
		do_read(kfb,ISNEXT + ISLOCK);
}
static file_is_started(KCSIO_BLOCK *kfb)
{
	if((LastIOis(START_EQ))||(LastIOis(START_NLT))||(LastIOis(START_GT)))
		return(1);
	else
		return(0);
}
/*----
               <<<<<<<< WRITE REWRITE DELETE  >>>>>>>>>>>>
------*/
/*----
Write, rewrite and delete are vanilla except that a hold should have
been issued before the rewrite or delete so these are released.
------*/
ksam_write(KCSIO_BLOCK *kfb)
{
	if(iswrite(kfb->_io_channel,kfb->_record))
		kfb->_status = iserrno;
}

ksam_rewrite(KCSIO_BLOCK *kfb)
{
	int rc;

	rc = isrewrite(kfb->_io_channel,kfb->_record);
	if(rc)
		kfb->_status = iserrno;
	isrelease(kfb->_io_channel);
}

ksam_delete(KCSIO_BLOCK *kfb)
{

	if(isdelete(kfb->_io_channel,kfb->_record))
		kfb->_status = iserrno;
	isrelease(kfb->_io_channel);
}

/*----
Get table_info (same as file_info under C-ISAM)
------*/
ksam_table_info(KCSIO_BLOCK *kfb)
{
	ksam_file_info(kfb);
}

/*----
The get_file_info() routine pulls the data on an open file and
sets up the c structure.
------*/
ksam_file_info(KCSIO_BLOCK *kfb)
{
	struct dictinfo d;
	int i;

/*
 * Extract basic info
 */
	if(isindexinfo(kfb->_io_channel,&d,0))
		{
		kfb->_status = iserrno;
		return;
		}
	kfb->_record_len = d.di_recsize;

	kfb->_space = d.di_nrecords;

/*
 * And all the keys.
 */
	clear_keys(kfb);
	if(d.di_nkeys > 0)
		{
		kfb->_altkey_count = d.di_nkeys - 1;
		for( i = 0; i < d.di_nkeys; ++i )
			{
			if(isindexinfo(kfb->_io_channel,&kfb->_key[i],i + 1))
				{
				kfb->_status = iserrno;
				return;
				}
/* If a key has no parts, then clear the key */ 	    /*	 24-Mar-1990*/
			if(kfb->_key[i].k_nparts == 0)
			    clear_one_key(&kfb->_key[i]);
			}
		}
}
static logit(KCSIO_BLOCK *kfb)
{
	if(kfb->_status == 0)
		return;
	kfberr(kfb);
}
/*
**	History:
**	$Log: klpi.c,v $
**	Revision 1.4  1996-09-17 19:34:12-04  gsl
**	drcs update
**
**
**
*/
