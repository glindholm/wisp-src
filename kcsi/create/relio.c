static char copyright[]="Copyright (c) 1988-1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
/*---
relio.c

IO for relative files. Relative files are binary files processed
by open, read and write logic.

------*/
#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include "kcsio.h"

static char sccsid[]="@(#)relio.c	1.10 3/19/94";

/*----
Extract the root file data and move in the space.
------*/
rel_file_space(kfb)
KCSIO_BLOCK *kfb;
{
	struct stat st;
	long records;

	stat(kfb->_sys_name,&st);
	records = st.st_size/kfb->_record_len;
	kfb->_space = records;
	return((int) st.st_size % kfb->_record_len);
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
rel_open_shared(kfb)
KCSIO_BLOCK *kfb;
{
	rel_open_io(kfb);
}
rel_open_input(kfb)
KCSIO_BLOCK *kfb;
{
	do_open(kfb,O_RDONLY);
}
rel_open_io(kfb)
KCSIO_BLOCK *kfb;
{
	do_open(kfb,O_RDWR);
}
/*----
Doopen should not be used for open output
------*/
static do_open(kfb,mode)
int mode;
KCSIO_BLOCK *kfb;
{
	errno = 0;
	kfb->_io_channel = open(kfb->_sys_name,mode);
	kfb->_status = e_trans(errno);
	kfb->_last_io_key = 0;
	kfb->_open_status = 1;
	kfb->_pos = lseek(kfb->_io_channel,0L,0);
	kfb->_rel_key = 0;
	rel_file_info(kfb);
}
/*----
A relative open output.
------*/
rel_open_output(kfb)
KCSIO_BLOCK *kfb;
{

	errno = 0;
	kfb->_io_channel = open(kfb->_sys_name,
			        O_WRONLY|O_CREAT|O_TRUNC,
			        0666);
	kfb->_space = 0;
	kfb->_status = e_trans(errno);
	if(errno)
		return;
	kfb->_last_io_key = 0;
	kfb->_open_status = 1;
	kfb->_pos = lseek(kfb->_io_channel,0L,0);
	kfb->_rel_key = 0;
	rel_file_info(kfb);
}

rel_close(kfb)
KCSIO_BLOCK *kfb;
{

	kfb->_status = errno = 0;
	if(kfb->_open_status == 0)
		return;
	close(kfb->_io_channel);
	kfb->_status = e_trans(errno);
	kfb->_open_status = 0;
	rel_file_space(kfb);
}
/*----
               <<<<<<<<  READ HOLD  >>>>>>>>>>>
------*/
/*----
A read record request assumes the primary key.
------*/
rel_read(kfb)
KCSIO_BLOCK *kfb;
{
	kfb->_io_key = 0;
	rel_read_keyed(kfb);
}

rel_hold(kfb)
KCSIO_BLOCK *kfb;
{
	kfb->_io_key = 0;
	rel_hold_keyed(kfb);
}

/*----
Skips null records. null record is all null for record length
------*/
rel_read_next(kfb)
KCSIO_BLOCK *kfb;
{
	int rc;

	errno = 0;
	rc = kfb->_record_len;
	while(rc > 0)
		{
		kfb->_pos = lseek(kfb->_io_channel,0L,1);
		rc = read(kfb->_io_channel,kfb->_record,kfb->_record_len);
		kfb->_rel_key = (kfb->_pos / kfb->_record_len) + 1;
		if(isrec(kfb->_record,kfb->_record_len))
			break;
		}
	if(rc == 0)
		kfb->_status = EENDFILE;
	else
		kfb->_status = e_trans(errno);
}
rel_read_previous(kfb)
KCSIO_BLOCK *kfb;
{
	int rc;

	errno = 0;
	if(kfb->_rel_key < 2)
		kfb->_status = EENDFILE;
	else
		{
		--kfb->_rel_key;
		rel_read_keyed(kfb);
		}
		
	
}

/*----
Returns true if memory is not nulls for the specified length.
------*/
static isrec(rec,len)
char *rec;
int len;
{
	while(len--)
		{
		if(*rec)
			return(1);
		++rec;
		}
	return(0);
}

/*----
And its complement.
------*/
static isnull(rec,len)
char *rec;
int len;
{
	return(!(isrec(rec,len)));
}

/*----
No holding logic for relative files.
------*/
rel_hold_next(kfb)
KCSIO_BLOCK *kfb;
{
	rel_read_next(kfb);
}


rel_read_keyed(kfb)
KCSIO_BLOCK *kfb;
{

	if( rel_key_not_ok(kfb) )
		return;
	errno = 0;
	rel_key_seek(kfb);
	read(kfb->_io_channel,kfb->_record,kfb->_record_len);
	if(isnull(kfb->_record,kfb->_record_len))
		kfb->_status = ENOREC;
	else
		kfb->_status = e_trans(errno);
}

/*----
Seek to the specified relative key.
------*/
static rel_key_seek(kfb)
KCSIO_BLOCK *kfb;
{
	long seekto;

	seekto = ( kfb->_rel_key - 1 ) * kfb->_record_len;
	kfb->_pos = lseek(kfb->_io_channel,seekto,0);
}


/*----
Returns true if the relative key lies within the existing file.
------*/
static rel_key_ok(kfb)
KCSIO_BLOCK *kfb;
{
	rel_file_space(kfb);
	if( (kfb->_rel_key < 1) ||
	    (kfb->_rel_key > kfb->_space) )
		{
		return(0);
		}
	return(1);
}

/*----
Not quite a complement as this also sets the status for return.
------*/
static rel_key_not_ok(kfb)
KCSIO_BLOCK *kfb;
{
	if(rel_key_ok(kfb))
		return(0);
	kfb->_status = ENOREC;
	return(1);
}

/*----
No holding logic for relative files.
------*/
rel_hold_keyed(kfb)
KCSIO_BLOCK *kfb;
{
	rel_read_keyed(kfb);
}

/*----
			<<<< STARTS >>>>
------*/
/*----
Starts are illegal on relative files.
------*/
rel_start_eq(kfb)
KCSIO_BLOCK *kfb;
{
	kfb->_status = EBADARG;
}

rel_start_nlt(kfb)
KCSIO_BLOCK *kfb;
{
	kfb->_status = EBADARG;
}

rel_start_gt(kfb)
KCSIO_BLOCK *kfb;
{
	kfb->_status = EBADARG;
}
rel_start_eq_keyed(kfb)
KCSIO_BLOCK *kfb;
{
	kfb->_status = EBADARG;
}

rel_start_nlt_keyed(kfb)
KCSIO_BLOCK *kfb;
{
	kfb->_status = EBADARG;
}

rel_start_gt_keyed(kfb)
KCSIO_BLOCK *kfb;
{
	kfb->_status = EBADARG;
}

rel_start_last(kfb)
KCSIO_BLOCK *kfb;
{
	rel_file_space(kfb);
	kfb->_rel_key = kfb->_space + 1;
}

/*----
Executes a start and sets up a new _last_io_key.
------*/
static do_start(kfb,mode)
KCSIO_BLOCK *kfb;
int mode;
{
	kfb->_status = EBADARG;
}

/*----
               <<<<<<<< WRITE REWRITE DELETE  >>>>>>>>>>>>
------*/
/*----
Write, rewrite and delete.
1.	Write extends the file the required amount if needed and
	writes. If write is on an existing record, it is overwritten.
2.	Rewrites only work on existing records.
3.	Deletes only work on existing records.
------*/
static char trex[2048];
rel_write(kfb)
KCSIO_BLOCK *kfb;
{
	long newsize;

/* 
 * If we are in the file then seek to the spot, otherwise move
 * to the logical end of file and start writing nulls until we
 * hit out spot.
*/
	if(rel_key_ok(kfb))
		{
		rel_key_seek(kfb);
		read(kfb->_io_channel,trex,kfb->_record_len);
		rel_key_seek(kfb);
		if(isrec(trex,kfb->_record_len))
			{
			kfb->_status = EDUPL;
			return;
			}
		}
	else
/* If we only want the next one, then this will work too */
	if(kfb->_rel_key == (kfb->_space + 1) )
		{
		rel_key_seek(kfb);
		}
	else
		{
		memset(trex,0,2048);
		newsize = kfb->_space + 1;
		while(newsize < kfb->_rel_key)
			{
			write(kfb->_io_channel,trex,kfb->_record_len);
			++newsize;
			}
		rel_file_space(kfb);
		rel_key_seek(kfb);
		}
	errno = 0;
	write(kfb->_io_channel,kfb->_record,kfb->_record_len);
	kfb->_status = e_trans(errno);
	rel_file_space(kfb);
}

rel_rewrite(kfb)
KCSIO_BLOCK *kfb;
{
	if(rel_key_not_ok(kfb))
		return;
	errno = 0;
	rel_key_seek(kfb);
	write(kfb->_io_channel, kfb->_record, kfb->_record_len);
	kfb->_status = e_trans(errno);
	rel_file_space(kfb);
}

rel_delete(kfb)
KCSIO_BLOCK *kfb;
{
	if(rel_key_not_ok(kfb))
		return;
	errno = 0;
	rel_key_seek(kfb);
	memset(kfb->_record,0,kfb->_record_len);
	write(kfb->_io_channel, kfb->_record, kfb->_record_len);
/*
 * Re-seek to where we were
 */
	lseek(kfb->_io_channel,kfb->_pos,0);
	kfb->_status = e_trans(errno);
	rel_file_space(kfb);
}

/*----
No unlocking needed.
------*/
rel_unlock(kfb)
KCSIO_BLOCK *kfb;
{
/*	kfb->_status = 0; */
}

/*----
The rel_file_info() routine pulls the data on an open file and
sets up the c structure.
------*/
rel_file_info(kfb)
KCSIO_BLOCK *kfb;
{

	kfb->_altkey_count = 0;

	clear_keys(kfb);

	if(!(kfb->_record_len))
		kfb->_record_len = 1;
/*
 * rel_file_space returns 0 if byte size of file is evenly divisible by
 * record_length.
 * Compare record length for even divisibility
 * into the actual file length
 * If the division didn't work force record len to 1 .
 */

	if(rel_file_space(kfb))
		kfb->_record_len = 1;
}
/*
Commented out by ZR 03/14/95 as per MJB.  This code moved into the
vscio.c module.

e_trans(code)
int code;
{
	if(code == 0)
		return(0);
	return(EBADF);
}
*/

/*
**	History:
**	$Log: relio.c,v $
**	Revision 1.4  1996-09-17 19:34:15-04  gsl
**	drcs update
**
**
**
*/
