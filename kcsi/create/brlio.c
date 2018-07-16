static char copyright[]="Copyright (c) 1988-1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
/*---
brlio.c

IO for Acucobol relative, and all binary sequential files.

cloned from relio.c

------*/
#include <stdio.h>
#include <ctype.h>

#ifdef	KCSI_VAX
#include <types.h>
#include <stat.h>
#include <file.h>
#else
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#endif

#include <errno.h>
#include "kcsio.h"
#include "kcsifunc.h"

static char sccsid[]="@(#)brlio.c	1.1 3/19/94";

static void do_open(KCSIO_BLOCK *kfb,int mode);
static int isrec(char *rec,int len);
static void brel_key_seek(KCSIO_BLOCK *kfb);
static int brel_key_ok(KCSIO_BLOCK *kfb);
static int brel_key_not_ok(KCSIO_BLOCK *kfb);
static void do_start(KCSIO_BLOCK *kfb,int mode);


/*----
Extract the root file data and move in the space.
------*/
int brel_file_space(KCSIO_BLOCK *kfb)
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
void brel_open_shared(KCSIO_BLOCK *kfb)
{
	brel_open_io(kfb);
}
void brel_open_input(KCSIO_BLOCK *kfb)
{
	do_open(kfb,O_RDONLY);
}
void brel_open_io(KCSIO_BLOCK *kfb)
{
	do_open(kfb,O_RDWR);
}
/*----
Doopen should not be used for open output
------*/
static void do_open(KCSIO_BLOCK *kfb,int mode)
{
	errno = 0;
	kfb->_io_channel = open(kfb->_sys_name,mode);
	kfb->_status = e_trans(errno);
	kfb->_last_io_key = 0;
	kfb->_open_status = 1;
	kfb->_pos = lseek(kfb->_io_channel,0L,0);
	kfb->_rel_key = 0;
	brel_file_info(kfb);
}
/*----
A relative open output.
------*/
void brel_open_output(KCSIO_BLOCK *kfb)
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
	brel_file_info(kfb);
}

void brel_close(KCSIO_BLOCK *kfb)
{

	kfb->_status = errno = 0;
	if(kfb->_open_status == 0)
		return;
	close(kfb->_io_channel);
	kfb->_status = e_trans(errno);
	kfb->_open_status = 0;
	brel_file_space(kfb);
}
/*----
               <<<<<<<<  READ HOLD  >>>>>>>>>>>
------*/
/*----
A read record request assumes the primary key.
------*/
void brel_read(KCSIO_BLOCK *kfb)
{
	kfb->_io_key = 0;
	brel_read_keyed(kfb);
}

void brel_hold(KCSIO_BLOCK *kfb)
{
	kfb->_io_key = 0;
	brel_hold_keyed(kfb);
}

/*----
Skips null records. null record is all null for record length
------*/
void brel_read_next(KCSIO_BLOCK *kfb)
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
void brel_read_previous(KCSIO_BLOCK *kfb)
{

	errno = 0;
	if(kfb->_rel_key < 2)
		kfb->_status = EENDFILE;
	else
		{
		--kfb->_rel_key;
		brel_read_keyed(kfb);
		}
		
	
}

/*----
Returns true if memory is not nulls for the specified length.
------*/
static int isrec(char *rec,int len)
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
void brel_hold_next(KCSIO_BLOCK *kfb)
{
	brel_read_next(kfb);
}


void brel_read_keyed(KCSIO_BLOCK *kfb)
{

	if( brel_key_not_ok(kfb) )
		return;
	errno = 0;
	brel_key_seek(kfb);
	read(kfb->_io_channel,kfb->_record,kfb->_record_len);
	if(isnull(kfb->_record,kfb->_record_len))
		kfb->_status = ENOREC;
	else
		kfb->_status = e_trans(errno);
}

/*----
Seek to the specified relative key.
------*/
static void brel_key_seek(KCSIO_BLOCK *kfb)
{
	long seekto;

	seekto = ( kfb->_rel_key - 1 ) * kfb->_record_len;
	kfb->_pos = lseek(kfb->_io_channel,seekto,0);
}


/*----
Returns true if the relative key lies within the existing file.
------*/
static int brel_key_ok(KCSIO_BLOCK *kfb)
{
	brel_file_space(kfb);
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
static int brel_key_not_ok(KCSIO_BLOCK *kfb)
{
	if(brel_key_ok(kfb))
		return(0);
	kfb->_status = ENOREC;
	return(1);
}

/*----
No holding logic for relative files.
------*/
void brel_hold_keyed(KCSIO_BLOCK *kfb)
{
	brel_read_keyed(kfb);
}

/*----
			<<<< STARTS >>>>
------*/
/*----
Starts are illegal on relative files.
------*/
void brel_start_eq(KCSIO_BLOCK *kfb)
{
	kfb->_status = EBADARG;
}

void brel_start_nlt(KCSIO_BLOCK *kfb)
{
	kfb->_status = EBADARG;
}

void brel_start_gt(KCSIO_BLOCK *kfb)
{
	kfb->_status = EBADARG;
}
void brel_start_eq_keyed(KCSIO_BLOCK *kfb)
{
	kfb->_status = EBADARG;
}

void brel_start_nlt_keyed(KCSIO_BLOCK *kfb)
{
	kfb->_status = EBADARG;
}

void brel_start_gt_keyed(KCSIO_BLOCK *kfb)
{
	kfb->_status = EBADARG;
}

void brel_start_last(KCSIO_BLOCK *kfb)
{
	brel_file_space(kfb);
	kfb->_rel_key = kfb->_space + 1;
}

/*----
Executes a start and sets up a new _last_io_key.
------*/
static void do_start(KCSIO_BLOCK *kfb,int mode)
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
void brel_write(KCSIO_BLOCK *kfb)
{
	long newsize;

/* 
 * If we are in the file then seek to the spot, otherwise move
 * to the logical end of file and start writing nulls until we
 * hit out spot.
*/
	if(brel_key_ok(kfb))
		{
		brel_key_seek(kfb);
		read(kfb->_io_channel,trex,kfb->_record_len);
		brel_key_seek(kfb);
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
		brel_key_seek(kfb);
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
		brel_file_space(kfb);
		brel_key_seek(kfb);
		}
	errno = 0;
	write(kfb->_io_channel,kfb->_record,kfb->_record_len);
	kfb->_status = e_trans(errno);
	brel_file_space(kfb);
}

void brel_rewrite(KCSIO_BLOCK *kfb)
{
	if(brel_key_not_ok(kfb))
		return;
	errno = 0;
	brel_key_seek(kfb);
	write(kfb->_io_channel, kfb->_record, kfb->_record_len);
	kfb->_status = e_trans(errno);
	brel_file_space(kfb);
}

void brel_delete(KCSIO_BLOCK *kfb)
{
	if(brel_key_not_ok(kfb))
		return;
	errno = 0;
	brel_key_seek(kfb);
	memset(kfb->_record,0,kfb->_record_len);
	write(kfb->_io_channel, kfb->_record, kfb->_record_len);
/*
 * Re-seek to where we were
 */
	lseek(kfb->_io_channel,kfb->_pos,0);
	kfb->_status = e_trans(errno);
	brel_file_space(kfb);
}

/*----
No unlocking needed.
------*/
void brel_unlock(KCSIO_BLOCK *kfb)
{
/*	kfb->_status = 0; */
}

/*----
The brel_file_info() routine pulls the data on an open file and
sets up the c structure.
------*/
void brel_file_info(KCSIO_BLOCK *kfb)
{

	kfb->_altkey_count = 0;

	clear_keys(kfb);

	if(!(kfb->_record_len))
		kfb->_record_len = 1;
/*
 * brel_file_space returns 0 if byte size of file is evenly divisible by
 * record_length.
 * Compare record length for even divisibility
 * into the actual file length
 * If the division didn't work force record len to 1 .
 */

	if(brel_file_space(kfb))
		kfb->_record_len = 1;
}
/*
**	History:
**	$Log: brlio.c,v $
**	Revision 1.5  1996-09-17 19:34:01-04  gsl
**	drcs update
**
**
**
*/
