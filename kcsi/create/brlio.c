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

#ifdef WIN32
#include <io.h>
#endif

#include <errno.h>
#include "kcsio.h"
#include "kcsifunc.h"

#ifndef O_BINARY
#define O_BINARY 0
#endif

static char sccsid[]="@(#)brlio.c	1.1 3/19/94";

static void do_open(KCSIO_BLOCK *kfb,int mode);
static int isrec(const char *rec,int len);
static int isnull(const char *rec,int len);
static void brel_key_seek(KCSIO_BLOCK *kfb);
static int brel_key_ok(KCSIO_BLOCK *kfb);
static int brel_key_not_ok(KCSIO_BLOCK *kfb);
static void do_start(KCSIO_BLOCK *kfb,int mode);
static void brel_commit(int fh);


/*----
Extract the root file data and move in the space.
------*/
int brel_file_space(KCSIO_BLOCK *kfb)
{
	struct stat st;

	if (0 != kfb->_status)
	{
		return 0;
	}

	if (-1 == stat(kfb->_sys_name,&st))
	{
		return 0;
	}
	else
	{
		kfb->_space = st.st_size/kfb->_record_len;
		return((int) st.st_size % kfb->_record_len);
	}
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
	do_open(kfb,O_RDWR|O_BINARY);
}
void brel_open_input(KCSIO_BLOCK *kfb)
{
	do_open(kfb,O_RDONLY|O_BINARY);
}
void brel_open_io(KCSIO_BLOCK *kfb)
{
	do_open(kfb,O_RDWR|O_BINARY);
}
/*----
Doopen should not be used for open output
------*/
static void do_open(KCSIO_BLOCK *kfb,int mode)
{
	kfb->_io_channel = open(kfb->_sys_name,mode);
	if (-1 == kfb->_io_channel)
	{
		kfb->_status = EBADF;
	}
	else
	{
		kfb->_status = 0;
	}
	kfb->_last_io_key = 0;
	kfb->_open_status = 1;
	kfb->_pos = lseek(kfb->_io_channel,0L,SEEK_SET);
	kfb->_rel_key = 0;
	kfb->_space = 0;
	brel_file_info(kfb);
}
/*----
A relative open output.
------*/
void brel_open_output(KCSIO_BLOCK *kfb)
{
	kfb->_io_channel = open(kfb->_sys_name,
			        O_WRONLY|O_CREAT|O_TRUNC|O_BINARY,
			        0666);
	kfb->_space = 0;
	if (-1 == kfb->_io_channel)
	{
		kfb->_status = EBADF;
		return;
	}
	kfb->_status = 0;

	kfb->_last_io_key = 0;
	kfb->_open_status = 1;
	kfb->_pos = lseek(kfb->_io_channel,0L,SEEK_SET);
	kfb->_rel_key = 0;
	brel_file_info(kfb);
}

void brel_close(KCSIO_BLOCK *kfb)
{

	kfb->_status = 0;
	if(kfb->_open_status == 0)
		return;

	if (-1 == close(kfb->_io_channel))
	{
		kfb->_status = EBADF;
	}
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

	rc = kfb->_record_len;
	while(rc > 0)
	{
		kfb->_pos = lseek(kfb->_io_channel,0L,SEEK_CUR);
		rc = read(kfb->_io_channel,kfb->_record,kfb->_record_len);
		kfb->_rel_key = (kfb->_pos / kfb->_record_len) + 1;
		if(isrec(kfb->_record,kfb->_record_len))
			break;
	}
	if(rc == 0)
		kfb->_status = EENDFILE;
	else /* rc == -1 */
		kfb->_status = EBADF;
}
void brel_read_previous(KCSIO_BLOCK *kfb)
{
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
static int isrec(const char *rec,int len)
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
static int isnull(const char *rec,int len)
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

	brel_key_seek(kfb);
	if (-1 == read(kfb->_io_channel,kfb->_record,kfb->_record_len))
		kfb->_status = EBADF;
	else if (isnull(kfb->_record,kfb->_record_len))
		kfb->_status = ENOREC;
	else
		kfb->_status = 0;
}

/*----
Seek to the specified relative key.
------*/
static void brel_key_seek(KCSIO_BLOCK *kfb)
{
	long seekto;

	seekto = ( kfb->_rel_key - 1 ) * kfb->_record_len;
	kfb->_pos = lseek(kfb->_io_channel,seekto,SEEK_SET);
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
	/* If we only want the next one, then this will work too */
	else if(kfb->_rel_key == (kfb->_space + 1) )
	{
		brel_key_seek(kfb);
	}
	else
	{
		memset(trex,'\0',2048);
		newsize = kfb->_space + 1;
		while(newsize < kfb->_rel_key)
		{
			if (-1 == write(kfb->_io_channel,trex,kfb->_record_len))
			{
				kfb->_status = EBADF;
				return;
			}
			++newsize;
		}

		brel_commit(kfb->_io_channel);
		brel_file_space(kfb);
		brel_key_seek(kfb);
	}

	if (-1 == write(kfb->_io_channel,kfb->_record,kfb->_record_len))
	{
		kfb->_status = EBADF;
		return;
	}
	kfb->_status = 0;

	brel_commit(kfb->_io_channel);
	brel_file_space(kfb);
}

void brel_rewrite(KCSIO_BLOCK *kfb)
{
	if(brel_key_not_ok(kfb))
		return;

	brel_key_seek(kfb);
	if (-1 == write(kfb->_io_channel, kfb->_record, kfb->_record_len))
	{
		kfb->_status = EBADF;
		return;
	}
	kfb->_status = 0;

	brel_commit(kfb->_io_channel);
	brel_file_space(kfb);
}

void brel_delete(KCSIO_BLOCK *kfb)
{
	if(brel_key_not_ok(kfb))
		return;

	brel_key_seek(kfb);
	memset(kfb->_record,0,kfb->_record_len);

	/*
	 * Write then Re-seek to where we were
	 */	
	if (-1 == write(kfb->_io_channel, kfb->_record, kfb->_record_len))
	{
		kfb->_status = EBADF;
		return;
	}
	brel_commit(kfb->_io_channel);
	if (-1 == lseek(kfb->_io_channel,kfb->_pos,SEEK_SET))
	{
		kfb->_status = EBADF;
		return;
	}

	kfb->_status = 0;
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

/*----
Call commit following a write() to ensure it has actually been written to
disk before calling stat() to get a size.
------*/
static void brel_commit(int fh)
{
#ifdef WIN32
	_commit(fh);
#endif	
}

/*
**	History:
**	$Log: brlio.c,v $
**	Revision 1.7  2001-11-15 19:17:54-05  gsl
**	Add error checking on stat()
**	Add commit() after write() for WIN32
**
**	Revision 1.5  1996-09-17 19:34:01-04  gsl
**	drcs update
**
**
**
*/
