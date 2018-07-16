static char copyright[]="Copyright (c) 1988-1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
/*---
rlaio.c

IO for binary relative files. Relative files are binary files processed
by open, read and write logic. Each record contains a final 0x0a byte.
This module only works with fixed length relative files. 

This record is 28 bytes long0x0a

Records that are missing contain all nulls. For example if records
1 and 3 were written, then record 2 would contain

28 nulls follwed by 0x00

Records are deleted (in MF COBOL) by writing zero to the 0x0a byte.
The record is not nulled out. However for this implementation,
deleted records will be nulled out.

------*/
#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include "kcsio.h"
#include "kcsifunc.h"

#ifndef O_BINARY
#define O_BINARY 0
#endif

static char sccsid[]="@(#)rlaio.c	1.5 3/19/94";

#define	RECORD_LEN	(kfb->_record_len)
#define	ACTUAL_LEN	(RECORD_LEN + 1)


static void do_open(KCSIO_BLOCK *kfb, int mode);
static int isrec(const char* rec,int len);
static void rel_key_seek(KCSIO_BLOCK *kfb);
static int rel_key_not_ok(KCSIO_BLOCK *kfb);
static void rel_commit(int fh);


/*----
Extract the root file data and move in the space.
------*/
int rel_file_space(KCSIO_BLOCK *kfb)
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
		kfb->_space = st.st_size/ACTUAL_LEN;
		return((int) st.st_size % ACTUAL_LEN);
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
void rel_open_shared(KCSIO_BLOCK *kfb)
{
	rel_open_io(kfb);
}
void rel_open_input(KCSIO_BLOCK *kfb)
{
	do_open(kfb,O_RDONLY | O_BINARY);
}
void rel_open_io(KCSIO_BLOCK *kfb)
{
	do_open(kfb,O_RDWR | O_BINARY);
}
/*----
Doopen should not be used for open output
------*/
static void do_open(KCSIO_BLOCK *kfb, int mode)
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
	rel_file_info(kfb);
}
/*----
A relative open output.
------*/
void rel_open_output(KCSIO_BLOCK *kfb)
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
	rel_file_info(kfb);
}

void rel_close(KCSIO_BLOCK *kfb)
{
	kfb->_status  = 0;
	if(kfb->_open_status == 0)
		return;

	if (-1 == close(kfb->_io_channel))
	{
		kfb->_status = EBADF;
	}
	kfb->_open_status = 0;
	rel_file_space(kfb);
}
/*----
               <<<<<<<<  READ HOLD  >>>>>>>>>>>
------*/
/*----
A read record request assumes the primary key.
------*/
void rel_read(KCSIO_BLOCK *kfb)
{
	kfb->_io_key = 0;
	rel_read_keyed(kfb);
}

void rel_hold(KCSIO_BLOCK *kfb)
{
	kfb->_io_key = 0;
	rel_hold_keyed(kfb);
}

/*----
Skips null records. null record is all null for record length
------*/
void rel_read_next(KCSIO_BLOCK *kfb)
{
	int rc;
	char rlen[1];

	while(1)
	{
		kfb->_pos = lseek(kfb->_io_channel,0L,SEEK_CUR);
		if(-1 == kfb->_pos)
		{
			kfb->_status = EBADF;
			return;
		}
		rc = read(kfb->_io_channel,kfb->_record,RECORD_LEN);
		if(0 == rc)
		{
			kfb->_status = EENDFILE;
			return;
		}
		if(rc != RECORD_LEN)
		{
			kfb->_status = EBADF;
			return;
		}
		rc = read(kfb->_io_channel,rlen,1);
		if(rc != 1)
		{
			kfb->_status = EBADF;
			return;
		}
		kfb->_rel_key = (kfb->_pos / ACTUAL_LEN) + 1;
		if(isrec(rlen,1))
		{
			kfb->_status = 0;
			return;
		}
	}
}

void rel_read_previous(KCSIO_BLOCK *kfb)
{
	while(1)
	{
		if(kfb->_rel_key <= 1)
		{
			kfb->_status = EENDFILE;
			return;
		}

		--kfb->_rel_key;
		rel_read_keyed(kfb);

		if (0 == kfb->_status)
		{
			return; /* Success */
		}

		if (ENOREC != kfb->_status)
		{
			return; /* Error */
		}
	}
}

/*----
Returns true if memory is not nulls for the specified length.
------*/
static int isrec(const char* rec,int len)
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
static int isnull(const char *rec, int len)
{
	return(!(isrec(rec,len)));
}

/*----
No holding logic for relative files.
------*/
void rel_hold_next(KCSIO_BLOCK *kfb)
{
	rel_read_next(kfb);
}


void rel_read_keyed(KCSIO_BLOCK *kfb)
{
	char rlen[2];

	if( rel_key_not_ok(kfb) )
		return;

	rel_key_seek(kfb);
	if (-1 == read(kfb->_io_channel,kfb->_record,RECORD_LEN) ||
	    -1 == read(kfb->_io_channel,rlen,1))
		kfb->_status = EBADF;
	else if(isnull(rlen,1))
		kfb->_status = ENOREC;
	else
		kfb->_status = 0;
}

/*----
Seek to the specified relative key.
------*/
static void rel_key_seek(KCSIO_BLOCK *kfb)
{
	long seekto;

	seekto = ( kfb->_rel_key - 1 ) * ACTUAL_LEN;
	kfb->_pos = lseek(kfb->_io_channel,seekto,SEEK_SET);
}


/*----
Returns true if the relative key lies within the existing file.
------*/
static int rel_key_ok(KCSIO_BLOCK *kfb)
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
static int rel_key_not_ok(KCSIO_BLOCK *kfb)
{
	if(rel_key_ok(kfb))
		return(0);
	kfb->_status = ENOREC;
	return(1);
}

/*----
No holding logic for relative files.
------*/
void rel_hold_keyed(KCSIO_BLOCK *kfb)
{
	rel_read_keyed(kfb);
}

/*----
			<<<< STARTS >>>>
------*/
/*----
Starts are illegal on relative files.
------*/
void rel_start_eq(KCSIO_BLOCK *kfb)
{
	kfb->_status = EBADARG;
}

void rel_start_nlt(KCSIO_BLOCK *kfb)
{
	kfb->_status = EBADARG;
}

void rel_start_gt(KCSIO_BLOCK *kfb)
{
	kfb->_status = EBADARG;
}

void rel_start_eq_keyed(KCSIO_BLOCK *kfb)
{
	kfb->_status = EBADARG;
}

void rel_start_nlt_keyed(KCSIO_BLOCK *kfb)
{
	kfb->_status = EBADARG;
}

void rel_start_gt_keyed(KCSIO_BLOCK *kfb)
{
	kfb->_status = EBADARG;
}

void rel_start_last(KCSIO_BLOCK *kfb)
{
	rel_file_space(kfb);
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

void rel_write(KCSIO_BLOCK *kfb)
{
	long newsize;
	char rlen[2];

/* 
 * If we are in the file then seek to the spot, otherwise move
 * to the logical end of file and start writing nulls until we
 * hit out spot.
*/
	if(rel_key_ok(kfb))
	{
		rel_key_seek(kfb);
		read(kfb->_io_channel,trex,RECORD_LEN);
		read(kfb->_io_channel,rlen,1);
		rel_key_seek(kfb);
		if(isrec(rlen,1))
		{
			kfb->_status = EDUPL;
			return;
		}
	}
/* If we only want the next one, then this will work too */
	else if(kfb->_rel_key == (kfb->_space + 1) )
	{
		rel_key_seek(kfb);
	}
	else
	{
		memset(trex,'\0',2048);
		memset(rlen,'\0',1);
		newsize = kfb->_space + 1;
		while(newsize < kfb->_rel_key)
		{
			if (-1 == write(kfb->_io_channel,trex,RECORD_LEN) ||
			    -1 == write(kfb->_io_channel,rlen,1))
			{
				kfb->_status = EBADF;
				return;
			}

			++newsize;
		}
		rel_commit(kfb->_io_channel);
		rel_file_space(kfb);
		rel_key_seek(kfb);
	}

	rlen[0] = 0x0a;
	if (-1 == write(kfb->_io_channel,kfb->_record,RECORD_LEN) ||
	    -1 == write(kfb->_io_channel,rlen,1))
	{
		kfb->_status = EBADF;
		return;
	}
	kfb->_status = 0;

	rel_commit(kfb->_io_channel);
	rel_file_space(kfb);
}

void rel_rewrite(KCSIO_BLOCK *kfb)
{
	char rlen[2];

	if(rel_key_not_ok(kfb))
		return;

	rel_key_seek(kfb);
	rlen[0] = 0x0a;
	if (-1 == write(kfb->_io_channel,kfb->_record,RECORD_LEN) ||
	    -1 == write(kfb->_io_channel,rlen,1))
	{
		kfb->_status = EBADF;
		return;
	}
	kfb->_status = 0;

	rel_commit(kfb->_io_channel);
	rel_file_space(kfb);
}

void rel_delete(KCSIO_BLOCK *kfb)
{
	char rlen[2];

	if(rel_key_not_ok(kfb))
		return;

	rel_key_seek(kfb);
	memset(kfb->_record,0,RECORD_LEN);
	rlen[0] = 0;
	/*
	 * Write then Re-seek to where we were
	 */	
	if (-1 == write(kfb->_io_channel,kfb->_record,RECORD_LEN) ||
	    -1 == write(kfb->_io_channel,rlen,1))
	{
		kfb->_status = EBADF;
		return;
	}
	rel_commit(kfb->_io_channel);
	if (-1 == lseek(kfb->_io_channel,kfb->_pos,SEEK_SET) )
	{
		kfb->_status = EBADF;
		return;
	}
	kfb->_status = 0;

	rel_file_space(kfb);
}

/*----
No unlocking needed.
------*/
void rel_unlock(KCSIO_BLOCK *kfb)
{
/*	kfb->_status = 0; */
}

/*----
The rel_file_info() routine pulls the data on an open file and
sets up the c structure.
------*/
void rel_file_info(KCSIO_BLOCK *kfb)
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

/*----
Call commit following a write() to ensure it has actually been written to
disk before calling stat() to get a size.
------*/
static void rel_commit(int fh)
{
#ifdef WIN32
	_commit(fh);
#endif	
}

/*
**	History:
**	$Log: rlaio.c,v $
**	Revision 1.9.2.2  2002/08/16 13:58:52  gsl
**	fix read_previous
**	
**	Revision 1.9.2.1  2002/08/15 15:25:24  gsl
**	Relative file patch
**	
**	Revision 1.9  2001/11/20 17:23:16  gsl
**	fix prototype
**	
**	Revision 1.8  2001-11-20 12:20:29-05  gsl
**	Add header
**
**	Revision 1.7  2001-11-15 19:18:57-05  gsl
**	Error check stat()
**	Add commit() after write() for WIN32
**
**	Revision 1.5  1998-10-13 15:15:41-04  gsl
**	Fix logic that deletes a releative record.
**
**	Revision 1.4  1997-10-02 15:45:59-04  gsl
**	fix warnings
**
**	Revision 1.3  1996-09-17 19:34:15-04  gsl
**	drcs update
**
**
**
*/