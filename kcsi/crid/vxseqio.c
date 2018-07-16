static char copyright[]="Copyright (c) 1988-1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
/*---
vxseqio.c

IO for sequential files.
Stream files with newline terminators

Very little is legal for a stream file, basically open close
read and write.

------*/
#include <stdio.h>
#include <ctype.h>
/* #include <errno.h> */
#include rms
#include "vax_rms.h"
#include "vaxiocod.h"
#include "kcsio.h"

static char sccsid[]="@(#)vxseqio.c	1.1 1/30/93";


extern int   f_errno;

/*----
            <<<<<<<<<  OPEN AND CLOSE ROUTINES   >>>>>>>>>>>
------*/

seq_open_shared(kfb)
KCSIO_BLOCK *kfb;
{
seq_open_io(kfb);
}
seq_open_input(kfb)
KCSIO_BLOCK *kfb;
{
	do_open(kfb,"r");
}

seq_open_io(kfb)
KCSIO_BLOCK *kfb;
{

do_open(kfb,"a+");

}
/*----
Doopen should not be used for open output
------*/
static do_open(kfb,mode)
char *mode;
KCSIO_BLOCK *kfb;
{
	f_errno = 0;
	kfb->_io_vector = (char *) fopen(kfb->_sys_name,mode);
	kfb->_status = e_trans(f_errno);
	kfb->_last_io_key = 0;
	kfb->_open_status = 1;
	kfb->_pos = ftell((FILE*)kfb->_io_vector);
	kfb->_rel_key = 0;
	seq_file_info(kfb);
}
/*----
A sequential open output.
------*/
seq_open_output(kfb)
KCSIO_BLOCK *kfb;
{
	f_errno = 0;
	kfb->_io_vector = (char *) fopen(kfb->_sys_name,"w");
	kfb->_status = e_trans(f_errno);
	if(f_errno)
		return;
	kfb->_last_io_key = 0;
	kfb->_open_status = 1;
	kfb->_pos = fseek((FILE*)kfb->_io_vector,0L,0);
	kfb->_rel_key = 0;
	seq_file_info(kfb);
}

seq_close(kfb)
KCSIO_BLOCK *kfb;
{

	kfb->_status = f_errno = 0;
	if(kfb->_open_status == 0)
		return;
	fclose((FILE *) kfb->_io_vector);
	kfb->_status = e_trans(f_errno);
	kfb->_open_status = 0;
	seq_file_space(kfb);
}
/*----
               <<<<<<<<  READ HOLD  >>>>>>>>>>>
------*/
/*----
A read record request assumes the primary key.
------*/
seq_read(kfb)
KCSIO_BLOCK *kfb;
{
	kfb->_status = EBADARG;
}

seq_hold(kfb)
KCSIO_BLOCK *kfb;
{
	kfb->_status = EBADARG;
}

/*----
Skips null records. null record is all null for record length
------*/
seq_read_next(kfb)
KCSIO_BLOCK *kfb;
{
	int len;

	f_errno = 0;
	if(!(fgets(kfb->_record,kfb->_record_len + 3,(FILE *) kfb->_io_vector)))
		{
		kfb->_status = EENDFILE;
		return;
		}
	while(len = strlen(kfb->_record)) 
		{
		--len;
		if(kfb->_record[len] == '\r')
			{
			kfb->_record[len] = 0;
			continue;
			}
		if(kfb->_record[len] == '\n')
			{
			kfb->_record[len] = 0;
			continue;
			}
		break;
		}
	kfb->_status = 0;

}

seq_read_previous(kfb)
KCSIO_BLOCK *kfb;
{
	kfb->_status = EBADARG;
}
/*----
No holding logic for sequential files.
------*/
seq_hold_next(kfb)
KCSIO_BLOCK *kfb;
{
	seq_read_next(kfb);
}


seq_read_keyed(kfb)
KCSIO_BLOCK *kfb;
{
	kfb->_status = EBADARG;
}
seq_start_last(kfb)
KCSIO_BLOCK *kfb;
{
	kfb->_status = EBADARG;
}


/*----
No holding logic for sequential files.
------*/
seq_hold_keyed(kfb)
KCSIO_BLOCK *kfb;
{
	seq_read_keyed(kfb);
}

/*----
			<<<< STARTS >>>>
------*/
/*----
Starts are illegal on sequential files.
------*/
seq_start_eq(kfb)
KCSIO_BLOCK *kfb;
{
	kfb->_status = EBADARG;
}

seq_start_nlt(kfb)
KCSIO_BLOCK *kfb;
{
	kfb->_status = EBADARG;
}

seq_start_gt(kfb)
KCSIO_BLOCK *kfb;
{
	kfb->_status = EBADARG;
}
seq_start_eq_keyed(kfb)
KCSIO_BLOCK *kfb;
{
	kfb->_status = EBADARG;
}

seq_start_nlt_keyed(kfb)
KCSIO_BLOCK *kfb;
{
	kfb->_status = EBADARG;
}

seq_start_gt_keyed(kfb)
KCSIO_BLOCK *kfb;
{
	kfb->_status = EBADARG;
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
1.	Write extends the file at the end.
2.	Rewrites illegal.
3.	Deletes illegal.
------*/
seq_write(kfb)
KCSIO_BLOCK *kfb;
{
	long pos;

/*
 * save the current file pointer
	pos = fseek((FILE*)kfb->_io_vector,0L,1);
 */

/*
 * Writes are automatically done at the end of the record because
 * of the "a+" or "w" open mode.
 */

	f_errno = 0;
	fputs(kfb->_record,(FILE *)kfb->_io_vector);
/* The VAX fputs already appends the newline charactr */
/*	fputs("\n",(FILE *)kfb->_io_vector); */
	kfb->_status = e_trans(f_errno);
/*
 * Then let us reposition.
	fseek((FILE*)kfb->_io_vector,pos,0);
 */
}

seq_rewrite(kfb)
KCSIO_BLOCK *kfb;
{
	kfb->_status = EBADARG;
}

seq_delete(kfb)
KCSIO_BLOCK *kfb;
{
	kfb->_status = EBADARG;
}

/*----
No unlocking needed.
------*/
seq_unlock(kfb)
KCSIO_BLOCK *kfb;
{
/*	kfb->_status = 0; */
}

/*----
The seq_file_info() routine pulls the data on an open file and
sets up the c structure.
------*/
seq_file_info(kfb)
KCSIO_BLOCK *kfb;
{

	kfb->_altkey_count = 0;

	clear_keys(kfb);

	if(!(kfb->_record_len))
		kfb->_record_len = 1;
}

/*----
SEQ_FILE_SPACE()
Just a dummy to allow a link to vscio
----*/
seq_file_space(kfb)
   KCSIO_BLOCK *kfb;
{
kfb->_space = 1;
}
/*
**	History:
**	$Log: vxseqio.c,v $
**	Revision 1.2  1996-09-17 19:45:54-04  gsl
**	drcs update
**
**
**
*/
