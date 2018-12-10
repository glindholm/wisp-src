/*
******************************************************************************
**
** KCSI - King Computer Services Inc.
**
** 
******************************************************************************
*/

/*---
seqio.c

IO for sequential files. 
Stream files with newline terminators

Very little is legal for a stream file, basically open close
read and write.

------*/
#include <stdio.h>
#include <ctype.h>
#include <errno.h>
#include "kcsio.h"
#include "kcsifunc.h"


/*----
Space cannot be determined and is always returned as 1.
------*/
int seq_file_space(KCSIO_BLOCK *kfb)
{
	kfb->_space = 1;
	return(0);
}

/*----
            <<<<<<<<<  OPEN AND CLOSE ROUTINES   >>>>>>>>>>>
------*/
/*----
Doopen should not be used for open output
------*/
static void do_open(kfb,mode)
char *mode;
KCSIO_BLOCK *kfb;
{
	kfb->_io_vector = (char *) fopen(kfb->_sys_name,mode);
	if (NULL == kfb->_io_vector)
	{
		kfb->_status = EBADF;
	}
	else
	{
		kfb->_status = 0;
	}
	kfb->_last_io_key = 0;
	kfb->_open_status = 1;
	kfb->_pos = ftell((FILE*)kfb->_io_vector);
	kfb->_rel_key = 0;
	seq_file_info(kfb);
}
/*----
A sequential open output.
------*/
void seq_open_output(KCSIO_BLOCK *kfb)
{
	kfb->_io_vector = (char *) fopen(kfb->_sys_name,"w");
	if (NULL == kfb->_io_vector)
	{
		kfb->_status = EBADF;
		return;
	}
	kfb->_status = 0;

	kfb->_last_io_key = 0;
	kfb->_open_status = 1;
	kfb->_pos = fseek((FILE*)kfb->_io_vector,0L,0);
	kfb->_rel_key = 0;
	seq_file_info(kfb);
}

void seq_close(KCSIO_BLOCK *kfb)
{

	kfb->_status = 0;
	if(kfb->_open_status == 0)
		return;
	if (EOF == fclose((FILE *) kfb->_io_vector))
	{
		kfb->_status = EBADF;
	}
	kfb->_open_status = 0;
	seq_file_space(kfb);
}
/*----
               <<<<<<<<  READ HOLD  >>>>>>>>>>>
------*/
/*----
A read record request assumes the primary key.
------*/
void seq_read(KCSIO_BLOCK *kfb)
{
	kfb->_status = EBADARG;
}

void seq_hold(KCSIO_BLOCK *kfb)
{
	kfb->_status = EBADARG;
}

/*----
Skips null records. null record is all null for record length
------*/
void seq_read_next(KCSIO_BLOCK *kfb)
{
	int len;

	if(!(fgets(kfb->_record,kfb->_record_len + 3,(FILE *) kfb->_io_vector)))
		{
		kfb->_status = EENDFILE;
		return;
		}
	len = strlen(kfb->_record);
	while(	(len)	&&
		(	(kfb->_record[len - 1] == '\r') || 
			(kfb->_record[len - 1] == '\n')	))
		{
		kfb->_record[len - 1] = 0;
		len = strlen(kfb->_record);
		}
	kfb->_status = 0;

}

void seq_read_previous(KCSIO_BLOCK *kfb)
{
	kfb->_status = EBADARG;
}
/*----
No holding logic for sequential files.
------*/
void seq_hold_next(KCSIO_BLOCK *kfb)
{
	seq_read_next(kfb);
}


void seq_read_keyed(KCSIO_BLOCK *kfb)
{
	kfb->_status = EBADARG;
}
void seq_start_last(KCSIO_BLOCK *kfb)
{
	kfb->_status = EBADARG;
}


/*----
No holding logic for sequential files.
------*/
void seq_hold_keyed(KCSIO_BLOCK *kfb)
{
	seq_read_keyed(kfb);
}

/*----
			<<<< STARTS >>>>
------*/
/*----
Starts are illegal on sequential files.
------*/
void seq_start_eq(KCSIO_BLOCK *kfb)
{
	kfb->_status = EBADARG;
}

void seq_start_nlt(KCSIO_BLOCK *kfb)
{
	kfb->_status = EBADARG;
}

void seq_start_gt(KCSIO_BLOCK *kfb)
{
	kfb->_status = EBADARG;
}
void seq_start_eq_keyed(KCSIO_BLOCK *kfb)
{
	kfb->_status = EBADARG;
}

void seq_start_nlt_keyed(KCSIO_BLOCK *kfb)
{
	kfb->_status = EBADARG;
}

void seq_start_gt_keyed(KCSIO_BLOCK *kfb)
{
	kfb->_status = EBADARG;
}

/*----
Executes a start and sets up a new _last_io_key.
------*/
#ifdef NOT_USED
static void do_start(KCSIO_BLOCK *kfb,int mode)
{
	kfb->_status = EBADARG;
}
#endif /* NOT_USED */

/*----
               <<<<<<<< WRITE REWRITE DELETE  >>>>>>>>>>>>
------*/
/*----
Write, rewrite and delete.
1.	Write extends the file at the end.
2.	Rewrites illegal.
3.	Deletes illegal.
------*/
void seq_write(KCSIO_BLOCK *kfb)
{

/*
 * save the current file pointer
	long pos;
	pos = fseek((FILE*)kfb->_io_vector,0L,1);
 */

/*
 * Writes are automatically done at the end of the record because
 * of the "a+" or "w" open mode.
 */

/*
 * Because of ambiguity in whether fputs attaches a \n or not,
 * we use fprintf here to write
 */
	if (0 > fprintf((FILE*)kfb->_io_vector,"%s\n",kfb->_record) )
	{
		kfb->_status = EBADF;
	}
	else
	{
		kfb->_status = 0;
	}

/* fputs() attaches a new line
	fputs(kfb->_record,(FILE *)kfb->_io_vector);
	fputs("\n",(FILE *)kfb->_io_vector);
*/
/*
 * Then let us reposition.
	fseek((FILE*)kfb->_io_vector,pos,0);
 */
}

void seq_rewrite(KCSIO_BLOCK *kfb)
{
	kfb->_status = EBADARG;
}

void seq_delete(KCSIO_BLOCK *kfb)
{
	kfb->_status = EBADARG;
}

/*----
No unlocking needed.
------*/
void seq_unlock(KCSIO_BLOCK *kfb)
{
/*	kfb->_status = 0; */
}

/*----
The seq_file_info() routine pulls the data on an open file and
sets up the c structure.
------*/
void seq_file_info(KCSIO_BLOCK *kfb)
{

	kfb->_altkey_count = 0;

	KCSI_clear_keys(kfb);

	if(!(kfb->_record_len))
		kfb->_record_len = 1;
}
/*---
Shared is the same as IO for C-ISAM. All C-ISAM fopens to be compatible
with COBOL require that the file be set up at the first record.
A COBOL open sets things up for the first record. A c-isam open
does not.
------*/
void seq_open_input(KCSIO_BLOCK *kfb)
{
	do_open(kfb,"r");
}
void seq_open_io(KCSIO_BLOCK *kfb)
{
	do_open(kfb,"a+");
}
void seq_open_shared(KCSIO_BLOCK *kfb)
{
	seq_open_io(kfb);
}


/*
**	History:
**	$Log: seqio.c,v $
**	Revision 1.10  2003/02/20 19:29:54  gsl
**	fix -Wall warnings
**	
**	Revision 1.9  2003/02/04 19:19:08  gsl
**	fix header
**	
**	Revision 1.8  2002/10/24 14:20:33  gsl
**	Make globals unique
**	
**	Revision 1.7  2001/11/15 20:10:14  gsl
**	Replace the errno checking with testing the return codes.
**	
**	Revision 1.6  1996-09-17 19:34:18-04  gsl
**	drcs update
**
**
**
*/
