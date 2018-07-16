/*
******************************************************************************
**
** KCSI - King Computer Services Inc.
**
** $Id:$
**
** 
** CVS
** $Source:$
** $Author: gsl $
** $Date:$
** $Revision:$
******************************************************************************
*/



/*----
First test that all the types are saved correctly
------*/

#include <stdio.h>

#include "kcsifunc.h"
#include "vscrglb.h"
#include "kwisp.h"
#include "shrthand.h"



static int field, block;

static void process_the_info(void);
static void start_one_field(CR_FLD *fld);
static void end_one_field(CR_FLD *fld);
static void process_one_file_field(CR_FLD *fld);
static void process_one_pad(CR_FLD *fld);
static void process_one_string(CR_FLD *fld);
static void process_one_seq(CR_FLD *fld);
static void process_unknown(CR_FLD *fld);
static void process_one_block(CR_BLK *blk);
static void start_one_blk(CR_BLK *blk);
static void end_one_blk(CR_BLK *blk);
static void cr_error_list(void);
static void print_error_record(void);
static void read_selected_record(CR_FLD *fld);
static void do_one_blk(CR_BLK *blk);
static void close_all_input(CR_BLK *blk);
static void close_input_file(CR_FLD *fld);
static void open_error_file(void);
static void close_error_file(void);
static void write_error_record(void);
static int record_is_too_low(CR_FLD *fld);
static int record_is_too_high(CR_FLD *fld);
static int record_is_selected(CR_FLD *fld);
static int ix_record_is_too_low(CR_FLD *fld);
static int rel_record_is_too_low(CR_FLD *fld);
static void build_pseudo_key(char *str, KCSIO_BLOCK *kfb);
static int mymemcmp(char *mem1, char *mem2, int len);
static int ix_record_is_too_high(CR_FLD *fld);
static int rel_record_is_too_high(CR_FLD *fld);


/*----
Main entry to these routines
----*/
void cr_create_file(void)
{
	char* ptr;
	if((ptr=getenv("CREATE_DEBUG_REPORT")) != NULL && *ptr == '1')
	{
		cr_print_the_spec();
	}
	process_the_info();
}

static void process_one_field(CR_FLD *fld)
{
	start_one_field(fld);
	switch(fld->type)
		{
		case CFIELD_FILE:
			process_one_file_field(fld);
			break;
		case CFIELD_PAD:
			process_one_pad(fld);
			break;
		case CFIELD_STRING:
			process_one_string(fld);
			break;
		case CFIELD_SEQUENCE:
			process_one_seq(fld);
			break;
		default:
			process_unknown(fld);
			break;
		}
	end_one_field(fld);
}

static void start_one_field(CR_FLD *fld)
{
}
static void end_one_field(CR_FLD *fld)
{
}

static void process_one_file_field(CR_FLD *fld)
{
	int inpos;

	++field;
	read_selected_record(fld);
	if(fld->kfb->_status != 0)
		return;
	inpos = fld->inpos + (fld->len * (fld->counter - 1));
	memcpy(&cr_out_rec[fld->outpos],&fld->kfb->_record[inpos],fld->len);
}
static void process_one_pad(CR_FLD *fld)
{
	++field;
	memset(&cr_out_rec[fld->outpos],fld->string[0],fld->len);
}
static void process_one_string(CR_FLD *fld)
{
	++field;
	memcpy(&cr_out_rec[fld->outpos],fld->string,fld->len);
}
static void process_one_seq(CR_FLD *fld)
{
	char work[101],format[25];

	++field;
	if(	(fld->curval > fld->endval) 	&&
		(fld->repeat)			)
		{
		fld->curval = fld->begval;
		}
	if(fld->curval == -1)
		{
		fld->curval = fld->begval;
		}
	else
		{
		fld->curval += fld->increment;
		}
	sprintf(format,"%%0%dld",fld->len);
	sprintf(work,format,fld->curval);
	memcpy(&cr_out_rec[fld->outpos],work,strlen(work));
}
static void process_unknown(CR_FLD *fld)
{
	++field;
}

static void process_one_block(CR_BLK *blk)
{
	while(1)
		{
		start_one_blk(blk);
		do_one_blk(blk);
		if(CR_block_is_done(blk))
			break;
		end_one_blk(blk);
		++blk->counter;
		}
	close_all_input(blk);
}

static void start_one_blk(CR_BLK *blk)
{
	field = 0;
}

/*----
If it is relative, set up the relative key.
write the record, and record an error if one occurred
increment the record count
----*/
static void end_one_blk(CR_BLK *blk)
{
	if(	(cr_out.ofile._org[0] == 'R') ||
		(cr_out.ofile._org[0] == 'B')	)
		cr_out.ofile._rel_key = cr_out.records + 1;

	cr_io(WRITE_RECORD);
	if(cr_out.ofile._status != 0)
		{
		if(cr_errlist)
			cr_error_list();
		++cr_out.errors;
		}
	else
		{
		++cr_out.records;
		}
}

static KCSIO_BLOCK error_kfb;
static char error_rec[133];

static void cr_error_list(void)
{
	if(error_kfb._open_status == 0)
		open_error_file();
	print_error_record();
}

static void print_error_record(void)
{
	int idx, len, ch;

	len = cr_out.ofile._record_len;
	if(len > 132)
		len = 132;
	memset(error_rec,0,133);
	for(idx = 0; idx < len; ++idx)
		{
		ch = cr_out_rec[idx] & 0xff;
		if(	(ch < ' ') || (ch > '~')	)
			ch = '.';
		error_rec[idx] = ch;
		}
	write_error_record();
}


static void open_error_file(void)
{
	long mode;

	error_kfb._org[0] = 'C';
	strcpy(error_kfb._io,OPEN_OUTPUT);
	strcpy(error_kfb._name,"##CERR  ");
	strcpy(error_kfb._library,"        ");
	strcpy(error_kfb._volume,"      ");
	error_kfb._record_len = 132;
	error_kfb._record = error_rec;
	mode = IS_PRINTFILE + IS_OUTPUT;
	kcsio_wfopen(mode,&error_kfb);
	KCSI_ccsio(&error_kfb, error_rec);
}

static void close_error_file(void)
{
	if(error_kfb._open_status == 0)
		return;
	cr_fileio(&error_kfb, CLOSE_FILE);
	strcpy(error_kfb._name,"        ");
}

static void write_error_record(void)
{
	cr_fileio(&error_kfb, WRITE_RECORD);
}


static void do_one_blk(CR_BLK *blk)
{
	ll_all(blk->fld,process_one_field);
}

/*----
The file is already created, so no need to open output so:
1.	Open NEW FILE IO
2.	Init block no, record count and error count
3.	Process all blocks
4.	Close the new file.
------*/
static void process_the_info(void)
{
	cr_io(OPEN_IO);
	block = 0;
	cr_out.records = cr_out.errors = 0L;
	ll_all(cr_blk,process_one_block);
	cr_io(CLOSE_FILE);
	close_error_file();
}

/*----
This section reads (and as necessary opens) each file in
in the block.
------*/

static void read_selected_record(CR_FLD *fld)
{
	long recount,increment;

	recount = 0;
	while(1)
		{
		if(fld->kfb->_open_status == 0)
			{
			cr_fileio(fld->kfb,OPEN_INPUT);
			fld->curval = 0;
			fld->counter = fld->count + 1;
			if(fld->count < 1)
				fld->count = 1;
			}
		if(fld->kfb->_status != 0)
			break;
		if(fld->counter >= fld->count)
			{
			increment = fld->increment;
			if(increment == 0)
				increment = 1;
			while(increment--)
				{
				cr_fileio(fld->kfb,READ_NEXT_RECORD);
				++fld->curval;
				if(fld->kfb->_status != 0)
					break;
				}
			fld->counter = 0;
			}
		if(fld->kfb->_status != 0)
			break;
		if(record_is_too_low(fld))
			{
			fld->counter += fld->count;
			continue;
			}
		if(record_is_too_high(fld))
			{
			fld->kfb->_status = EENDFILE;
			break;
			}
		if(!record_is_selected(fld))
			{
			fld->counter += fld->count;
			continue;
			}
		++fld->counter;
		break;
		}
}

static int record_is_too_low(CR_FLD *fld)
{
	if(fld->kfb->_org[0] == 'I')
		return(ix_record_is_too_low(fld));
	else
		return(rel_record_is_too_low(fld));
}

static char pseudo_key[2040];
static int ix_record_is_too_low(CR_FLD *fld)
{
	KCSIO_BLOCK *kfb;

	build_pseudo_key(pseudo_key,kfb = fld->kfb);
	if(mymemcmp(pseudo_key,fld->string,fld->keylen) < 0)
		return(1);
	return(0);
}
static void build_pseudo_key(char *str, KCSIO_BLOCK *kfb)
{
	int pidx, pos, posidx;
	struct keydesc *key;
	struct keypart *part;
	
	key = &kfb->_key[0];
	for(pidx = 0; pidx < key->k_nparts; ++pidx)
		{
		part = &key->k_part[pidx];
		for(pos = part->kp_start, posidx = 0;
			posidx < part->kp_leng;
			++pos, ++posidx)
			{
			*str = kfb->_record[pos];
			++str;
			}
		
		}
}

static int rel_record_is_too_low(CR_FLD *fld)
{
	if(fld->curval < fld->begval)
		return(1);
	return(0);
}

static int record_is_too_high(CR_FLD *fld)
{
	if(fld->kfb->_org[0] == 'I')
		return(ix_record_is_too_high(fld));
	else
		return(rel_record_is_too_high(fld));
}

static int ix_record_is_too_high(CR_FLD *fld)
{
	if(mymemcmp(pseudo_key,fld->endrange,fld->keylen) > 0)
		return(1);
	return(0);
}

/*----
Does what memcmp should do
------*/
static int mymemcmp(char *mem1, char *mem2, int len)
{
	int ch1, ch2;

	for( ; len; --len, ++mem1, ++mem2)
		{
		ch1 = (*mem1) & 0xff;
		ch2 = (*mem2) & 0xff;
		if((ch1 - ch2) < 0)
			return(-1);
		if(( ch1 - ch2) > 0)
			return(1);
		}
	return(0);
}


static int rel_record_is_too_high(CR_FLD *fld)
{
	if(fld->endval < 1)
		return(0);
	if(fld->curval > fld->endval)
		return(1);
	return(0);
}



/*----
If there is no comparison then the record is selected
------*/
static int record_is_selected(CR_FLD *fld)
{
	int rc;
	char *cmp;
	KCSIO_BLOCK *kfb;

	if(fld->compare[0] < '!')
		return(1);
	kfb = fld->kfb;
	cmp = fld->compare;
	rc = mymemcmp(&kfb->_record[fld->cinpos],fld->cstring,fld->clen);
	switch(rc)
		{
		case -1:
			if(	(Streq(cmp,"LT"))	||
				(Streq(cmp,"LE"))	||
				(Streq(cmp,"NE"))	)
				return(1);
			break;
		case 0:
			if(	(Streq(cmp,"EQ"))	||
				(Streq(cmp,"LE"))	||
				(Streq(cmp,"GE"))	)
				return(1);
			break;
		case 1:
			if(	(Streq(cmp,"GT"))	||
				(Streq(cmp,"GE"))	||
				(Streq(cmp,"NE"))	)
				return(1);
			break;
		}
	
	return(0);
}

/*----
Close up all input files
------*/

static void close_input_file(CR_FLD *fld)
{
	if(fld->type != CFIELD_FILE)
		return;

	if(fld->kfb->_open_status != 0)
		cr_fileio(fld->kfb,CLOSE_FILE);
}

static void close_all_input(CR_BLK *blk)
{
	ll_all(blk->fld,close_input_file);
}


/*----
A quick io routine to the cr_out.ofile.
------*/
void cr_io(char *io)
{
	cr_out.ofile._record = cr_out_rec;
	strcpy(cr_out.ofile._io,io);
	KCSI_gpwcsio(&cr_out.ofile,cr_out.ofile._record);
}

/*----
A quick io routine to any file 
------*/
void cr_fileio(KCSIO_BLOCK *kfb, char *io)
{
	long mode;

	mode = 0;
	if(kfb->_org[0] == 'I')
		mode += IS_INDEXED;

	strcpy(kfb->_io,io);
	if(*io == 'O')
		{
		if(!(strcmp(io,OPEN_OUTPUT))) 
			mode += IS_OUTPUT;
		kcsio_wfopen(mode,kfb);
		}
		
	KCSI_ccsio(kfb, kfb->_record);
}

/*
**	History:
**	$Log: vscrfile.c,v $
**	Revision 1.11  2003/05/19 13:52:25  gsl
**	CREATE_DEBUG_REPORT
**	
**	Revision 1.10  2003/02/04 19:19:08  gsl
**	fix header
**	
**	Revision 1.9  2002/11/14 16:03:29  gsl
**	Replace cr_debug the trace level
**	
**	Revision 1.8  2002/10/24 14:20:31  gsl
**	Make globals unique
**	
**	Revision 1.7  2002/10/23 20:39:04  gsl
**	make global name unique
**	
**	Revision 1.6  2002/10/17 16:36:37  gsl
**	move routines  from vscrmain.c to vscrfile.c
**	
**	Revision 1.5  2002/06/21 20:48:14  gsl
**	Rework the IS_xxx bit flags and now include from wcommon.h instead of duplicate
**	definitions.
**	
**	Revision 1.4  1996/10/02 22:13:20  gsl
**	Fix args in calls to ll_all()
**	
**	Revision 1.3  1996-10-02 09:09:11-07  gsl
**	Add standard headers
**	Fix prototypes and warnings
**	Change memcmp defined as mc to mymemcmp and removed define
**
**
**
*/
