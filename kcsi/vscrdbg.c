/*
******************************************************************************
**
** KCSI - King Computer Services Inc.
**
** 
******************************************************************************
*/



/*----
Prints a file spec.
------*/

#include <stdio.h>
#include "vscrglb.h"
#include "kwisp.h"
#include "shrthand.h"



static int field, block;

static KCSIO_BLOCK debug_kfb;
static char debug_rec[133];


static void end_one_field(CR_FLD *fld);
static void output_spec(void);
static void output_keys(KCSIO_BLOCK *kfb);
static void output_key(struct keydesc *key);
static void output_part(struct keypart *part);
static void process_one_file_field(CR_FLD *fld);
static void process_one_pad(CR_FLD *fld);
static void process_one_string(CR_FLD *fld);
static void process_one_seq(CR_FLD *fld);
static void process_unknown(CR_FLD *fld);
static void open_debug_file(void);
static void close_debug_file(void);
static void write_debug_record(void);
static void process_one_block(CR_BLK *blk);
static void process_the_info(void);


void cr_print_the_spec(void)
{
	process_the_info();
}

static void process_one_field(CR_FLD *fld)
{
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

static void end_one_field(CR_FLD *fld)
{
	write_debug_record();
}

static void output_spec(void)
{
	KCSIO_BLOCK *kfb;

	kfb=&cr_out.ofile;
	sprintf(debug_rec,"Output. File %s %s %s type=%c RECLEN=%d",
		kfb->_name,
		kfb->_library,
		kfb->_volume,
		kfb->_org[0],
		kfb->_record_len);
	write_debug_record();
	if(kfb->_org[0] == 'I')
		output_keys(kfb);
}

static void output_keys(KCSIO_BLOCK *kfb)
{
	struct keydesc *key;
	int idx;

	key = &kfb->_key[0];
	output_key(key);
	for(idx = 0; idx < kfb->_altkey_count; ++idx)
		{
		key = &kfb->_key[idx + 1];
		output_key(key);
		}
}

static void output_key(struct keydesc *key)
{
	int idx;
	struct keypart *part;
	
	sprintf(debug_rec,"   Key parts=%hd DUPS=%c ",
			key->k_nparts,
			key->k_flags&ISDUPS?'Y':'N');
	for(idx = 0; idx < key->k_nparts; ++idx)
		{
		part = &key->k_part[idx];
		output_part(part);
		}
	write_debug_record();
}

static void output_part(struct keypart *part)
{
	int len;

	len = strlen(debug_rec);
	sprintf(&debug_rec[len],"%d for %d ",
		part->kp_start,
		part->kp_leng);
}




static void process_one_file_field(CR_FLD *fld)
{
	++field;

	sprintf(debug_rec,"Fld %3d. File %s %s %s type=%c RECLEN=%d",
		field,
		fld->kfb->_name,
		fld->kfb->_library,
		fld->kfb->_volume,
		fld->kfb->_org[0],
		fld->kfb->_record_len);
	write_debug_record();
	sprintf(debug_rec,
		"   in=%d out=%d len=%d count=%d cin=%d clen=%d cmp=%s",
		fld->inpos,
		fld->outpos,
		fld->len,
		fld->count,
		fld->cinpos,
		fld->clen,
		fld->compare);
	write_debug_record();
	sprintf(debug_rec,
		"   cstr=<%s>",
		fld->cstring);
}
static void process_one_pad(CR_FLD *fld)
{
	++field;
	sprintf(debug_rec,"Fld %3d. Pad <%c> at %d for %d.",
		field,
		fld->string[0],
		fld->outpos,
		fld->len);
}
static void process_one_string(CR_FLD *fld)
{
	++field;
	sprintf(debug_rec,"Fld %3d. String at %d for %d is<%s>.",
		field,
		fld->outpos,
		fld->len,
		fld->string);
}
static void process_one_seq(CR_FLD *fld)
{

	++field;

	sprintf(debug_rec,
	"Fld %3d. Sequence at %d for %d from %ld to %ld %srepeating.",
		field,
		fld->outpos,
		fld->len,
		fld->begval,
		fld->endval,
		(fld->repeat?"":"not "));
}
static void process_unknown(CR_FLD *fld)
{
	++field;
	sprintf(debug_rec,
		"Fld %3d. Unknown, type = %d",
		field,
		fld->type);
}

static void open_debug_file(void)
{
	long mode;

	debug_kfb._org[0] = 'C';
	strcpy(debug_kfb._io,OPEN_OUTPUT);
	strcpy(debug_kfb._name,"##CDBG  ");
	strcpy(debug_kfb._library,"        ");
	strcpy(debug_kfb._volume,"      ");
	debug_kfb._record_len = 132;
	debug_kfb._record = debug_rec;
	mode = IS_PRINTFILE + IS_OUTPUT;
	kcsio_wfopen(mode,&debug_kfb);
	KCSI_ccsio(&debug_kfb, debug_rec);
}

static void close_debug_file(void)
{
	if(debug_kfb._open_status == 0)
		return;
	cr_fileio(&debug_kfb, CLOSE_FILE);
	strcpy(debug_kfb._name,"        ");
}

static void write_debug_record(void)
{
	cr_fileio(&debug_kfb, WRITE_RECORD);
}


static void process_one_block(CR_BLK *blk)
{
	sprintf(debug_rec,"Blk %3d. count=%ld.",++block,blk->count);
	write_debug_record();
	field = 0;
	ll_all(blk->fld,process_one_field);
}

static void process_the_info(void)
{
	block = 0;
	open_debug_file();
	output_spec();
	ll_all(cr_blk,process_one_block);
	close_debug_file();
}


/*
**	History:
**	$Log: vscrdbg.c,v $
**	Revision 1.7  2003/02/04 19:19:08  gsl
**	fix header
**	
**	Revision 1.6  2002/10/23 20:39:04  gsl
**	make global name unique
**	
**	Revision 1.5  2002/06/21 20:48:14  gsl
**	Rework the IS_xxx bit flags and now include from wcommon.h instead of duplicate
**	definitions.
**	
**	Revision 1.4  1996/10/02 22:12:54  gsl
**	Fix calls to ll_all()
**	
**	Revision 1.3  1996-10-02 09:08:18-07  gsl
**	Add standard headers
**	Fix prototypes and warnings
**
**
**
*/
