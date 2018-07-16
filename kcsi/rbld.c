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
This is the report writer portion of REPORT.
On entry it is passed the information entered from the OPTIONS selection
of REPORT.

From this point the basic logic flow is

1.	Read a record from input1

2.	Chain (read by key) input2

3.	Unroll both records into a pseudo record

4.	Perform any select actions

5.	Sort if needed

6.	Start Printing.


------*/
#include <stdio.h>

#include "kcsio.h"
#include "dtype.h"
#include "rptsrt.h"
#include "rptprm.h"
#include "rptglb.h"
#include "iocode.h"
#include "shrthand.h"
#include "cobioblk.h"
#include "kcsifunc.h"



static KCSIO_BLOCK skfb;
static KCSIO_BLOCK *save_kfb;

/*----
Shorthand
------*/
#define	THERE_IS_A_SORT		(rpt_rhd._sort)


static int read_first_record();
static int read_first_record_int();
static int read_first_record_ext();
static int read_next_rpt_record_int();
static int read_next_rpt_record_ext();
static int read_and_select();
static int read_and_build_record();
static int read_rpt_record();
static void build_record();
static void add_new_fields();
static void build_new_field(RPT_NF *nf);


/*----
A constructed record is composed of the the two input records combined
plus the constructed new fields. An unpacked record can expand
to 3090 characters. The new fields can consist of 10 fields of length
99 each (100 for easier math). The result is (2 * 3090) + (10 * 100)
or 7180 characters.
------*/

void KCSI_rptbld()
{
	int rc;

	rpt_total_records = 0;
	rpt_record_count = 0;
	rpt_sorted_records = 0;
	rc = read_first_record();
	if(rc == 0)
		{
		if(	(*rpt_caller == 'S')	||
			(*rpt_caller == '1')	)
			inq_write();
		else
			rpt_write(1);
		}
	else
		{
		if(	(*rpt_caller == 'S')	||
			(*rpt_caller == '1')	)
			;
		else
			rpt_write(0);
		}
}

/*----
Display progress for every 100 total or 100 processed.
------*/
/*
display_progress(total,count,nsort)
long total, count,nsort;
{
	int mcheck;
	char str_total[8],str_count[8],str_nsort[8];

	mcheck = total % 100;
	if(mcheck)
		return;

	sprintf(str_total,"%06ld",total);
	sprintf(str_count,"%06ld",count);
	sprintf(str_nsort,"%06ld",nsort);

	RPTPRG(str_total,str_count,str_nsort);
}
*/
/*----
The read first record logic decides whether a record is read and
returned, or whether a sort is done and a record is returned
from the sort. All the read logic uses 0 return when the read is
good
------*/
static int read_first_record()
{
	if(rpt_opt._ext_sort)
		return(read_first_record_ext());
	else
		return(read_first_record_int());
}
static int read_first_record_int()
{
	if(THERE_IS_A_SORT)
		{
		rpt_sort_init(rpt_sort,rpt_inp_rec_len);
		while(read_and_select() == 0)
			{
			rpt_sort_release(rpt_inp_rec);
			++rpt_sorted_records;
/*
			if(rpt_opt._progress)
				display_progress(rpt_total_records,
					rpt_record_count,rpt_sorted_records);
*/
			}
		return(rpt_sort_return(rpt_inp_rec));
		}
	else
		return(read_and_select());
}
/*----
Close the file if there is a sort, save the kfb and initialize 
a fresh one to be used for the sort.
------*/
static int read_first_record_ext()
{

	if(THERE_IS_A_SORT)
		{
		strcpy(rpt_def._kfb1->_io,CLOSE_FILE);
		KCSI_ccsio(rpt_def._kfb1,rpt_def._kfb1->_record);
		memcpy(&skfb,rpt_def._kfb1,sizeof(skfb));
		save_kfb = rpt_def._kfb1;
		rpt_def._kfb1 = &skfb;
		KCSI_call_ext_sort(rpt_sort,rpt_def._kfb1);
		kcsio_wfopen(0L,rpt_def._kfb1);
		strcpy(rpt_def._kfb1->_io,OPEN_INPUT);
		KCSI_ccsio(rpt_def._kfb1,rpt_def._kfb1->_record);
		}
	return(read_next_rpt_record_ext());
}

/*----
Return the next record from the data file, or the sort file.
------*/
int rpt_read_next_rpt_record()
{
	if(rpt_opt._ext_sort)
		return(read_next_rpt_record_ext());
	else
		return(read_next_rpt_record_int());
}
static int read_next_rpt_record_int()
{
	if(THERE_IS_A_SORT)
		{
		rpt_total_records = rpt_record_count;
		return(rpt_sort_return(rpt_inp_rec));
		}
	else
		return(read_and_select());
}
/*----
Close the file if it is the last record and reopen the original
------*/
static int read_next_rpt_record_ext()
{
	int rc;

	rc = read_and_select();
	if( (rc) && (THERE_IS_A_SORT) )
		{
		KCSI_ext_sort_close(rpt_def._kfb1);
		rpt_def._kfb1 = save_kfb;
		strcpy(rpt_def._kfb1->_io,OPEN_INPUT);
		KCSI_ccsio(rpt_def._kfb1,rpt_def._kfb1->_record);
		}
	return(rc);
}

/*----
Read a record and select it. Return non-zero when at end.
If record is not selected then loop back through read logic.
The select logic returns true if the record is selected.
------*/
static int read_and_select()
{
	while(1)
		{
		if(read_and_build_record())
			return(1);
		if(KCSI_rptsel())
			return(0);
		}
}

/*----
Read the two records and build a pseudo record.
------*/
static int read_and_build_record()
{
	if(read_rpt_record())
		return(1);
	build_record();
	return(0);
}

/*----
Issue a read next on the primary file and a read keyed on the
secondary if such exists.
Return -1 if the primary is at end.
------*/
static int read_rpt_record()
{
	strcpy(rpt_def._kfb1->_io,READ_NEXT_RECORD);
	memset(rpt_inp_rec1,0,2040);
	memset(rpt_inp_rec2,0,2040);
	KCSI_ccsio(rpt_def._kfb1,rpt_inp_rec1);
	if(rpt_def._kfb1->_status != 0)
		return(-1);
	++rpt_total_records;
/*
	if(rpt_opt._progress)
		display_progress(rpt_total_records,rpt_record_count,rpt_sorted_records);
*/
	if(rpt_def._rhd->_key_to_sec[0])
		{
		cvt_data(rpt_def._rhd->_old_key2,rpt_def._rhd->_old_key1);
		strcpy(rpt_def._kfb2->_io,READ_RECORD);
		KCSI_ccsio(rpt_def._kfb2,rpt_inp_rec2);
	        if(rpt_def._kfb2->_status != 0)
			memset(rpt_inp_rec2,0,2040);
		}
	return(0);
}

/*----
Build Record consists of unpacking each of the the two input records,
and adding in the constructed fields.
------*/
static void build_record()
{
	cvt_list(rpt_new,rpt_old);
	add_new_fields();
}

/*----
			NEW FIELD LOGIC
------*/

/*----
Add the user defined fields to end of the record.
------*/
static void add_new_fields()
{
	int i;
	RPT_NF *nf;

	nf = rpt_def._nf;
	for(i = 0; i < NF_ENTRY_COUNT ; ++i)
		{
		if(nf->_e._name[0])
			{
			build_new_field(nf);
			}
		++nf;
		}
}

/*----
Build the left hand operand into the passed destination by extracting
the field or the literal.
Build the right hand operand and combine them.
----*/
static void build_new_field(RPT_NF *nf)
{
	RPT_NFO *nfo;
	DTYPE *lop,*rop;

	nfo = &nf->_o[0];

	lop = &nf->_fld;

	if(nfo->_not_lit)
		rop = *(nfo->_pnew);
	else
		{
		rop = &nfo->_lit_op;
/*
		if(lop->_type & IS_NUM)
			swapsign(rop);
*/
		}

	cvt_data(lop,rop);
	if(!nfo->_not_lit)
		lop->_len = strlen(rpt_bld_fld);
	if(! (lop->_type & IS_NUM))
		lop->_len = rop->_len;
	lop->_pos = 0;

	while(nfo->_code != 0)
	{
		int op = nfo->_code;
		++nfo;
		if(nfo->_not_lit)
		{
			rop = *(nfo->_pnew);
		}
		else
		{
			rop = &nfo->_lit_op;
/*
			if(lop->_type & IS_NUM)
				swapsign(rop);
*/
		}
		KCSI_rptcmb(lop,op,rop);
	}
	rop = *(nf->_pnew);
	cvt_data(rop,lop);
}

/*----
Numeric literals may arrive with the sign trailing. Flip it to the
front of the data.
Not used (not needed ?)
static swapsign(operand)
DTYPE *operand;
{
	char *start, *end;
	int ch;

	end = start + (strlen(start) - 1);
	if(end <= start)
		return;
	ch = *end;
	if((ch == '+') || (ch == '-'))
		{
		*end = 0;
		KCSI_inschar(start,ch);
		}
}
------*/
/*
**	History:
**	$Log: rbld.c,v $
**	Revision 1.9  2003/02/20 19:29:55  gsl
**	fix -Wall warnings
**	
**	Revision 1.8  2003/02/04 19:19:09  gsl
**	fix header
**	
**	Revision 1.7  2002/10/24 15:48:31  gsl
**	Make globals unique
**	
**	Revision 1.6  2002/10/24 14:20:37  gsl
**	Make globals unique
**	
**	Revision 1.5  2002/10/23 21:07:26  gsl
**	make global name unique
**	
**	Revision 1.4  2002/10/23 20:39:07  gsl
**	make global name unique
**	
**	Revision 1.3  2002/07/25 15:20:26  gsl
**	Globals
**	
**	Revision 1.2  1996/09/17 23:45:43  gsl
**	drcs update
**	
**
**
*/
