static char copyright[]="Copyright (c) 1988-1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";

/*---- 
Copyright (c) 1989-93 King Computer Services, Inc. 
All rights reserved. 
------*/
/*-----
This routine receives the first record ready to be output to the
spin off file.
------*/
#include <stdio.h>
#include <ctype.h>

#include "dtype.h"
#include "rptprm.h"
#include "rptsrt.h"
#include "rptglb.h"
#include "iocode.h"
#include "kcsio.h"
#include "shrthand.h"
#include "kwisp.h"
#include "intdef.h"
#include "kcsifunc.h"

static char sccsid[]="@(#)iwrt.c	1.8 4/16/93";

static KCSIO_BLOCK *ofb;

static int save_org;

static void open_inq_file();
static void one_inq_record();
static void close_inq_file();


/*----
Do some initialization and start writing.
------*/
void inq_write()
{

	open_inq_file();
	ofb->_rel_key = 1;
	do{
/*
		if(rpt_opt._progress)
			display_progress(rpt_total_records,
				rpt_record_count,rpt_sorted_records);
*/
		++rpt_record_count;
		one_inq_record();
		++ofb->_rel_key;
	}while(read_next_rpt_record() == 0 );
	if(*rpt_caller < 'A')
		call_lmg_log_count(rpt_record_count);

	close_inq_file();
}

static int inq_scratch;

void set_inq_scratch(int code)
{
	inq_scratch = code;
}


static void open_inq_file()
{
	int4 mode;

	memcpy(rpt_def._kfb2,rpt_def._kfb1,sizeof(KCSIO_BLOCK));
	ofb = rpt_def._kfb2;
	memcpy(ofb->_name,rpt_PRT_FILE,8);
	memcpy(ofb->_library,rpt_PRT_LIB,8);
	memcpy(ofb->_volume,rpt_PRT_VOL,6);
	if(	(*rpt_caller < 'A')	&&
		(inq_scratch)		)
		{
		wargs(5L);
		SCRATCH("F", ofb->_name, ofb->_library, ofb->_volume, "    ");
		}
		
	ofb->_name[8] = 0;
	ofb->_library[8] = 0;
	ofb->_volume[6] = 0;
	ofb->_open_status = 0;
/* 
 * If output is explicitly requested to be consecutive then so be it.
 */
	if(save_org == 'R')
		ofb->_org[0] = 'R';
	if(save_org == 'C')
		ofb->_org[0] = 'C';
/*
	mode = 0;
	eoname = wfname(&mode,ofb->_volume,
			ofb->_library,ofb->_name,ofb->_sys_name);
	*eoname = 0;
*/
	mode = WISP_PRNAME + WISP_OUTPUT;
	if(ofb->_org[0] == 'I')
		mode += WISP_INDEXED;
	kcsio_wfopen(mode,ofb);
	strcpy(ofb->_io,OPEN_OUTPUT);
	ccsio(ofb,inp_rec2);
}
void set_inq_org(int org)
{
	save_org = org;
}

static void close_inq_file()
{
	strcpy(ofb->_io,CLOSE_FILE);
	ccsio(ofb,inp_rec2);
}
static void one_inq_record()
{
	memcpy(inp_rec2,inp_rec1,2040);
	strcpy(ofb->_io,WRITE_RECORD);
	ccsio(ofb,inp_rec2);

}
/*
**	History:
**	$Log: iwrt.c,v $
**	Revision 1.3  1996/09/17 23:45:39  gsl
**	drcs update
**	
**
**
*/
