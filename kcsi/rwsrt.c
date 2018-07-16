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


#include <stdio.h>
#include <ctype.h>
#include "rptsrt.h"
#include "kcsio.h"
#include "iocode.h"
#include "dtype.h"
#include "kcsifunc.h"

/*----
This whole module is pretty musch a dead issue now, and it should be
scrapped. I couldn't get it to work under UNIX, and WISPSORT doesn't
exist on the VAX so it's bye-bye time.

Need to locate module that calls this (probably in rbld.c) and delete all
references
------*/

static struct _sortdata{
	char input_file[8];
	char input_lib[8];
	char input_vol[6];
	char output_file[8];
	char output_lib[8];
	char output_vol[6];
	struct {
		char	pos[4];
		char	len[3];
		char	type[1];
		char	order[1];
		} sorts[8];
	}sortdata;

static char	file_type[1];
static int4	recsize;
static int4	sortcode;
static int4	retcode;

static char sort_work[5];



static void build_sort_data(sr,idx)
RPT_ASORT *sr;
int idx;
{

	sprintf(sort_work,"%04d",sr->_pos + 1);
	memcpy(sortdata.sorts[idx].pos,sort_work,4);
	sprintf(sort_work,"%03d",sr->_len);
	memcpy(sortdata.sorts[idx].len,sort_work,3);
	sortdata.sorts[idx].type[0] = sr->_type;
	switch(sr->_order)
		{
		case 'D':
			sortdata.sorts[idx].order[0] = 'D';
			break;
		default:
		case 'A':
			sortdata.sorts[idx].order[0] = 'A';
			break;
		}
}

/*----
The new file is relative unless the input file was consecutive
------*/
static void ext_sort_newfile(kfb)
KCSIO_BLOCK *kfb;
{
	if(kfb->_org[0] != 'C')
		kfb->_org[0] = 'R';
	memcpy(kfb->_name,sortdata.output_file,8);
	memcpy(kfb->_library,sortdata.output_lib,8);
	memcpy(kfb->_volume,sortdata.output_vol,6);
	strcpy(kfb->_prname,"SORTOUT ");
}

static void ext_sort_setup(sr,kfb)
RPT_ASORT *sr;
KCSIO_BLOCK *kfb;
{
	int idx;

	rpt_sort_squeeze(sr);
	memset(&sortdata,' ',sizeof(sortdata));
	recsize = 0;
	sortcode = 0;
	retcode = 0;

	memcpy(sortdata.input_file,kfb->_name,8);
	memcpy(sortdata.input_lib,kfb->_library,8);
	memcpy(sortdata.input_vol,kfb->_volume,6);
	memcpy(sortdata.output_file,"##SORT  ",8);
	for(idx = 0; idx < 8; ++idx)
		{
		if(sr->_len == 0)
			break;
		build_sort_data(sr,idx);
		++sr;
		}
	recsize = kfb->_record_len;
	WL_wswap(&recsize);
	switch(kfb->_org[0])
		{
		case 'R':
			file_type[0] = 'F';
			break;
		case 'I':
			file_type[0] = 'I';
			break;
		case 'C':
			file_type[0] = 'N';
			break;
		}

}

void KCSI_call_ext_sort(RPT_ASORT *sr,KCSIO_BLOCK *kfb)
{
	ext_sort_setup(sr,kfb);
	WL_set_va_count(5);
	WISPSORT((char*)&sortdata,file_type,&recsize,&sortcode,&retcode);

	ext_sort_newfile(kfb);
}


void KCSI_ext_sort_close(KCSIO_BLOCK *kfb)
{
	strcpy(kfb->_io,CLOSE_FILE);
	KCSI_ccsio(kfb,kfb->_record);
}

/*
**	History:
**	$Log: rwsrt.c,v $
**	Revision 1.12  2003/02/17 22:05:58  gsl
**	Fix ambiguous SORT reference
**	
**	Revision 1.11  2003/02/04 19:19:08  gsl
**	fix header
**	
**	Revision 1.10  2002/10/24 15:48:31  gsl
**	Make globals unique
**	
**	Revision 1.9  2002/10/23 20:39:05  gsl
**	make global name unique
**	
**	Revision 1.8  2002/10/21 15:26:41  gsl
**	Cleanup file types for WISPSORT
**	
**	Revision 1.7  2002/10/17 21:22:43  gsl
**	cleanup
**	
**	Revision 1.6  2002/10/17 17:17:22  gsl
**	Removed VAX VMS code
**	
**	Revision 1.5  2002/07/25 15:20:24  gsl
**	Globals
**	
**	Revision 1.4  2002/07/12 17:17:01  gsl
**	Global unique WL_ changes
**	
**	Revision 1.3  1996/09/25 01:00:03  gsl
**	Change longs -> int4
**	
**	Revision 1.2  1996-09-17 16:45:53-07  gsl
**	drcs update
**
**
**
*/
