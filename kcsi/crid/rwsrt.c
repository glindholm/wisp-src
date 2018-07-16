static char copyright[]="Copyright (c) 1988-1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";

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

static char sccsid[]="@(#)rwsrt.c	1.7 11/15/93";


static void build_sort_data(sr,idx)
SORT *sr;
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
SORT *sr;
KCSIO_BLOCK *kfb;
{
	int idx;

	sort_squeeze(sr);
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
	wswap(&recsize);
	switch(kfb->_org[0])
		{
		case 'R':
			file_type[0] = 'F';
			break;
		case 'I':
#ifdef	KCSI_ACU
			file_type[0] = 'A';
#endif
#ifdef	KCSI_LPI
			file_type[0] = 'C';
#endif
#ifdef	KCSI_MF
			file_type[0] = 'C';
#endif
#ifdef	KCSI_MFX
			file_type[0] = 'C';
#endif
			break;
		case 'C':
			file_type[0] = 'N';
			break;
		}

}

void call_ext_sort(SORT *sr,KCSIO_BLOCK *kfb)
{
	ext_sort_setup(sr,kfb);
#ifdef KCSI_VAX
	wargs(2L);
	SORTCALL(&sortdata,&retcode);
#else
	wargs(5L);
	WISPSORT((char*)&sortdata,file_type,&recsize,&sortcode,&retcode);
#endif	/* KCSI_VAX */

	ext_sort_newfile(kfb);
}


void ext_sort_close(KCSIO_BLOCK *kfb)
{
	strcpy(kfb->_io,CLOSE_FILE);
	ccsio(kfb,kfb->_record);
}

/*
**	History:
**	$Log: rwsrt.c,v $
**	Revision 1.3  1996-09-24 21:00:03-04  gsl
**	Change longs -> int4
**
**	Revision 1.2  1996-09-17 16:45:53-07  gsl
**	drcs update
**
**
**
*/
