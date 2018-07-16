static char copyright[]="Copyright (c) 1988-1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
/*----

This is a sort that allows any size input file
and up to 8 keys in ascencding or decending order.

It uses an isam file to execute the sort by writing
a pseudo keyed record. The key is created at the front of
the record by extracting the postions requested for the sort
and stuffing them at the front of the record. The returned record
returns only the actual record portion.

Creating the extra key allows special manipulation to be done
for signed fields and inverting fields to create descending
sorts.

------*/

/*----
			SORT ROUTINES
Basic flow for using these routines
the sort struct must be declared within the user program as

SORT sort[MAX_SORTS]; and should not be dereferenced until the
sort is completed.

1.	rpt_sort_clear()

2.	Initialize sort structs
        _sort[x]._pos   		0 based off set
	_sort[x]._len			length
	_sort[x]._order			D for descending A for ascending sort
	_sort[x]._numeric		1 for numeric field

3.	rpt_sort_init(sort,record_length)

4.	rpt_sort_release(record) until at-end

5.	rpt_sort_return(record) until non zero return code

------*/

#include <stdio.h>
#include <ctype.h>
#include "kcsio.h"
#include "rptsrt.h"
#include "iocode.h"
#include "kwisp.h"
#include "intdef.h"
#include "kcsifunc.h"


static char sccsid[]="@(#)rsrt.c	1.5 7/13/93";

static char work_rec[(MAX_KEY * MAX_SORTS) + (MAX_LEN * 2)];

static KCSIO_BLOCK sort_file;
/*static struct keydesc sort_key;*/
static SORT *sort;

#define UNIQUE_LEN	9	/* Added when necessary to make unique keys*/
static long unique;


#define	FULL_KEY_LEN(x)	(x->_key[0].k_leng)
#define	DATA_KEY_LEN(x)	(FULL_KEY_LEN(x) - UNIQUE_LEN)


static void sort_key_init(SORT *srt,struct keydesc *ky);
static void clean_up(KCSIO_BLOCK *sf);
static int sort_ret(KCSIO_BLOCK *sf,char *rec);
static char *bld_key(SORT* srt, char* dest, char* rec);


/*----
			SORT INITIALIZATION LOGIC
------*/
/*----
May be used to quickly clear out a sort struct array
------*/
void rpt_sort_clear(SORT *sr)
{
	int idx;
	for(idx = 0 ; idx < MAX_SORTS ; ++idx, ++sr)
		sr->_numeric = sr->_pos = sr->_len = sr->_order = 0;
}

static void name_the_sort_file(sf)
KCSIO_BLOCK *sf;
{
	int4 mode;

	strcpy(sf->_name,"##SORT  ");
	strcpy(sf->_library,"        ");
	strcpy(sf->_volume,"      ");
	strcpy(sf->_prname,"SORT    ");
	mode = IS_OUTPUT + IS_INDEXED; 
	kcsio_wfopen(mode,sf);
}

/*----
Initialize a sort file structure.
------*/
static void sort_file_init(sf,rl)
KCSIO_BLOCK *sf;
int rl;
{
	int temp;

	temp = FULL_KEY_LEN(sf);
	sf->_record_len = rl + temp;
	name_the_sort_file(sf);
}
/*----
Create a sort file
------*/
static int sort_file_create(sf,ky)
KCSIO_BLOCK *sf;
struct keydesc *ky;
{
	strcpy(sf->_org,"I");

	strcpy(sf->_io, OPEN_OUTPUT);
	KCSI_ccsio(sf,work_rec);
	if (sf->_status != 0)
	{
		return(sf->_status);
	}

	strcpy(sf->_io, CLOSE_FILE);
	KCSI_ccsio(sf,work_rec);
	if (sf->_status != 0)
	{
		return(sf->_status);
	}

	strcpy(sf->_io, OPEN_IO);
	KCSI_ccsio(sf,work_rec);
	if (sf->_status != 0)
	{
		return(sf->_status);
	}
	
	
	return(sf->_status);
}

/*----
Clean up loose ends in the sort specification and create sort file.
------*/
int rpt_sort_init(SORT *sr,int rl)
{
	rpt_sort_squeeze(sr);
	sort_key_init(sr,&sort_file._key[0]);
	sort_file_init(&sort_file,rl);
	sort_file_create(&sort_file,&sort_file._key[0]);
	sort = sr;
	unique = 0L;
	return(0);
}

/*----
Squeeze the sort defintion where the length is set to zero
as this is an illegal length for a sort.
------*/
void rpt_sort_squeeze(SORT *srt)
{
	int idx;
	SORT *tsrt;

	tsrt = srt;
	++tsrt;
	for(idx = 0 ; idx < (MAX_SORTS - 1) ; ++idx, ++tsrt)
		{
		if(srt->_len)
			++srt;
		else
		if(tsrt->_len)
			{
			srt->_len = tsrt->_len;
			srt->_pos = tsrt->_pos;
			srt->_order = tsrt->_order;
			srt->_numeric = tsrt->_numeric;
			++srt;
			}
		}
}
/*----
Build the sort key starting at position 0 with a length equal to the sum
of the lengths of the sort keys.
------*/
static void sort_key_init(SORT *srt,struct keydesc *ky)
{
	int idx;

	ky->k_start = 0;
	ky->k_type = 0;
	ky->k_leng = 0;
	ky->k_flags = 0;
	ky->k_nparts = 1;
	for(idx = 0 ; idx < MAX_SORTS ; ++idx, ++srt)
		ky->k_leng += srt->_len;
	ky->k_leng += UNIQUE_LEN;
}


/*----
			SORT OUTPUT LOGIC
------*/
static void add_unique(num,work)
long num;
char *work;
{
	char buf[10];
	char format[10];

	sprintf(format,"%%0%dld",UNIQUE_LEN);
	
	sprintf(buf,format,num);
	memcpy(work,buf,UNIQUE_LEN);
}

/*
  Write out the next sort file record.
  If the write results in a duplicate key, add the unique component and try again.
 */
static int sort_rel(sf,s,r)
KCSIO_BLOCK *sf;
SORT *s;
char *r;
{
	int temp;

	temp = FULL_KEY_LEN(sf);
	memcpy(bld_key(s,work_rec,r),r,(sf->_record_len - temp));

	strcpy(sf->_io,WRITE_RECORD);
	KCSI_ccsio(sf,work_rec);

	if (sf->_status == 0)
	{
		return 0; /* Success */
	}

	if (EDUPL != sf->_status)
	{
		/* 
		   The only error we are prepared to handle is a duplicate key.
		*/
		return sf->_status;
	}
	
	
	/* 
	   Write failed -- key is not unique, so add unique component and try again.
	*/
	if(unique > 999999999)
	{
		/* Exceeded allowed number of non-unique keys */
		return(sf->_status);
	}
		
	temp = DATA_KEY_LEN(sf);
	add_unique(++unique,&work_rec[temp]);

	strcpy(sf->_io,WRITE_RECORD);
	KCSI_ccsio(sf,work_rec);

	return(sf->_status);
}

/*----
Writes a record to the sort file.
------*/
void rpt_sort_release(char *r)
{
	sort_rel(&sort_file,sort,r);
}

/*----
Invert all bits character by character for len
------*/
static void invert(work,len)
char *work;
int len;
{
	while(len--)
		{
		*work ^= 0xff;
		++work;
		}
}


/*----
Pull the key fields from the main record into the start of the record.
and modify as needed.
------*/
static char *bld_key(SORT* srt, char* dest, char* rec)
{
	int idx;
	char *work;

	work = dest;

	for(idx = 0 ; idx < MAX_SORTS ; ++idx, ++srt)
		{
		if(srt->_len == 0)
			continue;
		memcpy(work,&rec[srt->_pos],srt->_len);
		if(srt->_numeric)
			{
			if(*work == ' ')
				*work = '+';
			else
			if(*work == '-')
				{
				*work = (char) 0xfe;
				invert(work,srt->_len);
				}
			}
		if(srt->_order == 'D')
			invert(work,srt->_len);
		work += srt->_len;
		}
	add_unique(0L,work);
	return(work + UNIQUE_LEN);
}

/*----
			SORT INPUT LOGIC
------*/

/*----
Return a record from the sort-file.
If the file is still open in output mode, then close and reopen in input
mode first.
------*/
int rpt_sort_return(char *rec)
{
	return(sort_ret(&sort_file,rec));
}

static int sort_ret(KCSIO_BLOCK *sf,char *rec)
{
	int rc;
	int temp;

	if(sf->_open_status == 1)
		{
		strcpy(sf->_io,CLOSE_FILE);
		KCSI_ccsio(sf,work_rec);
		strcpy(sf->_io,OPEN_INPUT);
		KCSI_ccsio(sf,work_rec);
		if(sf->_status)
			return(sf->_status);
		sf->_open_status = 2;
		}
	strcpy(sf->_io,READ_NEXT_RECORD);
	KCSI_ccsio(sf,work_rec);
	if(rc = sf->_status)
		{
		strcpy(sf->_io,CLOSE_FILE);
		KCSI_ccsio(sf,work_rec);
		clean_up(sf);
		}
	else
		{
		temp = FULL_KEY_LEN(sf);
		memcpy(rec,&work_rec[temp],sf->_record_len - temp);
		}
	return(rc);
}
/*----
Get rid of the indexed sort file.
------*/
static void clean_up(KCSIO_BLOCK *sf)
{
	char rc[4];

	WL_set_va_count(5);
	SCRATCH("F",sf->_name,sf->_library,sf->_volume,rc);
}

/*
**	History:
**	$Log: rsrt.c,v $
**	Revision 1.5.2.1  2002/11/12 15:56:36  gsl
**	Sync with $HEAD Combined KCSI 4.0.00
**	
**	Revision 1.11  2002/10/24 15:48:31  gsl
**	Make globals unique
**	
**	Revision 1.10  2002/10/23 20:39:06  gsl
**	make global name unique
**	
**	Revision 1.9  2002/10/17 21:22:43  gsl
**	cleanup
**	
**	Revision 1.8  2002/07/25 15:20:24  gsl
**	Globals
**	
**	Revision 1.7  2002/06/27 04:12:43  gsl
**	Clean up status/mode bits
**	
**	Revision 1.6  2002/06/21 20:48:15  gsl
**	Rework the IS_xxx bit flags and now include from wcommon.h instead of duplicate
**	definitions.
**	
**	Revision 1.5  2002/03/18 22:53:27  gsl
**	fix REPORT SORT problem with Micro Focus Server Express.
**	in sort_file_create() OPEN_OUTPUT to create then CLOSE_FILE then OPEN_IO
**	in sort_rel() remove the loop and only try adding a unique component if
**	the write failed with a duplicate key
**	
**	Revision 1.4  1997-10-02 15:36:13-04  gsl
**	Fix warnings
**
**	Revision 1.3  1996-09-17 19:45:50-04  gsl
**	drcs update
**
**
**
*/
