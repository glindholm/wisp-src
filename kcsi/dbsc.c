static char copyright[]="Copyright (c) 1988-1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
/*----
This includes the entry point for the DATENTRY entry screens.
------*/

#include <stdio.h>
#include "kcsifunc.h"
#include "datcob.h"
#include "shrthand.h"
#include "dglb.h"
#include "dbsc.h"


static char sccsid[]="@(#)dbsc.c	1.5 1/27/93";

/*----
Mods:
    30-Mar-1990 Modified to take spacing from the ch_header.
------*/

/*----
The values passed in are:
1.	CONTROL-FILE-HEADER Records.
2.	CF-T1-TABLE and CF-T2-TABLE (RCCTRLT1 and RCCTRLT2)
3.	One PIC 99 field to be used as a return code if a screen
	cannot be built.
4.	An IO block. For the closed control file.

CF-T1 and CF-T2 are already sorted in update sequence order by
CTRLARY.  The sequence sort is more of a work ordering.  All
non-updateable fields (that are not group tables) appear first.
Then updateables. Group tables appear before their children
mixed in with the updateables.

------*/

#define	Is_last_field(x)	(Memeq((x)->name,LAST_FIELD_NAME,8))
#define	Not_last_field(x)	(memcmp((x)->name,LAST_FIELD_NAME,8))
#define	Is_empty_field(x)	((x)->name[0] < 33)
#define	Not_empty_field(x)	((x)->name[0] > 32)
#define	Is_not_updateable(x)	((x)->noupdate)
#define	Is_updateable(x)	(!((x)->noupdate))
#define	Is_a_key(x)		(((x)->keynum) || ((x)->altkeynum))
#define	Is_a_group(x)		(((x)->noupdate) && ((x)->occurrences > 1))

static void init_bsc_load();

 
void DBSC(char *cf_hdrs,char *cf_t1,char *cf_t2,char *io_block,char *dio_block,char *ret_code)
{
	int rc;

/* Dummy call for the VAX linker */
	vax_dte_globals();
/* Clear the return Code */
	memcpy(ret_code,"00",2);
/* Point the io blocks in the right direction */
	dte_cf_t1 = cf_t1;
	dte_cf_t2 = cf_t2;
	dte_cf_hdrs = cf_hdrs;
	dte_cio_block = io_block;
	dte_dio_block = dio_block;

/* Load up and and set an error if any */
	if(!(rc = dbscl(cf_hdrs,cf_t1,cf_t2)))
		return;
	if(rc == 99)
		memcpy(ret_code,"99",2);
	if(rc == 98)
		memcpy(ret_code,"98",2);

}

/*----
This routine has a difference from the WANG. The Wang will bomb out
if spacing is set to two and the screen cannot be constructed.
This routine will try again at one to build the screen and.
------*/
int dbscl(char *hdrs,char *t1,char *t2)
{
/*
	datentry_spacing = 2;
*/

	cvt_record(ch_dest,ch_src,hdrs);
	if((datentry_spacing < 1) || (datentry_spacing > 2))
		datentry_spacing = 2;

/* Init for the load, and load it */
	while(1)
		{
		init_bsc_load();
		bsc_load(hdrs,t1,t2);
/* No updateable fields */
		if(field_count == 0)
			return(98);
/* All on the screen OK */
		if(next_row <= LAST_ROW)
			break;
/* Otherwise try single line spacing */
		if(datentry_spacing == 2)
			datentry_spacing = 1;
		else
/* Otherwise too much for the screen */
			return(99);
		}
	return(0);
}

static void init_bsc_load()
{
	int i;

	for(i = 0; i < MAX_DISPLAY_FIELDS; ++i)
		keys[i] = NULL;	
	for(i = 0; i < MAX_FIELDS; ++i)
		memset(&dtefld[i],0,sizeof(FIELD));
	memcpy(dtefld[LAST_FIELD].name,LAST_FIELD_NAME,8);
	field_count = 0;
	next_row = FIRST_ROW;
	next_col = FIRST_COL;
	d_new_screen = d_new_key = 1;
}

void bsc_load(char *hdr,char *t1,char *t2)
{
	int i;
	FIELD *fld;

	bsc_fld_set(hdr,t1,t2,&dtefld[0]);
	bsc_bld_scr(hdr,&dtefld[0]);
	for(i = 0; i < field_count; ++i)
		{
		fld = &dtefld[i];
		if(Is_updateable(fld))
			return;
		}
	field_count = 0;
}

void bsc_bld_scr(char *hdr,FIELD *fld)
{
	bsc_key_set(hdr,fld);
	bsc_accum_set(fld);
	bsc_fld_load(hdr,fld);
}
/*----
Convert all passed fields.
------*/
void bsc_fld_set(char *hdr,char *t1,char *t2,FIELD *fld)
{
	while(1)
		{
		cvt_fld(t1,t2);
		cpy_fld(fld,&wrk_field);
		fld->fac &= 0xff;
		if(Memeq(wrk_field.name,"        ",8))
			break;
		++fld;
		t1 += T1_ENTRY_LEN;
		t2 += T2_ENTRY_LEN;
		++field_count;
		}
}
/*----
Flag all fields that are keys. If a key is non updateable
then find all updateable elements and tag them.
------*/
void bsc_key_set(char *hdr,FIELD *fld)
{
	FIELD *savefld;
	int i, got_one;

	for(i = 0, savefld = fld; i < field_count; ++fld, ++i)
		set_a_key(hdr,fld);
	for(i = 0, fld = savefld; i < field_count; ++fld, ++i)
		{
		if(Is_a_key(fld) && Is_not_updateable(fld) )
			{
			got_one = set_a_key_element(savefld,fld);
			if (!(got_one))
				fld->noupdate = 0;
			}
		}
}

/*----
Flag all accumulator fields.
------*/
void bsc_accum_set(FIELD *fld)
{
	FIELD *sfld,*savefld;
	int fidx,sidx;

	savefld = fld;
	for(fidx = 0;fidx < field_count; ++fld, ++fidx)
		{
		if(fld->cumm_name[0] > ' ')
			{
			for(sidx = 0,sfld = savefld; sidx < field_count; ++sfld,++sidx)
				{
				if(Memeq(fld->cumm_name,sfld->name,8))
					sfld->is_accum = 1;
				}
			strunc(fld->cumm_name);
			}
		}
}

/*----
Scan the list of fields looking for updateable elements of a
non-updateable key and marking any if found. An element is part of
a key if it falls completely within the start and end position of the
key, and is updateable.
------*/
int set_a_key_element(FIELD *element,FIELD *key)
{
	int i, gotkey;

	for(i = 0, gotkey = 0; i < field_count; ++i, ++element)
		{
		if((is_within(key,element)) &&
		   (Is_updateable(element)) )
			{
			element->keynum = key->keynum;
			element->altkeynum = key->altkeynum;
			element->dups = key->dups;
			gotkey = 1;
			}
			
		}
	return(gotkey);
}

/*----
The field load updates all passed fields selecting those
that will be needed by this process. It basically discards non-updateable
fields. However certain non-updateable fields are used to extract data
for use in other fields.
A non-updateable field that is a key field is used to tag updateable
elements as part of the key.
A non-updateable field that is a group or parent field is used to
replicate all the children fields.

After each field is loaded, decisions are made about keeping the field
for use in datentry or discarding it.
1.	If the field is a key field, it is saved whether it is updateable
	or not.
2.	If the field is updateable and only occurs once, the field
	is saved and screen positions are worked out for the prompt
	and row and column.
3.	If the field is updateable and occurs more than once, the field
	is saved and copies of it are cloned to and filled in with
	prompt and occurrence information.
4.	If the field is a non-updateable group field, all of the children
	are located and screen positions are built for each occurrence of
	each child.
------*/
void bsc_fld_load(char *hdr,FIELD *fld)
{
	int key,update,group,fidx,gnflds;
	int	change_fld_cnt;

/* If it is relative file, then start with a record number field */
	if(file_is_relative())			/*22-Mar-1990*/
		do_update(&relative_record);	/*22-Mar-1990*/

	fidx = 0;

	while(fidx < field_count)
	{
		change_fld_cnt = 0;
		key = Is_a_key(fld);
		update = Is_updateable(fld);
		group = Is_a_group(fld);

		/* If it is none of the above, it is skipped */
		if((!key) && (!update) && (!group))
		{
			kill_fld_hole(fld);
			change_fld_cnt = -1;
		}
		else
		{
			/* If it is updateable the logic is simple */
			if(update)
			{
				change_fld_cnt = do_update(fld);
			}
			else
			{
				/*
				 * If it is a group then hold onto your hat (see do_group for more data).
				 * After a group is formed, the field count must be incremented by
				 * the number of additional fields created - 1 for the parent which
				 * is removed. The field index (fidx) must be incremented by the above,
				 * plus the count of fields in one parent.
				 */

				if(group)
				{
					change_fld_cnt = do_group(fld,&gnflds);
					kill_fld_hole(fld);
					change_fld_cnt -= 1;
					fidx += gnflds;
					fld += gnflds;
				}
			}
		}
		
		field_count += change_fld_cnt;
		++change_fld_cnt;
		fidx += change_fld_cnt;
		fld += change_fld_cnt;
	}
}
/*----
An updateable field may be either a single or an occurs. Returns the
number of additional fields created.
------*/
int do_update(FIELD *fld)
{
	int count;
	FIELD *nfld;
/* Anything less than 2 is an occurs 1 */
	if(fld->occurrences < 2)
		fld->occurrences = 1;

/* Set occurence to 0 for once only fields, otherwise start at 1 */
	if(fld->occurrences == 1)
		fld->occurrence = 0;
	else
		fld->occurrence = 1;

	count = 0;
/* Until all occurrences (or 1 only) are exhausted */
	while(1)
		{
		bld_fld(fld);
		if( (fld->occurrence == fld->occurrences) ||
		   (fld->occurrences == 1) )
			break;
/* Copy down one field */
		nfld = fld;
		++nfld;
		insert_fld(nfld,fld);
		++count;
		++fld;
/* Next occurrence number */
		++fld->occurrence;
/* New position in the record */
		fld->pos += fld->len;
		}
	return(count);
}
/*----
A group field is a non-updateable field that occurs more than once.
It should contain one or more updateable fields which will occur the number
of times the group occurs.

Group = DATA occurs 3 times and is not updateable. Within DATA are
NAME and NUMBER which are updateable. These will appear on the screen as
NAME (01) XXXXXX  NUMBER (01) 99 NAME (02) XXXXXX NUMBER (02) 99
NAME (03) XXXXXX  NUMBER (03) 99

Returns the number of additional fields created.
------*/
int do_group(FIELD *fld,int *gnflds)
{
	int total_len,count,nflds;
	FIELD *cfld,*efld,*sfld,*tfld;
/*
 * First locate the children of the group which should all be clumped
 * together right after the parent. Set sfld and efld to start and
 * ending range. Count the fields and get the total length.
 */
	efld = sfld = NULL;
	cfld = fld;
	++cfld;
	total_len = nflds = 0;
	while(1)
		{
		if((is_within(fld,cfld)) &&
		   (Is_updateable(cfld)) )
			{
			total_len += cfld->len;
			++nflds;
			cfld->occurrence = 1;
			if(sfld == NULL)
				sfld = cfld;
			efld = cfld;
			++cfld;
			}
		else
			break;
		}
/*
 * Build a field for each child.
 */
	if (0 == nflds)
	{
		/*
		**	If nflds==0 then there are no children.
		**	This case happens when a field in not-updatable and it has an occurs count.
		**	This was added after the fact to correct an signal 11, the following
		**	loop doesn't work if sfld and efld have not been set.
		*/

		*gnflds = 0;
		return 0;	
	}

	count = 0;
	while(1)
		{
/* Build field for each child */
		for(cfld = sfld; cfld <= efld; ++cfld)
			bld_fld(cfld);
/* Test for last occurrence. When child occurrence = parent occurrences */
		if(sfld->occurrence == fld->occurrences)
			break;
/* Build a set of copies of all fields */
		for(cfld = sfld; cfld <= efld; ++cfld)
			{
			tfld = cfld + nflds;
			insert_fld(tfld,cfld);
			++tfld->occurrence;
			tfld->pos += total_len;
			++count;
			}
/* Step up to the next set */
		sfld += nflds;
		efld += nflds;
		}
/* Return the number of elements in one group and the added field count*/
	*gnflds = nflds;
	return(count);	
}


/*----
Generate screen positioning logic for fields and prompts.
------*/

void bld_fld(FIELD *fld)
{
/*
 * Prompt length is field name length plus 6 for an occurs DATA (01)
 * if any.
 */
	strunc(fld->name);
	fld->plen = strlen(fld->name);
	if(fld->occurrence)
		fld->plen += 5;

	get_edit_len(fld);

/* Advance if prompt and field will extend passed the end of the screen */
	if((fld->plen + fld->edit_len + next_col) > 80 )
		{
		next_row += datentry_spacing;
		next_col = FIRST_COL;
		}
	
/* The prompt is positioned at the next available row and col */
	fld->prow = next_row;
	fld->pcol = next_col;

	next_col += (fld->plen + 1);
/* And the data */
	fld->frow = next_row;
	fld->fcol = next_col;
	next_col += (fld->edit_len + 1);
}

/*----
The edited length of the field is based on the internal length,
data type and editing options.
First calculate the default external length.
------*/
void get_edit_len(FIELD *fld)
{
	char pic[101];

	
	fld->edit_len = 
		get_pic_len(pic,fld->type,fld->len,fld->dec,fld->bin);
}

void cvt_fld(char *t1,char *t2)
{
/* Peel off the fields in the record */
	cvt_record(cf_t1_dest,cf_t1_src,t1);
	cvt_record(cf_t2_dest,cf_t2_src,t2);
}
/*----
Set any field that is a key with a flag indicating which kind.
------*/
int set_a_key(char *hdr,FIELD *fld)
{
	char *phdr;
	int i,mask;

	phdr = hdr;

/*Is it a primary*/
	phdr += CH_PRIMARY_KEY_POS;
	if(Memeq(phdr,fld->name,CH_PRIMARY_KEY_LEN))
		{
		fld->keynum = -1;
		return(1);
		}
/*or an alternate*/
	hdr += CA_TABLE_OFF;
	phdr = hdr;
	for(i = 0,mask = 0x8000; i < 16; ++i, mask >>= 1)
		{
		if(i == 8)
			{
			hdr += CA_TABLE_LEN;
			phdr = hdr;
			}
		if(Memeq(phdr,fld->name,CA_KEY_NAME_LEN))
			{
			fld->altkeynum |= mask;
			if(phdr[CA_DUPS_OFF] == 'Y')
				fld->dups |= mask;
			return(1);
			}
		phdr += CA_ENTRY_LEN;
		}
	return 0;
}

/*----
Insert a field in the array at the specified position.
------*/
int insert_fld(FIELD *dest,FIELD *src)
{
	if(make_fld_hole(dest))
		{
		cpy_fld(dest,src);
		return(1);
		}
	return(0);
}

void cpy_fld(FIELD *f1,FIELD *f2)
{
	memcpy(f1,f2,sizeof(FIELD));
}

/*----
Make a hole in the field array by shuffling everything down one
------*/
int make_fld_hole(FIELD *fld)
{
	FIELD *f,*g;

	f = fld;
	while((Not_last_field(fld)) && (Not_empty_field(fld)))
		++fld;
	if(Not_empty_field(fld))
		return(0);
	while(fld != f)
		{
		g = fld;
		--g;
		memcpy(fld,g,sizeof(FIELD));
		--fld;
		}
	return(1);
}
/*----
Delete a field in the field array by shuffling everything up one
------*/
int kill_fld_hole(FIELD *fld)
{
	FIELD *f,*g;

	f = fld;
	if(Is_last_field(fld++))
		return(0);
	while(1)
		{
		g = fld;
		--g;
		memcpy(g,fld,sizeof(FIELD));
		++fld;
		if(Is_empty_field(g))
			break;
		if(Is_last_field(g))
			{
			memset(g->name,' ',8);
			break;
			}
		}
	return(1);
}

int is_within(FIELD *outer,FIELD *inner)
{
	if((inner->pos < outer->pos) ||
                  ((inner->pos + inner->len) > (outer->pos + outer->len) ) )
		return(0);
	return(1);

}
/*
**	History:
**	$Log: dbsc.c,v $
**	Revision 1.6  1999/09/13 19:46:23  gsl
**	fix missing return code
**	
**	Revision 1.5  1998-03-25 13:52:05-05  gsl
**	Fix DATENTRY bug processing non-updateable fields with occurs count,
**	the do_group() logic failed if there was not any children.
**	version 2.92
**
**	Revision 1.4  1996-09-17 19:45:33-04  gsl
**	drcs update
**
**
**
*/
