/*
******************************************************************************
**
** KCSI - King Computer Services Inc.
**
** 
******************************************************************************
*/

/*----
Auxiliary routines for dmnt.c
------*/
#include <stdio.h>
#include <ctype.h>
#include "dbsc.h"
#include "dglb.h"
#include "shrthand.h"
#include "iocode.h"
#include "cobioblk.h"
#include "dtype.h"
#include "dmnt.h"
#include "kcsifunc.h"


static void move_one_to_screen(FIELD *fld);
static void bld_data_types(FIELD *fld,DTYPE *etype,DTYPE *rtype,DTYPE *itype);
static void add_accumulators();
static void sum_accumulators();
static void zero_accumulators();
static void sum_one_accumulator(FIELD *fld);
static void sum_this_field(FIELD *sum,FIELD *addend);
static void move_accumulators();
static void move_one_accumulator(FIELD *fld);
static void fac_all_prompts(int code);
static void fac_one_prompt(FIELD *fld,int code);
static void fac_one_field(FIELD *fld,int code);
static void fac_all_fields(int code);
static void dte_error_on(FIELD *fld);


/*----
			GENERAL UTILITY
------*/

/*----
Extract system dates and times and build the date/time stamps.
------*/
void KCSI_get_date_stamps()
{
/* Call the cobol routine */
	DTEDAT(dte_sys_d_ymd,dte_sys_d_mdy,dte_sys_t,dte_sys_jd);
/* Build the date time combination */
	memcpy(dte_sys_ymdhmsh,dte_sys_d_ymd,6);
	memcpy(&dte_sys_ymdhmsh[6],dte_sys_t,8);
}

/*----
Set up and error, error message and blinking bits
------*/
void KCSI_field_error(FIELD *fld,char *msg)
{
	dte_screen_error = 1;
	KCSI_make_error_message(msg);
	dte_error_on(fld);
}

void KCSI_make_error_message(char *msg)
{
	dte_init_message_field();
	memcpy(dte_message,msg,strlen(msg));
}

static void dte_error_on(FIELD *fld)
{
	char *fac;

	fac = fld->fld - 1;
	*fac |= ERROR_BIT;
}

void dte_space_out(char *mem,int len)
{
	memset(mem,' ',len);
}

/*----
			MOVE TO & FROM SCREEN
------*/
void dte_move_to_record()
{
	int i;

	add_accumulators();

	for(i = 0; dtefld[i].name[0] > ' '; ++i)
		{
		if(dtefld[i].frow)
			dte_move_one_to_record(&dtefld[i]);
		}
}

void dte_move_one_to_record(FIELD *fld)
{
	DTYPE scr_type,rec_type,i_type;

	bld_data_types(fld,&scr_type,&rec_type,&i_type);
	if(i_type._type == NLIT)
		{
		memcpy((i_type._base + i_type._pos),
		       (scr_type._base + scr_type._pos),
			scr_type._len);
		cvt_data(&scr_type,&i_type);
		}
	cvt_data(&rec_type,&scr_type);
}

void dte_move_to_screen()
{
	int i;

	for(i = 0; dtefld[i].name[0] > ' '; ++i)
		{
		if(dtefld[i].frow)
			move_one_to_screen(&dtefld[i]);
		}

}

static void move_one_to_screen(FIELD *fld)
{
	DTYPE scr_type,rec_type,i_type;

	bld_data_types(fld,&scr_type,&rec_type,&i_type);
	cvt_data(&scr_type,&rec_type);
}

static char tmp_fld[120];

static void bld_data_types(FIELD *fld,DTYPE *etype,DTYPE *rtype,DTYPE *itype)
{
	memset(etype,0,sizeof(DTYPE));
	memset(rtype,0,sizeof(DTYPE));
	memset(itype,0,sizeof(DTYPE));

	etype->_base = fld->fld;
	rtype->_base = dte_record;
	itype->_base = tmp_fld;

	etype->_pos = 0;
	rtype->_pos = fld->pos - 1;
	itype->_pos = 0;

	etype->_len = fld->edit_len;
	rtype->_len = fld->len;
	itype->_len = fld->edit_len;

	itype->_dec = etype->_dec = rtype->_dec = fld->dec;

	switch(fld->type)
		{
		case 'P':
			rtype->_type = APCK;
			etype->_type = TZON;
			itype->_type = NLIT;
			break;
		case 'Z':
			rtype->_type = AZON;
			etype->_type = TZON;
			itype->_type = NLIT;
			break;
		case 'U':
			rtype->_type = AUNS;
			etype->_type = DUNS;
			itype->_type = NLIT;
			break;
		case 'C':
			rtype->_type = ACHR;
			etype->_type = ACHR;
			itype->_type = ACHR;
			break;
		case 'B':
/* Default for most systems overridden by subsequent ifdefs*/
			rtype->_type = ABIN;
#ifdef KCSI_MFX
			rtype->_type = ABMN;
#endif
			if(!fld->bin)
				{
				etype->_type = DHEX;
				itype->_type = DHEX;
				}
			else
				{
				etype->_type = TZON;
				itype->_type = NLIT;
				}
			break;
		}
	
}

static void add_accumulators()
{
	zero_accumulators();
	sum_accumulators();
	move_accumulators();
}

static void zero_accumulators()
{
	int fidx;

	for (fidx = 0; fidx < KD_field_count; ++fidx)
		{
		dtefld[fidx].is_added = 0;
		if(dtefld[fidx].is_accum)
			dtefld[fidx].accum_work = 0;
		}	
}

static void sum_accumulators()
{
	int fidx;

	for (fidx = 0; fidx < KD_field_count; ++fidx)
		{
		if(dtefld[fidx].cumm_name[0] > ' ')
			sum_one_accumulator(&dtefld[fidx]);
		}	

}

static void sum_one_accumulator(FIELD *fld)
{
	int fidx;

	for (fidx = 0; fidx < KD_field_count; ++fidx)
		{
		if(Streq(dtefld[fidx].name,fld->cumm_name))
			sum_this_field(&dtefld[fidx],fld);
		}	
}

static void sum_this_field(FIELD *sum,FIELD *addend)
{
	char wrk_fld[33],*wrk_ptr,*src;
	double wrk_dbl;
	int slen;

	memset(wrk_fld,0,33);
	wrk_ptr = wrk_fld;
	*wrk_ptr++ = '+';
	
	slen = addend->edit_len;
	src  = addend->fld;
	while(slen)
		{
		if(isdigit((int)*src))
			*wrk_ptr++ = *src++;
		else
			{
			switch(*src)
				{
				case '.':
					*wrk_ptr++ = *src++;
					break;
				case '-':
				case '+':
					wrk_fld[0] = *src++;
					break;
				default:
					++src;
				}
			}
		--slen;
		}
	*wrk_ptr = 0;

	wrk_dbl = 0;

	sscanf(wrk_fld,"%lf",&wrk_dbl);
	sum->accum_work += wrk_dbl;
}

static void move_accumulators()
{
	int fidx;

	for (fidx = 0; fidx < KD_field_count; ++fidx)
		{
		if(dtefld[fidx].is_accum)
			move_one_accumulator(&dtefld[fidx]);
		}	
}

static void move_one_accumulator(FIELD *fld)
{
	
	DTYPE scr_type,rec_type,i_type;
	char wrk_fld[33];

	sprintf(wrk_fld,"%f",fld->accum_work);

	bld_data_types(fld,&scr_type,&rec_type,&i_type);
	i_type._base = wrk_fld;
	i_type._len = strlen(wrk_fld);
	cvt_data(&scr_type,&i_type);
	cvt_data(&rec_type,&scr_type);
}

void dte_init_record()
{
	memset(dte_record,0,2040);
}

void dte_init_message_field()
{
	dte_space_out(dte_message,MESSAGE_LEN);
}

/*----
Convert index to key integer.
------*/
int dte_key_from_idx(char *idx)
{
	int key,mask;

	key = kcsi_atoilen(idx,IDX_LEN);
	if(key > 16)
		key = -1;
	else
	if(key < 1)
		key = -1;
	else
		{
		mask = 0x8000;
		--key;
		key = mask >> key;
		}
	return(key);

}
/*----
			FAC ROUTINES
------*/
void dte_unprotect_all_fields()
{
	fac_all_fields(UNPROTECT_ALL);
}
void dte_protect_all_fields()
{
	fac_all_fields(PROTECT_ALL);
}
void dte_hide_all_fields()
{
	fac_all_fields(HIDE_FIELD);
}
void dte_protect_nomod_fields()
{
	fac_all_fields(PROTECT_NOMOD);
}
void dte_protect_key_fields()
{
	fac_all_fields(PROTECT_KEY);
}
void dte_unprotect_idx_fields(char *idx)
{
	int key,i;

	key = dte_key_from_idx(idx);
	for(i = 0; dtefld[i].name[0] > ' '; ++i )
		{
		if(dte_is_this_key(&dtefld[i],key))
			fac_one_field(&dtefld[i],UNPROTECT_ALL);
		}
}

int dte_is_this_key(FIELD *fld,int key)
{
	if(key == -1)
		{
		if(fld->keynum == key)
			return(1);
		}
	else
		{
		if(fld->altkeynum & key)
			return(1);
		}
	return(0);
}

static void fac_all_fields(int code)
{
	int i;

	for(i = 0; dtefld[i].name[0] > ' '; ++i )
		{
		fac_one_field(&dtefld[i],code);
		}
}
void dte_unprotect_rel_field()
{
	fac_one_field(&dte_relative_record,UNPROTECT_ALL);
}
void dte_protect_rel_field()
{
	fac_one_field(&dte_relative_record,PROTECT_ALL);
}

/*----
Various manipulations on the fac of of the relative record
number field.
------*/
void dte_hide_rel_field()
{
	fac_one_field(&dte_relative_record,HIDE_FIELD);
}

static void fac_one_field(FIELD *fld,int code)
{
	fac_t *fac;

	if(!fld->frow)
		return;
	/* Point fac at the field then backup one */
	fac = (fac_t *)(fld->fld - 1);
	switch(code)
		{
		case UNPROTECT_ALL:		/*unprotect all*/
			*fac = fld->fac;
			break;
		case PROTECT_KEY:	/*protect keys*/
			if(fld->keynum == -1)
				*fac = DISPLAY_FAC;
			break;
		case PROTECT_ALL:		/*protect_all*/
			*fac = DISPLAY_FAC;
			break;
		case PROTECT_NOMOD:		/*protect nomods*/
			if((fld->display == 2) || (fld->is_accum))
				*fac = DISPLAY_FAC;
			break;
		case HIDE_FIELD:
			*fac = HIDE_FAC;
			break;
		}

}

void dte_hide_all_prompts()
{
	fac_all_prompts(HIDE_FIELD);
}

void dte_display_all_prompts()
{
	fac_all_prompts(DIM_FIELD);
}

void dte_display_idx_prompts(char *idx)
{
	int key,i;

	key = dte_key_from_idx(idx);
	for(i = 0; dtefld[i].name[0] > ' '; ++i )
		{
		if(dte_is_this_key(&dtefld[i],key))
			fac_one_prompt(&dtefld[i],DIM_FIELD);
		}
}

static void fac_all_prompts(int code)
{
	int i;

	for(i = 0; dtefld[i].name[0] > ' '; ++i )
		{
		fac_one_prompt(&dtefld[i],code);
		}
}

/*----rel 22-Mar-1990
The fac of the prompt fo rthe record number.
------*/
void dte_hide_rel_prompt()
{
	fac_one_prompt(&dte_relative_record,HIDE_FIELD);
}

void dte_display_rel_prompt()
{
	fac_one_prompt(&dte_relative_record,DIM_FIELD);
}

static void fac_one_prompt(FIELD *fld,int code)
{
	char *fac;

	if(!fld->frow)
		return;
	fac = fld->pfac;
	switch(code)
		{
		case HIDE_FIELD:
			*fac = HIDE_FAC;
			break;
		case DIM_FIELD:
			*fac = DIM_FAC;
			break;
		}

}
/*
**	History:
**	$Log: daux.c,v $
**	Revision 1.12  2003/02/05 21:47:53  gsl
**	fix -Wall warnings
**	
**	Revision 1.11  2003/02/04 19:19:09  gsl
**	fix header
**	
**	Revision 1.10  2002/10/24 14:20:40  gsl
**	Make globals unique
**	
**	Revision 1.9  2002/10/23 21:07:28  gsl
**	make global name unique
**	
**	Revision 1.8  2002/10/23 20:39:10  gsl
**	make global name unique
**	
**	Revision 1.7  2002/10/17 21:22:41  gsl
**	cleanup
**	
**	Revision 1.6  2002/10/17 17:17:16  gsl
**	Removed VAX VMS code
**	
**	Revision 1.5  2002/07/25 15:20:29  gsl
**	Globals
**	
**	Revision 1.4  1996/09/25 01:01:40  gsl
**	Cast (fac_t *) to fix a warning
**	
**	Revision 1.3  1996-09-17 16:45:32-07  gsl
**	drcs update
**
**
**
*/
