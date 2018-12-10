/*
******************************************************************************
**
** KCSI - King Computer Services Inc.
**
** 
******************************************************************************
*/

/*----
1.	An unpacking table is built with codes for unpacking
	the input record(s) into the output record.
2.	Sort parameters are updated with position and length
3.	Data limits are update similarly.
4.	New Field definitions are also updated.

The report writer is primarily driven by position length and type of a field
within the record while the raw parameters are provided as field names. Without
this logic, every field would have to be processed by looking up the
field name in the rfl strucuture, and then extracting the parameters from
there.
------*/
#include <stdio.h>
#include <ctype.h>
#include "dtype.h"
#include "rptprm.h"
#include "rptcob.h"
#include "rptsrt.h"
#include "rptglb.h"
#include "shrthand.h"
#include "cridebug.h"
#include "kcsio.h"
#include "kcsifunc.h"


/*----
Indexes for the elements.
------*/
static int rflidx,rcbidx,nfidx,nfoidx,dlidx,dloidx,srtidx;

static RPT_RFL *rfl;
static RPT_NF  *nf;
static RPT_NFO *nfo;
static RPT_DL  *dl;
static RPT_DLO *dlo;
static RPT_RCB *rcb;
static RPT_SRT *srt;
/*----
Some Shorthand
------*/

static int locate_this_nf(RPT_RFL *r,RPT_NF *n,int d);
static int locate_one_rfl(RPT_RFL *r,int d);
static int val_for_cmb(int op,int type);
static int val_for_cmp(char *op);
static int same_element(ELEMENT *e1,ELEMENT *e2);
static int count_lit_dec(char *str);



/*----
Check if the passed rfl matches any nfo.
If it does, then have the nfo point to it.
------*/
static void locate_one_nfo(r)
RPT_RFL *r;
{
	RPT_NF *sav_nf;
	int sav_nfidx;

	TRACE_IN("locate_one_nfo");
	sav_nfidx = nfidx;
	sav_nf = nf;

	for(nfidx = 0,nf = &rpt_nf[0];
			nfidx < NF_ENTRY_COUNT;
			++nf,++nfidx)
		{
		if(nf->_e._name[0] > ' ' )
			{
			for(nfoidx = 0, nfo = &nf->_o[0];
			nfoidx < NF_OP_ENTRY_COUNT;
			++nfoidx, ++nfo)
				{
				if(same_element(&nfo->_e,&r->_e))
					nfo->_pnew = &r->_new;
				}
			}
		}
	nfidx = sav_nfidx;
	nf = sav_nf;
	TRACE_OUT("locate_one_nfo");
}

static int locate_rfls()
{
	int d;

	TRACE_IN("locate_rfls");
/*
 * One pass to resolve real fields and
 * real fields used as operands in new field defs.
 */
	d = 0;
	for(rflidx =0,rfl = &rpt_rfl[0];
			rflidx < RFL_ENTRY_COUNT;
			++rfl,++rflidx)
		{
		if(rfl->_e._name[0] == 0)
			{
			rpt_rfl[rflidx]._new = NULL;
			rpt_rfl[rflidx]._old = NULL;
			}
		else
		if((rfl->_e._name[0] > ' ') && ( rfl->_e._origin != 3))
			{
			d = locate_one_rfl(rfl,d);
			locate_one_nfo(rfl);
			}
		}
	
	TRACE_OUT("locate_rfls");
	return(d);
	
}

/*----
The unpack table provides a quick table of conversions so that records
may be read smoothly into the new output record.

1. Input length is as specified.
2. Input type is translated to one of the conversion types
3. INput position is set up with a base from a record (depending on origin)
   plus the position offset.

1. Output type is either unsigned_ascii zoned_ascii or characters.
2. Output length is 16 for numerics or the character length for char fields
3. Output position is the passed record position. This is modified by
   the length of the output field and the new position is returned.
------*/
static int locate_one_rfl(RPT_RFL *r,int d)
{
	DTYPE *n,*o;

	TRACE_IN("locate_one_rfl");
	o = r->_old;
	n = r->_new;

/*
 * Use the occurrence to establish the correct position in the
 * old record.
 */
	o->_pos += (o->_len * (r->_e._occurrence - 1));	

	if(r->_e._origin == 2)
		o->_base = rpt_inp_rec2;
	else
		o->_base = rpt_inp_rec1;

	n->_base = rpt_inp_rec;
	n->_pos = d;
	switch(o->_type)
		{
		case ACHR:
			n->_type = ACHR;
			n->_len = o->_len;
			break;
		case AUNS:
			n->_type = BUNS;
			n->_len  = BUNS_LEN;
			break;
		default:
			n->_type = BZON;
			n->_len  = BZON_LEN;
			break;
		}
	n->_dec = o->_dec;
	d += n->_len;
	TRACE_OUT("locate_one_rfl");
	return(d);
}


/*----
Determine the new position for a manufactured field.
1.	A new field is built at a space cleared for it in the
	structure with an offset of zero. The DTYPE element
	_fld is provided to point at this new area.
2.	
------*/
static int locate_nfs(d)
int d;
{
	TRACE_IN("locate_nfs");
	for(nfidx = 0,nf = &rpt_nf[0];
			nfidx < NF_ENTRY_COUNT;
			++nf,++nfidx)
		{
/* added to skip excess initialization */
		if(nf->_e._name[0] < ' ' )
			continue;
		nf->_fld._base = rpt_bld_fld;
		nf->_fld._pos  = 0;
		for(rflidx =0,rfl = &rpt_rfl[0];
			(rfl->_new)&&(rflidx < RFL_ENTRY_COUNT);
			++rfl,++rflidx)
			{
/* added to skip excess initialization */
			if(rfl->_e._name[0] == 0)
				continue;
			if(same_element(&rfl->_e,&nf->_e))
				{
				d = locate_this_nf(rfl,nf,d);
				locate_one_nfo(rfl);
				}
			}
		}
	TRACE_OUT("locate_nfs");
	return(d);
}

/*----
The ultimate resting place of a new field is at the current position
in the new record with the data typing being decided by parameters
passed in from COBOL. After situating it, the record position is updated.
------*/
static int locate_this_nf(RPT_RFL *r,RPT_NF *n,int d)
{
	int len;


	TRACE_IN("locate_this_nf");
	n->_pnew = &r->_new;
	r->_nf = n;
	if(n->_fld._type & IS_NUM)
		{
		n->_fld._type = BZON;
		n->_fld._len = BZON_LEN;
		}
	len = n->_fld._len;
	r->_new->_pos = d;
	r->_new->_len = len;
	r->_new->_type = n->_fld._type;
	r->_new->_dec = n->_fld._dec;
	r->_new->_base = rpt_inp_rec;
	TRACE_OUT("locate_this_nf");
	return(d + len);

}
/*----
For a new field to be legal:
1.	A first operand must exist.
2.	Every element must be locatable.
3.	The operators must agree with the operand type
------*/
static void val_this_nf_bld(n)
RPT_NF *n;
{
	RPT_NFO *nfo2;

	TRACE_IN("val_this_nf_bld");
	nf = n;
	nfo2 = &nf->_o[1];
	for(nfoidx = 0, nfo = &nf->_o[0];
			nfoidx < NF_OP_ENTRY_COUNT;
			++nfoidx, ++nfo)
		{
/* No first operand */
		if ( nfoidx == 0)
			{
			if( nfo->_e._name[0] <= ' ')
				{
				/* nf->_pnew == NULL; */
				break;
				}
			}
/* Illegal operand (location not known)*/
		if ((nfo->_e._name[0] > ' ') && (nfo->_pnew == NULL))
			{
			/* nf->_pnew == NULL; */
			break;
			}
/* If op code is not okay for this type of combine */
		if(!val_for_cmb(nfo->_code,nf->_fld._type))
			{
			nfo->_code = 0;
			break;
			}
/* Were at the end of the list */
		if ( nfoidx == (NF_OP_ENTRY_COUNT - 1))
			break;
/* Next operand doesn't exist */
		if( (nfo2->_not_lit) &&
		    (nfo2->_e._name[0] <= ' ') )
			{
			nfo->_code = 0;
			break;
			}
		++nfo2;
		}
	TRACE_OUT("val_this_nf_bld");
}


/*----
This is designed to prevent a new field slipping through that does
not have all the pieces necessary to build it.
------*/
static void val_nf_bld()
{

	TRACE_IN("val_nf_bld");
	for(rflidx =0,rfl = &rpt_rfl[0];
			(rfl->_new)&&(rflidx < RFL_ENTRY_COUNT);
			++rfl,++rflidx)
		{
		if(rfl->_e._origin == 3)
			{
			for(nfidx = 0,nf = &rpt_nf[0];
			nfidx < NF_ENTRY_COUNT;
			++nf,++nfidx)
				{
/* added to skip excess initialization */
				if(nf->_e._name[0] < '!' )
					continue;
				if(same_element(&rfl->_e,&nf->_e))
					{
					val_this_nf_bld(nf);
					}
				}
			}
		}
	TRACE_OUT("val_nf_bld");
}

/*----
For a data limit to be legal:
1.	A first operand must exist.
2.	Every element must be locatable.
3.	The operators must be legit.
------*/
static void val_this_dl_bld(dl)
RPT_DL *dl;
{

	TRACE_IN("val_this_dl_bld");
	for(dloidx = 0, dlo = &dl->_o[0];
			dloidx < DL_OP_ENTRY_COUNT;
			++dloidx, ++dlo)
		{
/* If op code is not okay for a compare or name not active
 * then zero out the request
 */
		if((!val_for_cmp(dlo->_code)) || (dlo->_e._name[0] < ' '))
			{
			dlo->_code[0] = 0;
			dlo->_e._name[0] = 0;
			break;
			}
/* No first operand */
		if ( dloidx == 0)
			{
			if(dlo->_e._name[0] < ' ')
				{
				/* dl->_pnew == NULL; */
				/* dl->_e._name[0] == 0; */
				break;
				}
			}
/* Illegal operand (location not known)*/
		if ((dlo->_e._name[0] > ' ') && (dlo->_pnew == NULL))
			{
			/* dl->_pnew == NULL; */
			/* dl->_e._name[0] == 0; */
			break;
			}
		}
	TRACE_OUT("val_this_dl_bld");
}

/*----
This is designed to prevent a data limit slipping through that does
not have all the pieces necessary to do the comparison.
------*/
static void val_dl_bld()
{

	TRACE_IN("val_dl_bld");
	for(rflidx =0,rfl = &rpt_rfl[0];
			(rfl->_new)&&(rflidx < RFL_ENTRY_COUNT);
			++rfl,++rflidx)
		{
/* added to skip excess initialization */
		if(rfl->_e._name[0] == 0)
			continue;
		for(dlidx = 0, dl = &rpt_dl[0];
			dlidx < LMG_DL_ENTRY_COUNT;
			++dlidx, ++dl)
			{
/* added to skip excess initialization */
			if(dl->_e._name[0] <= ' ')
				continue;
			if(same_element(&rfl->_e,&dl->_e))
				{
				val_this_dl_bld(dl);
				}
			}
		}
	TRACE_OUT("val_dl_bld");
}

static int val_for_cmb(int op,int type)
{
	TRACE_IN("val_for_cmb");
	if(type & IS_NUM)
		{
		if(op == '&')
			{
			TRACE_OUT("val_for_cmb");
			return(0);
			}
		else
			{
			TRACE_OUT("val_for_cmb");
			return(1);
			}
		}
	else
		{
		if(op == '&')
			{
			TRACE_OUT("val_for_cmb");
			return(1);
			}
		else
			{
			TRACE_OUT("val_for_cmb");
			return(0);
			}
		}

}
static int val_for_cmp(char *op)
{
	if(Streq(op,"EQ"))
		{
		TRACE_OUT("val_for_cmp");
		return(1);
		}
	else
	if(Streq(op,"GT"))
		{
		TRACE_OUT("val_for_cmp");
		return(1);
		}
	else
	if(Streq(op,"LT"))
		{
		TRACE_OUT("val_for_cmp");
		return(1);
		}
	else
	if(Streq(op,"NE"))
		{
		TRACE_OUT("val_for_cmp");
		return(1);
		}
	else
	if(Streq(op,"GE"))
		{
		TRACE_OUT("val_for_cmp");
		return(1);
		}
	else
	if(Streq(op,"LE"))
		{
		TRACE_OUT("val_for_cmp");
		return(1);
		}
	TRACE_OUT("val_for_cmp");
	return(0);
}
/*----
The sorts. The rcbs and the header record. This version of report
forces sorts only on the primary file.
------*/
static void tie_up_srts()
{

	TRACE_IN("tie_up_srts");
/* 1. Sorts */
	for(srtidx = 0, srt = &rpt_srt[0];
			srtidx < SRT_ENTRY_COUNT;
			++srtidx, ++srt)
		{
		if(srt->_e._name[0] <= ' ')
			continue;
		for(rflidx =0,rfl = &rpt_rfl[0];
			(rfl->_new)&&(rflidx < RFL_ENTRY_COUNT);
			++rfl,++rflidx)
			{
			if(rfl->_e._name[0] <= ' ')
				continue;
/* 2. Secondary columns flag.*/
			if(rfl->_col_sub_head[0])
				rpt_rhd._column_subs = 1;
			if(same_element(&rfl->_e,&srt->_e))
				{
				srt->_pnew = &rfl->_new;
				srt->_pold = &rfl->_old;
				rpt_rhd._sort = 1;
				if(rpt_opt._ext_sort)
					{
					rpt_sort[srtidx]._pos = rfl->_old->_pos;
					rpt_sort[srtidx]._len = rfl->_old->_len;
					}
				else
					{
					rpt_sort[srtidx]._pos = rfl->_new->_pos;
					rpt_sort[srtidx]._len = rfl->_new->_len;
					}
				rpt_sort[srtidx]._order = srt->_order;
				rpt_sort[srtidx]._numeric = rfl->_new->_type & IS_NUM ;
				switch(rfl->_old->_type)
					{
					default:
					case AUNS:
					case ACHR:
						rpt_sort[srtidx]._type = 'C';
						break;
					case APCK:
						rpt_sort[srtidx]._type = 'P';
						break;
					case AZON:
						rpt_sort[srtidx]._type = 'Z';
						break;
					case ABLH:    /* VAX Binary */
					case ABIN:
						rpt_sort[srtidx]._type = 'B';
						break;
					case ABMN:   /* Machine binary MF */
						rpt_sort[srtidx]._type = 'N';
						break;
					}
				}
			}
		}
	TRACE_OUT("tie_up_srts");
}

static void tie_up_rcbs()
{

	TRACE_IN("tie_up_rcbs");
/* 1. RCBS */
	for(rcbidx = 0, rcb = &rpt_rcb[0];
			rcbidx < RCB_ENTRY_COUNT;
			++rcbidx, ++rcb)
		{
		if(rcb->_e._name[0] <= ' ')
			continue;
		for(rflidx =0,rfl = &rpt_rfl[0];
			(rfl->_new)&&(rflidx < RFL_ENTRY_COUNT);
			++rfl,++rflidx)
			{
			if(rfl->_e._name[0] <= ' ')
				continue;
			if(same_element(&rfl->_e,&rcb->_e))
				{
				rcb->_pnew = &rfl->_new;
				rcb->_rfl = rfl;
				rfl->_rcbidx = rcbidx;
				rfl->_is_a_break = 1;
				rpt_rhd._control_fields = 1;
				rfl->_repeat_code = rcb->_repeat_code;
				}
			}
		}
	TRACE_OUT("tie_up_rcbs");
}
/*----
Match the primary or link to secondary key and fill it in
Because the link is done before the record is expanded, the
link is done through the old position and length.
This is set so that the occurs must match prim only
TESTCHECK how does occurs affect link from input1 to input2
------*/
static void tie_up_rhd()
{
	RPT_RFL *rfl;


	TRACE_IN("tie_up_rhd");
	if(rpt_rhd._key_to_sec[0] < '!')
		goto the_exit;
/*
	ELEMENT *e;
	for(rflidx = 0,rfl = &rpt_rfl[0];
			(rfl->_new)&&(rflidx < RFL_ENTRY_COUNT);
			++rfl,++rflidx)
		{
		if(rfl->_e._name[0] <= ' ')
			continue;
		e = &rfl->_e;
		if(Streq(e->_name,rpt_rhd._key_to_sec))
			{
			if((e->_origin == 1)
			    && (e->_occurrence == rpt_rhd._occurrence))
				{
				rpt_rhd._sec_file = 1;
				rpt_rhd._old_key1 = rfl->_old;
				break;
				}
			}

		}
*/
	rpt_rhd._sec_file = 1;
	rpt_rhd._old_key1 = rpt_rfl[80]._old;
	if(rpt_key_to_sec_idx > -1)
		{
		rfl = &rpt_rfl[rpt_key_to_sec_idx];
		rpt_rhd._old_key2 = rfl->_old;
		}
the_exit:
	TRACE_OUT("tie_up_rhd");
}

/*----
		DATA LIMIT FIELDS.
------*/
static void tie_up_dls()
{
	
	TRACE_IN("tie_up_dls");
	for(rflidx =0,rfl = &rpt_rfl[0];
			(rfl->_new)&&(rflidx < RFL_ENTRY_COUNT);
			++rfl,++rflidx)
		{
		if(rfl->_e._name[0] <= ' ')
			continue;
		for(dlidx = 0, dl = &rpt_dl[0];
			dlidx < LMG_DL_ENTRY_COUNT;
			++dlidx, ++dl)
			{
			if(dl->_e._name[0] <= ' ')
				continue;
			if(same_element(&rfl->_e,&dl->_e))
				{
				dl->_pnew = &rfl->_new;
				rpt_rhd._data_limit = 1;
				}
			for(dloidx = 0, dlo = &dl->_o[0];
				dloidx < DL_OP_ENTRY_COUNT;
				++dloidx, ++dlo)
				{
				if(dlo->_e._name[0] <= ' ')
					continue;
				if(same_element(&rfl->_e,&dlo->_e))
					{
					dlo->_pnew = &rfl->_new;
					}
				}
			}
		}
	TRACE_OUT("tie_up_dls");
}
	
/*----
			LITERALS
------*/
/*----
Every New field operand caries a spare DTYPE data item that
is set up to point at the literal. These are filled in.
------*/
static void tie_up_nfo_lits()
{
	TRACE_IN("tie_up_nfo_lits");
	for(nfidx = 0,nf = &rpt_nf[0];
			nfidx < NF_ENTRY_COUNT;
			++nf,++nfidx)
		{
		if(nf->_e._name[0] <= ' ')
			continue;
		for(nfoidx = 0, nfo = &nf->_o[0];
			nfoidx < NF_OP_ENTRY_COUNT;
			++nfoidx, ++nfo)
			{
			if(!(nfo->_not_lit))
				{
				nfo->_lit_op._base = nfo->_e._name;
				nfo->_lit_op._pos = 0;
				if(nf->_fld._type & IS_NUM)
					nfo->_lit_op._type = NLIT;
				else
					nfo->_lit_op._type = nf->_fld._type;
				nfo->_lit_op._len = strlen(nfo->_e._name);
				nfo->_lit_op._dec =
				    count_lit_dec(nfo->_lit_op._base);
				}
			}
		}
	TRACE_OUT("tie_up_nfo_lits");
}
/*----
Every Lit field operand caries a spare DTYPE data item that
is set up to point at the literal. These are filled in.
------*/
static void tie_up_dlo_lits()
{
	TRACE_IN("tie_up_dlo_lits");
	for(dlidx = 0, dl = &rpt_dl[0];
			dlidx < LMG_DL_ENTRY_COUNT;
			++dlidx, ++dl)
		{
		if((dl->_e._name[0] == 0) || (!dl->_pnew))
			continue;
		for(dloidx = 0, dlo = &dl->_o[0];
			dloidx < DL_OP_ENTRY_COUNT;
			++dloidx, ++dlo)
			{
			if(!(dlo->_not_lit))
				{
				dlo->_lit_op._base = dlo->_e._name;
				dlo->_lit_op._pos = 0;
				if((*dl->_pnew)->_type & IS_NUM)
					dlo->_lit_op._type = NLIT;
				else
					dlo->_lit_op._type = (*dl->_pnew)->_type;
				dlo->_lit_op._len = strlen(dlo->_e._name);
				dlo->_lit_op._dec =
				    count_lit_dec(dlo->_lit_op._base);
				}
			}
		}
	TRACE_OUT("tie_up_dlo_lits");
}
static int count_lit_dec(char *str)
{
	int dec;

	TRACE_IN("count_lit_dec");
	dec = -1;
	while(*str)
		{
		if(*str == '.')
			dec = 0;
		else
		if((isdigit((int)*str)) && (dec != -1))
			++dec;
		++str;
		}
	if(dec == -1)
		dec = 0;
	TRACE_OUT("count_lit_dec");
	return(dec);
}

/*----
			UTILITY
------*/

/*----
Elements are the same when all parts match.
------*/
static int same_element(ELEMENT *e1,ELEMENT *e2)
{
	int rc;
	
	TRACE_IN("same_element");
	rc = 1;
	if(e1->_origin != e2->_origin)
		rc = 0;
	if(e1->_occurrence != e2->_occurrence)
		rc = 0;
	if(strcmp(e1->_name,e2->_name))
		rc = 0;
	TRACE_OUT("same_element");
	return(rc);
}

/*----
Process the rfl several times looking for matches in
the other structures.
Do whatever tieing up is necessary to ensure that pointers and codes
match.
------*/
void rpt_tie()
{
	int d;

	TRACE_IN("rpt_tie");
/*
 * Clear the special processing flags. Although these are supposed
 * to be set up on entry. This routine fills them in based on the
 * actual data rather than trusting the passed values.
 */
	rpt_rhd._sec_file =
	rpt_rhd._sort =
	rpt_rhd._data_limit =
	rpt_rhd._control_fields =
	rpt_rhd._column_subs = 0;
/*
 * Clear the pointers that will be filled in.
 */
	rpt_rhd._old_key1 =
	rpt_rhd._old_key2 =
	0;
/*
 * Clear the request to the sort routine.
 */
	rpt_sort_clear(rpt_sort);
/*
 * Locate real and manufacutured fields.
 */
	d = locate_rfls();
	d = locate_nfs(d);
	rpt_inp_rec_len = d;

	tie_up_nfo_lits();
/*
 * In theory all the real and manucfatured fields and the operands to
 * build them now have locations in the new record.
 * Now we validate that a build will work for new fields. If it
 * will not then the field is discarded.
 */
	val_nf_bld();
	val_dl_bld();
/*
 * Once we have all the real and imaginary fields nailed down and
 * errors tossed out, then the sort and control breaks can be tied
 * to field values (and data limits).
 */
	tie_up_dls();
	tie_up_dlo_lits();

	tie_up_srts();
	tie_up_rcbs();
	tie_up_rhd();
	TRACE_OUT("rpt_tie");
}	

/*
**	History:
**	$Log: rtie.c,v $
**	Revision 1.10  2003/02/20 19:29:54  gsl
**	fix -Wall warnings
**	
**	Revision 1.9  2003/02/04 19:19:08  gsl
**	fix header
**	
**	Revision 1.8  2002/10/24 15:48:31  gsl
**	Make globals unique
**	
**	Revision 1.7  2002/10/24 14:20:34  gsl
**	Make globals unique
**	
**	Revision 1.6  2002/10/23 21:07:25  gsl
**	make global name unique
**	
**	Revision 1.5  2002/10/17 21:22:43  gsl
**	cleanup
**	
**	Revision 1.4  2000/06/13 22:14:35  gsl
**	Restore back to revision 1.2.
**	The changes caused a bug 1.3 (2.97 and 2.98)
**	The dl (Data Limits) structure was being corrupted so the limits (A EQ 'X')
**	were being removed, thus you always get all the records.
**	
**	Revision 1.3  1999-09-13 15:52:48-04  gsl
**	fix "==" vs "="
**
**	Revision 1.2  1996-09-17 19:45:50-04  gsl
**	drcs update
**
**
**
*/
