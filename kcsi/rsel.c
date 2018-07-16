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
			SELECT LOGIC

Is based on the presumption that the record has been built.

A selection consists of 10 sets of 10 conditions

Each condition is connected by an AND/OR and each set is connected
by and AND/OR

FIELD1	GT	24	AND
	LT	100	OR		<-- Set of three conditions
	=	999

	AND				<-- Set connector

FIELD2	=	FIELD3	OR
	=	66			<-- Set of 2 conditions


All are resolved by resolving ANDs first and then ORs

Each set is resolved into a table. When the set is resolved
the results are stored in a set_table. When all sets are resolved
the set_table is resolved to one condition.
------*/

#include <stdio.h>

#include "dtype.h"
#include "rptprm.h"
#include "rptsrt.h"
#include "rptglb.h"
#include "shrthand.h"
#include "kcsio.h"
#include "rlmg.h"
#include "kcsifunc.h"

typedef struct _truth{
	int _result;
	int _connector;
	}TT;



static int csel(DTYPE *l,RPT_DLO *dlo);
static int resolve_table(TT *t);


/*----
Select for listmgmt.
1.	If its a record range then do it and skip out.
2.	If test for sero and space does not eliminate the field then 3
3.	Test as usual
------*/
static void lmg_select(dl,tbl,l_new)
RPT_DL *dl;
TT *tbl;
DTYPE *l_new;
{
	DTYPE *hi;

	if(dl->lmg_rec_no)
		{
		hi = &dl->_o[1]._lit_op;
		tbl->_result = KCSI_comp_rec_num(rpt_total_records,l_new,hi);
		return;
		}
/*
	tbl->_result = 1;
	if((dl->lmg_skip_zero) &&  (l_new->_type & IS_NUM))
		tbl->_result = (!(KCSI_comp_zero(l_new)));
	if((dl->lmg_skip_space) &&  (!(l_new->_type & IS_NUM)))
		tbl->_result = (!(KCSI_comp_space(l_new)));
	if(tbl->_result)
		tbl->_result = csel(l_new,&dl->_o[0]);
*/
	tbl->_result = 0;
	if(l_new->_type & IS_NUM)
		{
		if(KCSI_comp_zero(l_new))
			{
			if(dl->lmg_skip_zero == EXCLUDE_THEM)
				{
				tbl->_result = 0;
				return;
				}
			else
			if(dl->lmg_skip_zero == INCLUDE_THEM)
				{
				tbl->_result = 1;
				return;
				}
			}
		}
	if(!(l_new->_type & IS_NUM))
		{
		if(KCSI_comp_space(l_new))
			{
			if(dl->lmg_skip_space == EXCLUDE_THEM)
				{
				tbl->_result = 0;
				return;
				}
			else
			if(dl->lmg_skip_space == INCLUDE_THEM)
				{
				tbl->_result = 1;
				return;
				}
			}
		}
	tbl->_result = csel(l_new,&dl->_o[0]);
}

/*----
Select for report
------*/
static void rpt_select(dl,tbl,l_new)
RPT_DL *dl;
TT *tbl;
DTYPE *l_new;
{
	tbl->_result = csel(l_new,&dl->_o[0]);
}

static void clear_table(t)
TT *t;
{
	int i;
	for(i = 0; i < LMG_DL_ENTRY_COUNT ; ++i)
		memset(t++,0,sizeof(TT));
}


/*----
The ctable does not need to be LMG_DL_ENTRY_COUNT long, but
stable and ctable use the same routines for clearing and resolving the
table so we do need them the same size. Only 10 are used in csel
------*/
static int csel(DTYPE *l,RPT_DLO *dlo)
{
	TT ctable[LMG_DL_ENTRY_COUNT];
	int cidx;
	DTYPE *l_new;

	clear_table(ctable);
	ctable[0]._result = 1;
	for(cidx = 0; cidx < 10 ; ++cidx)
		{
		if(dlo->_code[0] == 0)
			break;
		if(dlo->_not_lit)
			l_new = *dlo->_pnew;
		else
			l_new = &dlo->_lit_op;
		ctable[cidx]._result = KCSI_rptcmp(l,dlo->_code,l_new);
		if(!(ctable[cidx]._connector = dlo->_connector))
			break;
		++dlo;
/* No next connector if it is the last comparison */
		if(cidx == 9)
			{
			ctable[cidx]._connector = 0;
			break;
			}
/* Or the next operator doesn't exist */
		if(dlo->_code[0] == 0)
			{
			ctable[cidx]._connector = 0;
			break;
			}

		}
	return(resolve_table(ctable));
}


static int resolve_table(TT *t)
{
	TT *sav_t,*s;
	int i,maxidx;

	if(*rpt_caller == 'R')
		maxidx = DL_ENTRY_COUNT;
	else
		maxidx = LMG_DL_ENTRY_COUNT;
	sav_t = s = t;
	++t;
	for(i = 0; i < (maxidx - 1) ; ++i)
		{
		if(s->_connector == 0)
			break;
		else
		if(s->_connector == 'A')
			{
			s->_result &= t->_result;
			s->_connector = t->_connector;
			++t;
			}
		else
			{
			++s;
			++t;
			}		
		}
	s = t = sav_t;
	++t;
	for(i = 0; i < (maxidx - 1) ; ++i)
		{
		if(s->_connector == 0)
			break;
		else
		if(s->_connector == 'O')
			{
			s->_result |= t->_result;
			s->_connector = t->_connector;
			++t;
			}
		}
	return(sav_t->_result);
}

int KCSI_rptsel()
{
	RPT_DL *dl;
	TT stable[LMG_DL_ENTRY_COUNT];
	int sidx,maxidx;
	DTYPE *l_new;

	dl = rpt_def._dl;

	if(*rpt_caller == 'R')
		maxidx = DL_ENTRY_COUNT;
	else
		maxidx = LMG_DL_ENTRY_COUNT;

	clear_table(stable);
/*
 * If there are no conditions then prepare to return true.
 */
	stable[0]._result = 1;
/* Check all 10 data sets */
	for(sidx = 0 ; sidx < maxidx ; ++sidx)
		{
		if((dl->_e._name[0] == 0) || (!dl->_pnew))
			break;
		l_new = *dl->_pnew;
/*
 * Special tests when rpt_caller is listmgmt. First check if we
 * are skipping spaces or zeroes. If so test if the field is such
 * and skip else test as usual.
 */
		if(*rpt_caller < ':')
			lmg_select(dl,&stable[sidx],l_new);
		else
			rpt_select(dl,&stable[sidx],l_new);
		if(!(stable[sidx]._connector = dl->_set_connector))
			break;
		++dl;
/* No next set connector if it is the last set */
		if(sidx == (maxidx - 1))
			{
			stable[sidx]._connector = 0;
			break;
			}
/* Or the next set has no value */
		if((dl->_e._name[0] == 0) || (!dl->_pnew))
			{
			stable[sidx]._connector = 0;
			break;
			}

		}
	return(resolve_table(stable));
}
/*
**	History:
**	$Log: rsel.c,v $
**	Revision 1.6  2003/02/04 19:19:08  gsl
**	fix header
**	
**	Revision 1.5  2002/10/24 14:20:34  gsl
**	Make globals unique
**	
**	Revision 1.4  2002/10/23 20:39:06  gsl
**	make global name unique
**	
**	Revision 1.3  2002/10/17 17:56:19  gsl
**	Rename variables new to l_new
**	
**	Revision 1.2  1996/09/17 23:45:49  gsl
**	drcs update
**	
**
**
*/
