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
Globals.
------*/
#include <stdio.h>
#include "dtype.h"
#include "rptcob.h"
#include "rptprm.h"
#include "rptsrt.h"
#include "kcsio.h"
#include "kcsifunc.h"

/*----
Report Options conversions and fielding. Generates actual code.
------*/

/*----
These 81 elements are the critcal path thru the data. They contain
where the field was, and where it is in the new record. The work field
is used for conversions.
------*/


void rpt_globals()
{
	/* NULL routine to force  linker to resolve this module */
}

DTYPE	rpt_old[RFL_ENTRY_COUNT + 1];
DTYPE	rpt_new[RFL_ENTRY_COUNT + 1];
DTYPE	wrk_dtype;

RPT_OPT	rpt_opt;

DTYPE rpt_opt_src[]={
	{RPT_ID},
	{RPT_DATE},
/*	{RPT_DEVICE},		Special Conversion	*/
/*	{RPT_FILES},		Not used		*/
	{RPT_OPTION},
	{RPT_ONLY},
	{RPT_PAGE},
	{RPT_LINE_1},
	{RPT_LINE_2},
	{RPT_LINE_3},
	{RPT_SPACING_1},
	{RPT_SPACING_2},
	{RPT_SPACING_3},
	{RPT_EXT_SORT},
	{NULL,0,0,0,0}};

DTYPE rpt_opt_dest[]={
	{rpt_opt._id,0,0,BTRN,0},
	{rpt_opt._date,0,0,BTRN,0},
/*	{rpt_opt._device,0,0,CINT,0},	Special conversion*/
	{(char*)&rpt_opt._option,0,0,CINT,0},
	{(char*)&rpt_opt._only,0,0,CINT,0},
	{(char*)&rpt_opt._page,0,0,CINT,0},
	{(char*)&rpt_opt._line[0],0,0,CINT,0},
	{(char*)&rpt_opt._line[1],0,0,CINT,0},
	{(char*)&rpt_opt._line[2],0,0,CINT,0},
	{(char*)&rpt_opt._spacing[0],0,0,CINT,0},
	{(char*)&rpt_opt._spacing[1],0,0,CINT,0},
	{(char*)&rpt_opt._spacing[2],0,0,CINT,0},
	{(char*)&rpt_opt._ext_sort,0,0,CINT,0},
	{NULL,0,0,0,0}};

RPT_RHD rpt_rhd;
DTYPE rpt_rhd_src[]={
	{RHD_SEC_FILE},
	{RHD_SORT},
	{RHD_DATA_LIMIT},
	{RHD_CONTROL_FIELDS},
	{RHD_COLUMN_SUBS},
	{NULL,0,0,0,0}};
DTYPE rpt_rhd_dest[]={
	{(char*)&rpt_rhd._sec_file,0,0,CINT,0},
	{(char*)&rpt_rhd._sort,0,0,CINT,0},
	{(char*)&rpt_rhd._data_limit,0,0,CINT,0},
	{(char*)&rpt_rhd._control_fields,0,0,CINT,0},
	{(char*)&rpt_rhd._column_subs,0,0,CINT,0},
	{NULL,0,0,0,0}};

DTYPE rpt_rdf_src[]={
	{RDF_KEY_TO_SEC},
	{RDF_KEY_OCCURRENCE},
	{NULL,0,0,0,0}};
DTYPE rpt_rdf_dest[]={
	{rpt_rhd._key_to_sec,0,0,BTRN,0},
	{(char*)&rpt_rhd._occurrence,0,0,BONE,0},
	{NULL,0,0,0,0}};
/*------
Because this is an array, it is processed into a work area first
and then copied to the actual element.
------*/
RPT_RCB wrk_rcb;
RPT_RCB rpt_rcb[RCB_ENTRY_COUNT + 1];
DTYPE rpt_rcb_src[]={
	{RCB_NAME},
	{RCB_OCCURRENCE},
	{RCB_ORIGIN},
	{RCB_ACTION_CODE},
	{RCB_REPEAT_CODE},
	{NULL,0,0,0,0}};
DTYPE rpt_rcb_dest[]={
	{wrk_rcb._e._name,0,0,BTRN,0},
	{(char*)&wrk_rcb._e._occurrence,0,0,BONE,0},
	{(char*)&wrk_rcb._e._origin,0,0,CINT,0},
	{(char*)&wrk_rcb._action_code,0,0,CINT,0},
	{(char*)&wrk_rcb._repeat_code,0,0,CINT,0},
	{NULL,0,0,0,0}};

DTYPE rpt_rcd_src[]={
	{RCD_BREAK},
	{RCD_COL},
	{NULL,0,0,0,0}};
DTYPE rpt_rcd_dest[]={
	{wrk_rcb._break,0,0,BTRN,0},
	{(char*)&wrk_rcb._col,0,0,BPOS,0},
	{NULL,0,0,0,0}};
/*------
Because this is an array, it is processed into a work area first
and then copied to the actual element.
------*/
RPT_RFL wrk_rfl;
RPT_RFL rpt_rfl[RFL_ENTRY_COUNT + 1];
DTYPE rpt_rfl_src[]={
/*	{RFL_LINE},		Special conversion */
	{RFL_SEQ},
	{RFL_NAME},
	{RFL_OCCURRENCE},
	{RFL_COL_HEAD},
	{RFL_COL_SUB_HEAD},
	{RFL_SPACES},
	{RFL_EXT_SIZE},
	{RFL_ZERO_CHAR},
	{RFL_ZERO_COUNT},
	{RFL_SIGN_CONTROL},
	{RFL_DEC_CARRY},
	{RFL_INSERT_1_CHAR},
	{RFL_INSERT_1_COUNT},
	{RFL_INSERT_2_CHAR},
	{RFL_INSERT_2_COUNT},
	{RFL_INSERT_3_CHAR},
	{RFL_INSERT_3_COUNT},
	{RFL_DOLLAR_SIGN},
	{RFL_COMMA},
	{RFL_TOTAL},
	{RFL_MAX},
	{RFL_MIN},
	{RFL_AVG},
	{RFL_ORIGIN},
	{NULL,0,0,0,0}};

DTYPE rpt_rfl_dest[]={
/*	{(char*)&wrk_rfl._line,0,0,CINT,0},		Special conversion */
	{(char*)&wrk_rfl._lseq[0],0,0,CINT,0},
	{wrk_rfl._e._name,0,0,BTRN,0},
	{(char*)&wrk_rfl._e._occurrence,0,0,BONE,0},
	{wrk_rfl._col_head,0,0,BTRN,0},
	{wrk_rfl._col_sub_head,0,0,BTRN,0},
	{(char*)&wrk_rfl._spaces,0,0,CINT,0},
	{(char*)&wrk_rfl._ext_size,0,0,CINT,0},
	{(char*)&wrk_rfl._zero_char,0,0,CCHR,0},
	{(char*)&wrk_rfl._zero_count,0,0,CINT,0},
	{(char*)&wrk_rfl._sign_control,0,0,CINT,0},
	{(char*)&wrk_rfl._dec_carry,0,0,CINT,0},
	{(char*)&wrk_rfl._insert_char[0],0,0,BCHR,0},
	{(char*)&wrk_rfl._insert_count[0],0,0,CINT,0},
	{(char*)&wrk_rfl._insert_char[1],0,0,BCHR,0},
	{(char*)&wrk_rfl._insert_count[1],0,0,CINT,0},
	{(char*)&wrk_rfl._insert_char[2],0,0,BCHR,0},
	{(char*)&wrk_rfl._insert_count[2],0,0,CINT,0},
	{(char*)&wrk_rfl._dollar_sign,0,0,CINT,0},
	{(char*)&wrk_rfl._comma,0,0,CINT,0},
	{(char*)&wrk_rfl._total,0,0,CINT,0},
	{(char*)&wrk_rfl._max,0,0,CINT,0},
	{(char*)&wrk_rfl._min,0,0,CINT,0},
	{(char*)&wrk_rfl._avg,0,0,CINT,0},
	{(char*)&wrk_rfl._e._origin,0,0,CINT,0},
	{NULL,0,0,0,0}};

/*------
Because this is an array, it is processed into a work area first
and then copied to the actual element.
------*/
DTYPE rpt_crf_src[]={
	{CRF_TYPE},
	{CRF_LEN},
	{CRF_POS},
	{CRF_DEC},
/*	{CRF_OCCURRENCES},	*/
	{NULL,0,0,0,0}};

DTYPE rpt_crf_dest[]={
	{(char*)&wrk_dtype._type,0,0,BDTP,0},
	{(char*)&wrk_dtype._len,0,0,CINT,0},
	{(char*)&wrk_dtype._pos,0,0,BPOS,0},
	{(char*)&wrk_dtype._dec,0,0,CINT,0},
/*	{(char*)&wrk_rfl._ctrl_occurrences,0,0,BONE,0},	*/
	{NULL,0,0,0,0}};
/*------
Because this is an array, it is processed into a work area first
and then copied to the actual element.
------*/
RPT_RTT wrk_rtt;
RPT_RTT rpt_rtt[RTT_ENTRY_COUNT + 1];
DTYPE rpt_rtt_src[]={
	{RTT_TITLE},
	{NULL,0,0,0,0}};
DTYPE rpt_rtt_dest[]={
	{wrk_rtt._title,0,0,BTRN,0},
	{NULL,0,0,0,0}};
/*------
Because this is an array, it is processed into a work area first
and then copied to the actual element.
------*/
RPT_SRT wrk_srt;
RPT_SRT rpt_srt[SRT_ENTRY_COUNT + 1];
DTYPE	wrk_sort;
/* DTYPE	_sort[SRT_ENTRY_COUNT + 1]; */

DTYPE rpt_srt_src[]={
	{SRT_NAME},
	{SRT_OCCURRENCE},
	{SRT_ORDER},
	{NULL,0,0,0,0}};
DTYPE rpt_srt_dest[]={
	{wrk_srt._e._name,0,0,BTRN,0},
	{(char*)&wrk_srt._e._occurrence,0,0,BONE,0},
	{(char*)&wrk_srt._order,0,0,CCHR,0},
	{NULL,0,0,0,0}};
/*------
Because this is an array, it is processed into a work area first
and then copied to the actual element. Most of a dlo requires
alternate conversion depending on whether it is a lit or not.
------*/
RPT_DLO wrk_dlo;
DTYPE rpt_dlo_src[]={
	{DL_OP_CODE},
	{DL_OP_CONNECTOR},
	{DL_NOT_LIT_CODE},
	{NULL,0,0,0,0}};
DTYPE rpt_dlo_dest[]={
	{wrk_dlo._code,0,0,BTRN,0},
	{(char*)&wrk_dlo._connector,0,0,CCHR,0},
	{(char*)&wrk_dlo._not_lit,0,0,CCHR,0},
	{NULL,0,0,0,0}};

DTYPE rpt_dlo_lit_src[]={
	{DL_LIT_20},
	{NULL,0,0,0,0}};
DTYPE rpt_dlo_lit_dest[]={
	{wrk_dlo._e._name,0,0,CSTR,0},
	{NULL,0,0,0,0}};

DTYPE rpt_dlo_name_src[]={
	{DL_OP_NAME},
	{DL_OP_OCCURRENCE},
	{DL_OP_ORIGIN},
	{NULL,0,0,0,0}};

DTYPE rpt_dlo_name_dest[]={
	{wrk_dlo._e._name,0,0,BTRN,0},
	{(char*)&wrk_dlo._e._occurrence,0,0,BONE,0},
	{(char*)&wrk_dlo._e._origin,0,0,CINT,0},
	{NULL,0,0,0,0}};

RPT_DL	wrk_dl;
RPT_DL	rpt_dl[LMG_DL_ENTRY_COUNT + 1];

DTYPE rpt_dl_src[]={
	{DL_NAME},
	{DL_OCCURRENCE},
	{DL_ORIGIN},
	{DL_SET_CONNECTOR},
	{NULL,0,0,0,0}};

DTYPE rpt_dl_dest[]={
	{wrk_dl._e._name,0,0,BTRN,0},
	{(char*)&wrk_dl._e._occurrence,0,0,BONE,0},
	{(char*)&wrk_dl._e._origin,0,0,CINT,0},
	{(char*)&wrk_dl._set_connector,0,0,CCHR,0},
	{NULL,0,0,0,0}};

/*------
Because this is an array, it is processed into a work area first
and then copied to the actual element. Most of a nfo requires
alternate conversion depending on whether it is a lit or not.
------*/
RPT_NFO wrk_nfo;
DTYPE rpt_nfo_src[]={
	{NF_OP_CODE},
	{NF_NOT_LIT_CODE},
	{NULL,0,0,0,0}};

DTYPE rpt_nfo_dest[]={
	{(char*)&wrk_nfo._code,0,0,CCHR,0},
	{(char*)&wrk_nfo._not_lit,0,0,CCHR,0},
	{NULL,0,0,0,0}};

DTYPE rpt_nfo_lit_src[]={
	{NF_LIT_20},
	{NULL,0,0,0,0}};
DTYPE rpt_nfo_lit_dest[]={
	{wrk_nfo._e._name,0,0,CSTR,0},
	{NULL,0,0,0,0}};

DTYPE rpt_nfo_name_src[]={
	{NF_NAME},
	{NF_OP_OCCURRENCE},
	{NF_OP_ORIGIN},
	{NULL,0,0,0,0}};

DTYPE rpt_nfo_name_dest[]={
	{wrk_nfo._e._name,0,0,BTRN,0},
	{(char*)&wrk_nfo._e._occurrence,0,0,BONE,0},
	{(char*)&wrk_nfo._e._origin,0,0,CINT,0},
	{NULL,0,0,0,0}};

RPT_NF	wrk_nf;
RPT_NF	rpt_nf[NF_ENTRY_COUNT + 1];

DTYPE rpt_nf_src[]={
	{NF_NAME},
	{NF_TYPE},
	{NF_LEN},
	{NF_DEC},
	{NULL,0,0,0,0}};

DTYPE rpt_nf_dest[]={
	{wrk_nf._e._name,0,0,BTRN,0},
	{(char*)&wrk_nf._fld._type,0,0,BDTP,0},
	{(char*)&wrk_nf._fld._len,0,0,CINT,0},
	{(char*)&wrk_nf._fld._dec,0,0,CINT,0},
	{NULL,0,0,0,0}};

/*----
The input and output records.
------*/
char rpt_inp_rec1[2041];
char rpt_inp_rec2[2041];
char rpt_inp_rec[7080];
int  rpt_inp_rec_len;

int  rpt_key_to_sec_idx;

/*----
The sort
------*/

RPT_ASORT rpt_sort[8];

RPT_DEF rpt_def;

char rpt_bld_fld[132];

char *rpt_PRT_FILE;
char *rpt_PRT_LIB;
char *rpt_PRT_VOL;

char *rpt_caller = "R";
long rpt_total_records,rpt_record_count,rpt_sorted_records;

/*
**	History:
**	$Log: rglb.c,v $
**	Revision 1.8  2003/02/17 22:05:58  gsl
**	Fix ambiguous SORT reference
**	
**	Revision 1.7  2003/02/04 19:19:08  gsl
**	fix header
**	
**	Revision 1.6  2002/10/24 14:20:35  gsl
**	Make globals unique
**	
**	Revision 1.5  2002/10/23 21:07:25  gsl
**	make global name unique
**	
**	Revision 1.4  2002/10/17 17:17:21  gsl
**	Removed VAX VMS code
**	
**	Revision 1.3  2002/07/25 15:20:25  gsl
**	Globals
**	
**	Revision 1.2  1996/09/17 23:45:48  gsl
**	drcs update
**	
**
**
*/
