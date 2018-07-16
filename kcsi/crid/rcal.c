static char copyright[]="Copyright (c) 1988-1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
/*----
This is the report writer portion of REPORT.
On entry it is passed the information entered from the OPTIONS selection
of REPORT.
This module initializes internal structures from the passed COBOL information.
------*/
#include <stdio.h>

/*
#define	PROFILE
*/

#ifdef PROFILE
#include <mon.h>
#endif
#include "dtype.h"
#include "rptprm.h"
#include "rptsrt.h"
#include "rptcob.h"
#include "rptglb.h"
#include "shrthand.h"
#include "kcsio.h"
#include "cobioblk.h"
#include "kcsifunc.h"

static char sccsid[]="@(#)rcal.c	1.13 8/14/93";

#ifdef PROFILE
extern etext;
#define	PROFILE_BUFSIZE	500000
#define	PROFILE_NFUNCS	600
static WORD profile_buf[PROFILE_BUFSIZE];
#endif

static KCSIO_BLOCK kfb1;
static KCSIO_BLOCK kfb2;

/*----
Within a nf op, lits are translated differently from
named fields.
------*/
static void cvt_one_nfo(d,s,record)
DTYPE *d,*s;
char *record;
{
	cvt_record(d,s,record);
	if(charat(record,NF_NOT_LIT_CODE) == 'X')
		cvt_record(rpt_nfo_name_dest,rpt_nfo_name_src,record);
	else
		cvt_record(rpt_nfo_lit_dest,rpt_nfo_lit_src,record);
}

/*----
Within a dl op, lits are translated differently from
named fields.
------*/
static void cvt_one_dlo(d,s,record)
DTYPE *d,*s;
char *record;
{
	cvt_record(d,s,record);
	if(charat(record,DL_NOT_LIT_CODE) == 'X')
		cvt_record(rpt_dlo_name_dest,rpt_dlo_name_src,record);
	else
		cvt_record(rpt_dlo_lit_dest,rpt_dlo_lit_src,record);
}

static void cvt_rfl(d,s,record)
DTYPE *d,*s;
char *record;
{
	int i;

	for(i = 0; i< RFL_ENTRY_COUNT; ++i)
		{
		Clear(rpt_rfl[i]);
		rpt_rfl[i]._old = &_old[i];
		rpt_rfl[i]._new = &_new[i];
		if((charat(record,RFL_NAME)) > ' ')
			{
			memcpy(&wrk_rfl,&rpt_rfl[i],sizeof(wrk_rfl));
			cvt_one_rfl(d,s,record);
			memcpy(&rpt_rfl[i],&wrk_rfl,sizeof(wrk_rfl));
			}
		record += RFL_ENTRY_LEN;
		}
}

void cvt_one_rfl(DTYPE *d,DTYPE *s,char *record)
{

	cvt_record(d,s,record);

/*
 * Line code, x y z converted to 3 2 1
 * or forced to zero if the sequence no indicates it is
 * an unused field.
 */
	if(wrk_rfl._lseq[0] > 40)
		wrk_rfl._line = 0;
	else
	if(fieldeq(record,RFL_LINE,"X"))
		wrk_rfl._line = 3;
	else
	if(fieldeq(record,RFL_LINE,"Y"))
		wrk_rfl._line = 2;
	else
	if(fieldeq(record,RFL_LINE,"Z"))
		wrk_rfl._line = 1;
	else
		wrk_rfl._line = 0;
		
	if(fieldeq(record,RFL_SIGN_CONTROL,"CR"))
		wrk_rfl._sign_control = 1;
	else
	if(fieldeq(record,RFL_SIGN_CONTROL,"DB"))
		wrk_rfl._sign_control = 2;

	if(fieldeq(record,RFL_DEC_CARRY," "))
		wrk_rfl._dec_carry = -1;
/*
 * If the count fields are blank, then the insert characters are set
 * to null
 */
	if(fieldeq(record,RFL_INSERT_1_COUNT," "))
		wrk_rfl._insert_char[0] = 0;
	if(fieldeq(record,RFL_INSERT_2_COUNT," "))
		wrk_rfl._insert_char[1] = 0;
	if(fieldeq(record,RFL_INSERT_3_COUNT," "))
		wrk_rfl._insert_char[2] = 0;

/*
 * FIelds with an origin of 3 are given a length of -1 which causes
 * them to be skipped at conversion time.
 */
	if(wrk_rfl._e._origin == 3)
		wrk_rfl._old->_len = -1;

}
/*----
The crf field conversions are done by testing for the existence of
a report field with an origin of 1 or 2 (3's are created within report).
When found, the corresponding crf record is assumed to be at the
same index entry.
------*/
static void cvt_crf(d,s,record)
DTYPE *d,*s;
char *record;
{
	int i;

	for(i = 0 ; i < RFL_ENTRY_COUNT ; ++i)
		{
		if((rpt_rfl[i]._e._origin == 1) || (rpt_rfl[i]._e._origin == 2))
			{
			memcpy(&wrk_rfl,&rpt_rfl[i],sizeof(wrk_rfl));
			memcpy(&wrk_dtype,&_old[i],sizeof(wrk_dtype));
			cvt_record(d,s,record);
			memcpy(&_old[i],&wrk_dtype,sizeof(wrk_dtype));
			memcpy(&rpt_rfl[i],&wrk_rfl,sizeof(wrk_rfl));
			}
		record += CRF_ENTRY_LEN;
		}
}

/*----
The new Fields
------*/

static void cvt_nfo(nfo,d,s,record)
RPT_NFO *nfo;
DTYPE *d,*s;
char *record;
{
	int i;

	for(i = 0; i < 10 ; ++i)
		{
		memset(nfo,0,sizeof(RPT_NFO));
		if(charat(record,NF_OP_NAME) > ' ')
			{
			memcpy(&wrk_nfo,nfo,sizeof(wrk_nfo));
			cvt_one_nfo(d,s,record);
			wrk_nfo._not_lit = (wrk_nfo._not_lit == 'X');
			memcpy(nfo,&wrk_nfo,sizeof(wrk_nfo));
			}
		record += NF_OP_ENTRY_LEN;
		++nfo;
		}
}

static void cvt_nf(d,s,record)
DTYPE *d,*s;
char *record;
{

	int i;
	for (i = 0; i < 10 ; ++i)
		{
		Clear(rpt_nf[i]);
		if(charat(record,NF_NAME) > ' ')
			{
			memcpy(&wrk_nf,&rpt_nf[i],sizeof(wrk_nf));
			cvt_record(d,s,record);
			wrk_nf._e._occurrence = 1;
			wrk_nf._e._origin = 3;
			if(wrk_nf._fld._type & IS_NUM)
				{
				wrk_nf._fld._type = BZON;
				wrk_nf._fld._len = BZON_LEN;
				}
			cvt_nfo(&wrk_nf._o[0],rpt_nfo_dest,rpt_nfo_src,
				record + NFO_ENTRY_POS);
			memcpy(&rpt_nf[i],&wrk_nf,sizeof(wrk_nf));
			}
		record += NF_ENTRY_LEN;
		}
}

/*----
Convert all values in the RPT-OPTIONS data item into a useable
C structure.
------*/
static void cvt_rpt_opt(d,s,record)
DTYPE *d,*s;
char *record;
{
	cvt_record(d,s,record);

/* Special Conversions */
	if(rpt_opt._option == -2)
		rpt_opt._option = atoifield(record,RPT_OPTION);

	if(fieldeq(record,RPT_DEVICE,"PRINTER "))
		rpt_opt._device = 1;
	else
		rpt_opt._device = 0;
}

/*----
Convert various flags from the header (RHD) record into true/false
switches.
------*/
static void cvt_rhd(d,s,record)
DTYPE *d,*s;
char *record;
{
	cvt_record(d,s,record);
}
/*----
Pull the name of the key to the secondary file and its
occurrence number.
------*/
static void cvt_rdf(d,s,record)
DTYPE *d,*s;
char *record;
{
	cvt_record(d,s,record);
}
/*----
Control Breaks
------*/
static void cvt_one_rcb(d,s,record)
DTYPE *d,*s;
char *record;
{
	cvt_record(d,s,record);
	if(wrk_rcb._action_code == -2)
		wrk_rcb._action_code = atoifield(record,RCB_ACTION_CODE);
		
}

static void cvt_rcb(d,s,record)
DTYPE *d,*s;
char *record;
{
	int i;

	for(i = 0; i< 5; ++i)
		{
		Clear(rpt_rcb[i]);
		if(charat(record,RCB_NAME) > ' ')
			{
			memcpy(&wrk_rcb,&rpt_rcb[i],sizeof(wrk_rcb));
			cvt_one_rcb(d,s,record);
			memcpy(&rpt_rcb[i],&wrk_rcb,sizeof(wrk_rcb));
			}
		record += RCB_ENTRY_LEN;
		}
}

static void cvt_rcd(d,s,record)
DTYPE *d,*s;
char *record;
{
	int i;

	for(i = 0; i< 5; ++i)
		{
		if(charat(record,RCD_BREAK) > ' ')
			{
			memcpy(&wrk_rcb,&rpt_rcb[i],sizeof(wrk_rcb));
			cvt_record(d,s,record);
			memcpy(&rpt_rcb[i],&wrk_rcb,sizeof(wrk_rcb));
			}
		record += RCD_ENTRY_LEN;
		}
}

static void cvt_rtt(d,s,record)
DTYPE *d,*s;
char *record;
{
	int i;

	for(i = 0; i< 6; ++i)
		{
		Clear(rpt_rtt[i]);
		memcpy(&wrk_rtt,&rpt_rtt[i],sizeof(wrk_rtt));
		cvt_record(d,s,record);
		memcpy(&rpt_rtt[i],&wrk_rtt,sizeof(wrk_rtt));
		record += RTT_ENTRY_LEN;
		}
}
/*----
Force an origin of 1 for sorts.
------*/
static void cvt_srt(d,s,record)
DTYPE *d,*s;
char *record;
{
	int i;

	for(i = 0; i< 8; ++i)
		{
		Clear(rpt_srt[i]);
		if(charat(record,SRT_NAME) > ' ')
			{
			memcpy(&wrk_srt,&rpt_srt[i],sizeof(wrk_srt));
			cvt_record(d,s,record);
			wrk_srt._e._origin = 1;
			memcpy(&rpt_srt[i],&wrk_srt,sizeof(wrk_srt));
			}
		record += SRT_ENTRY_LEN;
		}
}

static void cvt_dlo(dlo,d,s,record)
RPT_DLO *dlo;
DTYPE *d,*s;
char *record;
{
	int i;

	for(i = 0; i < DLO_ENTRY_COUNT ; ++i)
		{
		memset(dlo,0,sizeof(RPT_DLO));
		if(charat(record,DL_OP_NAME) > ' ')
			{
			memcpy(&wrk_dlo,dlo,sizeof(wrk_dlo));
			cvt_one_dlo(d,s,record);
			wrk_dlo._not_lit = (wrk_dlo._not_lit == 'X');
			memcpy(dlo,&wrk_dlo,sizeof(wrk_dlo));
			}
		record += DL_OP_ENTRY_LEN;
		++dlo;
		}
}

static void cvt_dl(d,s,record)
DTYPE *d,*s;
char *record;
{
	int i;
	for (i = 0; i < LMG_DL_ENTRY_COUNT ; ++i)
		{
		Clear(rpt_dl[i]);
		if(charat(record,DL_NAME) > ' ')
			{
			memcpy(&wrk_dl,&rpt_dl[i],sizeof(wrk_dl));
			cvt_record(d,s,record);
			cvt_dlo(&wrk_dl._o[0],rpt_dlo_dest,
				rpt_dlo_src,record + DLO_ENTRY_POS);
			memcpy(&rpt_dl[i],&wrk_dl,sizeof(wrk_dl));
			}
		record += DL_ENTRY_LEN;
		}
}

/*----
The actual report writer. Access from cobol as

	CALL "KCSIRPTW" USING
		RPT-OPTIONS		(WSRPTOPT)
		RHD-RECORD		(WSRPTREC)
		RDF-RECORD		(WSRPTREC)
		RCB-ENTRIES		(WSRTPREC)
		RCD-RECORDS		(WSRPTREC)
		RFL_RECORDS		(WSRPTREC)
		CRF-RECORDS		(RCCTRLF)
		RTT-RECORDS		(WSRPTREC)
		ST_SORTS		(WSRPTREC)
		DL-FIELDS		(WSRPTREC)
		NF-FIELDS		(WSRPTREC)
		INPUT1-IO-BLOCK		(LKDIO)		Assumed to be open
		INPUT2-IO-BLOCK		(LKDIO)		Assumed to be open
		PRINTER-FILE
		PRINTER-LIBRARY
		PRINTER-VOLUME
		THIS_CALLER		("R" for report "I" for Inquiry "S" spinoff)
to be continued.
CRF-RECORDS and RFL-RECORDS must appear in the same order.
------*/
/*----
1.  Unpack all the records from there COBOL equivalents.
2.  Tie the elements and pointers together.
------*/
void RPTCALL(cob_rpt_opt,cob_rpt_rhd,cob_rpt_rdf,
	 cob_rpt_rcb,cob_rpt_rcd,cob_rpt_rfl,
	 cob_rpt_crf,cob_rpt_rtt,cob_rpt_srt,
	 cob_rpt_dl,cob_rpt_nf,
	 cob_input1,cob_input2,
	 pf,pl,pv,caller,kts)
char 	*cob_rpt_opt,
	*cob_rpt_rhd,
	*cob_rpt_rdf,
	*cob_rpt_rcb,
	*cob_rpt_rcd,
	*cob_rpt_rfl,
	*cob_rpt_crf,
	*cob_rpt_rtt,
	*cob_rpt_srt,
	*cob_rpt_dl,
	*cob_rpt_nf,
	*cob_input1,
	*cob_input2,
	*pf,*pl,*pv,*caller,*kts;
{

#ifdef PROFILE
	monitor((int(*)())2,&etext,profile_buf,PROFILE_BUFSIZE,PROFILE_NFUNCS);
#endif

/* Vax linker dummy routine */
	vax_rpt_globals();
/*
 * Make the print file library and volume global.
 */
	rpt_PRT_FILE = pf;
	rpt_PRT_LIB = pl;
	rpt_PRT_VOL = pv;
/*
 * The actual report definition records And the control equivalents
 * are critical to the operation to extract the positions and lengths
 * of the items.
 */
	memset(_old,0,(sizeof(_old[0]) * (RFL_ENTRY_COUNT + 1)));
	memset(_new,0,(sizeof(_new[0]) * (RFL_ENTRY_COUNT + 1)));
	cvt_rfl(rpt_rfl_dest,rpt_rfl_src,cob_rpt_rfl);
	cvt_crf(rpt_crf_dest,rpt_crf_src,cob_rpt_crf);
/*New Fields are converted next to get the positions*/
	cvt_nf(rpt_nf_dest,rpt_nf_src,cob_rpt_nf);
/* Options */
	cvt_rpt_opt(rpt_opt_dest,rpt_opt_src,cob_rpt_opt);
/* This is a temporary fix to prevent the WISPSORT() from being used */
/*	rpt_opt._ext_sort = 1; */
/* Header (RHD) record */
	cvt_rhd(rpt_rhd_dest,rpt_rhd_src,cob_rpt_rhd);
/* Data files record (RDF) */
	cvt_rdf(rpt_rdf_dest,rpt_rdf_src,cob_rpt_rdf);
/* Control Breaking Logic (RCB and RCD records) */
	cvt_rcb(rpt_rcb_dest,rpt_rcb_src,cob_rpt_rcb);
	cvt_rcd(rpt_rcd_dest,rpt_rcd_src,cob_rpt_rcd);
/* The report titles */
	cvt_rtt(rpt_rtt_dest,rpt_rtt_src,cob_rpt_rtt);
/* The sorts */	
	cvt_srt(rpt_srt_dest,rpt_srt_src,cob_rpt_srt);
/* Data Limits*/
	cvt_dl(rpt_dl_dest,rpt_dl_src,cob_rpt_dl);
/* Index to the key to the secondary file */
	rpt_key_to_sec_idx = atoilen(kts,2);
	--rpt_key_to_sec_idx;
/* Tie up the loose pointers */
	tie_rpt();

/*
 * The file io data which travels as part of the system io block
 * may be out of alignment because of COBOL's ability to operate
 * with unaligned data. To force correct alignment, it must be
 * copied out to local version of the io structure.
 */
	memcpy(&kfb1,&cob_input1[SYS_IO_BLOCK_POS],sizeof(KCSIO_BLOCK));
	memcpy(&kfb2,&cob_input2[SYS_IO_BLOCK_POS],sizeof(KCSIO_BLOCK));
/*
 * Save the organization of the second io block. This is used by
 * Inquiry to indicate that an output extract file is to become
 * consecutive.
 */
	set_inq_org(cob_input2[ORG_POS]);


/* Set up pointers to all items and pass them in one structure to the next
 * module.
 */

	rpt_def._opt = &rpt_opt;
	rpt_def._rhd = &rpt_rhd;
	rpt_def._rcb = &rpt_rcb[0];
	rpt_def._rfl = &rpt_rfl[0];
	rpt_def._rtt = &rpt_rtt[0];
	rpt_def._srt = &rpt_srt[0];
	rpt_def._dl  = &rpt_dl[0];
	rpt_def._nf  = &rpt_nf[0];
	rpt_def._old = &_old[0];
	rpt_def._new = &_new[0];
	rpt_def._kfb1 = &kfb1;
	rpt_def._kfb2 = &kfb2;
	rpt_caller = caller;
	rptbld(&rpt_def);
#ifdef PROFILE
	monitor((int(*)())0,0,0,0,0);
#endif
}

/*
**	History:
**	$Log: rcal.c,v $
**	Revision 1.2  1996-09-17 19:45:43-04  gsl
**	drcs update
**
**
**
*/
