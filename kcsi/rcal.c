static char copyright[]="Copyright (c) 1988-1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
/*----
This is the report writer portion of REPORT.
On entry it is passed the information entered from the OPTIONS selection
of REPORT.
This module initializes internal structures from the passed COBOL information.
------*/
#include <stdio.h>
#include <stdarg.h>

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

static void debug_report_def(void);

/*----
Within a nf op, lits are translated differently from
named fields.
------*/
static void cvt_one_nfo(d,s,record)
DTYPE *d,*s;
char *record;
{
	cvt_record(d,s,record);
	if(KCSI_charat(record,NF_NOT_LIT_CODE) == 'X')
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
	if(KCSI_charat(record,DL_NOT_LIT_CODE) == 'X')
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
		rpt_rfl[i]._old = &rpt_old[i];
		rpt_rfl[i]._new = &rpt_new[i];
		if((KCSI_charat(record,RFL_NAME)) > ' ')
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
	if(KCSI_fieldeq(record,RFL_LINE,"X"))
		wrk_rfl._line = 3;
	else
	if(KCSI_fieldeq(record,RFL_LINE,"Y"))
		wrk_rfl._line = 2;
	else
	if(KCSI_fieldeq(record,RFL_LINE,"Z"))
		wrk_rfl._line = 1;
	else
		wrk_rfl._line = 0;
		
	if(KCSI_fieldeq(record,RFL_SIGN_CONTROL,"CR"))
		wrk_rfl._sign_control = 1;
	else
	if(KCSI_fieldeq(record,RFL_SIGN_CONTROL,"DB"))
		wrk_rfl._sign_control = 2;

	if(KCSI_fieldeq(record,RFL_DEC_CARRY," "))
		wrk_rfl._dec_carry = -1;
/*
 * If the count fields are blank, then the insert characters are set
 * to null
 */
	if(KCSI_fieldeq(record,RFL_INSERT_1_COUNT," "))
		wrk_rfl._insert_char[0] = 0;
	if(KCSI_fieldeq(record,RFL_INSERT_2_COUNT," "))
		wrk_rfl._insert_char[1] = 0;
	if(KCSI_fieldeq(record,RFL_INSERT_3_COUNT," "))
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
			memcpy(&wrk_dtype,&rpt_old[i],sizeof(wrk_dtype));
			cvt_record(d,s,record);
			memcpy(&rpt_old[i],&wrk_dtype,sizeof(wrk_dtype));
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
		if(KCSI_charat(record,NF_OP_NAME) > ' ')
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
		if(KCSI_charat(record,NF_NAME) > ' ')
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
		rpt_opt._option = KCSI_atoifield(record,RPT_OPTION);

	if(KCSI_fieldeq(record,RPT_DEVICE,"PRINTER "))
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
		wrk_rcb._action_code = KCSI_atoifield(record,RCB_ACTION_CODE);
		
}

static void cvt_rcb(d,s,record)
DTYPE *d,*s;
char *record;
{
	int i;

	for(i = 0; i< 5; ++i)
		{
		Clear(rpt_rcb[i]);
		if(KCSI_charat(record,RCB_NAME) > ' ')
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
		if(KCSI_charat(record,RCD_BREAK) > ' ')
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
		if(KCSI_charat(record,SRT_NAME) > ' ')
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
		if(KCSI_charat(record,DL_OP_NAME) > ' ')
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
		if(KCSI_charat(record,DL_NAME) > ' ')
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

/*  linker dummy routine */
	rpt_globals();
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
	memset(rpt_old,0,(sizeof(rpt_old[0]) * (RFL_ENTRY_COUNT + 1)));
	memset(rpt_new,0,(sizeof(rpt_new[0]) * (RFL_ENTRY_COUNT + 1)));
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
	rpt_key_to_sec_idx = kcsi_atoilen(kts,2);
	--rpt_key_to_sec_idx;
/* Tie up the loose pointers */
	rpt_tie();

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
	rpt_def._old = &rpt_old[0];
	rpt_def._new = &rpt_new[0];
	rpt_def._kfb1 = &kfb1;
	rpt_def._kfb2 = &kfb2;
	rpt_caller = caller;

    if (getenv("CRID_DEBUG_REPORT_DEF"))
    {
        debug_report_def();
    }

    KCSI_rptbld();
#ifdef PROFILE
	monitor((int(*)())0,0,0,0,0);
#endif
}
#define NN(s) ((s)? (s):("(NULL)"))

void crid_debug_print(const char* format, ...)
{
	va_list ap;
	FILE *logfile = fopen("crid_debug_print.log","a");

	va_start(ap, format);
	vfprintf(logfile, format, ap);
	va_end(ap);

	fclose(logfile);
}

void crid_debug_print_DTYPE(const char* name, DTYPE* dtype)
{
    if (dtype != NULL)
    {
        crid_debug_print(
            "%s->_pos =%d\n",  name, dtype->_pos);
        crid_debug_print(
            "%s->_len =%d\n",  name, dtype->_len);
        crid_debug_print(
            "%s->_type =%d\n", name, dtype->_type);
        crid_debug_print(
            "%s->_dec =%d\n",  name, dtype->_dec);
        crid_debug_print(
            "%s->_base =[%*.*s]\n",  name, dtype->_len, dtype->_len, dtype->_base);
    }
    else
    {
        crid_debug_print(
            "%s = (NULL)\n",  name);
    }
}

void crid_debug_print_ELEMENT(const char* name, ELEMENT* e)
{
    if (NULL == e)
    {
        crid_debug_print("%s = (NULL)\n", name);
        return;
    }

    crid_debug_print(
        "%s->_name       =[%.20s]\n",  name, NN(e->_name));
    crid_debug_print(
        "%s->_occurrence =%d\n",  name, e->_occurrence);
    crid_debug_print(
        "%s->_origin     =%d\n",  name, e->_origin);
}

void crid_debug_print_RPT_RFL(const char* name, RPT_RFL* rfl)
{
    char subname[100];

    crid_debug_print("RPT_RFL - Report Field\n");
    if (NULL == rfl)
    {
        crid_debug_print("%s = (NULL)\n", name);
        return;
    }
    sprintf(subname,"%s->_e",name);
    crid_debug_print_ELEMENT(subname, &rfl->_e);
    crid_debug_print(
        "%s->_line         =%d\n",  name, rfl->_line);
    crid_debug_print(
        "%s->_col_head     =[%.26s]\n",  name, NN(rfl->_col_head));
    crid_debug_print(
        "%s->_col_sub_head =[%.26s]\n",  name, NN(rfl->_col_sub_head));
    crid_debug_print(
        "%s->_ext_size     =%d\n",  name, rfl->_ext_size);
    crid_debug_print(
        "%s->_col          =%d\n",  name, rfl->_col);
}

void crid_debug_print_RPT_SRT(const char* name, RPT_SRT* srt)
{
    char subname[100];

    crid_debug_print("RPT_SRT - Report Sorts\n");
    if (NULL == srt)
    {
        crid_debug_print("%s = (NULL)\n", name);
        return;
    }

    sprintf(subname,"%s->_e",name);
    crid_debug_print_ELEMENT(subname, &srt->_e);
    crid_debug_print(
        "%s->_order          =%d\n",  name, srt->_order);
}

void crid_debug_print_RPT_NFO(const char* name, RPT_NFO* sp)
{
    char subname[100];

    crid_debug_print("RPT_NFO - Report New Field Operands\n");
    if (NULL == sp)
    {
        crid_debug_print("%s = (NULL)\n", name);
        return;
    }

    sprintf(subname,"%s->_e",name);
    crid_debug_print_ELEMENT(subname, &sp->_e);
    crid_debug_print(
        "%s->_code          =%d\n",  name, sp->_code);
    crid_debug_print(
        "%s->_not_lit       =%d\n",  name, sp->_not_lit);
    sprintf(subname,"%s->_lit_op",name);
    crid_debug_print_DTYPE(subname, &sp->_lit_op);

}

void crid_debug_print_RPT_NF(const char* name, RPT_NF* sp)
{
    char subname[100];
    int i;

    crid_debug_print("RPT_NF - Report New Fields\n");
    if (NULL == sp)
    {
        crid_debug_print("%s = (NULL)\n", name);
        return;
    }

    sprintf(subname,"%s->_e",name);
    crid_debug_print_ELEMENT(subname, &sp->_e);

    for(i=0; i< NF_OP_ENTRY_COUNT; i++)
    {
        sprintf(subname,"%s->_o[%d]",name, i);
        crid_debug_print_RPT_NFO(name, &sp->_o[i]);

        if ('\0' == sp->_o[i]._e._name[0])
        {
            break;
        }
    }

    sprintf(subname,"%s->_fld",name);
    crid_debug_print_DTYPE(subname, &sp->_fld);
}

void crid_debug_print_RPT_DLO(const char* name, RPT_DLO* sp)
{
    char subname[100];

    crid_debug_print("RPT_DLO - Report Data Limit Object\n");
    if (NULL == sp)
    {
        crid_debug_print("%s = (NULL)\n", name);
        return;
    }

    sprintf(subname,"%s->_e",name);
    crid_debug_print_ELEMENT(subname, &sp->_e);
    crid_debug_print(
        "%s->_code       =[%.3s]\n",  name, NN(sp->_code));
    crid_debug_print(
        "%s->_connector  =%d\n",  name, sp->_connector);
    crid_debug_print(
        "%s->_not_lit    =%d\n",  name, sp->_not_lit);
    if (NULL == sp->_pnew)
    {
        crid_debug_print(
            "%s->_pnew  = (NULL)\n",  name);
    }
    else
    {
        sprintf(subname,"%s->_pnew",name);
        crid_debug_print_DTYPE(subname, *sp->_pnew);
    }
    sprintf(subname,"%s->_lit_op",name);
    crid_debug_print_DTYPE(subname, &sp->_lit_op);
}

void crid_debug_print_RPT_DL(const char* name, RPT_DL* sp)
{
    char subname[100];
    int i;

    crid_debug_print("RPT_DL - Report Data Limits\n");
    if (NULL == sp)
    {
        crid_debug_print("%s = (NULL)\n", name);
        return;
    }

    sprintf(subname,"%s->_e",name);
    crid_debug_print_ELEMENT(subname, &sp->_e);
    if (NULL == sp->_pnew)
    {
        crid_debug_print(
            "%s->_pnew  = (NULL)\n",  name);
    }
    else
    {
        sprintf(subname,"%s->_pnew",name);
        crid_debug_print_DTYPE(subname, *sp->_pnew);
    }

    for(i=0; i< DL_OP_ENTRY_COUNT; i++)
    {
        sprintf(subname,"%s->_o[%d]",name, i);
        crid_debug_print_RPT_DLO(subname, &sp->_o[i]);

        if ('\0' == sp->_o[i]._e._name[0])
        {
            break;
        }
    }
    crid_debug_print(
        "%s->_set_connector  =%d\n",  name, sp->_set_connector);
}


void crid_debug_print_RPT_RCB(const char* name, RPT_RCB* rcb)
{
    char subname[100];

    crid_debug_print("RPT_RCB - Report Control breaks\n");
    if (NULL == rcb)
    {
        crid_debug_print("%s = (NULL)\n", name);
        return;
    }

    sprintf(subname,"%s->_e",name);
    crid_debug_print_ELEMENT(subname, &rcb->_e);

    crid_debug_print(
        "%s->_action_code =%d\n",  name, rcb->_action_code);
    crid_debug_print(
        "%s->_repeat_code =%d\n",  name, rcb->_repeat_code);
    crid_debug_print(
        "%s->_break       =[%.41s]\n",  name, NN(rcb->_break));
    crid_debug_print(
        "%s->_col         =%d\n",  name, rcb->_col);
    crid_debug_print(
        "%s->_rec_count   =%ld\n",  name, rcb->_rec_count);
    crid_debug_print(
        "%s->_brk_val     =[%.133s]\n",  name, NN(rcb->_brk_val));
    crid_debug_print(
        "%s->_is_breaking =%d\n",  name, rcb->_is_breaking);

    sprintf(subname,"%s->_rfl",name);
    crid_debug_print_RPT_RFL(subname, rcb->_rfl);
}

static void debug_report_def(void)
{
    char name[100];
    int i;

    crid_debug_print("---------------------\n");

    crid_debug_print("RPT_OPT - Report Options\n");
    crid_debug_print(
        "rpt_def._opt->_id        =[%.9s]\n",  rpt_def._opt->_id);
    crid_debug_print(
        "rpt_def._opt->_date      =[%.9s]\n",  rpt_def._opt->_date);
    crid_debug_print(
        "rpt_def._opt->_device    =%d\n",   rpt_def._opt->_device);
    crid_debug_print(
        "rpt_def._opt->_option    =%d\n",   rpt_def._opt->_option);
    crid_debug_print(
        "rpt_def._opt->_only      =%d\n",   rpt_def._opt->_only);
    crid_debug_print(
        "rpt_def._opt->_page      =%d\n",   rpt_def._opt->_page);
    crid_debug_print(
        "rpt_def._opt->_line[0]   =%d\n",   rpt_def._opt->_line[0]);
    crid_debug_print(
        "rpt_def._opt->_line[1]   =%d\n",   rpt_def._opt->_line[1]);
    crid_debug_print(
        "rpt_def._opt->_line[2]   =%d\n",   rpt_def._opt->_line[2]);
    crid_debug_print(
        "rpt_def._opt->_spacing[0]=%d\n",   rpt_def._opt->_spacing[0]);
    crid_debug_print(
        "rpt_def._opt->_spacing[1]=%d\n",   rpt_def._opt->_spacing[1]);
    crid_debug_print(
        "rpt_def._opt->_spacing[2]=%d\n",   rpt_def._opt->_spacing[2]);
    crid_debug_print(
        "rpt_def._opt->_ext_sort  =%d\n",   rpt_def._opt->_ext_sort);

    crid_debug_print("RPT_RDH - Report Header\n");
    crid_debug_print(
        "rpt_def._rhd->_sec_file  =%d\n",  rpt_def._rhd->_sec_file);
    crid_debug_print(
        "rpt_def._rhd->_sort      =%d\n",  rpt_def._rhd->_sort);
    crid_debug_print(
        "rpt_def._rhd->_data_limit=%d\n",  rpt_def._rhd->_data_limit);
    crid_debug_print(
        "rpt_def._rhd->_control_fields=%d\n",  rpt_def._rhd->_control_fields);
    crid_debug_print(
        "rpt_def._rhd->_column_subs   =%d\n",  rpt_def._rhd->_column_subs);
    crid_debug_print(
        "rpt_def._rhd->_key_to_sec    =[%.9s]\n",  rpt_def._rhd->_key_to_sec);
    crid_debug_print(
        "rpt_def._rhd->_occurrence    =%d\n",  rpt_def._rhd->_occurrence);
    crid_debug_print_DTYPE("rpt_def._rhd->_old_key1",rpt_def._rhd->_old_key1);
    crid_debug_print_DTYPE("rpt_def._rhd->_old_key2",rpt_def._rhd->_old_key2);

    for(i=0; i < 5 ; i++)
    {
        sprintf(name,"rpt_def._rcb[%d]",i);
        crid_debug_print_RPT_RCB(name,&rpt_def._rcb[i]);

        if ('\0' == rpt_def._rcb[i]._e._name[0])
        {
            break;
        }
    }

    /* Fields in the report */
    for(i=0; i<RFL_ENTRY_COUNT; i++)
    {
        sprintf(name,"rpt_def._rfl[%d]",i);
        crid_debug_print_RPT_RFL(name, &rpt_def._rfl[i]);

        if ('\0' == rpt_def._rfl[i]._e._name[0])
        {
            break;
        }
    }

    /* Sort critria */
    for(i=0; i < 8 ; i++)
    {
        sprintf(name,"rpt_def._srt[%d]",i);
        crid_debug_print_RPT_SRT(name,&rpt_def._srt[i]);

        if ('\0' == rpt_def._srt[i]._e._name[0])
        {
            break;
        }
    }

    /* Data Limits */
    for(i=0; i < LMG_DL_ENTRY_COUNT ; i++)
    {
        sprintf(name,"rpt_def._dl[%d]",i);
        crid_debug_print_RPT_DL(name,&rpt_def._dl[i]);

        if ('\0' == rpt_def._dl[i]._e._name[0] &&
            0    == rpt_def._dl[i]._e._occurrence )
        {
            break;
        }
    }

    /* New Fields */
    for(i=0; i < 10 ; i++)
    {
        sprintf(name,"rpt_def._nf[%d]",i);
        crid_debug_print_RPT_NF(name,&rpt_def._nf[i]);

        if ('\0' == rpt_def._nf[i]._e._name[0])
        {
            break;
        }
    }

}


/*
**	History:
**	$Log: rcal.c,v $
**	Revision 1.4.2.1  2002/11/12 15:56:31  gsl
**	Sync with $HEAD Combined KCSI 4.0.00
**	
**	Revision 1.9  2002/10/24 14:20:35  gsl
**	Make globals unique
**	
**	Revision 1.8  2002/10/23 21:07:25  gsl
**	make global name unique
**	
**	Revision 1.7  2002/10/23 20:39:07  gsl
**	make global name unique
**	
**	Revision 1.6  2002/10/17 17:17:20  gsl
**	Removed VAX VMS code
**	
**	Revision 1.5  2002/07/25 15:20:26  gsl
**	Globals
**	
**	Revision 1.4  2002/04/22 18:25:58  gsl
**	formating
**	
**	Revision 1.3  2000-06-13 18:02:23-04  gsl
**	add debug logging of the report definition.
**
**	Revision 1.2  1996-09-17 19:45:43-04  gsl
**	drcs update
**
**
**
*/
