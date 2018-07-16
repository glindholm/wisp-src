static char copyright[]="Copyright (c) 1988-1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
/*-----
This routine receives the first record ready to be formatted for printing.
In order to preserve the WANG GETPARM etc logic, the actual printing is done
by passing the formatted record to a WANG COBOL program that opens
and prints.
------*/
/*----
Mods:
 Feb 27, 1992
	Fix in printing titles.
 Feb 27 1990
	Fixed bug in rpt_write() do-loop causing display_progress to be
	called even when the progress flag was not set.
 Mar 30, 1990
	Changed the close prt file logic to keep and close for display
	purposes.
------*/
#include <stdio.h>
#include <ctype.h>

#include "dtype.h"
#include "rptprm.h"
#include "rptsrt.h"
#include "rptglb.h"
#include "iocode.h"
#include "kcsio.h"
#include "shrthand.h"
#include "cridebug.h"
#include "shrthand.h"
#include "rlmg.h"
#include "kcsifunc.h"

#define	PRINT_LEN	132
#define	PRINT_MAX	(PRINT_LEN + 1)

static char sccsid[]="@(#)rwrt.c	1.20 12/5/93";
/*----
Lots of space for all the record types.
-------*/
static char no_records[PRINT_MAX]="No records printed.";
static char hdr_1[PRINT_MAX],hdr_2[PRINT_MAX],hdr_3[PRINT_MAX];
static char *hdr[]={hdr_1,hdr_2,hdr_3};
static char sub_1[PRINT_MAX],sub_2[PRINT_MAX],sub_3[PRINT_MAX];
static char *sub[]={sub_1,sub_2,sub_3};
static char detail_1[PRINT_MAX],detail_2[PRINT_MAX],detail_3[PRINT_MAX];
static char *detail[]={detail_1,detail_2,detail_3};

static char run_line[PRINT_MAX];
static char blank_line[PRINT_MAX];
static char title_line[PRINT_MAX];
static char break_1[PRINT_MAX],break_2[PRINT_MAX],break_3[PRINT_MAX];
static char *break_line[]={break_1,break_2,break_3};

static char total_line[PRINT_MAX] = "    Total value of Fields";
static char max_line[PRINT_MAX]   = "    Maximum value of Fields";
static char min_line[PRINT_MAX]   = "    Minimum value of Fields";
static char avg_line[PRINT_MAX]   = "    Average value of Fields";
static char rec_line[PRINT_MAX]   = "    Number of records selected = ";
static char prt_line[PRINT_MAX]   = "";


/*----
Record counters, line counters etc.
------*/
static int line_count;
static int page_no;
static int report_width;
static long recs[6];		/*for each level*/
static long trecs;              /* total recs */
static int last_record;

/*----
Codes for passing to the COBOL printer
------*/
static char open_code[] = "OO";
static char ff_code[] = "FF";
static char wr_code[] = "WW";
static char close_code[] = "CC";
static char disp_code[] = "DD";
static char keep_code[] = "KK"; 		/*   29-Mar-1990*/


/*----
The form_feed option
------*/

#define	KCSI_REPORTFF_KW	"kcsi_reportff"

static int kcsi_reportff=1;


/*----
Non-int functions
------*/

char	*cpsize(),*format_numeric(),*cpxsize(),
	*zero_suppress(),*format_a_field();
double	get_real_value();

static RPT_RFL *next_rfl(int line, int* seq);

/*----
Shorthand
------*/
#define	FIELD_ADDRESS(x)	(x->_new->_base + x->_new->_pos)
#define	ITS_NUMERIC(x)		(x->_new->_type & IS_NUM)

static void clear_lines(l)
char **l;
{
	int j;

	TRACE_IN("clear_lines");
	for(j = 0; j < 3 ; ++j)
		{
		memset(*l,' ',PRINT_LEN);
		(*l)[PRINT_LEN] = 0;
		++l;
		}
	TRACE_OUT("clear_lines");
}
static cmpfield(d,rfl)
char *d;
RPT_RFL *rfl;
{
	int rc;
	TRACE_IN("cmpfield");
	rc = (memcmp(d,FIELD_ADDRESS(rfl),rfl->_new->_len));
	TRACE_OUT("cmpfield");
	return(rc);
}

static void io_to_rptpln(code,record)
char *code;
char *record;
{
	TRACE_IN("io_to_rptpln");
	rptpln(code,record,rpt_PRT_FILE,rpt_PRT_LIB,rpt_PRT_VOL);
	TRACE_OUT("io_to_rptpln");

}
/*----
Setting up an io function that can be initialized from the
outside prevents numerous listmgmt routines from having to
linked into report functions.
------*/

static void lmg_dummy_output(char *io,char *rec)
{
	kcsitrace(4,"LISTMGMT","OUTPUT", "Attempted LISTMGMT output when not set");
}

static void (*lmg_output)(char *,char *) = lmg_dummy_output;

static void io_to_lmg(io,rec)
char *io,*rec;
{
	TRACE_IN("io_to_lmg");
	(*lmg_output)(io,rec);
	TRACE_OUT("io_to_lmg");
}

static void print_a_line(s)
char *s;
{
	TRACE_IN("print_a_line");
	strfil(s);
	if(*rpt_caller > '9')
		io_to_rptpln(PRINT_LINE,s);
	else
		io_to_lmg(END_RECORD,blank_line);
	++line_count;
	TRACE_OUT("print_a_line");
}

static void linefeed()
{
	TRACE_IN("linefeed");
	print_a_line(blank_line);
	TRACE_OUT("linefeed");
}

static void linefeeds(count)
int count;
{
	TRACE_IN("linefeeds");
	while(count--)
		linefeed();
	TRACE_OUT("linefeeds");
}

static void formfeed()
{
	TRACE_IN("formfeed");
	io_to_rptpln(FORM_FEED,blank_line);
	line_count = 0;
	TRACE_OUT("formfeed");

}

/*----
Check for level breaks working from highest to lowest.
If a break occurs at a higher one then all lower ones break
as well.
------*/
static void check_for_breaks()
{
	int i,breaking;
	RPT_RCB *rcb;
	RPT_RFL *rfl;

	TRACE_IN("check_for_breaks");
	for(i=4, rcb = &rpt_rcb[4], breaking = 0; i >= 0; --i, --rcb)
		{
		if(rfl = rcb->_rfl)
			{
/* If the count Option only is specified then all breaking is suppressed*/
			if(rpt_record_count == 1)
				{
				/*cpyfield(rcb->_brk_val,rfl);*/
				rcb->_is_breaking = rfl->_is_breaking = 0;
				}
			else
/* 
 * If this level is breaking then throw the breaking switch so that lower
 * levels break.
 */
			if((cmpfield(rcb->_brk_val,rfl)) || (breaking))
				{
				rcb->_is_breaking = rfl->_is_breaking = 1;
				/*cpyfield(rcb->_brk_val,rfl);*/
				breaking = 1;
				}
			else
/* Otherwise we're not breaking */
				rcb->_is_breaking = rfl->_is_breaking = 0;
			}
		}
	TRACE_OUT("check_for_breaks");
}
static void cpyfield(d,rfl)
char *d;
RPT_RFL *rfl;
{
	TRACE_IN("cpyfield");
	memcpy(d,FIELD_ADDRESS(rfl),rfl->_new->_len);
	TRACE_OUT("cpyfield");
}

/*----
set level breaks working from highest to lowest.
------*/
static void set_new_breaks()
{
	int idx,breaking;
	RPT_RCB *rcb;
	RPT_RFL *rfl;

	TRACE_IN("set_new_breaks");
	for(idx = 4, rcb = &rpt_rcb[4], breaking = 0; idx >= 0; --idx, --rcb)
		{
		if(rfl = rcb->_rfl)
			{
			cpyfield(rcb->_brk_val,rfl);
			}
		}
	TRACE_OUT("set_new_breaks");
}

/*----
Process all fields. Look for level breaks and totaling.
------*/
static void format_one_field(rfl,d,seq)
RPT_RFL *rfl;
char *d;
int seq;
{

	TRACE_IN("format_one_field");
	if(ITS_NUMERIC(rfl))
		rfl->_value = get_real_value(FIELD_ADDRESS(rfl), 
				rfl->_new->_dec,rfl->_dec_carry);
	else
		rfl->_value = 0;
	if((seq = rfl->_lseq[0]) < 80)
		{
		if((!rfl->_is_a_break) ||
		   (rfl->_is_breaking) || 
		   (rfl->_repeat_code) ||
                   (trecs == 1))
			{
			if(*rpt_caller > '9')
				format_a_field(rfl,d + rfl->_col,0);
			else
				{
				format_a_field(rfl,d,0);
				io_to_lmg(WRITE_FIELD,d);
				}
			}
		}
	TRACE_OUT("format_one_field");
}


/*----
Format all the fields in one line. If no fields were found, then
zero out the line marker.
------*/
static void format_one_line(dtl,line)
char *dtl;
int line;
{
	int seq;
	RPT_RFL *rfl;

	TRACE_IN("format_one_line");
	seq = 0;

	while(rfl = next_rfl(line+1,&seq))
		{
		format_one_field(rfl,dtl,seq);
		}
/* Zero out any empty line */
	if(!seq)
		{
		for(seq = 0; seq < 3; ++seq)
			{
			if(rpt_opt._line[seq] == (line + 1))
				rpt_opt._line[seq] = 0;
			}
		}
	TRACE_OUT("format_one_line");
}

static void format_lines()
{
	int i;

	TRACE_IN("format_lines");
	clear_lines(detail);
	for(i=0;i<3;++i)
		{
		format_one_line(detail[i],i);
		strunc(detail[i]);
		}
	TRACE_OUT("format_lines");
}

static void set_one_rfl_value(rfl)
RPT_RFL *rfl;
{
	TRACE_IN("set_one_rfl_value");
	if((rfl->_total) || (rfl->_avg))
		rfl->_val[0]._total += rfl->_value;

	if(rfl->_max)
		rfl->_val[0]._max = max(rfl->_value , rfl->_val[0]._max);

	if(rfl->_min)
		rfl->_val[0]._min = min(rfl->_value, rfl->_val[0]._min);
	TRACE_OUT("set_one_rfl_value");
}

static void set_rfl_values()
{
	RPT_RFL *rfl;
	int idx;

	TRACE_IN("set_rfl_values");
	for( idx = 0 , rfl = &rpt_rfl[0] ;
             (rfl->_e._name[0]) && (idx < RFL_ENTRY_COUNT);
	     ++idx, ++rfl)
		{
		if(ITS_NUMERIC(rfl))
			set_one_rfl_value(rfl);
		}
	TRACE_OUT("set_rfl_values");
	
}

/*----
Modified to adjust for edited decimals
------*/
double get_real_value(address,dec,ddec)
char *address;
int dec,ddec;
{
	double fl;
	char format[101];
	char result[101];

	TRACE_IN("get_real_value");
	fl = 0;

	sscanf(address,"%16lf",&fl);
	if(ddec == -1)
		{
		TRACE_OUT("get_real_value");
		return(fl);
		}
	while(dec--)
		fl /= 10;
	if(ddec == dec)
		{
		TRACE_OUT("get_real_value");
		return(fl);
		}
	/* Force rounding with the new decimal values */
	sprintf(format,"%%+0%d.%dlf",17 - ddec,ddec);
	sprintf(result,format,fl);
	sscanf(result,"%17lf",&fl);
	TRACE_OUT("get_real_value");
	return(fl);
}

static there_is_a_break()
{
	int i,rc;
	RPT_RCB *rcb;

	TRACE_IN("there_is_a_break");
	rc = 0;
	for(i = 0,rcb = &rpt_rcb[0];i<RCB_ENTRY_COUNT;++i,++rcb)
		{
		if(rcb->_is_breaking)
			{
			rc = 1;
			break;
			}
		}
	TRACE_OUT("there_is_a_break");
	return(rc);
}

static void rollup(val1)
SUM *val1;
{
	SUM *val2;

	TRACE_IN("rollup");
	val2 = val1;
	++val2;

	val2->_total += val1->_total;

	val2->_min = min( val1->_min, val2->_min);

	val2->_max = max( val1->_max, val2->_max);

	TRACE_OUT("rollup");
}

static void earlier_breaks(idx)
int idx;
{
	RPT_RCB *rcb;
	RPT_RFL *rfl,wrfl;
	DTYPE dtp;

	if(!rpt_opt._only)
		return;
	for(;idx < RCB_ENTRY_COUNT; ++idx)
		{
		if(!(rpt_rcb[idx]._rfl))
			break;
		rcb = &rpt_rcb[idx];
		rfl = rcb->_rfl;
/*
 * Make a working copy of the rfl and DTYPE structures and
 * tie them together.
 */
		memcpy(&wrfl,rfl,sizeof(wrfl));
		memcpy(&dtp,rfl->_new,sizeof(dtp));
		wrfl._new = &dtp;
		wrfl._new->_base = rcb->_brk_val;
		wrfl._new->_pos = 0;
		format_a_field(&wrfl,&break_line[rfl->_line - 1][rfl->_col],0);
		}
}

static void build_a_break(rfl,level,code)
RPT_RFL *rfl;
int level;
{
	RPT_RFL wrfl;
	SUM *val;
	DTYPE dtp;

	char wrk_f[17];
	double value;
	int dec;

	TRACE_IN("build_a_break");

	memset(&dtp,0,sizeof(DTYPE));
/*
 * Make a working copy of the rfl and DTYPE structures and
 * tie them together.
 */
	memcpy(&wrfl,rfl,sizeof(wrfl));
	memcpy(&dtp,rfl->_new,sizeof(dtp));
	wrfl._new = &dtp;
	wrfl._new->_base = wrk_f;
	wrfl._new->_pos = 0;
/*
 * If we are not at the left hand edge of the page then make the
 * field 1 column wider for totaling.
 */

	if(rfl->_col)
		{
		++wrfl._ext_size;
		--wrfl._col;
		}
/*
 * Although all flags can be set, only one is honored at a time.
 */
	val = &rfl->_val[level];
	if(code == 1)
		value = val->_total;
	else
	if(code == 2)
		value = val->_max;
	else
	if(code == 3)
		value = val->_min;
	else
	if(code == 4)
		value = val->_total / recs[level];
/*
 * the value is un-decimalized and formatted
 */
	dec = rfl->_new->_dec;
	while(dec--)
		value *= 10;	
	sprintf(wrk_f,"%+016.0f",value);
	format_a_field(&wrfl,&break_line[rfl->_line - 1][wrfl._col],0);
	if(code == 1)
		earlier_breaks(level);
	TRACE_OUT("build_a_break");
}



static void init_breaks(rcb)
RPT_RCB *rcb;
{
	int i;

	TRACE_IN("init_breaks");
	for(i = 0; i < RCB_ENTRY_COUNT ; ++i)
		{
		strcpy(rcb->_brk_val,"");
		rcb->_rec_count = 0;
		if(rcb->_e._name[0])
			memcpy(rcb->_brk_val,
				&inp_rec[(*rcb->_pnew)->_pos],
				(*rcb->_pnew)->_len);
		++rcb;
		}
	TRACE_OUT("init_breaks");
}

static void init_a_sum(val)
SUM *val;
{
	TRACE_IN("init_a_sum");
	val->_total = 0;
	val->_max = 0;
	val->_min = 999999999999999.0;
	TRACE_OUT("init_a_sum");
}

static void init_one_sum(val)
SUM *val;
{
	int i;
	TRACE_IN("init_one_sum");
	for(i = 0; i < 6 ; ++i)
		{
		init_a_sum(val);
		++val;
		}
	TRACE_OUT("init_one_sum");

}
static void init_sums(rfl)
RPT_RFL *rfl;
{
	int i;

	TRACE_IN("init_sums");
	for(i = 0 ; i < RFL_ENTRY_COUNT ; ++rfl, ++i)
		{
		if(rfl->_e._name[0] <= ' ')
			continue;
		init_one_sum(rfl->_val);
		}
	TRACE_OUT("init_sums");
}



static void bld_one_legend(hptr,sptr,i)
char *hptr, *sptr;
int i;
{
	int seq,column;
	RPT_RFL *rfl;
	int w1,w2,cwidth;

	TRACE_IN("bld_one_legend");
	column = seq = 0;

	while(rfl = next_rfl(i+1,&seq))
		{
		if(seq > 97)
			break;
/*
 * The column starts after skipping spaces.
 */
		column += rfl->_spaces;
		w1 = strlen(rfl->_col_head);
		w2 = strlen(rfl->_col_sub_head);
/*
 * Column width is the larger of header, subheader or external size
 */
		cwidth = max(w1,w2);
		cwidth = max(cwidth,rfl->_ext_size);

/*
 * And if the width is going to exceed 132 then bail out (some sort of
 * error in the report definition file).
 */
		if( (column + cwidth) > PRINT_LEN)
			break;

/*
 * Now copy in the header and sub header
 */

		cpsize(hptr + column,rfl->_col_head,w1);
		cpsize(sptr + column,rfl->_col_sub_head,w2);
/*
 * If the column width is 2 or more wider than the size of the data (header or
 * sub header is wider than the data, then we do a little centering
 * of the data under the headings.
 */
		w1 = cwidth;
		w2 = column;
		while ( (w1 - rfl->_ext_size) > 2)
			{
			++w2;
			w1 -= 2;
			}
		rfl->_col = w2;


		column += cwidth;
		}
	report_width = max (column, report_width);
	TRACE_OUT("bld_one_legend");
}

/*----
bld_legends actually does two jobs, establishing the location of the
data on the report as well as the column headings
------*/
static void bld_legends()
{
	int i;

	TRACE_IN("bld_legends");
	clear_lines(hdr);
	clear_lines(sub);
	for(i=0;i<3;++i)
		{
		bld_one_legend(hdr[i],sub[i],i);
		strunc(hdr[i]);
		strunc(sub[i]);
		}

	if(report_width > 80)
		report_width = PRINT_LEN;
	else
		report_width = 80;
	TRACE_OUT("bld_legends");
}


/*----
Returns the next rfl whose line equals the passed line, and whose
sequence is the next number higher that the passed sequence.
------*/
static RPT_RFL *next_rfl_rpt(line,seq)
int line;
int *seq;
{
	RPT_RFL *rfl,*r;
	int i;

	TRACE_IN("next_rfl_rpt");
	r = NULL;
	for(i=0,rfl = &rpt_rfl[0];(i<RFL_ENTRY_COUNT) &&(rfl->_new);++i,++rfl)
		{
		if((rfl->_line == line) && (rfl->_lseq[0] > *seq))
			{
			if(r == NULL)
				r = rfl;
			else
			if(rfl->_lseq[0] < r->_lseq[0])
				r = rfl;
			}
		}
	TRACE_OUT("next_rfl_rpt");
	if(r)
		*seq = r->_lseq[0];
	return(r);
}

static RPT_RFL *next_rfl_lmg(line,seq)
int line,*seq;
{
	RPT_RFL *rfl,*r;
	int idx,jdx,gseq;

	TRACE_IN("next_rfl_lmg");
	r = NULL;
	gseq = 0;
	for(idx=0,rfl = &rpt_rfl[0];
		(idx<RFL_ENTRY_COUNT) &&(rfl->_new);
			++idx,++rfl)
		{
		for(jdx = 0; jdx < 4 ; ++jdx)
			{
			if((rfl->_line == line) && (rfl->_lseq[jdx] > *seq))
				{
				if(r == NULL)
					{
					r = rfl;
					gseq = rfl->_lseq[jdx];
					}
				else
				if(rfl->_lseq[jdx] < gseq)
					{
					r = rfl;
					gseq = rfl->_lseq[jdx];
					}
				}
			}
		}
	TRACE_OUT("next_rfl_lmg");
	if(r)
		*seq = gseq;
	return(r);
}

static RPT_RFL *next_rfl(int line, int* seq)
{
	if(*rpt_caller > '9')
		return(next_rfl_rpt(line,seq));
	else
		return(next_rfl_lmg(line,seq));
}
/*----
Run line is date and page numbering.

99/99/99                  Page: 0001
------*/
static void print_run_line(atitle)
char *atitle;
{
	char format[20];

	TRACE_IN("print_run_line");
	/*sprintf(format,"%%-%dsPage: %%04d",report_width - 12);*/
/*
	sprintf(format,"%%-%dsPage: %%04d",115);
*/
	sprintf(format,"%%-%dsPage: %%04d",114);
/*
	sprintf(run_line,format,rpt_opt._date,page_no);
*/
	sprintf(run_line,
		"%-8s%-28s%-60s%-27s%-5s%04d",
		rpt_opt._date,
		" ",
		atitle,
		" ",
		"Page:",
		page_no);

	print_a_line(run_line);
	TRACE_OUT("print_run_line");
	
}

static print_a_line_if(s)
char *s;
{
	int rc;
	TRACE_IN("print_a_line_if");
	rc = 0;
	if(*s)
		{
		print_a_line(s);
		rc = 1;
		}
	TRACE_OUT("print_a_line_if");
	return(rc);
}

static void print_a_title(title)
char *title;
{
	char wtitle[133];
	TRACE_IN("print_a_title");
/*
	strcpy(wtitle,title);
*/
	sprintf(wtitle,"%36s%-60s"," ",title);
/*
	strcenter(title_line,title,report_width);
*/
	print_a_line_if(wtitle);
	TRACE_OUT("print_a_title");
}

static void print_titles()
{
	int i;
	TRACE_IN("print_titles");
	print_run_line(rpt_rtt[0]._title);
	for(i = 1; i < 3 ; ++i)
		{
		print_a_title(rpt_rtt[i]._title);
		}
	linefeed();
	TRACE_OUT("print_titles");
}

static void print_headers()
{
	int i;
	TRACE_IN("print_headers");
	if(rpt_rtt[3]._title[0])
		{
		print_run_line(rpt_rtt[3]._title);
		for(i = 4; i < 6 ; ++i)
			{
			print_a_title(rpt_rtt[i]._title);
			}
		linefeed();
		}
	else
		print_titles();
	TRACE_OUT("print_headers");
}

/*----
Legends are printed in _line order
------*/
static void print_legends()
{
	int idx,line,lf;

	TRACE_IN("print_legends");
	for(idx = 0;idx < 3;++idx)
		{
		line = rpt_opt._line[idx] - 1;
		if((line > -1) && (line < 3))
			{
			lf = print_a_line_if(hdr[line]);
			print_a_line_if(sub[line]);
			if(lf)
				linefeeds(rpt_opt._spacing[idx]);
			}
		}
	linefeed();
	TRACE_OUT("print_legends");
}

static void start_a_page()
{
	TRACE_IN("start_a_page");
	++page_no;

	if(	(page_no > 1)	||
		(kcsi_reportff > 0)	)
		formfeed();
	else
		line_count = 0;
/*
	print_run_line();
*/
	if(page_no == 1)
		print_titles();
	else
		print_headers();

	print_legends();
	TRACE_OUT("start_a_page");
}

static void start_new_page()
{
	TRACE_IN("start_new_page");
	if(*rpt_caller < ':')		/* If Called from list mgmt */
		{
		line_count = 0;		/* Dont start a page */
		}
	else
		{
		start_a_page();
		}
	TRACE_OUT("start_new_page");
}

/*----
Center dest in src assuming dest has length of dlen + 1;
If src is empty then dest is made empty.
------*/
/*
static strcenter(dest,src,dlen)
char *dest,*src;
int dlen;
{
	int slen,center;

	slen = strlen(src);

	memset(dest,0,dlen);
	if(slen)
		{
		center = (dlen - slen) / 2;
		memset(dest,' ',center);
		strcat(dest,src);
		}
}
*/
static void print_one_line(s)
char *s;
{
	TRACE_IN("print_one_line");
	if(line_count > rpt_opt._page)
		start_new_page();
	print_a_line(s);
	TRACE_OUT("print_one_line");
}

/*----
Lines are printed in _line order
------*/
static void print_lines()
{
	int idx,line;

	TRACE_IN("print_lines");
	if(rpt_opt._only)
		{
		TRACE_OUT("print_lines");
		return;
		}
	for(idx = 0;idx < 3;++idx)
		{
		line = rpt_opt._line[idx] - 1;
		if((line > -1) && (line < 3))
			{
			print_one_line(detail[line]);
			linefeeds(rpt_opt._spacing[idx]);
			}
		}
	TRACE_OUT("print_lines");
}

static print_one_line_if(s)
char *s;
{
	int rc;
	TRACE_IN("print_one_line_if");
	rc = 0;
	if(*s)
		{
		print_one_line(s);
		rc = 1;
		}
	TRACE_OUT("print_one_line_if");
	return(rc);
}

static void open_prt_file()
{
	TRACE_IN("open_prt_file");
	memcpy(rpt_PRT_FILE,"##REPO  ",8);
	if(*rpt_caller > '9')
		io_to_rptpln(OPEN_PRINTER_FILE,blank_line);
	else
		io_to_lmg(OPEN_PRINTER_FILE,blank_line);
	TRACE_OUT("open_prt_file");
}

static void close_prt_file()
{
	TRACE_IN("close_prt_file");
	if(*rpt_caller > '9')
		io_to_rptpln(CLOSE_PRINTER_FILE,blank_line);
	else
		io_to_lmg(CLOSE_PRINTER_FILE,blank_line);
	TRACE_OUT("close_prt_file");
}

/*   29-Mar-1990*/
static void keep_prt_file()
{
	TRACE_IN("keep_prt_file");
	io_to_rptpln(CLOSE_AND_KEEP,blank_line);
	TRACE_OUT("keep_prt_file");

}

static void display_prt_file()
{
	TRACE_IN("display_prt_file");
	io_to_rptpln(DISPLAY_FILE,blank_line);
	TRACE_OUT("display_prt_file");
}


void set_lmg_output_func(void (*func)(char*,char*))
{
	if(func)
		lmg_output = func;
}

static void lmg_dummy_count(long count)
{
	kcsitrace(4,"LISTMGMT","COUNT", "Attempted to log LISTMGMT count when not set.");
}

static void (*lmg_log_count)(long) = lmg_dummy_count;

void set_lmg_count_func(func)
void (*func)(long);
{
	if(func)
		lmg_log_count = func;
}

void call_lmg_log_count(long count)
{
	(*lmg_log_count)(count);
}

/*----
End summary is a report level level break. ie Totals averages etc for
all records processed.
------*/
static void end_summary(code,idx)
int code,idx;
{
	int jdx,line,bld;
	RPT_RFL *rfl;

	TRACE_IN("end_summary");
	clear_lines(break_line);
	bld = 0;
/* For all fields that have been tagged with a summary option */
	for( jdx = 0,rfl = &rpt_rfl[0]; 
 	     (jdx < RFL_ENTRY_COUNT) && (rfl->_e._name[0] > ' ');
	     ++jdx,++rfl)
		{
		if(rfl->_lseq[0] > 97)
			continue;
		if( ((rfl->_total) && (code == 1)) ||
		    ((rfl->_max)   && (code == 2)) ||
		    ((rfl->_min)   && (code == 3)) ||
		    ((rfl->_avg)   && (code == 4)) )
			{
			build_a_break(rfl,idx,code);
			bld = 1;
			}
		}
/* All fields except Totals have a title line printed first */
	if(bld)
		{
		switch(code)
			{
			case 1:
/*				print_one_line(total_line);	*/
				break;
			case 2:
				print_one_line(max_line);
				break;
			case 3:
				print_one_line(min_line);
				break;
			case 4:
				print_one_line(avg_line);
				break;
			}

/* Break lines are printed in _line order */
		for(jdx = 0; jdx < 3; ++jdx)
			{
			line = rpt_opt._line[jdx] - 1;
			if((line > -1) && (line < 3))
				{
				strunc(break_line[line]);
				print_one_line_if(break_line[line]);
				}
			}
		}
	TRACE_OUT("end_summary");
}


/*----
If any count option was specified than an end of report trailer
is printed.
------*/
static void end_report()
{
	RPT_RCB *rcb;
	int code,idx;

	TRACE_IN("end_report");
/* 
 * When a break occurs, each level is summed to the next higher level,
 * therefore the report totals are at the first level above the highest break
 * This will be level 0 if there are no breaking fields or 6 if all levels
 * are used. The summary values structure carries carries six levels
 * of summary, while control breaks are only 5 levels deep. The unused level
 * 6 carries grand totals when all 5 levels are used.
 */
	for(idx = 0, rcb = &rpt_rcb[0]; idx < RCB_ENTRY_COUNT; ++idx, ++rcb)
		{
		if(!rcb->_rfl)
			break;
		}
	for(code = 1; code < 5; ++code)
		end_summary(code,idx);

/* Record count totals appear for and _option type flag */
	if(rpt_opt._option != 0)
		{
		sprintf(prt_line,"%s %ld",rec_line,rpt_record_count);
		print_one_line(prt_line);
		}
	TRACE_OUT("end_report");
}
/*----
Roll up values to the next higher level and print.
Except on the last record. Roll up but do not print.
------*/

static void end_one_level(rcb,level)
RPT_RCB *rcb;
int level;
{
	int jdx,line,code;
	RPT_RFL *rfl;
	SUM *val;

	TRACE_IN("end_one_level");
	clear_lines(break_line);
	memcpy(&break_line[0][rcb->_col],rcb->_break,strlen(rcb->_break));
/* For all report fields */
	for( jdx = 0,rfl = &rpt_rfl[0]; 
	     (jdx < RFL_ENTRY_COUNT) && (rfl->_e._name[0] > ' ');
	     ++jdx,++rfl)
		{
		code = 0;
		if(rfl->_total)
			code = 1;
/*
Apparently in Wang REPORT only totals are printed at level breaks
Max Min and Avg only appear for the report grand totals
		else
		if(rfl->_max)
			code = 2;
		else
		if(rfl->_min)
			code = 3;
		else
		if(rfl->_avg)
			code = 4;
*/
/*
 * All values at this level are rolled up  to the next higher
 * level, used to build the break, and then cleared.
 */
		if(code)
			{
			val = &rfl->_val[level];
			rollup(val);
			recs[level + 1] += recs[level];
			build_a_break(rfl,level,code);
			init_a_sum(val);
			recs[level] = 0;

			}
		}
/*
	if(!last_record)
		{
		for(jdx = 0;jdx < 3;++jdx)
			{
			line = rpt_opt._line[jdx] - 1;
			if((line > -1) && (line < 3))
				{
				strunc(break_line[line]);
				print_one_line_if(break_line[line]);
				}
			}
		}
*/
/* Modified so that it will control break on last record */
	for(jdx = 0;jdx < 3;++jdx)
		{
		line = rpt_opt._line[jdx] - 1;
		if((line > -1) && (line < 3))
			{
			strunc(break_line[line]);
			print_one_line_if(break_line[line]);
			}
		}

/*Modified to control break on last record.*/

	if(rcb->_action_code == -1)
		start_new_page();
	else
		linefeeds(rcb->_action_code);
	TRACE_OUT("end_one_level");
}


/*----
Print breaks from lowest to highest levels.
------*/

static void end_all_levels()
{
	RPT_RCB *rcb;
	int level;

	TRACE_IN("end_all_levels");
	if(*rpt_caller > '9')
		{
		/* linefeed(); */
		for(	level = 0,	rcb  = &rpt_rcb[0];
			level<RCB_ENTRY_COUNT; 
			++level,	++rcb)
			{
			if((rcb->_rfl))
				{
				end_one_level(rcb,level);
				}
			}
		}	
	TRACE_OUT("end_all_levels");
}

static void end_old_levels()
{
	RPT_RCB *rcb;
	int level;

	TRACE_IN("end_old_levels");
	/* linefeed(); */
	for(level=0,rcb  = &rpt_rcb[0];level < RCB_ENTRY_COUNT; ++level,++rcb)
		{
		if((rcb->_is_breaking) && (rcb->_rfl))
			end_one_level(rcb,level);
		}
	TRACE_OUT("end_old_levels");
}


static void print_one_record()
{
	TRACE_IN("print_one_record");
	if(line_count > rpt_opt._page)
		start_new_page();
	
	check_for_breaks();
	format_lines();
	if(there_is_a_break())
		{
		end_old_levels();
		}
	set_new_breaks();
	print_lines();
	set_rfl_values();
	TRACE_OUT("print_one_record");
}


/*----
Do some initialization and start printing.
------*/
void rpt_write(int code)
{
	int i;
#ifdef	KCSI_UNIX
	char *reportff;
#endif

	TRACE_IN("rpt_write");
#ifdef	KCSI_UNIX
	reportff = getenv(KCSI_REPORTFF_KW);
	if(reportff)
		{
		if(*reportff == '0')
			kcsi_reportff = 0;
		}

#endif
	init_breaks(rpt_def._rcb);
	init_sums(rpt_def._rfl);
	report_width = 0;
	for(i = 0 ; i < 6; ++i)
		recs[i] = 0;
	bld_legends();

	line_count = 999;
	last_record = page_no = 0;
	trecs = 0;

	open_prt_file();
	do{
		if(code == 0)
			{
			print_one_line(no_records);
			break;
			}
		++rpt_record_count;
		++trecs;
		++recs[0];
		print_one_record();
		if((rpt_opt._option > 0) && 
			(rpt_record_count >= rpt_opt._option))
			break;
	}while(read_next_rpt_record() == 0 );

	last_record = 1;

	end_all_levels();
	end_report();
/*   29-Mar-1990*/
	if((rpt_opt._device != 1) || (*rpt_caller == 'I'))
		{
		keep_prt_file();
		display_prt_file();
		}
	else
		close_prt_file();
	if(*rpt_caller < 'A')
		call_lmg_log_count(rpt_record_count);
	TRACE_OUT("rpt_write");

}


/*
**	History:
**	$Log: rwrt.c,v $
**	Revision 1.5  2002/04/22 15:53:10  gsl
**	Change crid_error_trace() toe kcsitrace()
**	
**	Revision 1.4  1997-10-02 15:38:15-04  gsl
**	fix warnings
**
**	Revision 1.3  1997-10-02 09:46:52-04  gsl
**	Fix warnings
**
**	Revision 1.2  1996-09-17 19:45:52-04  gsl
**	drcs update
**
**
**
*/
